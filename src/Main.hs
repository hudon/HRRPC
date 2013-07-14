{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import           Control.Monad.IO.Class
import           System.Process (runInteractiveProcess, ProcessHandle)
import qualified System.IO as IO
import           System.IO.Error (isEOFError)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe
import qualified Control.Exception as E

type ProcHandles = (IO.Handle, IO.Handle, IO.Handle, ProcessHandle)

main :: IO ()
main = do
  putStrLn "Starting Rrpc server..."
  r <- runInteractiveProcess "R" ["--slave", "--silent"] Nothing
        $ Just [ ("R_PROFILE", "rpc_server/server.r")
               , ("R_SERVER_SOURCE", "rpc_server/some_script.r") ]
  quickHttpServe $ site r


site :: ProcHandles  -> Snap ()
site rProc =
    ifTop (serveDirectory "static") <|>
    route [ ("send-cmd", cmdHandler rProc)
          ] <|>
    dir "static" (serveDirectory "static")


-- If input is EOF, return []. Otherwise, return all input lines
hGetLines :: IO.Handle -> IO [String]
hGetLines hin = do
  input <- E.try $ IO.hGetLine hin
  case input of
    Left e ->
      if isEOFError e
        then return []
        else E.ioError e
    Right line -> do
      ls <- hGetLines hin
      return (line:ls)


-- TODO: restart R proc on fail

-- Call an R function with the given param and return verbatim response
-- Only 0-255 of Unicode is supported (Char8)
cmdHandler :: ProcHandles -> Snap ()
cmdHandler (rin, rout, rerr, _) = do
  cmd <- getParam "cmd"
  res <- liftIO $ do
    let c = C.append (fromJust cmd) (C.pack "\n")
    -- Send cmd to R proc and get result
    IO.hPutStr rin $ C.unpack c
    IO.hFlush rin
    getResults rout rerr
  writeBS $ C.pack res
  where
    -- If stdout has nothing, returns what's in stderr...
    -- We're assuming the R RPC proc returns 1 line of results.
    getResults :: IO.Handle -> IO.Handle -> IO String
    getResults hout herr = do
      o <- E.try $ IO.hGetLine hout
      case o of
        Left e -> if isEOFError e then hGetLines herr >>= return . unlines else E.ioError e
        Right line -> return line

