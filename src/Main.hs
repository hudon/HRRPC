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
--import           System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import           Data.Maybe
import qualified Control.Exception as E

type ProcHandles = (IO.Handle, IO.Handle, IO.Handle, ProcessHandle)

main :: IO ()
main = do
  putStrLn "Starting Rrpc server..."
  r@(hin, hout, _, _) <-
    runInteractiveProcess "R" ["--slave", "--vanilla"] Nothing
      $ Just [ ("R_PROFILE", "rpc_server/server.r")
            , ("R_SERVER_SOURCE", "rpc_server/some_script.r") ]
  --hPutStr hin "3"
  --hFlush hin
  --hGetLines hout >>= print
  --BS.hGetNonBlocking hout 4098 >>= print
  quickHttpServe $ site r


hGetLines :: IO.Handle -> IO [String]
hGetLines hin = do
  input <- E.try $ IO.hGetLine hin
  case input of
    Left e ->
      if isEOFError e
        then return []
        else E.ioError e
    Right line -> do
      lines <- hGetLines hin
      return (line:lines)


site :: ProcHandles  -> Snap ()
site rProc =
    ifTop (serveDirectory "static") <|>
    route [ ("send-cmd", cmdHandler rProc)
          ] <|>
    dir "static" (serveDirectory "static")

-- TODO: restart R proc on fail

-- Call an R function with the given param and return verbatim response
-- Only 0-255 of Unicode is supported (Char8)
cmdHandler :: ProcHandles -> Snap ()
cmdHandler (rin, rout, rerr, _) = do
  cmd <- getParam "cmd"
  res <- liftIO $ do
    let c = C.append (fromJust cmd) (C.pack "\n")
    -- Send cmd to R proc and get result
    putStrLn $ C.unpack c
    IO.hPutStr rin $ C.unpack c
    IO.hFlush rin
    getResults rout rerr
  writeBS $ C.pack res
  where
    -- If stdout has nothing, returns what's in stderr...
    getResults :: IO.Handle -> IO.Handle -> IO String
    getResults hout herr = do
      o <- hGetLines rout
      case o of
        [] -> hGetLines herr >>= return . unlines
        xs -> return $ unlines xs
