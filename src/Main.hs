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
import qualified Control.Exception as E
import           Data.ByteString (ByteString)
import           Data.ByteString.UTF8 (fromString, toString)

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
    route [("R", rpcHandler rProc)
          ] <|>
    dir "static" (serveDirectory "static")

rpcHandler :: ProcHandles -> Snap ()
rpcHandler r = do
  modifyResponse $ setContentType "application/json"
  -- TODO handle Nothing
  Just bs <- getParam "rpc"
  liftIO (rRPC r bs) >>= writeBS . fromString

rRPC :: ProcHandles -> ByteString -> IO String
rRPC (hin, hout, herr, _) bs = do
  IO.hPutStrLn hin . toString $ bs
  IO.hFlush hin
  getRPCResult hout herr

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

-- If stdout has nothing, returns what's in stderr...
-- We're assuming the R RPC proc returns 1 line of results.
getRPCResult :: IO.Handle -> IO.Handle -> IO String
getRPCResult hout herr = do
  o <- E.try $ IO.hGetLine hout
  case o of
    Left e -> if isEOFError e then hGetLines herr >>= return . unlines else E.ioError e
    Right line -> return line

