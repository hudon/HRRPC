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
import           Data.Aeson (ToJSON, encode, (.=), object, toJSON)
import           Data.Text (Text)
import           Data.ByteString.Lazy (toStrict)
import           Data.ByteString.UTF8 (fromString, toString)

type ProcHandles = (IO.Handle, IO.Handle, IO.Handle, ProcessHandle)

data RPC = RPC
  { func :: Text
  , params :: [Text]
  } deriving Show

instance ToJSON RPC where
  toJSON (RPC f ps) = object ["method" .= f, "params" .= ps]

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
    route [ ("R-rand-vec", randVecHandler rProc)
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


-- If stdout has nothing, returns what's in stderr...
-- We're assuming the R RPC proc returns 1 line of results.
getResults :: IO.Handle -> IO.Handle -> IO String
getResults hout herr = do
  o <- E.try $ IO.hGetLine hout
  case o of
    Left e -> if isEOFError e then hGetLines herr >>= return . unlines else E.ioError e
    Right line -> return line


randVecHandler :: ProcHandles -> Snap ()
randVecHandler r = liftIO (rRPC r) >>= writeBS . fromString

-- aeson encodes to a lazy bytestring, so we make it strict and use
-- utf8-string's from and to String functions
rRPC :: ProcHandles -> IO String
rRPC (hin, hout, herr, _) = do
  IO.hPutStrLn hin . toString . toStrict $ encode (RPC {func = "fib", params = ["12"]})
  IO.hFlush hin
  getResults hout herr
