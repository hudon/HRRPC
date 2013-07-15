{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import           Control.Monad
import           Control.Monad.IO.Class
import           System.Process (runInteractiveProcess, ProcessHandle)
import qualified System.IO as IO
import           System.IO.Error (isEOFError)
import qualified Control.Exception as E
import           Data.Aeson (FromJSON(..), (.:), ToJSON(..), encode, (.=), object, Value(..), decode)
import           Data.Aeson.Types (emptyArray)
import           Data.Text (Text, unpack, pack)
import           Data.ByteString.Lazy (toStrict, fromStrict, ByteString)
import           Data.ByteString.UTF8 (fromString, toString)
import           Data.Maybe (fromJust)

type ProcHandles = (IO.Handle, IO.Handle, IO.Handle, ProcessHandle)

-- Remote Procedure Call
data RPC = RPC
  { func :: Text
  , params :: Value
  } deriving Show

-- Remote Procedure Result
data RPR = RPR
  { version :: Text
  , result :: Value
  }

instance ToJSON RPC where
  toJSON (RPC f ps) = object ["method" .= f, "params" .= ps]

instance FromJSON RPR where
  parseJSON (Object v) = RPR <$>
                         v .: "HRRPC" <*>
                         v .: "result"
  parseJSON _          = mzero

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
          , ("R-fib", fibHandler rProc)
          , ("R-df", dfHandler rProc)
          ] <|>
    dir "static" (serveDirectory "static")

randVecHandler :: ProcHandles -> Snap ()
randVecHandler r = rRPCHandler r "user_func" emptyArray

fibHandler :: ProcHandles -> Snap ()
fibHandler r = do
  n <- getParam "fib-n"
  rRPCHandler r "fib" (bsToNumValue n)
  where
    -- TODO handle Nothing
    bsToNumValue = toJSON . (read :: String -> Double) . toString . fromJust

dfHandler :: ProcHandles -> Snap ()
dfHandler r = rRPCHandler r "SomeDF" emptyArray


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


-- TODO pull the result out of the JSON

-- TODO handle Nothing
toRPR :: String -> RPR
toRPR = fromJust . decode . toLazyBS

rRPCHandler :: ProcHandles -> Text -> Value -> Snap()
rRPCHandler r f p = liftIO (rRPC r f p) >>= writeLBS . getResult
  where
    getResult :: String -> ByteString
    getResult = encode . result . toRPR

-- aeson encodes to a lazy bytestring, so we make it strict and use
-- utf8-string's from and to String functions
rRPC :: ProcHandles -> Text -> Value -> IO String
rRPC (hin, hout, herr, _) f p = do
  IO.hPutStrLn hin . toStrictString $ encode (RPC {func=f, params=p})
  IO.hFlush hin
  getRPCResult hout herr

toStrictString :: ByteString -> String
toStrictString = toString . toStrict

toLazyBS :: String -> ByteString
toLazyBS = fromStrict . fromString
