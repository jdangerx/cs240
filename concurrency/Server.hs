module Server where

import Control.Concurrent
import Control.Monad
import qualified Data.Set as S
import System.IO

import Network.Socket

type Msg = String
type UserID = String

main :: IO ()
main = do
  chan <- newChan
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 9876 iNADDR_ANY)
  listen sock 2
  mainLoop chan sock 0

mainLoop :: Chan (Int, Msg) -> Socket -> Int -> IO ()
mainLoop chan sock usernum = do
  accept sock >>= forkIO . perClient chan usernum
  mainLoop chan sock $ usernum+1

ioLoop :: Handle -> (Handle -> Msg -> IO ()) -> IO ()
ioLoop hdl processMsg = do
  msg <- hGetLine hdl
  processMsg hdl msg
  if msg == "q\r"
    then hPutStrLn hdl "Bye!" >> hClose hdl
    else ioLoop hdl processMsg

perClient :: Chan (Int, Msg) -> Int -> (Socket, SockAddr) -> IO ()
perClient chan usernum (sock, _) = do
  writeChan chan (-1, "User #" ++ show usernum ++ "has joined")
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  hPutStrLn hdl $ "You are #" ++ show usernum
  chan' <- dupChan chan
  forkIO $ forever $
    readChan chan' >>=
     (\(num, m) -> unless (num == usernum) $ hPutStrLn hdl (show num ++ ": " ++ m))
  ioLoop hdl (\_ m -> writeChan chan (usernum, m))
  writeChan chan (-1, "User #" ++ show usernum ++ "has left")
