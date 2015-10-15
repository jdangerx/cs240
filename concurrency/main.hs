{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Typeable

-- exceptions

data MyError = MyError String deriving (Show, Typeable)

instance Exception MyError

catcher :: IO a -> IO (Maybe a)
catcher action = fmap Just action `catch` handler
  where handler (MyError msg) = do putStrLn msg; return Nothing

pureCatcher :: a -> IO (Maybe a)
pureCatcher a =
  (a `seq` return (Just a))
  `catch` \(SomeException _) -> return Nothing

seqList :: [a] -> b -> b
seqList (a:as) b = a `seq` (as `seqList` b)
seqList [] b = b

-- concurrency!

wait :: IO ()
wait =
  do
    tid <- myThreadId
    threadDelay 1000000
    mask (\_ -> print $ show tid ++ " waited for a second")

waitInParallel :: IO ()
waitInParallel =
  do
    forkIO wait
    wait

data TimedOut = TimedOut deriving (Eq, Show, Typeable)
instance Exception TimedOut

timeout :: Int -> IO a -> IO (Maybe a)
timeout usec action =
  do
    ptid <- myThreadId
    let timer = threadDelay usec >> throwTo ptid TimedOut
        parent =
          do timerId <- forkIO timer
             result <- action
             killThread timerId
             return $ Just result
    catchJust
      (\e -> if e == TimedOut then Just e else Nothing)
      parent
      (const $ return Nothing)


timeout' :: Int -> IO a -> IO (Maybe a)
timeout' usec action =
  do
    ptid <- myThreadId
    let timer = do
          threadDelay usec
          throwTo ptid TimedOut
        parent = mask $ \unmask ->
          do timerId <- forkIO timer
             result <- unmask action
             killThread timerId
             return $ Just result
    catchJust
      (\e -> if e == TimedOut then Just e else Nothing)
      parent
      (const $ return Nothing)

timeout'' :: Int -> IO a -> IO (Maybe a)
timeout'' usec action =
  do
    ptid <- myThreadId
    let timer = threadDelay usec >> throwTo ptid TimedOut
        parent =
          bracket (forkIO timer) killThread (const $ Just <$> action)
    catchJust
      (\e -> if e == TimedOut then Just e else Nothing)
      parent
      (const $ return Nothing)

claimMVar :: MVar String -> IO ()
claimMVar m =
  do
    tid <- myThreadId
    oldtid <- takeMVar m
    putMVar m $ show tid
    print $ show tid ++ " has claimed the variable from " ++ oldtid ++ "!"
    return ()

mvarWar :: IO ()
mvarWar =
  do
    m <- newEmptyMVar
    tid <- myThreadId
    putMVar m $ show tid
    forkIO $ claimMVar m
    threadDelay 500000
    forkIO $ claimMVar m
    threadDelay 500000
    void . forkIO $ claimMVar m

pingpong :: Bool -> Int -> IO()
pingpong doPrint n' = --doPrint lets you use criterion
  do
    mvc <- newEmptyMVar
    mvp <- newEmptyMVar
    let parent n | n > 0 = when doPrint (putStr $ " " ++ show n)
                           >> putMVar mvc n -- waits until dec takes the value before putting another in
                           >> takeMVar mvp -- waits for dec to write new value
                           >>= parent
                 | otherwise = return ()
        decrement = takeMVar mvc -- wait until parent writes to it to take value
                    >>= \n ->
                         when doPrint (print $ "decrementing " ++ show n)
                         >> putMVar mvp (n - 1)
                    >> decrement
    tid <- forkIO decrement
    parent n' `finally` killThread tid

type Mutex = MVar ThreadId

mutexCreate :: IO Mutex
mutexCreate = newEmptyMVar

mutexLock, mutexUnlock :: Mutex -> IO ()
mutexLock m = myThreadId >>= putMVar m
mutexUnlock m =
  do
    lockId <- tryTakeMVar m
    myId <- myThreadId
    unless (lockId == Just myId) $ error "cannot unlock mutex"

data Cond = Cond (MVar [MVar ()])

condCreate :: IO Cond
condCreate = liftM Cond $ newMVar []

condWait :: Mutex -> Cond -> IO ()
condWait m (Cond waiters) =
  do
    me <- newEmptyMVar
    modifyMVar_ waiters $ \others -> return $ others ++ [me]
    mutexUnlock m -- if m is locked by someone else, throw error, else we are ok
    takeMVar me `finally` mutexLock m -- sleep until something is put in `me`

condSignal, condBroadcast :: Cond -> IO ()
condSignal (Cond waiters) =
  modifyMVar_ waiters wakeOne
  where
    wakeOne [] = return []
    wakeOne (w:ws) = putMVar w () >> return ws -- pop one off the waiters queue
condBroadcast (Cond waiters) =
  modifyMVar_ waiters wakeAll
  where
    wakeAll ws = mapM_ (`putMVar` ()) ws >> return []

-- channels

data Item a = Item a (Stream a)
type Stream a = MVar (Item a)
data Chan' a = Chan' (MVar (Stream a)) (MVar (Stream a))
-- sort of a linked list of MVars

newChan' :: IO (Chan' a)
newChan' =
  newEmptyMVar >>= \emptyMVar ->
  Chan' <$> newMVar emptyMVar <*> newMVar emptyMVar
  -- both head and tail point at same empty box

writeChan' :: Chan' a -> a -> IO ()
writeChan' (Chan' _ w) a =
  do
    empty <- newEmptyMVar
    modifyMVar_ w $
      \oldEmpty -> putMVar oldEmpty (Item a empty) >> return empty
    -- old tail box now points to a box that holds the new value and
    -- points at new tail box


main :: IO ()
main = return ()

