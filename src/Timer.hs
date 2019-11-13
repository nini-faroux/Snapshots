module Timer (
    liftTimer 
  , liftWaitTimer
  , liftFork
  , liftReadMVar
  , liftDelay
  ) where 

import Control.Concurrent.STM (TMVar, TVar, newEmptyTMVar, newTVar, readTMVar, putTMVar, readTVar, atomically)
import Control.Concurrent     (MVar, newEmptyMVar, readMVar, putMVar, forkIO, threadDelay, forkFinally)
import Control.Monad.IO.Class (MonadIO, liftIO)

data State = Start | Stop
type Timer = (TVar State, TMVar ())

liftFork :: MonadIO m => IO () -> m (MVar ()) 
liftFork action = liftIO $ forkThread action 

forkThread :: IO () -> IO (MVar ())
forkThread action = do
    lock <- newEmptyMVar
    _ <- forkFinally action (\_ -> putMVar lock ())
    return lock

liftReadMVar :: MonadIO m => MVar a -> m a 
liftReadMVar var = liftIO $ readMVar var 

liftTimer :: MonadIO m => Int -> m Timer 
liftTimer n = liftIO $ timer n 

liftWaitTimer :: MonadIO m => Timer -> m () 
liftWaitTimer t = liftIO $ waitTimer t

waitTimer :: Timer -> IO ()
waitTimer (_, timer) = atomically $ readTMVar timer

liftDelay :: MonadIO m => Int -> m ()
liftDelay n = liftIO $ threadDelay n

timer :: Int -> IO Timer
timer n = do
    state <- atomically $ newTVar Start
    timer <- atomically newEmptyTMVar
    forkIO $ do
        threadDelay n
        atomically $ do
            state' <- readTVar state
            case state' of
                Start -> putTMVar timer ()
                Stop  -> return ()
    return (state, timer)
