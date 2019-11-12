module Timer where 

import Control.Concurrent.STM 
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)

data State = Start | Stop
type Timer = (TVar State, TMVar ())

liftTimer :: MonadIO m => Int -> m Timer 
liftTimer n = liftIO $ timer n 

liftWaitTimer :: MonadIO m => Timer -> m () 
liftWaitTimer t = liftIO $ waitTimer t

waitTimer :: Timer -> IO ()
waitTimer (_, timer) = atomically $ readTMVar timer

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
