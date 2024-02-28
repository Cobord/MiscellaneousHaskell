{-# LANGUAGE GADTs #-}
import Data.Time ( getCurrentTime, UTCTime )
import Control.AutoUpdate
    ( defaultUpdateSettings,
      mkAutoUpdate,
      UpdateSettings(updateFreq, updateAction) )
import Control.Monad (forM_)

class Clocked a where
    doNow :: UTCTime -> IO a

data MyClocked where
  MyClocked :: UTCTime -> MyClocked
instance Show MyClocked where
    show (MyClocked x) = show x
instance Clocked MyClocked where
    doNow = pure . MyClocked

main :: IO ()
main = do
    getTime <- mkAutoUpdate defaultUpdateSettings
                { updateAction = getCurrentTime>>=doNow
                , updateFreq = 1000 -- once every millisecond
                }
    forM_ [0..10] (\i -> do
                        currentTime <- getTime
                        print $ "Current value of time, possibly not fully up to date: " ++ show (currentTime :: MyClocked)
                    )