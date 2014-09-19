{-# OPTIONS_GHC -O2 -fno-warn-hi-shadowing -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Lorenz.Loop
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : MIT
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- The main loop of the Lorenz program.
-------------------------------------------------------------------------------


module Lorenz.Loop
( mainLoop
) where

import Lorenz.Data
import Lorenz.Events
import Lorenz.Logic(update)
import Lorenz.OpenGL(draw)
import Graphics.UI.SDL.Time
import Data.Word(Word32)




-- | Maximum frames per second.
maxFPS :: Word32
maxFPS = 60




-- | Main loop of Lorenz.
mainLoop :: App -> IO ()
mainLoop app = 
    handleEvents app                >>=
    handleMouseMovements            >>=
    update                          >>=
    (\x -> draw x >> return x)      >>=
    --limitFPS                        >>=
    (\x -> case stateRunning . appState $ app of
            Running -> mainLoop x
            Paused  -> mainLoop x
            Quited  -> return ()
        )




-- | Limit FPS to that listed in the App settings.
limitFPS :: App -> IO App
limitFPS app@(App { appTime = at@(AppTime { timeFrame = t0 }) })= do
        currentTicks <- getTicks
        let tickDifference = currentTicks - t0
            delayTicks     = if tickDifference > frameLength
                                then 0
                                else frameLength - tickDifference
        delay delayTicks
        return $ app { appTime = at { timeFrame = currentTicks }}
    where
        frameLength = 1000 `div` maxFPS
