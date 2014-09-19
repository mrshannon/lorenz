{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Lorenz.App
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : MIT
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Main application code for Lorenz.
-------------------------------------------------------------------------------


module Lorenz.App
( initilizeApp
, initilizeSDL
, initilizeGL
, mainLoop
) where

import Lorenz.Data
import Lorenz.SDL(initilizeSDL)
import Lorenz.OpenGL(initilizeGL)
import Lorenz.Loop(mainLoop)




-- | Take the application Settings and construct an App object.
initilizeApp :: App
initilizeApp = App
    { appWindow = AppWindow
        { windowWidth = 1024
        , windowHeight = 720
        }
    , appState = AppState
        { stateRunning  = Running
        }
    , appTime = AppTime
        { timeFrame = 0
        , timeUpdate = 0
        }
    }
