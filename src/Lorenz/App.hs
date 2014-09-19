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
import Graphics.Rendering.OpenGL.GL.Tensor(Vertex3(..))



-- | Take the application Settings and construct an App object.
initilizeApp :: App
initilizeApp = App
    { appWindow = AppWindow
        { windowWidth  = 1024
        , windowHeight = 720
        }
    , appState = AppState
        { stateRunning       = Running
        , stateModifierScale = 1.0
        }
    , appTime = AppTime
        { timeFrame  = 0
        , timeUpdate = 0
        }
    , appView = AppView
        { viewScale     = 1.0
        , viewCenter    = Vertex3 0.0 0.0 0.0
        , viewAzimuth   = (-22.0)
        , viewElevation =   22.0
        }
    , appMouse = AppMouse
        { mousePosition = (0, 0)
        , mouseLeft     = Up
        , mouseMiddle   = Up
        , mouseRight    = Up
        }
    , appKeyboard = AppKeyboard
    , appFunction = AppFunction
        { initX     = 1.0
        , initY     = 1.0
        , initZ     = 1.0
        , funS      = 10.0
        , funR      = 28.0
        , funB      = 2.7777
        , funDT     = 0.001
        , funTFinal = 50.0
        }
    , appData = Nothing
    }
