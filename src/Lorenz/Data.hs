{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Lorenz.Data
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : MIT
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Data sctructures used by Lorenz
-------------------------------------------------------------------------------


module Lorenz.Data
( App(..)
, AppWindow(..)
, AppState(..)
, AppTime(..)
, StateRunning(..)
, AppView(..)
, AppMouse(..)
, AppKeyboard(..)
, KeyState(..)
, AppFunction(..)
, AppData(..)
) where

import Data.Word
import Graphics.Rendering.OpenGL.Raw.Types(GLfloat)
import Graphics.Rendering.OpenGL.Help(Vertex3f)


data App = App
    { appWindow     :: AppWindow
    , appState      :: AppState
    , appTime       :: AppTime
    , appView       :: AppView
    , appMouse      :: AppMouse
    , appKeyboard   :: AppKeyboard
    , appFunction   :: AppFunction
    , appData       :: Maybe AppData
    } deriving(Show)


data AppWindow = AppWindow
    { windowWidth       :: !Int
    , windowHeight      :: !Int
    } deriving(Show)


data AppState = AppState
    { stateRunning          :: StateRunning
    , stateModifierScale    :: GLfloat
    } deriving(Show)


data StateRunning = Running | Paused | Quited deriving(Show)


data AppTime = AppTime
    { timeFrame     :: !Word32
    , timeUpdate    :: !Word32
    } deriving(Show)


data AppView = AppView
    { viewScale     :: GLfloat
    , viewCenter    :: Vertex3f
    , viewAzimuth   :: GLfloat
    , viewElevation :: GLfloat
    } deriving(Show)


data AppMouse = AppMouse
    { mousePosition :: (Int, Int)
    , mouseLeft     :: KeyState
    , mouseMiddle   :: KeyState
    , mouseRight    :: KeyState
    } deriving(Show)


data AppKeyboard = AppKeyboard deriving(Show)


data KeyState = Up | Down deriving(Show)


data AppFunction = AppFunction
    { initX     :: Double
    , initY     :: Double
    , initZ     :: Double
    , funS      :: Double
    , funR      :: Double
    , funB      :: Double
    , funDT     :: Double
    , funTFinal :: Double
    } deriving(Show)


data AppData = AppData
    { dataTime  :: [GLfloat]
    , dataVerts :: [Vertex3f]
    } deriving(Show)
