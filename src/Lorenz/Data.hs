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
) where

import Data.Word


data App = App
    { appWindow     :: AppWindow
    , appState      :: AppState
    , appTime       :: AppTime
    }


data AppWindow = AppWindow
    { windowWidth       :: !Int
    , windowHeight      :: !Int
    }


data AppState = AppState
    { stateRunning :: StateRunning
    }


data StateRunning = Running | Paused | Quited


data AppTime = AppTime
    { timeFrame     :: !Word32
    , timeUpdate    :: !Word32
    }
