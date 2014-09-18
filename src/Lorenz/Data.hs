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
, AppSettings(..)
, AppState(..)
, StateRunning(..)
) where


data App = App
    { appSettings   :: AppSettings
    , appState      :: AppState
    }


data AppSettings = AppSettings


data AppState = AppState
    { stateRunning :: StateRunning
    }


data StateRunning = Running | Paused | Quit
