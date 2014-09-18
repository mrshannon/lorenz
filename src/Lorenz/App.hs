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
( defaultSettings
, initilizeApp
, initilizeSDL
, initilizeGL
, mainLoop
) where

import Lorenz.Data
import Lorenz.SDL(initilizeSDL)
import Lorenz.OpenGL(initilizeGL)
import Lorenz.Loop(mainLoop)


-- | Default application settings.
defaultSettings :: AppSettings
defaultSettings = AppSettings


-- | Take the application Settings and construct an App object.
initilizeApp :: AppSettings -> App
initilizeApp settings =
    App
        settings
        (AppState Running)
