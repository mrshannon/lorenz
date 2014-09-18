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
import Lorenz.SDL



-- | Default application settings.
defaultSettings :: Settings
defaultSettings = Settings


-- | Take the application Settings and construct an App object.
initilizeApp :: Settings -> App
initilizeApp settings = App settings


-- | Initilize OpenGL
initilizeGL :: App -> IO App
initilizeGL app = putStrLn "Initilizing OpenGL..." >> return app


-- | Run the application.
mainLoop :: App -> IO ()
mainLoop _ = putStrLn "Running application..."
