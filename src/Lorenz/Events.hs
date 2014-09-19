{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Lorenz.Events
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : MIT
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Event handlers.
-------------------------------------------------------------------------------

module Lorenz.Events
( handleEvents
) where

import Lorenz.Data
import Lorenz.OpenGL(reshape, draw)
import Graphics.UI.SDL




-- | Poll SDL for events and handle them.
handleEvents :: App -> IO App
handleEvents app = pollEvent >>= (\e -> case e of
        NoEvent     -> return app
        _           -> handleEvent app e >>= handleEvents
    )




-- | Eats all SDL events and throws them away.
clearEventQueue :: IO ()
clearEventQueue = pollEvent >>= (\e -> case e of
        NoEvent     -> return ()
        _           -> clearEventQueue
    )




-- | Handle any single event.
handleEvent :: App -> Event -> IO App

-- Window resize.
handleEvent app (VideoResize width height) = do

        -- Update SDL and OpenGL for new window size.
        reshape width height

        -- Clear the event queue to avoid undesired inputs during the resize.
        clearEventQueue

        -- Return an unchanged App object.
        return app

-- Window needs redraw.
handleEvent app VideoExpose = do
        draw app
        return app

-- Close the application.
handleEvent app@(App { appState = s }) Quit = do
        return $ app { appState = s { stateRunning = Quited }}

-- Any non matched event.
handleEvent app _ = return app
