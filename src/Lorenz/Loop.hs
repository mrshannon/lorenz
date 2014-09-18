{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -Werror #-}

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
import Lorenz.Events(handleEvents)
import Lorenz.Logic(update)
import Lorenz.OpenGL(redraw)


-- | Main loop of Lorenz.
mainLoop :: App -> IO ()
mainLoop app = handleEvents app >>= return . update
    >>= (\x -> redraw x >> return x)
    >>= (\x -> case stateRunning . appState $ app of
            Running -> mainLoop x
            Paused  -> mainLoop x
            Quit    -> return ()
        )
