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


-- | Poll SDL for events and handle them.
handleEvents :: App -> IO App
handleEvents app = do
    putStrLn "Handling events..."
    return app
