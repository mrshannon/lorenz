{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Lorenz.Logic
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : MIT
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Handles non even driven logi
-------------------------------------------------------------------------------


module Lorenz.Logic
( update
) where

import Lorenz.Data




-- | Perform logic operation on the App state and return a new App.
update :: App -> IO App
update = return . id
