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
( Settings(..)
, App(..)
) where



data Settings = Settings

data App = App Settings

