{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Main
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : MIT
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Displays the Lorenz attractor in 3D with OpenGL.
-------------------------------------------------------------------------------


module Main where


main :: IO ()
main = initilizeApp >>= initilizeSDL >>= initilizeGL >>= mainLoop
