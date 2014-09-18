{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Lorenz.SDL
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : MIT
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- SDL Specific code.
-------------------------------------------------------------------------------


module Lorenz.SDL
( initilizeSDL
) where

import Lorenz.Data
import Control.Monad
import Graphics.UI.SDL


screenWidth :: Int
screenWidth = 1280

screenHeight :: Int
screenHeight = 720

screenBpp :: Int
screenBpp = 32


-- | OpenGL attributes to apply to window.
glAttributes :: [(GLAttr, GLValue)]
glAttributes = 
    [ (glRedSize        ,   8) -- 8 bit red buffer.
    , (glGreenSize      ,   8) -- 8 bit green buffer.
    , (glBlueSize       ,   8) -- 8 bit blue buffer.
    , (glAlphaSize      ,   8) -- 8 bit alpha buffer.
    , (glDepthSize      ,  24) -- 24 bit depth buffer.
    , (glDoubleBuffer   ,   1) -- Use double buffers.
    ]


-- | Apply a list of tuples of OpenGL attributes.
glApplyAttributes :: [(GLAttr, GLValue)] -> IO ()
glApplyAttributes atts = forM_ atts $ uncurry glSetAttribute


-- | Initilize SDL and open a window with an OpenGL context.
initilizeSDL :: App -> IO App
initilizeSDL app = withInit [InitEverything] $ do
        glApplyAttributes glAttributes
        _ <- setVideoMode screenWidth screenHeight screenBpp [OpenGL]
        setCaption "Lorenz (vINSERT VERSION)" []
        delay 10000
        return app
