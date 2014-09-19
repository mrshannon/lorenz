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
import Lorenz.OpenGL(reshape)
import Control.Monad
import Graphics.UI.SDL
import Data.Version(showVersion)
import Paths_lorenz(version)




-- | OpenGL attributes to apply to window.
glAttributes :: [(GLAttr, GLValue)]
glAttributes = 
    [ (glRedSize                ,   8) -- 8 bit red buffer.
    , (glGreenSize              ,   8) -- 8 bit green buffer.
    , (glBlueSize               ,   8) -- 8 bit blue buffer.
    , (glAlphaSize              ,   8) -- 8 bit alpha buffer.
    , (glDepthSize              ,  24) -- 24 bit depth buffer.
    , (glDoubleBuffer           ,   1) -- Use double buffers.
    , (glMultiSampleSamples     ,   8)
    ]




-- | Apply a list of tuples of OpenGL attributes.
glApplyAttributes :: [(GLAttr, GLValue)] -> IO ()
glApplyAttributes atts = forM_ atts $ uncurry glSetAttribute




-- TODO: There is a small bug that will cause the projection to be off until a
--       resize on tiling window managers.  This is not a problem on floating
--       window managers.
-- | Initilize SDL and open a window with an OpenGL context.
initilizeSDL :: (App -> IO ()) -> App -> IO ()
initilizeSDL nextAction app@(App { appWindow = appWin}) = 
    withInit [InitEverything] $ do

        -- Set OpenGL attributes and create a window.
        glApplyAttributes glAttributes
        reshape screenWidth screenHeight
        -- _ <- setVideoMode screenWidth screenHeight screenBpp [OpenGL, Resizable]
        setCaption ("Lorenz (v" ++ showVersion version ++ ")") "Lorenz"

        -- Run the next action.
        nextAction app

    where
        AppWindow screenWidth screenHeight = appWin
