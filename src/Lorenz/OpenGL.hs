{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Lorenz.OpenGL
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : MIT
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- OpenGL functions specific to Lorenz
-------------------------------------------------------------------------------


module Lorenz.OpenGL
( initilizeGL
, draw
, reshape
) where

import Lorenz.Data
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL(setVideoMode, SurfaceFlag(..))
import Graphics.UI.SDL.Video(glSwapBuffers)
import Graphics.Rendering.OpenGL.Help
import Graphics.Rendering.OpenGL.Draw
import qualified Graphics.UI.GLUT.Initialization as GLUT



-- | Initilize OpenGL
initilizeGL :: (App -> IO ()) -> App -> IO ()
initilizeGL nextAction app = do
    _ <- GLUT.initialize "" [""]
    depthFunc $= Just Lequal
    nextAction app




-- | Redraw the scene.
draw :: App -> IO ()
draw _ = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    preservingMatrix $ do
        scale (0.7::GLfloat) (0.7::GLfloat) (0.7::GLfloat)
        drawLabeledAxes
    flush
    glSwapBuffers
    printErrors




-- TODO: This function causes flickering during resize.
-- | Reshape the window.
reshape :: Int -> Int -> IO ()
reshape width height = do

        _ <- setVideoMode width height 32 [OpenGL, Resizable]
        
        -- Set viewport as entire window.
        viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))

        -- Apply the projection.
        matrixMode $= Projection
        loadIdentity
        if w2h >= 1
            -- Wider than taller.
            then ortho (-w2h) (w2h) (-1.0)   (1.0)   (-1.0) (1.0)
            -- Taller than wider.
            else ortho (-1.0) (1.0) (-1/w2h) (1/w2h) (-1.0) (1.0)

        -- Reset the modelview matrix.
        matrixMode $= Modelview 0
        loadIdentity

    where
        -- Width to height ratio.
        w2h = if height > 0
                then (fromIntegral width)/(fromIntegral height)
                else 1

