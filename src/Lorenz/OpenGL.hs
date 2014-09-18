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
, redraw
) where

import Lorenz.Data
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL(delay)
import Graphics.UI.SDL.Video(glSwapBuffers)


-- | Initilize OpenGL
initilizeGL :: (App -> IO ()) -> App -> IO ()
initilizeGL nextAction app = do
    depthFunc $= Just Lequal
    nextAction app


-- | Redraw the scene.
redraw :: App -> IO ()
redraw _ = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    delay 100
    renderPrimitive Polygon $ do
        color $ Color3 (1.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
        vertex $ Vertex2 ( 0.0::GLfloat) ( 0.5::GLfloat)
        color $ Color3 (0.0::GLfloat) (1.0::GLfloat) (0.0::GLfloat)
        vertex $ Vertex2 ( 0.5::GLfloat) (-0.5::GLfloat)
        color $ Color3 (0.0::GLfloat) (0.0::GLfloat) (1.0::GLfloat)
        vertex $ Vertex2 (-0.5::GLfloat) (-0.5::GLfloat)
    flush
    glSwapBuffers
