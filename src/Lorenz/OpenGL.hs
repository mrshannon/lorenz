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
draw (App {appView = view, appData = d, appFunction = f}) = do

    -- Clear buffers and matrix.
    clear [ColorBuffer, DepthBuffer]
    loadIdentity

    -- Apply view transformations.
    let s = 1.0/(viewScale view)
    scale s s s
    translate $ fmap negate (vertexToVector3 $ viewCenter view)
    rotate (viewElevation view) xVector3f
    rotate (viewAzimuth view) yVector3f

    -- Rotate into: vertical z, right y, and outward x
    rotate (-90) zVector3f
    rotate (-90) yVector3f

    -- Draw axes.
    preservingMatrix $ do
        scale (0.7::GLfloat) (0.7::GLfloat) (0.7::GLfloat)
        drawLabeledAxes

    -- Draw Lorenz.
    preservingMatrix $ do
        scale (0.02::GLfloat) (0.02::GLfloat) (0.02::GLfloat)
        color yellow
        drawFunction d

    -- Draw status bar.
    color white
    windowPos (Vertex2 5 5 :: Vertex2 GLint)
    drawDisplay f

    -- Flush and swap buffers.
    flush
    glSwapBuffers

    -- Print any OpenGL errors.
    printErrors




-- | Display status bar.
drawDisplay :: AppFunction -> IO ()
drawDisplay (AppFunction x0 y0 z0 s r b dt tn) = do
    glutPrint $ "x0 = " ++ (show x0)
        ++ ", y0 = " ++ (show y0)
        ++ ", z0 = " ++ (show z0)
        ++ ", s = " ++ (show s)
        ++ ", r = " ++ (show r)
        ++ ", b = " ++ (show b)
        ++ ", dt = " ++ (show dt)
        ++ ", tn = " ++ (show tn)





-- | Plot the app function.
drawFunction :: Maybe AppData -> IO ()
drawFunction Nothing = return ()
drawFunction (Just (AppData _ vs)) = renderPrimitive LineStrip $ mapM_ vertex vs



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
            then ortho (-w2h) (w2h) (-1.0)   (1.0)   (-20.0) (20.0)
            -- Taller than wider.
            else ortho (-1.0) (1.0) (-1/w2h) (1/w2h) (-20.0) (20.0)

        -- Reset the modelview matrix.
        matrixMode $= Modelview 0
        loadIdentity

    where
        -- Width to height ratio.
        w2h = if height > 0
                then (fromIntegral width)/(fromIntegral height)
                else 1

