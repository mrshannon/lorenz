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
, handleMouseMovements
) where

import Lorenz.Data
import Lorenz.OpenGL(reshape, draw)
import Graphics.UI.SDL
import Graphics.Rendering.OpenGL.Raw.Types(GLfloat)



-- | Poll for mouse movements.
handleMouseMovements :: App -> IO App
handleMouseMovements app@(App { appMouse = mouse }) = do

        -- Get mouse state.
        (x1, y1, _) <- getMouseState

        -- Compare to the previous mouse state.
        let (x0, y0) = mousePosition $ mouse
            dx = x1 - x0
            dy = y1 - y0

            -- Apply new mouse position.
            newApp = app { appMouse = mouse { mousePosition = (x1, y1) } }

        -- Handle the movement.
        handleMouseMov newApp mouse dx dy




-- | Handle a mouse move.
handleMouseMov :: App -> AppMouse -> Int -> Int -> IO App

-- Movement with right button down.
handleMouseMov app@(App { appWindow = window, appMouse = mouse })
    (AppMouse _ _ _ Down) dx dy = do
        
        -- Get window size
        let width = fromIntegral . windowWidth $ window
            height = fromIntegral . windowHeight $ window

            -- Calculate change in rotation.
            s = stateModifierScale . appState $ app
            dAzimuth = s*(fromIntegral dx)*0.05
            dElevation = s*(fromIntegral dy)*0.05

            -- Calculate new mouse position after warp.
            x1 = (width `div` 2)
            y1 = (height `div` 2)
            newApp = app { appMouse = mouse { mousePosition = (x1, y1) } }

        -- Warp the mouse to the center of the screen.
        warpMouse (fromIntegral x1) (fromIntegral y1)

        -- Modify the view.
        return $ modifyView newApp dAzimuth dElevation 1.0

-- Any unmatched mouse movement.
handleMouseMov app _ _ _ = return app




-- | Change the view properties.
modifyView :: App -> GLfloat -> GLfloat -> GLfloat -> App
modifyView app@(App { appView = av }) dAzimuth dElevation multScale =

    -- Calculate new view properties
    let AppView { viewAzimuth = a, viewElevation = e, viewScale = s } = av 
        newAzimuth = a + dAzimuth
        newElevation = e + dElevation
        newScale = s*multScale

    -- Apply the new view properties.
    in  app { appView = av { viewAzimuth = newAzimuth,
                             viewElevation = newElevation,
                             viewScale = newScale } }






-- | Poll SDL for events and handle them.
handleEvents :: App -> IO App
handleEvents app = pollEvent >>= (\e -> case e of
        NoEvent     -> return app
        _           -> handleEvent app e >>= handleEvents
    )




-- | Eats all SDL events and throws them away.
clearEventQueue :: IO ()
clearEventQueue = pollEvent >>= (\e -> case e of
        NoEvent     -> return ()
        _           -> clearEventQueue
    )




-- | Handle any single event.
handleEvent :: App -> Event -> IO App

-- Window resize.
handleEvent app (VideoResize width height) = do

        -- Update SDL and OpenGL for new window size.
        reshape width height

        -- Clear the event queue to avoid undesired inputs during the resize.
        clearEventQueue

        -- Return an unchanged App object.
        return app

-- Window needs redraw.
handleEvent app VideoExpose = do
        draw app
        return app

-- Close the application.
handleEvent app@(App { appState = s }) Quit = do
        return $ app { appState = s { stateRunning = Quited }}

-- Key Down.
handleEvent app (KeyDown (Keysym { symKey = key})) = 
    handleKeyDown app $ key

-- Key Up.
handleEvent app (KeyUp (Keysym { symKey = key})) = 
    handleKeyUp app $ key

-- Mouse Button Down.
handleEvent app (MouseButtonDown x y b) =
    handleMouseDown app (fromIntegral x) (fromIntegral y) b

-- Mouse Button Up.
handleEvent app (MouseButtonUp x y b) =
    handleMouseUp app (fromIntegral x) (fromIntegral y) b

-- Any non matched event.
handleEvent app _ = return app




-- | Handle mouse down events.
handleMouseDown :: App -> Int -> Int-> MouseButton -> IO App

-- Right button down.
handleMouseDown app@(App { appMouse = mouse }) _ _ ButtonRight = do
    showCursor False
    return $ app { appMouse = mouse { mouseRight = Down } }

-- Mouse wheel down.
handleMouseDown app _ _ ButtonWheelDown =
    return $ modifyView app 0.0 0.0 1.1

-- Mouse wheel up.
handleMouseDown app _ _ ButtonWheelUp =
    return $ modifyView app 0.0 0.0 0.9

handleMouseDown app _ _ _ = return app




-- | Handle mouse down events.
handleMouseUp :: App -> Int -> Int-> MouseButton -> IO App

-- Right button up.
handleMouseUp app@(App { appMouse = mouse }) _ _ ButtonRight = do
    showCursor True
    return $ app { appMouse = mouse { mouseRight = Up } }

-- Any non matched mouse button down event.
handleMouseUp app _ _ _ = return app




-- | Handle key down events.
handleKeyDown :: App -> SDLKey -> IO App

-- Left Shift.
handleKeyDown app@(App { appState = state }) SDLK_LSHIFT =
    return $ app { appState = state { stateModifierScale = 0.1 } }

-- Right Shift.
handleKeyDown app@(App { appState = state }) SDLK_RSHIFT =
    return $ app { appState = state { stateModifierScale = 0.1 } }

-- Left Ctrl.
handleKeyDown app@(App { appState = state }) SDLK_LCTRL =
    return $ app { appState = state { stateModifierScale = 10.0 } }

-- Right Ctrl.
handleKeyDown app@(App { appState = state }) SDLK_RCTRL =
    return $ app { appState = state { stateModifierScale = 10.0 } }

-- Any non matched key down event.
handleKeyDown app _ = return app




-- | Handle key down events.
handleKeyUp :: App -> SDLKey -> IO App

-- Left Shift.
handleKeyUp app@(App { appState = state }) SDLK_LSHIFT =
    return $ app { appState = state { stateModifierScale = 1.0 } }

-- Right Shift.
handleKeyUp app@(App { appState = state }) SDLK_RSHIFT =
    return $ app { appState = state { stateModifierScale = 1.0 } }

-- Left Ctrl.
handleKeyUp app@(App { appState = state }) SDLK_LCTRL =
    return $ app { appState = state { stateModifierScale = 1.0 } }

-- Right Ctrl.
handleKeyUp app@(App { appState = state }) SDLK_RCTRL =
    return $ app { appState = state { stateModifierScale = 1.0 } }


-- Any non matched key down event.
handleKeyUp app _ = return app



