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
, solveLorenz
) where

import Lorenz.Data
import Data.Number.FloatToFloat
import Data.Packed.Vector as V
import Data.Packed.Matrix as M
import Numeric.GSL
import Graphics.Rendering.OpenGL.Raw.Types(GLfloat)
import Graphics.Rendering.OpenGL.GL.Tensor(Vertex3(..))
import Graphics.Rendering.OpenGL.Help




-- | Perform logic operation on the App state and return a new App.
update :: App -> IO App
update = return . id




-- | Solve the Lorenz attractor.
solveLorenz :: AppFunction -> ([GLfloat], [Vertex3f])
solveLorenz (AppFunction x0 y0 z0 s r b dt tn) = (times, verts)
    where
        times = [0.0,(floatToFloat dt)..(floatToFloat tn)] :: [GLfloat]
        timesVector = V.fromList $ map floatToFloat times
        solutionMatrix = odeSolve (lorenz' s r b) [x0, y0, z0] timesVector
        verts = matrixToVerts solutionMatrix




-- | Convert HMatrix to a list of verticies.
matrixToVerts :: M.Matrix Double -> [Vertex3f]
matrixToVerts mat = map converter (M.toLists mat)
    where
        converter (x:y:z:_) = Vertex3 (floatToFloat x)
                                      (floatToFloat y)
                                      (floatToFloat z)
        converter _ = error "wrong number of elements"




-- | Lorenz attractor function.
-- σ -> ρ -> β -> t -> (x, y, z) -> (dxdt, dydt, dzdt)
lorenz :: Num a => a -> a -> a -> a -> (a, a, a) -> (a, a, a)
lorenz s r b _ (x, y, z) = (dxdt, dydt, dzdt)
    where
        dxdt = s * (y - x)
        dydt = x * (r - z) - y
        dzdt = x * y - b * z




-- | Same as the lorenz attractor function but uses lists instead of tuples and
-- thus can have a runtime error.
-- σ -> ρ -> β -> t -> [x, y, z] -> [dxdt, dydt, dzdt]
lorenz' :: Num a => a -> a -> a -> a -> [a] -> [a]
lorenz' s r b t (x:y:z:_) = [dxdt, dydt, dzdt]
    where 
        (dxdt, dydt, dzdt) = lorenz s r b t (x, y, z)
lorenz' _ _ _ _ _ = error "lorenz' needs a list with exactly 3 elements"
