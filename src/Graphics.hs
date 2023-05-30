module Graphics
  (doGraphics
  ,PatchColor,red,green,blue,white,black,gray,pink,brown,plum,hunter,purple)
where

import Control.Monad
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import World
-- import Control.Concurrent.Thread
type PatchColor = (GLfloat,GLfloat,GLfloat)

black :: PatchColor
black = (0.0,0.0,0.0)
red :: PatchColor
red   = (1.0,0.0,0.0)
green :: PatchColor
green = (0.0,1.0,0.0)
blue :: PatchColor
blue  = (0.0,0.0,1.0)
white :: PatchColor
white = (1.0,1.0,1.0)
gray :: PatchColor
gray = (0.5, 0.5, 0.5)
pink :: PatchColor
pink = (0.52, 0.39, 0.39)
brown :: PatchColor
brown = (0.42, 0.26, 0.15)
plum :: PatchColor
plum = (0.917647, 0.678431, 0.917647)
hunter :: PatchColor
hunter  = (0.13, 0.37, 0.31)
purple :: PatchColor
purple = (0.73, 0.16, 0.96)

patch :: Loc -> PatchColor -> IO ()
patch (x,y) (r,g,b) = do
  color $ Color3 r g b
  rect (Vertex2 xf yf) (Vertex2 (xf+1) (yf+1))
  where xf = fromIntegral x :: GLfloat
        yf = fromIntegral y :: GLfloat

display :: (a -> PatchColor) -> IORef (World a) -> IO ()
display colorf worldRef = do
  world <- get worldRef
  clear [ColorBuffer]
  let locs = locationsIn (worldBounds world)
  forM_ locs $ \loc -> patch loc (colorf $ cellAt world loc)
  swapBuffers

idle :: IORef (World a) -> (World a -> World a) -> IO ()
idle w evolve = do
  w $~ evolve
  -- delay 1000
  postRedisplay Nothing

reshape :: Size -> IO ()
reshape (Size w h) = do
  let x = max w h
  let size = Size x x
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

doGraphics :: (a -> PatchColor) -> (World a -> World a) -> World a -> IO ()
doGraphics colorf evolvef world = do
  (progname,args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  window <- createWindow "Conway"
  windowSize $= Size 500 500
  let ((x1,y1),(x2,y2)) = worldBounds world
  ortho2D (fromIntegral x1) (fromIntegral x2)
          (fromIntegral y1) (fromIntegral y2)
  worldRef <- newIORef world
  displayCallback $= display colorf worldRef
  reshapeCallback $= Just reshape
  idleCallback $= Just (idle worldRef evolvef)
  mainLoop
