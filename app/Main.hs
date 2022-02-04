module Main where

import Rectangle
import Drawable

import Control.Monad ( when, unless )
import Control.Exception ( bracket )

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

points :: [(GLfloat,GLfloat,GLfloat)]
points = [(0.5, 0.8, 0.0),(0.8, 0.5, 0.0),(1.0, -0.5, 0.0)]

-- Window resize callback
windowResizeCallback :: GLFW.WindowSizeCallback
windowResizeCallback window width height = do
  -- Update viewport with new width/height
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

-- Callback for when a key is pressed
keyCallback :: GLFW.KeyCallback
keyCallback window key scanCode keyState modKeys = do
  when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
    (GLFW.setWindowShouldClose window True)

-- Callback for when a joystick is connected/disconnected
joystickCallback :: GLFW.JoystickCallback
joystickCallback joystick joystickState = do
  joystickName <- GLFW.getJoystickName joystick
  case joystickState of
    GLFW.JoystickState'Connected ->
      putStrLn $ show joystickName ++ " connected"
    GLFW.JoystickState'Disconnected ->
      putStrLn $ show joystickName ++ " disconnected"

-- | Ensures that we only run GLFW code while it's initialized, and also that we
-- always terminate it when we're done.
bracketGLFW :: IO () -> IO ()
bracketGLFW act = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
    when initWorked act

render :: GLFW.Window -> IO ()
render window = do
    -- Drawing/Rendering
    GL.clearColor $= GL.Color4 0.1 0.1 0.1 1.0 -- Background color
    GL.clear [GL.ColorBuffer]
    let color3f r g b = GL.color $ GL.Color3 r g (b :: GLfloat)
        vertex3f x y z = GL.vertex $ GL.Vertex3 x y (z :: GLfloat)
        vertex2f x y = GL.vertex $ GL.Vertex2 x (y :: GLfloat)
    -- White Crosshair
    GL.renderPrimitive GL.Lines $ do
      color3f 1 1 1
      vertex2f (-1) 0
      vertex2f 1 0
      vertex2f 0 (-1)
      vertex2f 0 1
    -- Green 2D Line
    -- GL.renderPrimitive GL.Lines $ do
    --   color3f 0 1 0
    --   vertex2f (-0.5) 0.5
    --   vertex2f 0.5 0.5
    -- Red 3D Line
    GL.renderPrimitive GL.Lines $ do
      color3f 1 0 0
      vertex3f (-0.5) 0.5 (2)
      vertex3f 0.5 0.5 (0)
    -- Overlapping triangles
    -- GL.renderPrimitive GL.TriangleFan $ do
    --   color3f 1 0 0
    --   vertex2f 0 0
    --   vertex2f 0 0.2
    --   vertex2f 0.2 0.2
    -- GL.renderPrimitive GL.TriangleFan $ do
    --   color3f 0 1 0
    --   vertex2f 0.05 0.25
    --   vertex2f 0.05 0.05
    --   vertex2f 0.25 0.05
    -- GL.renderPrimitive GL.TriangleFan $
    --   mapM_ (\(x, y, z) -> GL.vertex $ GL.Vertex3 x y z) points
    GLFW.swapBuffers window

loop :: GLFW.Window -> IO ()
loop window = do
  -- Get whether or not the window should close
  shouldClose <- GLFW.windowShouldClose window
  -- If the window shouldn't close execute do block
  unless shouldClose $ do
    -- Poll events from set callbacks
    GLFW.pollEvents
    -- Render
    -- render window

    let r = Rectangle 100 100 50 50 5 5 (Just 20) (Just 20)
    draw r window 10 10

    -- Loop again
    loop window
  -- Destroy window
  GLFW.destroyWindow window
  -- Terminate GLFW
  GLFW.terminate

main :: IO ()
main = do
  GLFW.init
  GLFW.defaultWindowHints
  Just window <- GLFW.createWindow 800 600 "Window" Nothing Nothing
  GLFW.setWindowSizeCallback window (Just windowResizeCallback)
  GLFW.setKeyCallback window (Just keyCallback)
  GLFW.setJoystickCallback (Just joystickCallback)
  GLFW.makeContextCurrent (Just window)
  loop window