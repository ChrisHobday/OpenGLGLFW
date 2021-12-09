module Main where

import Control.Monad ( when, unless )
import Control.Exception ( bracket )

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

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

loop :: GLFW.Window -> IO ()
loop window = do
  -- Get whether or not the window should close
  shouldClose <- GLFW.windowShouldClose window
  -- If the window shouldn't close execute do block
  unless shouldClose $ do
    -- Poll events from set callbacks
    GLFW.pollEvents
    -- Drawing/Rendering
    GL.clearColor $= Color4 1.0 0.3 0.3 1.0
    GL.clear [ColorBuffer]
    renderPrimitive Triangles $
      mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
    GLFW.swapBuffers window
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
  GLFW.setKeyCallback window (Just keyCallback)
  GLFW.setJoystickCallback (Just joystickCallback)
  GLFW.makeContextCurrent (Just window)
  loop window