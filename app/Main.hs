module Main where

import Lib

import Control.Monad (when)
import Control.Exception (bracket)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types



winWidth = 800
winHeight = 600
winTitle = "Hello Window"

-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback :: GLFW.KeyCallback
keyCallback window key scanCode keyState modKeys = do
  -- jsp <- GLFW.joystickPresent GLFW.Joystick'1
  -- print jsp
  print key
  when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
    (GLFW.setWindowShouldClose window True)

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

main :: IO ()
main = do
  GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  GLFW.windowHint (GLFW.WindowHint'Resizable False)
  maybeWindow <- GLFW.createWindow winWidth winHeight winTitle Nothing Nothing
  case maybeWindow of
    Nothing -> putStrLn "Failed to create a GLFW window!"
    Just window -> do
        -- enable keys
        GLFW.setKeyCallback window (Just keyCallback)
        -- set joystick callback
        GLFW.setJoystickCallback (Just joystickCallback)
        -- calibrate the viewport
        GLFW.makeContextCurrent (Just window)
        (x,y) <- GLFW.getFramebufferSize window
        glViewport 0 0 (fromIntegral x) (fromIntegral y)
        -- enter our main loop
        let loop = do
                shouldContinue <- not <$> GLFW.windowShouldClose window
                when shouldContinue $ do
                    -- event poll
                    GLFW.pollEvents
                    -- drawing
                    glClearColor 0.2 0.3 0.3 1.0
                    glClear GL_COLOR_BUFFER_BIT
                    -- swap buffers and go again
                    GLFW.swapBuffers window
                    loop
        loop
  GLFW.terminate
