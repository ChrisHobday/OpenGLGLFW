module Drawable
  ( Drawable ( draw ))
  where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL

-- Typeclass for datatypes that are drawable to a screen
class Drawable d where
-- TODO Implement drawable functions with OpenGL or X11/Wayland (possibly windows/mac/android/ios libs too)
  draw :: d -> GLFW.Window -> Int -> Int -> IO () -- Draw a given drawable to a given screen
  -- drawRelative :: d -> Screen -> Int -> Int -> IO () -- Draw a given drawable to a given screen relative to a given position (x, y)
