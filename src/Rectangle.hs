module Rectangle
  ( Rectangle ( Rectangle )
  , defaultRectangle )
  where

import Drawable ( Drawable ( draw ) )
-- import Direction   ( Direction  ( North, East, South, West ) )
-- import Hoverable   ( Hoverable  ( hovered ) )
-- import Moveable    ( Moveable   ( move, moveLeft, moveRight, moveUp, moveDown ) )
-- import Resizable   ( Resizable  ( resize, addWidth, addHeight ) )
-- import Splittable  ( Splittable ( split ) )
-- import Snappable   ( Snappable  ( snap ) )

import Data.Maybe  ( Maybe, fromMaybe )
import Data.Either ( Either     ( Left, Right ) )

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL

-- Default rectangle for testing and quickly making a basic rectangle
defaultRectangle = Rectangle 10 10 10 10 5 5 (Just 20) (Just 20)

-- Datatype to represent a rectangle
data Rectangle = Rectangle
  { x             :: Int           -- The x position of a rectangle
  , y             :: Int           -- The y position of a rectangle
  , width         :: Int           -- The width of a rectangle
  , height        :: Int           -- The height of a rectangle
  , minimumWidth  :: Int           -- The minimum width of a rectangle (default should be 0)
  , minimumHeight :: Int           -- The minimum height of a rectangle (default should be 0)
  , maximumWidth  :: Maybe Int     -- The maximum width of a rectangle
  , maximumHeight :: Maybe Int     -- The maximum height of a rectangle
  -- , background    :: Maybe Image   -- The background of the rectangle
  -- , border        :: Border        -- The border of the rectangle
  -- , rounding      :: Int           -- The amount of rounding on the corners
  } deriving ( Show, Eq )
instance Drawable Rectangle where
  draw r w xo yo = do
    (windowWidth, windowHeight) <- GLFW.getFramebufferSize w
    let drawX  = fromIntegral (x r) / (fromIntegral windowWidth / 2)
        drawY  = fromIntegral (y r) / (fromIntegral windowHeight / 2)
        drawX2 = drawX + fromIntegral (width r) / (fromIntegral windowWidth / 2)
        drawY2 = drawY + fromIntegral (height r) / (fromIntegral windowHeight / 2)
    putStrLn "--"
    print (windowWidth, windowHeight)
    putStrLn "--"
    putStrLn "--"
    print drawX
    print drawY
    print drawX2
    print drawY2
    putStrLn "--"
    -- Draw
    GL.clearColor $= GL.Color4 0.1 0.1 0.1 1.0 -- Background color
    GL.clear [GL.ColorBuffer]
    let color3f r g b = GL.color $ GL.Color3 r g (b :: GLfloat)
        vertex3f x y z = GL.vertex $ GL.Vertex3 x y (z :: GLfloat)
        vertex2f x y = GL.vertex $ GL.Vertex2 x (y :: GLfloat)
    GL.renderPrimitive GL.TriangleFan $ do
      color3f 1 0 0
      vertex2f drawX drawY
      vertex2f drawX drawY2
      vertex2f drawX2 drawY2
      vertex2f drawX2 drawY
    GLFW.swapBuffers w
    return ()
    -- GL.renderPrimitive GL.Lines $ do
    --   color3f 0 0 1
    --   vertex2f (-1) 0
    --   vertex2f 1 0
    --   vertex2f 0 (-1)
    --   vertex2f 0 1
-- instance Hoverable Rectangle where
--   hovered r px py
--     | px >= x r &&
--       px <= x r + width r &&
--       py >= y r &&
--       py <= y r + height r = True
--     | otherwise            = False
-- instance Moveable Rectangle where
--   move r x y    = r { x = x, y = y }
--   moveLeft r i  = r { x = x r - i, y = y r }
--   moveRight r i = r { x = x r + i, y = y r }
--   moveUp r i    = r { x = x r, y = y r + i }
--   moveDown r i  = r { x = x r, y = y r - i }
-- instance Resizable Rectangle where
--   resize r w h  = r { width = max (minimumWidth r) (min w (fromMaybe w (maximumWidth r)))
--                     , height = max (minimumHeight r) (min h (fromMaybe h (maximumHeight r))) }
--   addWidth r i  = r { width = max (minimumWidth r) (min newWidth (fromMaybe newWidth (maximumWidth r))) }
--                     where newWidth = i + width r
--   addHeight r i = r { height = max (minimumHeight r) (min newHeight (fromMaybe newHeight (maximumHeight r))) }
--                     where newHeight = i + height r
-- instance Splittable Rectangle where
--   -- TODO Implement a more elegant solution for split
--   -- TODO Bug where new width/height and new maximum width/height round different and don't keep correct proportionately
--   split r d p
--     | d == East =
--       if oldRecWidth < minimumWidth r then
--         Left r
--       else
--         Right (r { width = oldRecWidth, maximumWidth = oldRecMaxWidth }, r { x = x r + oldRecWidth, width = newRecWidth, minimumWidth = 0, minimumHeight = 0, maximumWidth = newRecMaxWidth })
--     | d == West =
--       if oldRecWidth < minimumWidth r then
--         Left r
--       else
--         Right (r { x = x r + newRecWidth, width = oldRecWidth, maximumWidth = oldRecMaxWidth }, r { width = newRecWidth, minimumWidth = 0, minimumHeight = 0, maximumWidth = newRecMaxWidth })
--     | d == North =
--       if oldRecHeight < minimumHeight r then
--         Left r
--       else
--         Right (r { height = oldRecHeight, maximumHeight = oldRecMaxHeight }, r { y = y r + oldRecHeight, height = newRecHeight, minimumWidth = 0, minimumHeight = 0, maximumHeight = newRecMaxHeight })
--     | d == South =
--       if oldRecHeight < minimumHeight r then
--         Left r
--       else
--         Right (r { y = y r + newRecHeight, height = oldRecHeight, maximumHeight = oldRecMaxHeight }, r { height = newRecHeight, minimumWidth = 0, minimumHeight = 0, maximumHeight = newRecMaxHeight })
--     where newRecWidth     = round (fromIntegral (width r) * p)
--           oldRecWidth     = width r - newRecWidth
--           newRecHeight    = round (fromIntegral (height r) * p)
--           oldRecHeight    = height r - newRecHeight
--           -- newRecMaxWidth  = newRecWidth * (width r / maximumWidth r)
--           newRecMaxWidth  = round . (* p) . fromIntegral <$> maximumWidth r
--           oldRecMaxWidth  = (-) <$> maximumWidth r <*> newRecMaxWidth
--           newRecMaxHeight = round . (* p) . fromIntegral <$> maximumHeight r
--           oldRecMaxHeight = (-) <$> maximumHeight r <*> newRecMaxHeight
-- instance Snappable Rectangle where
--   snap r px py w h = resize (move r px py) w h
