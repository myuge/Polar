-- Front-end of point to plane(polar opposite of point) transformation program.
-- in polarity a point in our ordinary space corresponds to a plane.
-- so, plane is our element of geometrical object.

import Data.String
import Data.Text
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Time
import System.Environment
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Gdk.DrawWindow
import Graphics.UI.Gtk.Gdk.Drawable
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.Gdk.Pixmap
--import PictContext
-- App
import Polar

data Palette = Palette {
  red :: GCValues,
  orange :: GCValues,
  yellow :: GCValues,
  green :: GCValues,
  blue :: GCValues,
  indigo :: GCValues,
  violet :: GCValues
  }

main :: IO ()
main = do
  args <- System.Environment.getArgs
  if Prelude.length args /= 6 then
      putStrLn "Usage: polar x0 y0 z0 r a times"
  else
      do
        initGUI
        builder <- builderNew 
        builderAddFromFile builder "Pict.glade"
        window <- builderGetObject builder castToWindow "window1"
        onDestroy window mainQuit
        canvas <- builderGetObject builder castToDrawingArea "drawingarea1"
        let palette = initPalette
        let pictType = "polar"
        let x0 = read (args !! 0) :: Double
        let y0 = read (args !! 1) :: Double
        let z0 = read (args !! 2) :: Double
        let r  = read (args !! 3) :: Double
        let a  = read (args !! 4) :: Double
        let times  = read (args !! 5) :: Int
        let context = Polar.getContext x0 y0 z0 r a times
        widgetShowAll window
        dw <- widgetGetDrawWindow canvas
        pixmap <- pixmapNew (Just dw) 1000 1000 Nothing
        drawPict (castToDrawable pixmap) palette context
        onExpose canvas $ \_ -> redraw dw (castToDrawable pixmap)
        saveButton <- builderGetObject builder castToButton "button1"
        onClicked saveButton $ savePict (castToDrawable pixmap) pictType args
        closeButton <- builderGetObject builder castToButton "button2"
        onClicked closeButton $ do
          widgetDestroy window
        mainGUI

initPalette :: Palette
initPalette = Palette {
  red = newGCValues{ foreground = (Color 58339 8995 8738) }, -- 227,38,34
  orange = newGCValues{ foreground = (Color 62965 37265 257) }, -- 245,145,1
  yellow = newGCValues{ foreground = (Color 62708 58853 0) }, -- 244,228,0
  green = newGCValues{ foreground = (Color 0 36494 23387) }, -- 0,142,91
  blue = newGCValues{ foreground = (Color 9766 29041 45746) }, -- 38,113,178
  indigo = newGCValues{ foreground = (Color 17476 20046 39321) }, -- 68,78,153
  violet = newGCValues{ foreground = (Color 28013 14649 35723) } -- 109,57,139
  }

drawPict :: Drawable -> Palette -> [Plane] -> IO Bool
drawPict d palette context = 
  do
    gc <- gcNew d

    -- set background color white
    gcSetValues gc newGCValues{ foreground = (Color 65025 65025 65025) }
    drawRectangle d gc True 0 0 1000 1000

    --mapM (drawPlane d gc palette) context
    drawPlanes d gc palette context
    return True

drawPlanes :: Drawable -> GC -> Palette -> [Plane] -> IO()
drawPlanes d gc palette (pln:plns) =
  do
--    drawPlane d gc palette pln ((Prelude.length plns) `mod` 7)
    drawPlane d gc palette pln (((Prelude.length plns)+6) `mod` 7)
    drawPlanes d gc palette plns
--    drawPlane d gc palette ((pln:plns) !! 0) 3
--    drawPlane d gc palette ((pln:plns) !! 1) 2
--    drawPlane d gc palette ((pln:plns) !! 2) 2
--    drawPlane d gc palette ((pln:plns) !! 3) 2
--    drawPlane d gc palette ((pln:plns) !! 4) 4
--    drawPlane d gc palette ((pln:plns) !! 5) 5
--    drawPlane d gc palette ((pln:plns) !! 6) 6
--    return ()
drawPlanes d gc palette [] =
  return ()

drawPlane :: Drawable -> GC -> Palette -> Plane -> Int -> IO()
drawPlane d gc palette pln ncol =
  do
    Main.drawLines d gc palette (fst pln) ncol
    Main.drawLines d gc palette (snd pln) ncol

drawLines :: Drawable -> GC -> Palette -> Lines -> Int -> IO()
drawLines d gc palette (l:ls) ncol =
  do
    setColor gc palette ncol
    drawLine d gc (asPoint (fst l)) (asPoint (snd l))
    Main.drawLines d gc palette ls ncol
drawLines _ _ _ [] _ =
    return ()

setColor :: GC -> Palette -> Int -> IO ()
setColor gc palette ncol =
  do
    let color = asColor palette ncol
    gcSetValues gc color

asColor :: Palette -> Int -> GCValues
asColor palette n =
  case n of
    0 -> red palette
    1 -> orange palette
    2 -> yellow palette
    3 -> green palette    
    4 -> blue palette
    5 -> indigo palette
    6 -> violet palette

redraw :: DrawWindow -> Drawable -> IO Bool
redraw dw drawable = 
  do
    gc <- gcNew dw
    drawDrawable dw gc drawable 0 0 0 0 (-1) (-1)
    return True

savePict :: Drawable -> String -> [String] -> IO ()
savePict drawable pictType args =
  do  
    Just pixbuf <- pixbufGetFromDrawable drawable (Rectangle 0 0 999 999)
    timestamp <- getTimestamp
    pixbufSave pixbuf ("./" ++ pictType ++ (toString args) ++ ".png") (Data.Text.pack "png") ([]::[([Char],[Char])])
    --pixbufSave pixbuf ("./" ++ pictType ++ (timestamp) ++ ".png") (Data.Text.pack "png") ([]::[([Char],[Char])])

toString :: [String] -> String
toString [] = ""
toString (a:as) = "_" ++ a ++ (toString as)

getTimestamp :: IO [Char]
getTimestamp = formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getZonedTime

asPoint :: Point2D -> Point
asPoint p = (round(fst(p))+500, round(snd(p))+500)

