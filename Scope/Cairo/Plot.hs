{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Scope.Cairo.Plot (
  -- * Handle an expose event
    updateCanvas

  -- * Write a PNG of the current view
  , writePng
) where

import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Data.Time (UTCTime, formatTime)
import Data.ZoomCache (TimeStamp(..), prettyTimeStamp)
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as M
import System.Locale (defaultTimeLocale)

import Scope.Layer
import Scope.Types
import Scope.View

import Scope.Cairo.IORef
import Scope.Cairo.Render
import Scope.Cairo.Types

----------------------------------------------------------------

updateCanvas :: IORef (Scope ViewCairo) -> IO Bool
updateCanvas ref = do
    scope <- readIORef ref
    let c = canvas . viewUI . view $ scope
    win <- G.widgetGetDrawWindow c
    (width, height) <- G.widgetGetSize c
    G.renderWithDrawable win $ plotWindow width height ref
    return True

writePng :: FilePath -> IORef (Scope ViewCairo) -> IO ()
writePng path ref = do
    scope <- readIORef ref
    let c = canvas . viewUI . view $ scope
    (width, height) <- G.widgetGetSize c
    C.withImageSurface C.FormatARGB32 width height $ \ result -> do
        C.renderWith result $ plotWindow width height ref
        C.surfaceWriteToPNG result path

----------------------------------------------------------------

plotWindow :: Int -> Int -> IORef (Scope ViewCairo) -> C.Render ()
plotWindow width height ref = do
    scope <- liftIO $ readIORef ref
    prologue width height
    modifyIORefM ref plotLayers
    plotTimeline scope
    plotCursor scope

-- Set up stuff
prologue :: Int -> Int -> C.Render ()
prologue wWidth wHeight = do
  -- Define viewport coords as (-1.0, -1.0) - (1.0, 1.0)
  let width   = 1.0
      height  = 2.0
      scaleX  = realToFrac wWidth  / width
      scaleY  = realToFrac wHeight / height

  -- style and color
  C.setLineCap C.LineCapRound
  C.setLineJoin C.LineJoinRound
  C.setLineWidth $ 1 / max scaleX scaleY
  C.setSourceRGBA 0.5 0.7 0.5 0.5

  -- Set up user coordinates
  C.scale scaleX scaleY
  -- center origin vertically
  C.translate 0 (height / 2)
  -- positive y-axis upwards
  let flipY = M.Matrix 1 0 0 (-1) 0 0
  C.transform flipY

----------------------------------------------------------------------

plotCursor :: Scope ViewCairo -> C.Render ()
plotCursor scope = maybe (return ()) f pointerX
    where
        View{..} = view scope
        f :: CanvasX -> C.Render ()
        f (CanvasX cX) = do
            C.setSourceRGBA 0 0.7 0 0.4
            C.moveTo cX (-1.0)
            C.lineTo cX 1.0
            C.stroke

----------------------------------------------------------------------

class Coordinate a => Timelineable a where
    timeLabel :: a -> String
    toCanvas :: Scope ui -> a -> CanvasX

instance Timelineable TimeStamp where
    timeLabel = prettyTimeStamp
    toCanvas = timeStampToCanvas

instance Timelineable UTCTime where
    timeLabel = formatTime defaultTimeLocale "%Y-%m-%d %T"
    toCanvas = utcToCanvas

plotTimeline :: Scope ViewCairo -> C.Render ()
plotTimeline scope = do
    case (dataToUTC scope viewX1, dataToUTC scope viewX2) of
        (Just s, Just e) -> do
            plotAllTicks s e
            plotAllLabels s e
        _ ->  case (dataToTimeStamp scope viewX1, dataToTimeStamp scope viewX2) of
            (Just s, Just e) -> do
                plotAllTicks s e
                plotAllLabels s e
            _                -> return ()
    maybe (return ()) plotArrow pointerX
    where
        View{..} = view scope

        plotAllTicks :: Timelineable a => a -> a -> C.Render ()
        plotAllTicks s e = do
            plotTicks 0.001 0.05 s e
            plotTicks 0.01 0.1 s e
            plotTicks 0.02 1.0 s e
            plotTicks 0.04 5.0 s e
            plotTicks 0.06 10.0 s e
            plotTicks 0.08 60.0 s e
            plotTicks 0.10 3600.0 s e

        plotTicks :: Timelineable a => Double -> Double -> a -> a -> C.Render ()
        plotTicks len step start end =
            when doDraw $ mapM_ (plotTick len start) (map fromDouble [s, s+step .. end'])
            where
                doDraw = (end' - start') / step < 100
                s = (fromIntegral (floor (start'/step) :: Integer)) * step
                start' = toDouble start
                end' = toDouble end

        plotTick :: Timelineable a => Double -> a -> a -> C.Render ()
        plotTick len _unify ts = do
            let (CanvasX cX) = toCanvas scope ts
            C.setSourceRGBA 0 0 0 1.0
            C.moveTo cX 0.90
            C.lineTo cX (0.90 + len)
            C.stroke

        plotAllLabels :: Timelineable a => a -> a -> C.Render ()
        plotAllLabels start end =
            mapM_ (\s -> plotLabels s start end) steps
            where
                readable x = let viz = (end' - start') / x in (viz < 5 && viz >= 1)
                steps = take 1 . filter readable $ [3600, 60, 10, 5, 1, 0.1, 0.05]
                start' = toDouble start
                end' = toDouble end

        plotLabels :: Timelineable a => Double -> a -> a -> C.Render ()
        plotLabels step start end = keepState $ do
            let flipY = M.Matrix 1 0 0 (-2.2) 0 0
            C.transform flipY

            let s = (fromIntegral (floor (start'/step) :: Integer)) * step
            mapM_ (plotLabel start . fromDouble) [s, s+step .. end']
            where
                start' = toDouble start
                end' = toDouble end

        plotLabel :: Timelineable a => a -> a -> C.Render ()
        plotLabel _unify ts = do
            let CanvasX cX = toCanvas scope ts
            drawString (timeLabel ts) cX (-0.44)

drawString :: String -> Double -> Double -> C.Render ()
drawString s x y = do
    C.selectFontFace "sans" C.FontSlantNormal C.FontWeightNormal
    C.setFontSize 0.02
    C.moveTo x y
    C.textPath s
    C.fillPreserve

plotArrow :: CanvasX -> C.Render ()
plotArrow (CanvasX cX) = do
    C.setSourceRGBA 0 0 0 0.9
    C.moveTo (cX-0.004) (0.99)
    C.lineTo (cX+0.004) (0.99)
    C.lineTo cX (0.98)
    C.fill


