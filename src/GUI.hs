{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-} -- TimeStampable
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind -fno-warn-orphans #-}
--
-- Based on Gtk2Hs/demo/cairo/Drawing2.hs 
-- Author: Johan Bockgård <bojohan@dd.chalmers.se>
--
-- This code is in the public domain.
--

module GUI (
    guiMain
) where

import Prelude hiding (catch)

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad.CatchIO
import Control.Monad.Reader
import Data.Function (on)
import qualified Data.IntMap as IM
import Data.IORef
import Data.List (groupBy)
import Data.Maybe
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.ZoomCache.Numeric
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo.Internal (Render(..))
import Graphics.Rendering.Cairo.Types (Cairo)
import qualified Graphics.Rendering.Cairo.Matrix as M
import qualified System.Random.MWC as MWC

-- import Text.Printf

import Paths_scope as My
import Scope.View
import Scope.Types

----------------------------------------------------------------------

windowWidth, windowHeight :: Int
windowWidth   = 500
windowHeight  = 500

{-
-- Write image to file
_writePng :: IO ()
_writePng =
  C.withImageSurface C.FormatARGB32 width height $ \ result -> do
      C.renderWith result $ plotWindow width height
      C.surfaceWriteToPNG result "Draw.png"
  where width  = windowWidth
        height = windowHeight
-}

-- Display image in window
guiMain :: Chan String -> [String] -> IO ()
guiMain chan args = do
  _ <- G.initGUI

  window <- G.windowNew
  G.widgetSetSizeRequest window windowWidth windowHeight
  G.widgetSetAppPaintable window True
  G.widgetSetDoubleBuffered window True

  vbox <- G.vBoxNew False 0
  G.containerAdd window vbox

  ui <- G.uiManagerNew

  filename <- My.getDataFileName "data/actions.ui"
  G.uiManagerAddUiFromFile ui filename

  let getWidget = fmap fromJust . G.uiManagerGetWidget ui

  -- Menubar
  fma <- G.actionNew "FMA" "File" Nothing Nothing
  ema <- G.actionNew "EMA" "Edit" Nothing Nothing
  vma <- G.actionNew "VMA" "View" Nothing Nothing
  hma <- G.actionNew "HMA" "Help" Nothing Nothing

  -- File menu
  newa <- G.actionNew "NEWA" "New" (Just "Just a Stub") (Just G.stockNew)
  newa `G.on` G.actionActivated $ myNew
  opena <- G.actionNew "OPENA" "Open" (Just "Just a Stub") (Just G.stockOpen)
  savea <- G.actionNew "SAVEA" "Save" (Just "Just a Stub") (Just G.stockSave)
  saveasa <- G.actionNew "SAVEASA" "Save As" (Just "Just a Stub") (Just G.stockSaveAs)
  quita <- G.actionNew "QUITA" "Quit" (Just "Just a Stub") (Just G.stockQuit)
  quita `G.on` G.actionActivated $ myQuit window chan

  let fChooser action label = G.fileChooserDialogNew Nothing (Just window) action
          [(G.stockCancel, G.ResponseCancel), (label, G.ResponseAccept)]

  openDialog <- fChooser G.FileChooserActionOpen G.stockOpen
  demoPath <- My.getDataFileName "demo"
  G.fileChooserSetCurrentFolder openDialog demoPath

  opena `G.on` G.actionActivated $ G.widgetShow openDialog

  saveDialog <- fChooser G.FileChooserActionSave G.stockSave
  savea `G.on` G.actionActivated $ G.widgetShow saveDialog
  saveasa `G.on` G.actionActivated $ G.widgetShow saveDialog
  saveDialog `G.on` G.response $ myFileSave saveDialog

  -- Edit menu
  cut1 <- G.actionNew "cut1" "Cut" (Just "Just a Stub") (Just G.stockCut)
  cut1 `G.on` G.actionActivated $ myCut
  copy1 <- G.actionNew "copy1" "Copy" (Just "Just a Stub") (Just G.stockCopy)
  copy1 `G.on` G.actionActivated $ myCopy
  paste1 <- G.actionNew "paste1" "Paste" (Just "Just a Stub") (Just G.stockPaste)
  paste1 `G.on` G.actionActivated $ myPaste
  delete1 <- G.actionNew "delete1" "Delete" (Just "Just a Stub") (Just G.stockDelete)
  delete1 `G.on` G.actionActivated $ myDelete

  -- Help menu
  -- About dialog
  aboutdialog <- G.aboutDialogNew
  abouta <- G.actionNew "ABOUTA" "About" (Just "Just a Stub") Nothing
  abouta `G.on` G.actionActivated $ G.widgetShow aboutdialog
  aboutdialog `G.on` G.response $ const $ G.widgetHide aboutdialog

  -- Action group
  agr <- G.actionGroupNew "AGR"
  mapM_ (G.actionGroupAddAction agr) [fma, ema, vma, hma]
  mapM_ (\act -> G.actionGroupAddActionWithAccel agr act Nothing)
      [ newa, opena, savea, saveasa, quita
      , cut1, copy1, paste1, delete1
      , abouta
      ]

  G.uiManagerInsertActionGroup ui agr 0

  menubar <- getWidget "/ui/menubar1"
  G.boxPackStart vbox menubar G.PackNatural 0

  adj <- G.adjustmentNew (0.0) (0.0) (1.0) (0.1) 1.0 1.0
  drawingArea <- G.drawingAreaNew

  let scope = scopeNew drawingArea adj
  scopeRef <- newIORef scope

  mapM_ (modifyIORefM scopeRef . addLayersFromFile) args
  openDialog `G.on` G.response $ myFileOpen scopeRef openDialog

  adj `G.onValueChanged` (scroll scopeRef)

  G.boxPackStart vbox drawingArea G.PackGrow 0

  drawingArea `G.on` G.buttonPressEvent $ G.tryEvent $ buttonDown scopeRef
  drawingArea `G.on` G.buttonReleaseEvent $ G.tryEvent $ buttonRelease scopeRef
  drawingArea `G.on` G.scrollEvent $ G.tryEvent $ wheel scopeRef
  drawingArea `G.on` G.motionNotifyEvent $ G.tryEvent $ motion scopeRef
  drawingArea `G.on` G.keyPressEvent $ G.tryEvent $ keyDown scopeRef
  G.widgetAddEvents drawingArea
    [ G.KeyPressMask
    , G.KeyReleaseMask
    -- , G.PointerMotionMask
    , G.ButtonMotionMask
    , G.ScrollMask
    ]

  G.widgetSetCanFocus drawingArea True

  drawingArea `G.on` G.exposeEvent $ G.tryEvent $ do
    liftIO $ updateCanvas scopeRef
    return ()

  scrollbar <- G.hScrollbarNew adj
  G.boxPackStart vbox scrollbar G.PackNatural 0

  statusbar <- G.statusbarNew
  G.boxPackStart vbox statusbar G.PackNatural 0

  G.onDestroy window ((myWriteChan chan "quit") >> G.mainQuit)

  G.widgetShowAll window
  G.mainGUI

myQuit :: G.WidgetClass cls => cls -> Chan String -> IO ()
myQuit window chan = do
  G.widgetDestroy window
  myWriteChan chan "quit"

myWriteChan :: Chan String -> String -> IO ()
myWriteChan chan s = do writeChan chan s
                        yield
myNew :: IO ()
myNew = putStrLn "New"

myFileOpen :: IORef Scope -> G.FileChooserDialog -> G.ResponseId -> IO ()
myFileOpen scopeRef fcdialog response = do
  case response of
    G.ResponseAccept -> do
        Just filename <- G.fileChooserGetFilename fcdialog
        scopeModifyMUpdate scopeRef (addLayersFromFile filename)
    _ -> return ()
  G.widgetHide fcdialog

myFileSave :: G.FileChooserDialog -> G.ResponseId -> IO ()
myFileSave _ _ = return ()

myCut :: IO ()
myCut = putStrLn "Cut"

myCopy :: IO ()
myCopy = putStrLn "Copy"

myPaste :: IO ()
myPaste = putStrLn "Paste"

myDelete :: IO ()
myDelete = putStrLn "Delete"

updateCanvas :: IORef Scope -> IO Bool
updateCanvas ref = do
    scope <- readIORef ref
    let c = canvas . view $ scope
    win <- G.widgetGetDrawWindow c
    (width, height) <- G.widgetGetSize c
    G.renderWithDrawable win $ plotWindow width height scope
    return True

----------------------------------------------------------------

scopeAlign :: IORef Scope -> CanvasX -> DataX -> IO ()
scopeAlign ref cx dx = scopeModifyUpdate ref (scopeModifyView (viewAlign cx dx))

scopeMoveLeft :: IORef Scope -> IO ()
scopeMoveLeft ref = do
    scope <- readIORef ref
    let View{..} = view scope
    scopeAlign ref (CanvasX 0.0) viewX2

scopeMoveRight :: IORef Scope -> IO ()
scopeMoveRight ref = do
    scope <- readIORef ref
    let View{..} = view scope
    scopeAlign ref (CanvasX 1.0) viewX1

----------------------------------------------------------------

scopeZoomIn :: IORef Scope -> Double -> IO ()
scopeZoomIn ref = scopeZoomInOn ref (CanvasX 0.5)

scopeZoomOut :: IORef Scope -> Double -> IO ()
scopeZoomOut ref = scopeZoomOutOn ref (CanvasX 0.5)

scopeZoomInOn :: IORef Scope -> CanvasX -> Double -> IO ()
scopeZoomInOn ref focus mult = scopeZoomOutOn ref focus (1.0/mult)

scopeZoomOutOn :: IORef Scope -> CanvasX -> Double -> IO ()
scopeZoomOutOn ref focus mult =
    scopeModifyUpdate ref (scopeModifyView (viewZoomOutOn focus mult))

scopeModifyMUpdate :: IORef Scope -> (Scope -> IO Scope) -> IO ()
scopeModifyMUpdate ref f = do
    modifyIORefM ref f
    View{..} <- view <$> readIORef ref
    G.adjustmentSetValue adj (toDouble viewX1)
    G.adjustmentSetPageSize adj $ toDouble (distance viewX1 viewX2)
    G.widgetQueueDraw canvas

scopeModifyUpdate :: IORef Scope -> (Scope -> Scope) -> IO ()
scopeModifyUpdate ref f = do
    modifyIORef ref f
    View{..} <- view <$> readIORef ref
    G.adjustmentSetValue adj (toDouble viewX1)
    G.adjustmentSetPageSize adj $ toDouble (distance viewX1 viewX2)
    G.widgetQueueDraw canvas

----------------------------------------------------------------

_canvasToScreen :: G.DrawingArea -> CanvasX -> IO ScreenX
_canvasToScreen c (CanvasX cX) = do
    (width, _height) <- G.widgetGetSize c
    return $ ScreenX (fromIntegral width * cX)

screenToCanvas :: G.DrawingArea -> ScreenX -> IO CanvasX
screenToCanvas c (ScreenX sX) = do
    (width, _height) <- G.widgetGetSize c
    return $ CanvasX (sX / fromIntegral width)

----------------------------------------------------------------

buttonDown :: IORef Scope -> G.EventM G.EButton ()
buttonDown ref = do
    (x, _y) <- G.eventCoordinates
    liftIO $ do
        c <- canvas . view <$> readIORef ref
        cX <- screenToCanvas c (ScreenX x)
        modifyIORef ref (scopeModifyView (viewButtonDown cX))

buttonRelease :: IORef Scope -> G.EventM G.EButton ()
buttonRelease ref = liftIO $ modifyIORef ref (scopeModifyView viewButtonRelease)

motion :: IORef Scope -> G.EventM G.EMotion ()
motion ref = do
    (x, _y) <- G.eventCoordinates
    liftIO $ do
        View{..} <- view <$> readIORef ref
        cX <- screenToCanvas canvas (ScreenX x)
        scopeModifyUpdate ref $ scopeModifyView (viewButtonMotion cX)

wheel :: IORef Scope -> G.EventM G.EScroll ()
wheel ref = do
    (x, _y) <- G.eventCoordinates
    dir <- G.eventScrollDirection
    liftIO $ do
        scope <- readIORef ref
        let View{..} = view scope
        cX <- screenToCanvas canvas (ScreenX x)
        case dir of
            G.ScrollUp   -> scopeZoomInOn  ref cX 1.2
            G.ScrollDown -> scopeZoomOutOn ref cX 1.2
            _            -> return ()

scroll :: IORef Scope -> IO ()
scroll ref = do
    val <- G.adjustmentGetValue =<< adj . view <$> readIORef ref
    scopeModifyUpdate ref $ scopeModifyView (viewMoveTo val)

----------------------------------------------------------------

-- Some keys we are interested in, from:
-- http://cgit.freedesktop.org/xorg/proto/x11proto/plain/keysymdef.h
#define XK_Home                          0xff50
#define XK_Left                          0xff51  /* Move left, left arrow */
#define XK_Up                            0xff52  /* Move up, up arrow */
#define XK_Right                         0xff53  /* Move right, right arrow */
#define XK_Down                          0xff54  /* Move down, down arrow */
#define XK_Page_Up                       0xff55
#define XK_Page_Down                     0xff56
#define XK_End                           0xff57  /* EOL */

keyDown :: IORef Scope -> G.EventM G.EKey ()
keyDown ref = do
    v <- G.eventKeyVal
    -- n <- G.eventKeyName
    -- liftIO . putStrLn $ printf "Key %s (%d) pressed" n v
    liftIO $ case v of
        XK_Home -> scopeAlign ref (CanvasX 0.0) (DataX 0.0)
        XK_End  -> scopeAlign ref (CanvasX 1.0) (DataX 1.0)
        XK_Up   -> scopeZoomIn  ref 2.0
        XK_Down -> scopeZoomOut ref 2.0
        XK_Left  -> scopeMoveRight ref
        XK_Right -> scopeMoveLeft ref
        _ -> return ()

----------------------------------------------------------------

foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

keepState :: C.Render t -> C.Render ()
keepState render = do
  C.save
  _ <- render
  C.restore

----------------------------------------------------------------

plotWindow :: Int -> Int -> Scope -> C.Render ()
plotWindow width height scope = do
    prologue width height (view scope)
    plotLayers scope

-- Set up stuff
prologue :: Int -> Int -> View -> C.Render ()
prologue wWidth wHeight View{..} = do
  -- Define viewport coords as (-1.0, -1.0) - (1.0, 1.0)
  let width   = 1.0
      height  = 2.0
      xmax    = 1.0
      xmin    = 0.0
      ymax    = 1.0
      ymin    = -1.0
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

  grid xmin xmax ymin ymax


-- Grid and axes
grid :: Double -> Double -> Double -> Double -> C.Render ()
grid xmin xmax ymin ymax =
  keepState $ do
  C.setSourceRGBA 0 0 0 0.7
  -- axes
  C.moveTo 0 ymin; C.lineTo 0 ymax; C.stroke
  C.moveTo xmin 0; C.lineTo xmax 0; C.stroke
  -- grid
  C.setDash [0.01, 0.99] 0
  foreach [xmin .. xmax] $ \ x ->
      do C.moveTo x ymin
         C.lineTo x ymax
         C.stroke

----------------------------------------------------------------

instance MonadCatchIO C.Render where
  m `catch` f = mapRender (\m' -> m' `catch` \e -> runRender $ f e) m
  block       = mapRender block
  unblock     = mapRender unblock

mapRender :: (ReaderT Cairo IO m1 -> ReaderT Cairo IO m) -> Render m1 -> Render m
mapRender f = Render . f . runRender

----------------------------------------------------------------

class TimeStampable a where
    timeStamp :: a -> TimeStamp

instance TimeStampable (TimeStamp, a) where
    timeStamp (ts, _) = ts

instance TimeStampable (Summary a) where
    timeStamp = summaryEntry

----------------------------------------------------------------

plotLayers :: Scope -> C.Render ()
plotLayers scope = mapM_ f layersByFile
    where
        f :: [ScopeLayer] -> C.Render ()
        f ls = keepState $ plotFileLayers (fn . head $ ls) ls scope
        layersByFile = groupBy ((==) `on` fn) (layers scope)
        fn (ScopeLayer l) = filename l

plotFileLayers :: FilePath -> [ScopeLayer] -> Scope -> C.Render ()
plotFileLayers path layers scope =
    I.fileDriverRandom (I.joinI $ enumCacheFile identifiers (I.sequence_ is)) path
    where
        identifiers = standardIdentifiers
        is = map (plotLayer scope) layers

plotLayer :: Scope -> ScopeLayer -> I.Iteratee [Stream] Render ()
plotLayer scope (ScopeLayer Layer{..}) =
    I.joinI . filterTracks [trackNo] . I.joinI . convEnee $ foldData
    where
        v@View{..} = view scope

        foldData = do
            dropWhileB (before (viewStartTime scope v))
            I.joinI . I.takeWhileE (before (viewEndTime scope v)) $ render plotter

        render (LayerMap f) = do
            d0'm <- I.tryHead
            case d0'm of
                Just d0 -> I.foldM renderMap (toX d0) >> return ()
                Nothing -> return ()
            where
                renderMap x0 d = do
                    let x = toX d
                    f x0 (x-x0) d
                    return x
        render (LayerFold f b00) = do
            d0'm <- I.tryHead
            case d0'm of
                Just d0 -> I.foldM renderFold (toX d0, b00) >> return ()
                Nothing -> return ()
            where
                renderFold (x0, b0) d = do
                    let x = toX d
                    b <- f x0 (x-x0) b0 d
                    return (x, b)

        toX :: Timestampable a => a -> Double
        toX = toDouble . timeStampToCanvas scope . fromJust . timestamp

-- |Skip all elements while the predicate is true, but also return the last false element
--
-- The analogue of @List.dropWhile@
dropWhileB :: (Monad m, LL.ListLike s el) => (el -> Bool) -> I.Iteratee s m ()
dropWhileB p = I.liftI step
  where
    step (I.Chunk str)
      | LL.null left = I.liftI step
      | otherwise    = I.idone () (I.Chunk left)
      where
        left = llDropWhileB p str
    step stream      = I.idone () stream
{-# INLINE dropWhileB #-}

{- | Drops all elements form the start of the list that satisfy the
       function. -}
llDropWhileB :: LL.ListLike full item => (item -> Bool) -> full -> full
llDropWhileB = dw LL.empty
    where
        dw prev func l
            | LL.null l = LL.empty
            | func (LL.head l) = dw (LL.take 1 l) func (LL.tail l)
            | otherwise = LL.append prev l

----------------------------------------------------------------------
-- Raw data

plotRaw :: Double -> LayerFoldFunc (TimeStamp, Double) (Maybe Double)
plotRaw yR x w Nothing (ts, y) = plotRaw yR x w (Just y) (ts, y)
plotRaw yR x w (Just y0) (_ts, y) = do
    C.moveTo x     y0
    C.lineTo (x+w) (y * 2.0 {- (viewY2 v - viewY1 v)-} / yR)
    return (Just y)

----------------------------------------------------------------------
-- Summary data

plotSummary :: Double -> Double -> Double -> Double
            -> LayerFoldFunc (Summary Double) (Maybe (Summary Double))
plotSummary dYRange r g b x w Nothing s =
    plotSummary dYRange r g b x w (Just s) s
plotSummary dYRange r g b x w (Just s0) s = do
    C.setSourceRGBA r g b 0.3
    C.moveTo x     (y (numMax sd0))
    C.lineTo (x+w) (y (numMax sd))
    C.lineTo (x+w) (y (numMin sd))
    C.lineTo x     (y (numMin sd0))
    C.fill

    C.setSourceRGB (r*0.6) (g*0.6) (b*0.6)
    C.moveTo x     (y (numAvg sd0))
    C.lineTo (x+w) (y (numAvg sd))
    C.stroke
    return (Just s)
    where
        sd0 = summaryData s0
        sd = summaryData s
        y v = v * 4.0 / dYRange

----------------------------------------------------------------------

scopeModifyView :: (View -> View) -> Scope -> Scope
scopeModifyView f scope = scope{ view = f (view scope) }

----------------------------------------------------------------------
-- Random, similar colors

type RGB = (Double, Double, Double)

genColor :: RGB -> Double -> MWC.GenIO -> IO RGB
genColor (r, g, b) a gen = do
    let a' = 1.0 - a
    r' <- MWC.uniformR (0.0, a') gen
    g' <- MWC.uniformR (0.0, a') gen
    b' <- MWC.uniformR (0.0, a') gen
    return (r*a + r', g*a + g', b*a * b')

genColors :: Int -> RGB -> Double -> IO [RGB]
genColors n rgb a = MWC.withSystemRandom (replicateM n . genColor rgb a)

----------------------------------------------------------------------

layersFromFile :: FilePath -> IO ([ScopeLayer], Maybe (TimeStamp, TimeStamp))
layersFromFile path = do
    tracks <- IM.keys . cfSpecs <$> I.fileDriverRandom (iterHeaders standardIdentifiers) path
    colors <- genColors (length tracks) (0.9, 0.9, 0.9) (0.5)
    foldl1 merge <$> mapM (\t -> I.fileDriverRandom (iterLayers t) path) (zip tracks colors)
    where
        merge :: ([ScopeLayer], Maybe (TimeStamp, TimeStamp))
              -> ([ScopeLayer], Maybe (TimeStamp, TimeStamp))
              -> ([ScopeLayer], Maybe (TimeStamp, TimeStamp))
        merge (ls1, bs1) (ls2, bs2) = (ls1 ++ ls2, mergeBounds bs1 bs2)

        iterLayers (trackNo, color) = layers trackNo color <$>
            wholeTrackSummaryDouble standardIdentifiers trackNo

        layers :: TrackNo -> RGB -> Summary Double -> ([ScopeLayer], Maybe (TimeStamp, TimeStamp))
        layers trackNo rgb s = ([ ScopeLayer (rawLayer trackNo s)
                                , ScopeLayer (sLayer trackNo rgb s)
                                ]
                               , Just (summaryEntry s, summaryExit s))

        rawLayer :: TrackNo -> Summary Double -> Layer (TimeStamp, Double)
        rawLayer trackNo s = Layer path trackNo (summaryEntry s) (summaryExit s)
            enumDouble (LayerFold (plotRaw (yRange s)) Nothing)

        sLayer :: TrackNo -> RGB -> Summary Double -> Layer (Summary Double)
        sLayer trackNo (r, g, b) s = Layer path trackNo (summaryEntry s) (summaryExit s)
            (enumSummaryDouble 1) (LayerFold (plotSummary (yRange s) r g b) Nothing)

        yRange :: Summary Double -> Double
        yRange s = 2 * ((abs . numMin . summaryData $ s) + (abs . numMax . summaryData $ s))

mergeBounds :: Ord a => Maybe (a, a) -> Maybe (a, a) -> Maybe (a, a)
mergeBounds a               Nothing         = a
mergeBounds Nothing         b               = b
mergeBounds (Just (a1, a2)) (Just (b1, b2)) = Just (min a1 b1, max a2 b2)

addLayersFromFile :: FilePath -> Scope -> IO Scope
addLayersFromFile path scope = do
    (newLayers, newBounds) <- layersFromFile path
    let oldBounds = bounds scope
        mb = mergeBounds oldBounds newBounds
        t = case oldBounds of
                Just ob -> if oldBounds == mb
                               then id
                               else scopeTransform (mkTSDataTransform ob (fromJust mb))
                _ -> id
    return $ (t scope) { layers = layers scope ++ newLayers
                       , bounds = mb
                       }

modifyIORefM :: IORef a -> (a -> IO a) -> IO ()
modifyIORefM ref f = do
    x <- readIORef ref
    x' <- f x
    writeIORef ref x'

----------------------------------------------------------------

