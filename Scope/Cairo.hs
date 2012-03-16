{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-unused-do-bind #-}

module Scope.Cairo (
    -- * Types
      ViewCairo(..)

    -- * Scope ViewCairo
    , scopeCairoNew

    , scopeModifyMUpdate
    , scopeModifyUpdate

    , modifyIORefM

    -- * Utils
    , keepState
) where

import Prelude hiding (catch)

import Control.Applicative ((<$>))
import Control.Monad.CatchIO
import Control.Monad.Reader
import Data.IORef
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo.Internal (Render(..))
import Graphics.Rendering.Cairo.Types (Cairo)
import qualified Graphics.UI.Gtk as G

import Scope.Types hiding (m, b)
import Scope.View

----------------------------------------------------------------------

data ViewCairo = ViewCairo
    { frame  :: G.VBox
    , canvas :: G.DrawingArea
    , adj    :: G.Adjustment
    }

scopeCairoNew :: IO (IORef (Scope ViewCairo))
scopeCairoNew = do
    vbox <- G.vBoxNew False 0

    adj <- G.adjustmentNew (0.0) (0.0) (1.0) (0.1) 1.0 1.0

    drawingArea <- G.drawingAreaNew
    G.boxPackStart vbox drawingArea G.PackGrow 0

    scrollbar <- G.hScrollbarNew adj
    G.boxPackStart vbox scrollbar G.PackNatural 0

    scopeRef <- newIORef $ scopeNew (ViewCairo vbox drawingArea adj)

    adj `G.onValueChanged` (scroll scopeRef)

    drawingArea `G.on` G.buttonPressEvent $ G.tryEvent $ buttonDown scopeRef
    drawingArea `G.on` G.buttonReleaseEvent $ G.tryEvent $ buttonRelease scopeRef
    drawingArea `G.on` G.scrollEvent $ G.tryEvent $ wheel scopeRef
    drawingArea `G.on` G.motionNotifyEvent $ G.tryEvent $ motion scopeRef
    drawingArea `G.on` G.keyPressEvent $ G.tryEvent $ keyDown scopeRef
    G.widgetAddEvents drawingArea
        [ G.KeyPressMask
        , G.KeyReleaseMask
        , G.PointerMotionMask
        , G.ButtonMotionMask
        , G.ScrollMask
        ]

    G.widgetSetCanFocus drawingArea True

    return scopeRef

----------------------------------------------------------------

scopeModifyMUpdate :: IORef (Scope ViewCairo)
                   -> (Scope ViewCairo -> IO (Scope ViewCairo))
                   -> IO ()
scopeModifyMUpdate ref f = do
    modifyIORefM ref f
    viewCairoUpdate =<< view <$> readIORef ref

scopeModifyUpdate :: IORef (Scope ViewCairo)
                  -> (View ViewCairo -> View ViewCairo)
                  -> IO ()
scopeModifyUpdate ref f = do
    modifyIORef ref (scopeModifyView f)
    viewCairoUpdate =<< view <$> readIORef ref

viewCairoUpdate :: View ViewCairo -> IO ()
viewCairoUpdate View{..} = do
    G.adjustmentSetValue (adj viewUI) (toDouble viewX1)
    G.adjustmentSetPageSize (adj viewUI) $ toDouble (distance viewX1 viewX2)
    G.widgetQueueDraw (canvas viewUI)

modifyIORefM :: MonadIO m => IORef a -> (a -> m a) -> m ()
modifyIORefM ref f = do
    x <- liftIO $ readIORef ref
    x' <- f x
    liftIO $ writeIORef ref x'

----------------------------------------------------------------------

instance MonadCatchIO C.Render where
  m `catch` f = mapRender (\m' -> m' `catch` \e -> runRender $ f e) m
  block       = mapRender block
  unblock     = mapRender unblock

mapRender :: (ReaderT Cairo IO m1 -> ReaderT Cairo IO m) -> Render m1 -> Render m
mapRender f = Render . f . runRender

instance ScopeRender C.Render where
    renderCmds = keepState . mapM_ cairoDrawCmd

----------------------------------------------------------------------

cairoDrawCmd :: DrawCmd -> C.Render ()
cairoDrawCmd (SetRGB  r g b)   = C.setSourceRGB  r g b
cairoDrawCmd (SetRGBA r g b a) = C.setSourceRGBA r g b a
cairoDrawCmd (MoveTo (x,y))    = C.moveTo x y

cairoDrawCmd (LineTo (x,y))    = do
    C.lineTo x y
    C.stroke

cairoDrawCmd (FillPoly [])         = return ()
cairoDrawCmd (FillPoly ((x,y):ps)) = do
    C.moveTo x y
    mapM_ (uncurry C.lineTo) ps
    C.fill

----------------------------------------------------------------

keepState :: C.Render t -> C.Render ()
keepState render = do
  C.save
  _ <- render
  C.restore

----------------------------------------------------------------

scroll :: IORef (Scope ViewCairo) -> IO ()
scroll ref = do
    val <- G.adjustmentGetValue =<< adj . viewUI . view <$> readIORef ref
    scopeModifyUpdate ref (viewMoveTo val)

----------------------------------------------------------------

_canvasToScreen :: G.DrawingArea -> CanvasX -> IO ScreenX
_canvasToScreen c (CanvasX cX) = do
    (width, _height) <- G.widgetGetSize c
    return $ ScreenX (fromIntegral width * cX)

screenToCanvas :: ViewCairo -> ScreenX -> IO CanvasX
screenToCanvas vc (ScreenX sX) = do
    (width, _height) <- G.widgetGetSize (canvas vc)
    return $ CanvasX (sX / fromIntegral width)

----------------------------------------------------------------

buttonDown :: IORef (Scope ViewCairo) -> G.EventM G.EButton ()
buttonDown ref = do
    (x, _y) <- G.eventCoordinates
    liftIO $ do
        vc <- viewUI . view <$> readIORef ref
        cX <- screenToCanvas vc (ScreenX x)
        scopeModifyUpdate ref (viewButtonDown cX)

buttonRelease :: IORef (Scope ViewCairo) -> G.EventM G.EButton ()
buttonRelease ref = liftIO $ modifyIORef ref (scopeModifyView viewButtonRelease)

motion :: IORef (Scope ViewCairo) -> G.EventM G.EMotion ()
motion ref = do
    (x, _y) <- G.eventCoordinates
    liftIO $ do
        View{..} <- view <$> readIORef ref
        cX <- screenToCanvas viewUI (ScreenX x)
        scopeModifyUpdate ref (viewButtonMotion cX)

wheel :: IORef (Scope ViewCairo) -> G.EventM G.EScroll ()
wheel ref = do
    (x, _y) <- G.eventCoordinates
    dir <- G.eventScrollDirection
    liftIO $ do
        scope <- readIORef ref
        let View{..} = view scope
        cX <- screenToCanvas viewUI (ScreenX x)
        case dir of
            G.ScrollUp   -> scopeModifyUpdate ref (viewZoomInOn cX 1.2)
            G.ScrollDown -> scopeModifyUpdate ref (viewZoomOutOn cX 1.2)
            _            -> return ()

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

keyDown :: IORef (Scope ViewCairo) -> G.EventM G.EKey ()
keyDown ref = do
    v <- G.eventKeyVal
    -- n <- G.eventKeyName
    -- liftIO . putStrLn $ printf "Key %s (%d) pressed" n v
    let f = case v of
                XK_Home -> Just viewMoveStart
                XK_End  -> Just viewMoveEnd
                XK_Up   -> Just $ viewZoomIn 2.0
                XK_Down -> Just $ viewZoomOut 2.0
                XK_Left  -> Just viewMoveRight
                XK_Right -> Just viewMoveLeft
                _ -> Nothing

    maybe (return ()) (liftIO . scopeModifyUpdate ref) f

