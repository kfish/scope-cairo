{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Scope.Cairo.Events (
      scopeCairoDefaultEvents
) where

import Prelude hiding (catch)

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.IORef
import qualified Graphics.UI.Gtk as G

import Scope.Types hiding (m, b)
import Scope.View

import Scope.Cairo.IORef
import Scope.Cairo.Types

----------------------------------------------------------------

scopeCairoDefaultEvents :: IORef (Scope ViewCairo) -> IO ()
scopeCairoDefaultEvents scopeRef = do
    ViewCairo{..} <- viewUI . view <$> readIORef scopeRef

    adj `G.onValueChanged` (scroll scopeRef)

    canvas `G.on` G.buttonPressEvent $ G.tryEvent $ buttonDown scopeRef
    canvas `G.on` G.buttonReleaseEvent $ G.tryEvent $ buttonRelease scopeRef
    canvas `G.on` G.scrollEvent $ G.tryEvent $ wheel scopeRef
    canvas `G.on` G.motionNotifyEvent $ G.tryEvent $ motion scopeRef
    canvas `G.on` G.keyPressEvent $ G.tryEvent $ keyDown scopeRef
    G.widgetAddEvents canvas
        [ G.KeyPressMask
        , G.KeyReleaseMask
        , G.PointerMotionMask
        , G.ButtonMotionMask
        , G.ScrollMask
        ]

    G.widgetSetCanFocus canvas True

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

