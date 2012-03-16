{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-unused-do-bind #-}

module Scope.Cairo (
    -- * Types
      ViewCairo(..)

    -- * Scope ViewCairo
    , scopeCairoNew
    , viewCairoInit

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
    { canvas :: G.DrawingArea
    , adj    :: G.Adjustment
    }

scopeCairoNew :: G.DrawingArea -> G.Adjustment -> IO (IORef (Scope ViewCairo))
scopeCairoNew c adj = do
    scopeRef <- newIORef $ scopeNew (viewCairoInit c adj)
    adj `G.onValueChanged` (scroll scopeRef)
    return scopeRef

viewCairoInit :: G.DrawingArea -> G.Adjustment -> ViewCairo
viewCairoInit c a = ViewCairo c a

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

