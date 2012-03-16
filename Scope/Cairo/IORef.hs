{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Scope.Cairo.IORef (
      scopeModifyMUpdate
    , scopeModifyUpdate
    , modifyIORefM
) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO, liftIO)
import Data.IORef
import qualified Graphics.UI.Gtk as G

import Scope.Types hiding (m, b)

import Scope.Cairo.Types

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

