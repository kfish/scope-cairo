{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Scope.Cairo (
    -- * Types
      ViewCairo(..)

    -- * Scope ViewCairo
    , scopeCairoNew

    -- * Utils
    , keepState

    , module Scope.Cairo.IORef
) where

import Control.Monad.Reader
import Data.IORef
import qualified Graphics.UI.Gtk as G

import Scope.Types hiding (m, b)

import Scope.Cairo.Events
import Scope.Cairo.IORef
import Scope.Cairo.Render
import Scope.Cairo.Types

----------------------------------------------------------------------

scopeCairoNew :: IO (IORef (Scope ViewCairo))
scopeCairoNew = do
    vbox <- G.vBoxNew False 0

    adj <- G.adjustmentNew (0.0) (0.0) (1.0) (0.1) 1.0 1.0

    drawingArea <- G.drawingAreaNew
    G.boxPackStart vbox drawingArea G.PackGrow 0

    scrollbar <- G.hScrollbarNew adj
    G.boxPackStart vbox scrollbar G.PackNatural 0

    scopeRef <- newIORef $ scopeNew (ViewCairo vbox drawingArea adj)

    scopeCairoDefaultEvents scopeRef

    return scopeRef

