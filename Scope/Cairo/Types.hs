{-# OPTIONS -Wall #-}

module Scope.Cairo.Types (
    -- * Types
      ViewCairo(..)
) where

import qualified Graphics.UI.Gtk as G

----------------------------------------------------------------------

data ViewCairo = ViewCairo
    { frame  :: G.VBox
    , canvas :: G.DrawingArea
    , adj    :: G.Adjustment
    }

