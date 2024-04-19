module Buttons (elButtons) where

import qualified Data.Text as T
import Data.Functor ((<&>))
import Reflex.Dom.Core (Event,DomBuilder,leftmost)

import CWidget (buttonClass)

elButtons :: DomBuilder t m => [T.Text] -> m (Event t T.Text)
elButtons txs = mapM (\tx -> (tx <$) <$> buttonClass "pad2" tx) txs <&> leftmost
