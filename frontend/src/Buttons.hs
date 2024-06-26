module Buttons (evElButtons,evElButtonsH,elChoice) where

import Control.Monad.Fix (MonadFix)
import qualified Data.Text as T
import Data.Functor ((<&>))
import Data.Tree (Tree(..),Forest)
import Reflex.Dom.Core (Event,DomBuilder,MonadHold,PostBuild,Dynamic
                       ,leftmost,toggle,widgetHold,now,updated,switchHold)

import CWidget (evElButton,evElButtonH)

evElButtons :: DomBuilder t m => Forest T.Text -> m (Event t (Tree T.Text))
evElButtons trs = 
  mapM (\tr@(Node tx _) -> (tr <$) <$> evElButton "pad2" tx) trs <&> leftmost

evElButtonsH :: 
  ( DomBuilder t m
  , PostBuild t m
  ) => Dynamic t Bool -> Forest T.Text -> m (Event t (Tree T.Text))
evElButtonsH dyB trs = 
  mapM (\tr@(Node tx _) -> (tr <$) <$> evElButtonH dyB "pad2" tx) trs <&> leftmost 

elChoice ::
  ( DomBuilder t m , PostBuild t m , MonadHold t m , MonadFix m
  ) => Event t T.Text -> Tree T.Text -> m (Event t T.Text) 
elChoice e (Node _ []) = pure e 
elChoice _ (Node _ frs) = mdo
  dyBool <- toggle True evBH 
  evBH <- evElButtonsH dyBool frs
  let evT = fmap (\(Node tx _) -> tx) evBH
  ev <- updated <$> widgetHold ((T.empty <$) <$> now) (elChoice evT <$> evBH)
  switchHold evT ev

