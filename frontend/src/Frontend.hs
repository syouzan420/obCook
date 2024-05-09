module Frontend where

import Common.Api (commonStuff)
import Common.Route (FrontendRoute (..))
--import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Fix (MonadFix)
import qualified Data.Text as T
import Data.Tree (Tree(..),Forest)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Route (R)
import Obelisk.Generated.Static (static)

import Reflex.Dom.Core 
  ( text, dynText, el, elClass, elAttr, blank , (=:) 
  , never, accumDyn
  , DomBuilder, MonadHold, PostBuild
--  , Prerender, Performable, PerformEvent, TriggerEvent
  )

import CWidget (elChara, elSpace)

import Buttons (elChoice)
import CookTree (cookTree)


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = frontendHead 
  , _frontend_body = frontendBody 
  }

frontendHead :: DomBuilder t m => m ()
frontendHead = do
  el "title" $ text "Cook"
  elAttr
    "meta"
    ( "name" =: "viewport"
        <> "contents" =: "width=device-width, initial-scale=1.0"
    )
    blank

  elAttr
    "link"
    ("href" =: $(static "main.css")
      <> "type" =: "text/css"
      <> "rel" =: "stylesheet")
    blank

frontendBody :: 
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
--  , MonadIO (Performable m)
--  , PerformEvent t m
--  , Prerender t m
--  , TriggerEvent t m
  ) => m ()
frontendBody = do 
  el "h1" $ text "Button Test"
  el "p" $ text $ T.pack commonStuff
  elSpace
  elChara
  elSpace
  elClass "div" "butn" $ do
    evCoice <- elChoice never (Node T.empty cookTree) 
    dynText =<< accumDyn (\a b -> a <> "..." <> b) T.empty evCoice


testNodes :: Forest T.Text
testNodes = [Node "a" [Node "d" [],Node "e" [Node "h" []]],Node "b" [Node "f" [Node "g" []]],Node "c" []]
