{-# LANGUAGE OverloadedStrings #-}
module CookTree where

import Data.Tree (Tree(..),Forest)
import qualified Data.Text as T

cookTree :: Forest T.Text
cookTree = [Node "q" [],Node "j" [Node "p" [],Node "l" [Node "o" [],Node "n" [],Node "m" []],Node "k" []],Node "\12354" [Node "f" [Node "i" [],Node "h" [],Node "\12358" []],Node "c" [Node "e" [],Node "d" []],Node "b" []]]