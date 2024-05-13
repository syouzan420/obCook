{-# LANGUAGE OverloadedStrings #-}
module CookTree where

import Data.Tree (Tree(..))
import qualified Data.Text as T

cookTree :: Tree T.Text
cookTree = Node "root" [Node "\12390\12395\12434" [Node "\12434" [],Node "\12395" [],Node "\12381\12375\12390" []],Node "\12377\12427" [Node "\28263\12363\12377" [],Node "\20837\12428\12427" []],Node "\23481\22120" [Node "\40634\32178" [],Node "\25163\37707" [],Node "\23567\30399" [Node "\40658" [Node "\23567" [],Node "\20013" []],Node "\30333" []],Node "\12393\12435\12406\12426" [Node "\38738" [],Node "\30333" [],Node "\40658" []]],Node "\26448\26009" [Node "\12383\12428" [Node "\36771\27833" [],Node "\40635\36771" [],Node "\32993\40635" [],Node "\21619\22092" [Node "30" [],Node "80" []],Node "\22633" [Node "10" [],Node "40" []],Node "\28611\21402" []],Node "\37326\33756" [Node "\22679\12375" [],Node "\36890\24120" []],Node "\40634" [Node "\21322\20998" [Node "\32048" [],Node "\22826" []],Node "\26222\36890" [Node "\32048" [],Node "\22826" []],Node "\22823\30427" [Node "\32048" [],Node "\22826" []]]]]