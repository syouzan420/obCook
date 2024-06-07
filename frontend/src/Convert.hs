{-# LANGUAGE OverloadedStrings #-}
module Convert where

import qualified Data.Text as T

import File (fileRead, fileWrite)

data Tree a = Node a [Tree a] deriving (Eq,Show)
type Forest a = [Tree a]

takeForest :: Tree a -> Forest a
takeForest (Node _ fr) = fr

--instance Show a => (Show (Tree a)) where
--  show (Node t fr) = show t<>" ["<>concatMap show fr<>"] " 

lstToTree :: [T.Text] -> Tree T.Text
lstToTree = head . foldl ltot [Node "root" []] 

ltot :: Forest T.Text -> T.Text -> Forest T.Text
ltot frs@((Node tx fr):trs) x = case x of  
  "(" -> Node "(" [] :frs
  ")" -> let ((Node tx' fr'):trs') = trs
          in if null fr' then Node tx' fr :trs  
                         else let ((Node tx'' _):trs'') = fr' 
                               in Node tx' (Node tx'' fr :trs'') :trs' 
  t   -> Node tx (Node t [] :fr) :trs


convC :: IO ()  
convC = do
  ws <- T.words . addSpace <$> fileRead cookFile
  let tr = lstToTree ws
  let frStr = show tr
  putStrLn frStr
  let mainText = T.pack frStr 
  let headerText = "{-# LANGUAGE OverloadedStrings #-}\nmodule CookTree where\n\nimport Data.Tree (Tree(..))\nimport qualified Data.Text as T\n\ncookTree :: Tree T.Text\ncookTree = "
  let wholeText = headerText <> mainText
  fileWrite outputFile wholeText

addSpace :: T.Text -> T.Text
addSpace = T.replace "(" " ( " . T.replace ")" " ) "

cookFile :: FilePath
cookFile = "../../assets/cookTree.txt"

outputFile :: FilePath
outputFile = "CookTree.hs"
