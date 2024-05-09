{-# LANGUAGE OverloadedStrings #-}
module Convert where

import qualified Data.Text as T

import File (fileRead, fileWrite)

data Tree a = Node a [Tree a] deriving (Eq,Show)
type Forest a = [Tree a]

--instance Show a => (Show (Tree a)) where
--  show (Node t fr) = show t<>" ["<>concatMap show fr<>"] " 

lstToTree :: Forest T.Text -> [T.Text] -> Tree T.Text
lstToTree [] [] = Node T.empty [] 
lstToTree [] (x:xs) = lstToTree [Node x []] xs
lstToTree frs [] = head frs
lstToTree frs@((Node tx fr):trs) (x:xs) =
   case x of
    "(" -> lstToTree (Node "(" [] :frs) xs 
    ")" -> let (Node tx' fs':trs') = trs 
            in if null fs' then lstToTree [Node tx' fr] xs
                           else let ((Node t _):tl) = fs'
                                 in lstToTree (Node tx' (Node t fr:tl):trs') xs 
    t -> lstToTree (Node tx (Node t []:fr):trs) xs 

convC :: IO ()  
convC = do
  ws <- T.words . addSpace <$> fileRead cookFile
  let (Node _ fr) = lstToTree [Node "root" []] ws
  let frStr = show fr
  let mainText = T.pack frStr 
  let headerText = "{-# LANGUAGE OverloadedStrings #-}\nmodule CookTree where\n\nimport Data.Tree (Tree(..),Forest)\nimport qualified Data.Text as T\n\ncookTree :: Forest T.Text\ncookTree = "
  let wholeText = headerText <> mainText
  fileWrite outputFile wholeText
  --mapM_ (putStrLn . T.unpack) ws

addSpace :: T.Text -> T.Text
addSpace = T.replace "(" " ( " . T.replace ")" " ) "

cookFile :: FilePath
cookFile = "../../assets/cookTree.txt"

outputFile :: FilePath
outputFile = "CookTree.hs"
