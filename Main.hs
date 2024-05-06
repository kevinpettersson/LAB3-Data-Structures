{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}
{-# LANGUAGE BlockArguments #-}
module Main where
  
import AATree

--------------------------------------------------------------------------------
main :: IO ()
main = do
  contents <- getContents
  let 
    wordList      = words contents
    tree          = foldr insert emptyTree wordList
    treeSize      = size tree
    treeHeight    = height tree
    treeChecked   = checkTree tree
    optimalHeight = ceiling (logBase 2 (fromIntegral (treeSize + 1))) - 1
    heightRatio   = fromIntegral treeHeight / fromIntegral optimalHeight
    first20       = take 20 (inorder tree)

  putStrLn ("Size: " ++ show treeSize)
  putStrLn ("Height: " ++ show treeHeight)
  putStrLn ("Optimal height: " ++ show optimalHeight)
  putStrLn ("Height / Optimal height " ++ show heightRatio) 
  putStrLn ("checkTree: " ++ show treeChecked)
  putStrLn ("First 20 words: " ++ show first20)


    
  -- split the data into words and build an AA tree
  -- use foldl

  -- calculate and print statistics
  -- use fromIntegral/ceiling/logBase


--------------------------------------------------------------------------------

