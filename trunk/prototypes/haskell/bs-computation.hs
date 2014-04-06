module Main where
import Data.List 
--A tree is a list of values [a] in which we have ID,MTTF, MTTR, etc. and the children nodes [Tree t]
data Tree a = Empty
            | Node [a] [Tree a] 
	deriving (Eq,Ord,Show,Read)


main :: IO ()
main  =
   do
      putStrLn "Begin program"
      let aMyTree = Node [1,4,3] [Node [2,3,4][]]
      print aMyTree
      print (mysum aMyTree)
      print (height aMyTree)
      putStrLn "End program"

mysum :: Num a => Tree a -> a
mysum (Node a []) = a !! 1
mysum (Node a [l]) = a !! 1 + mysum l

height :: Num a => Tree a -> a
height (Node a []) = 1
height (Node a [l]) = 1 + height l

