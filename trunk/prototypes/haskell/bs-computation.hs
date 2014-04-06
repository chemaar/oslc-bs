module Main where

data Tree a = Empty
            | Node a [Tree a] 
	deriving (Eq,Ord,Show,Read)


main :: IO ()
main  =
   do
      putStrLn "Begin program"
      let aMyTree = Node 5 [Node 3 []] 
      print aMyTree
      print (mysum aMyTree)
      print (height aMyTree)
      putStrLn "End program"


mysum :: Num a => Tree a -> a
mysum Empty     = 0
mysum (Node a []) = a 
mysum (Node a [l]) = a + mysum l

height :: Num a => Tree a -> a
height Empty     = 0
height (Node a []) = 1
height (Node a [l]) = 1 + height l

