module TypDeklarationen where

data BinaryTree x = Leaf x
                  | Branch (BinaryTree x) (BinaryTree x)
                  deriving (Eq, Show) 


data BinTree a = Empty 
               | Knot (BinTree a) a (BinTree a)
       
-- 1) 
-- we need deriving 'show' to display the result.

-- data Zahl = Eins 
--    | Zwei 
--    | Drei
--    deriving Show 
-- plus Eins Eins = Zwei

-- (+) can not be used for Datatype "Zahl"
-- f :: [Zahl] -> Zahl -> Zahl
-- f xs y = if xs == [] then y else (x + y)
--    where
--        x :: Zahl
--        x = head xs

createBinaryTree :: BinaryTree Integer
createBinaryTree = display (Branch (Branch (Leaf 1) (Leaf 3)) (Leaf 2)) 

display :: BinaryTree Integer -> BinaryTree Integer
display (Leaf x) = Leaf x
display (Branch left right) = Branch (display left) (display right)

binaryTreeToList :: BinTree a -> [a]
binaryTreeToList Empty = []
binaryTreeToList (Knot left wert right) = binaryTreeToList left ++ [wert] ++ binaryTreeToList right


listeZuBinBaum [] = Empty
listeZuBinBaum (x:xs) = insert xs (Knot Empty x Empty)

insert [] binbaum = binbaum
insert (x:xs) Empty = insert xs (Knot Empty x Empty)
insert (x:xs) (Knot links wert rechts) 
         |  x > wert = insert xs (Knot links wert (insert [x] rechts)) 
         |  otherwise = insert xs (Knot (insert [x] links) wert rechts)
         
         
aufruf = binaryTreeToList (listeZuBinBaum [3,8,2,1]) 


-- display :: BinaryTree Integer -> BinaryTree Integer
-- display Leaf value = show value
-- display (Branch ((left x) (right x))) value = (display left) (display right) 

