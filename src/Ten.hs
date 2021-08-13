module Ten where

data NestedList a = Elem a | List [NestedList a]

last' :: [a] -> a
last' []     = error "lista vazia"
last' [x]    = x
last' (x:xs) = last' (xs)

lastOne :: [a] -> a
lastOne [] = error "lista vazia"
lastOne (x:y:[]) = x;
lastOne (x:y:ys) = lastOne(y:ys)

element_at :: [a] -> Int -> a
element_at [] _ = error "lista vazia"
element_at xs y = xs !! y

myLength :: (Num b) => [a] -> b
myLength [] = 0
myLength (xs:xss) = 1 + myLength xss

myReverse :: (Enum a) => [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Ord a) => [a] -> Bool
isPalindrome xs = result
  where
    result = reverse xs `compare` xs == EQ

