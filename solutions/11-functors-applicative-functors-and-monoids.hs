import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show, Eq)

-- (Value 1) ((Value 2) ((Value 3) Empty))

-- Make the list a Functor
instance Functor List where
         fmap f Empty = Empty
         fmap f (Value x xs) = Value (f x) (fmap f xs)

-- fmap (\x -> x + 99) ((Value 1) ((Value 2) ((Value 3) Empty)))

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists Empty xs = xs
combineLists (Value x xs) b = Value x (combineLists xs b)

-- combineLists ((Value 1) ((Value 2) ((Value 3) Empty))) (fmap (\x -> x + 99) ((Value 1) ((Value 2) ((Value 3) Empty))))

-- Make our list a Monoid
instance Monoid (List a) where
         mempty = Empty
         mappend = combineLists

-- Make our list an Applicative
instance Applicative List where
         pure x = Value x Empty
         Empty <*> _ = Empty
         (Value x xs) <*> other = (fmap x other) `mappend` (xs <*> other)

-- Make sure that the List obeys the laws for Applicative and Monoid

-- Applicative laws
-- pure f <*> x = fmap f x
testApplicativeLaws = 
                let rule1 = (pure (+3) <*> ((Value 1) ((Value 2) ((Value 3) Empty)))) == (fmap (+3) ((Value 1) ((Value 2) ((Value 3) Empty))))
                    rule2 = (pure id <*> (Value 1 Empty)) == Value 1 Empty
                in rule1 && rule2


-- Monoid laws

-- 1.
-- mempty identity with respect to mappend
-- mempty `mappend` x = x

-- 2.
-- mempty identity with respect to mappend
-- x `mappend` mempty = x

-- 3.
-- Associative
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
testMonoidLaws =
           let rule1 = mempty `mappend` (Value 3 Empty) == (Value 3 Empty)
               rule2 = (Value 3 Empty) `mappend` mempty == (Value 3 Empty)
               rule3 =  (((Value 1 Empty) `mappend` (Value 2 Empty)) `mappend` (Value 3 Empty)) == ((Value 1 Empty) `mappend` ((Value 2 Empty) `mappend` (Value 3 Empty)))
           in rule1 && rule2 && rule3


-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)
testPlusTwo = plusTwo <$> twoValueList

-- Use <$> and <*> on the lists with a binary function
testApplicativeBinaryFunction = (+) <$> twoValueList <*> twoValueList

-- Create some lists of binary functions
twoBinaryFunctionsList :: Num a => List (a -> a -> a)
twoBinaryFunctionsList = (pure (+)) `mappend` (pure (*))
-- Use <*> on the binary functions list and the number lists
testApplicativeTwoBinaryFunctionsList = twoBinaryFunctionsList <*> twoValueList <*> (pure 1)