{-
 - Create a type called Validation
 - The type constructor takes one parameter
 - There are two Values: 
 -   Success, which takes that parameter and
 -   Fail String, which represents a failure, and a reason for that failure
 -}
data Validation a = Success a | Fail String deriving (Eq, Show) 

-- Make the Validation a Monad
instance Monad Validation where
         return x = Success x
         Success x >>= f = f x
         Fail err >>= f = Fail err

-- Monad laws

-- 1. Left identity
-- return x >>= f == f x

-- 2. Right identity
-- m >>= return == m

-- 3. Associativity
-- (m >>= f) >>= g == m >>= (\x -> f x >>= g) 

testMonadLaws =
              let f = \n -> Success (n + 99)
                  f' = \n -> Fail "#lolrails" :: Validation Int
                  g = \n -> Success (n - 5)
                  g' = \n -> Fail "#fail" :: Validation Int
                  x = 5
                  m = Success 42
                  m' = Fail "#lolphp" :: Validation Int
                  left_identity = (return x >>= f) == f x
                  left_identity' = (return x >>= f') == f' x
                  right_identity = (m >>= return) == m
                  right_identity' = (m' >>= return) == m'
                  associativity = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
                  associativity' = ((m' >>= f') >>= g') == (m' >>= (\x -> f' x >>= g'))
              in left_identity && left_identity' && right_identity && right_identity' && associativity && associativity'

{-
 - Create a function, positiveCheck, which takes a number and returns a successful Validation if it's positive, 
 - and a failed Validation with a String message if not.
 -}
positiveCheck :: (Num a, Ord a) => a -> Validation a
positiveCheck x = if x >= 0 then Success x else Fail ("Negative number encountered: " ++ (show x))

{-
 - Create a function, evenCheck, which returns a successful Validation if it's even,
 - and a failed Validation with a string message if it's odd
 -}
evenCheck :: (Integral a)  =>  a -> Validation a
evenCheck x = if (even x) then Success x else Fail ("Odd number encountered: " ++ (show x))

{-
 - Write a function which uses positiveCheck and evenCheck to make sure a number is both positive and even
 -}
positiveAndEvenCheck :: (Num a, Ord a, Integral a) => a -> Validation a
positiveAndEvenCheck x = do
                           positiveCheck x
                           evenCheck x

