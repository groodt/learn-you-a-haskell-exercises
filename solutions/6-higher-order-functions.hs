-- Sum the numbers between two values recursively, assuming a < b when the function is first called
-- Inclusive or exclusive?
sumInts :: Int -> Int -> Int
sumInts a b
        | a == b = 0
        | otherwise = a + sumInts (a + 1) b

-- Define a square function
sq :: Int -> Int
sq x = x * x

-- Sum the squares between two numbers. This function should be similar to the sumInts function
sumSquares :: Int -> Int -> Int
sumSquares a b
           | a == b = 0
           | otherwise = sq a + sumSquares (a + 1) b

-- Define a higher order sum function which accepts an (Int -> Int) function to apply to all integers between two values.
-- Again this should look similar to the sumInts and sumSquares functions
higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum intApplication a b
               | a == b = 0
               | otherwise = (intApplication a) + higherOrderSum intApplication (a + 1) b

-- Define the square sum in terms of higherOrderSum
hoSumSquares :: Int -> Int -> Int
hoSumSquares a b = higherOrderSum sq a b

-- Define the sum between two values in terms of higherOrderSum
-- Note there is no parameter on the function definition
-- Try to use a lambda if possible
hoSumInts :: Int -> Int -> Int
hoSumInts = (\a b -> higherOrderSum id a b)

-- Create a new higher order method which generalises over the function provided by sum (That is, (+) :: Int -> Int -> Int)
-- You will also need to generalise the base case
-- You can also define the function signature yourself, which leaves you free to define the parameters and their order
-- Not sure what the question means. Does it mean redefine plus? Or define f such that f (+) 0 xs == sum xs ?
higherOrderSequenceApplication binaryF start xs = foldr binaryF start xs

-- Define a factorial method using the higherOrderSequenceAppliction
hoFactorial :: Int -> Int
hoFactorial x = higherOrderSequenceApplication (*) 1 [1..x]

