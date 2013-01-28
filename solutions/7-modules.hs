import DistanceConversions

-- First, complete the DistanceConversions.hs module, and import it here
-- create a higher-order function for converting an area between two dimensions
-- using the functions defined in the DistanceConversions module

-- Wow, this example is wrong.
-- 9 inches == 28.86cm
-- Example areaConv inchesToCentimetres 9 = 58.0644 
areaConv :: (Float -> Float) -> Float -> Float
areaConv f x = f x

-- define a function for converting square inches into square centimetres
sqInToSqCm :: Float -> Float
sqInToSqCm = \x -> (inchesToCentimetres (sqrt x)) ^ 2

-- define a function for converting square chains (22 yards) to square metres
sqChainsToSqM :: Float -> Float
sqChainsToSqM = \x -> (yardsToMetre . chainsToYards $ (sqrt x)) ^ 2
