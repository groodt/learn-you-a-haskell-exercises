module DistanceConversions
( yardsToFeet
, feetToInches
, inchesToCentimetres
, yardsToMetre
, chainsToYards
) where

-- Define yards to feet
yardsToFeet ::  Float -> Float
yardsToFeet y = y / 3

yardsToMetre :: Float -> Float
yardsToMetre y = y * 0.9144

-- Define feet to inches
feetToInches :: Float -> Float
feetToInches f = f / 12

-- Define inches to centimetres
inchesToCentimetres :: Float -> Float
inchesToCentimetres i = i * 2.54

chainsToYards :: Float -> Float
chainsToYards c = c * 22

