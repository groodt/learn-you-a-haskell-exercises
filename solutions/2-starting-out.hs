{-
 -Once you've installed Haskell from http://www.haskell.org/platform/, load the interpreter with the command ghci.
 -
 -You can load (and reload) this file in the interpreter with the command: ":l 2-starting-out.hs"
 -
 -The first function has been completed as an example.All the other functions currently return zero.
 -They can be implemented in one line using the material covered in http://learnyouahaskell.com/starting-out
 -
 -All indices are zero based.
 -}

-- Find the penultimate element in list l
penultimate l = last (init l)

-- Find the kth element in list l
findK k l = head (drop (k) l)

-- Determine if list l is a palindrome
isPalindrome l = l == reverse l

{-
 - Duplicate the elements in list xs
 - Hint: The "concat [l]" function flattens a list of lists into a single list. 
 - (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
 -
 - For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
 -}
duplicate xs = concat [[x, x] | x <- xs]

{-
 - Imitate the functinality of zip
 - The function "min x y" returns the lower of values x and y
 -}
ziplike xs ys = [(xs !! i, ys !! i) | i <- [0..(min (length xs) (length ys) - 1)]]

-- Split a list l at element k into a tuple: The first part up to and including k, the second part after k
splitAtIndex k l = (take (succ k) l, drop (succ k) l)

-- Drop the kth element in list l
dropK k l = (take k l) ++ (drop (succ k) l)

-- Extract elements between ith and kth element in list l. Including i, but not k
slice i k l = (take i l) ++ (drop k l)

-- Insert element x in list l at index k
insertElem x k l = (take k l) ++ [x] ++ (drop k l)

-- Rotate list l n places
rotate n l = undefined

