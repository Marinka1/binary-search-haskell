module Main (main) where
main :: IO ()

binarysearch :: Int -> [Int] -> Int -> Int -> [Int]

binarysearch _ _ l r
    | l > r = []

binarysearch x sortedList l r
    | sortedList !! m == x = [m]
    | sortedList !! m < x = binarysearch x sortedList (m+1) r
    | otherwise = binarysearch x sortedList l (m-1)
    where
        m = ((r-l) `div` 2 + l)




main = do
  let numbers = [1..100]
  putStrLn "enter a value to find: "
  inputX <- getLine
  putStrLn (show numbers)
  let x = (read inputX :: Int)
  putStrLn (show (binarysearch x numbers 0 ((length numbers)-1)))