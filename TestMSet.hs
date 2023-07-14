import MultiSet


-- Read a file and return an MSet containing the ciao of each word
readMSet :: String -> IO (MSet String)
readMSet filename = do
  contents <- readFile filename
  let wordsList = words contents

  return $ foldl add (MS []) (map ciao wordsList) --what if it's an infinite stream of bytes?

  -- Write a multiset to a file
writeMSet :: MSet String -> String -> IO ()
writeMSet (MS []) _ = return ()
writeMSet (MS ((x,n):xs)) filename = do
  appendFile filename ("<" ++ x ++ "> - <" ++ show n ++ ">\n")
  writeMSet (MS xs) filename

-- Main function
main :: IO ()
main = do
  m1 <- readMSet "my_aux_files/anagram.txt"
  m2 <- readMSet "my_aux_files/anagram-s1.txt"
  m3 <- readMSet "my_aux_files/anagram-s2.txt"
  m4 <- readMSet "my_aux_files/margana2.txt"
  
  let result1 = shallowEquals m1 m4
  let result2 = m1 == (union m2 m3)

  
  putStrLn $ "i. Multisets m1 and m4 are not equal, but they have the same elements? " ++ show result1
  putStrLn $ "ii. Multiset m1 is equal to the union of multisets m2 and m3? " ++ show result2
  
  writeMSet m1 "anag-out.txt"
  writeMSet m4 "gana-out.txt" 

  -- debug prints:
  -- putStrLn $ "m1: \n " ++ show m1
  -- putStrLn $ "m1: \n " ++ show m2
  -- putStrLn $ "m1: \n " ++ show m3
  -- putStrLn $ "m4: \n " ++ show m4


-- Utils
ciao :: String -> String
ciao str = qsort $ map toLower str
    where qsort [] = []
          qsort (x:xs) = qsort [y | y <- xs, y < x] ++ [x] ++ qsort [y | y <- xs, y >= x]

-- Check if a string is a "ciao string"
isCiao :: String -> Bool
isCiao str = str == ciao str

-- the following is needed if we want to check whether a word is actually composed by letters
-- isAlphaWord :: String -> Bool 
-- isAlphaWord [] = True
-- isAlphaWord (x:xs) = isAlpha x && isAlphaWord xs
--     where isAlpha x = x `elem` (['a'..'z'] ++ ['A'..'Z']) 

toLower :: Char -> Char
toLower c
    | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise = c
