import MusicResources

makeStatsList :: [(Char,[(Int,Char)])]

makeStatsList = helperMakeStatsList chars

helperMakeStatsList :: String -> [(Char,[(Int,Char)])]

helperMakeStatsList [] = []
helperMakeStatsList (x:xs) = (cellgenerator x):helperMakeStatsList xs 

occurenceCount :: Char -> [Char] -> Int

occurenceCount a [] = 0
occurenceCount a (x:xs) | a == x = 1 + occurenceCount a xs
			| otherwise = occurenceCount a xs

removeOccurences :: [Char] -> [Char]

removeOccurences [] = []
removeOccurences (x:xs) = x:helperRemoveOccurences x (removeOccurences xs)

helperRemoveOccurences :: Char -> [Char] -> [Char]

helperRemoveOccurences a [] = []
helperRemoveOccurences a (x:xs) | a == x = helperRemoveOccurences a xs
				| otherwise = x:helperRemoveOccurences a xs

pairGenerator :: [Char] -> [Char] -> [(Int,Char)]

pairGenerator [] x = []
pairGenerator (x:xs) l = ((occurenceCount x l),x):pairGenerator xs l

insertionSort :: [(Int,Char)] -> [(Int,Char)]

insertionSort [] = []
insertionSort (x:xs) =  helperInsertionSort x (insertionSort xs)

helperInsertionSort :: (Int,Char) -> [(Int,Char)] -> [(Int,Char)]

helperInsertionSort x [] = [x]
helperInsertionSort (x,a) ((y,b):ys) | x < y = (y,b):helperInsertionSort (x,a) ys
				     | otherwise = (x,a):(y,b):ys

occurenceGenerator :: Char -> [String] -> [Char]

occurenceGenerator a [] = []
occurenceGenerator a (x:xs) = helperOccurenceGenerator a x ++ (occurenceGenerator a xs)

helperOccurenceGenerator :: Char -> [Char] -> [Char]

helperOccurenceGenerator a [] = []
helperOccurenceGenerator a [x] = []
helperOccurenceGenerator a (x:y:ys) | a == x = y:helperOccurenceGenerator a (y:ys)
				    | otherwise = helperOccurenceGenerator a (y:ys)

cellgenerator :: Char -> (Char,[(Int,Char)])

cellgenerator a = (a,insertionSort (pairGenerator (removeOccurences x) x)) where x = occurenceGenerator a training

compose :: Char -> Int -> [Char]
compose x 0 = []
compose x n | (length occ) == 0 = error "Error on Random"
	    | otherwise = (y:ys) 
	      where occ = occurenceGenerator x training
		    y = occ!!(randomZeroToX ((length occ)-1))
		    ys = compose y (n-1) 


















