row2col :: [a] -> [[a]]
row2col rowvector = 
    case rowvector of 
        [] -> [[]]
        [x]-> [[x]]
        (x:xs) -> [[element] | element<-rowvector]

horzcat :: [[a]] -> [[a]] -> [[a]]
horzcat = zipWith (\l1 l2 -> l1 ++ l2)

transpose :: [[a]] -> [[a]]
transpose matrix =
        case matrix of        
            []      -> [[]]
            [v]     -> row2col v
            (v:vs)  -> horzcat (row2col v) (transpose vs)

(*^) :: (Num a) => [[a]] -> [[a]] -> [[a]]
(*^) mat1 mat2 
    | multpossible = [[innerproduct u v | v <- transpose mat2] | u<- mat1]
    | otherwise = error "Matrix shapes incompatible"
    where multpossible = ((length mat1) == (length $ head mat2)) 
          innerproduct = \x y -> foldl (+) 0 (zipWith (*) x y) 

