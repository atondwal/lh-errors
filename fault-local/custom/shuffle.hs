import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Array
 
{- Ineffficent, but straightforward implementation -}
bestShuffle :: Eq a => [a] -> [a]
bestShuffle s = minimumBy (compare `on` score s) $ permutations s

score :: Eq a => [a] -> [a] -> Int
score old new = length $ filter id $ zipWith (==) old new
 
{- Hard to human verify, so let's machine-check it against the "obvious"
   one -}


shuffle :: (Ord a, Eq a) => [a] -> [a]
shuffle s = elems $ array bs $ f positions letters
  where positions =
            concat $ sortBy (compare `on` length) $
            map (map fst) $ groupBy ((==) `on` snd) $
            sortBy (compare `on` snd) $ zip [0..] s
        letters = map (orig !) positions
 
        f [] [] = []
        f (p : ps) ls = (p, ls !! i) : f ps (removeAt i ls)
          where i = fromMaybe 0 $ findIndex (/= o) ls
                o = orig ! p
 
        orig = listArray bs s
        bs = (0, length s - 1)
 
removeAt :: Int -> [a] -> [a]
removeAt 0 (x : xs) = xs
-- body is supposed to be x : removeAt (i - 1) xs
removeAt i (x : xs) = x : x : removeAt (i - 1) xs
