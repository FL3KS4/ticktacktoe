import Data.Foldable (traverse_)
import Data.Array.Unboxed
import Data.Array.ST
import GHC.ST (ST)

type Position = (Int, Int)
type Field s = STUArray s Position Char
type FrozenField = UArray Position Char
type Move = (Char, Position)

type Result = [String]
pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))


--testing purpose
--picture :: Result
--picture = ["****"]

--create empty array
empty :: Position -> FrozenField
empty corner = runSTUArray (newArray ((1, 1), corner) ' ')

--code that adjusts the input and output
positionsToMoves :: [Position] -> [Move]
positionsToMoves = zip (cycle ['x', 'o'])

--array to lists
arrayToLists :: FrozenField -> [[Char]]
arrayToLists u =
    let ((minHeight, minWidth), (maxHeight, maxWidth)) = bounds u
    in [ [ u ! (row, column) | column <- [minWidth.. maxWidth] ] | row <- [minHeight.. maxHeight] ]

--apply moves on field    
applyMoves :: FrozenField -> [Move] -> FrozenField
applyMoves start xs = runSTUArray (foldST applyMove (thaw start) xs)
  where
    foldST :: (a -> b -> ST s ()) -> ST s a -> [b] -> ST s a
    foldST f start' moves = do
        u <- start'
        traverse_ (f u) moves
        return u

    applyMove :: Field s -> Move -> ST s ()
    applyMove u (x, i) = writeArray u i x

--starting function
ticktack :: Position -> [Position] -> IO ()
ticktack corner = pp . arrayToLists . applyMoves (empty corner) . positionsToMoves
