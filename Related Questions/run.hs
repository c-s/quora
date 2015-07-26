import Control.Monad
import Control.Applicative
import Data.List (unfoldr, partition, splitAt, findIndex, minimumBy)
import Debug.Trace
import Data.Ord (comparing)

main :: IO ()
main = do
  n  <- readLn :: IO Int
  tsStr <- getLine :: IO String
  let ts = (read <$> (take n $ unfoldr (\str -> Just (takeWhile (/=' ') str, tail $ dropWhile (/= ' ') str)) tsStr)) :: [Int]

  graphStrs <- filter (any (== ' ')) <$> lines <$> getContents
  -- create a list of pairs.
  let graph = (\str ->
                  let Just splitIndex = findIndex (==' ') str
                      (start, end) = splitAt splitIndex str
                  in ((read start) :: Int, (read $ tail end) :: Int)) <$> graphStrs
  -- create a tree from the graph.
  let tree = createTree (fst $ head graph) graph
  -- create a tree with a node denoting
  -- (node index, time to read this node, expected time to read until no more questions are available starting from this node).
  let timeTree = calculateTimeTree ts tree
  -- find the node with the minimum time to read til no more questions are available.
  let minimum = findMinimum timeTree
  --print the minimum.
  print $ fst minimum

data Tree a = Node a [Tree a]
            deriving (Show)

createTree :: Int -> [(Int, Int)] -> Tree Int
createTree seed graph =
  let (inset, outset) = partition (\(x, y) -> x == seed || y == seed) graph
      children = (\(x, y) -> if x == seed then y else x) <$> inset
   in
   Node seed $ (flip createTree outset) <$> children

-- | given a list of times to read each question, a tree of questions
-- returns a tree of (question, expected time to read children questions until no more are available).
calculateTimeTreeSub1 :: [Int] -> Tree Int -> Tree (Int, Double, Double)
calculateTimeTreeSub1 times (Node node []) = Node (node, fromIntegral $ times !! (node - 1), 0) []
calculateTimeTreeSub1 times (Node node trees) =
  let subTrees = calculateTimeTreeSub1 times <$> trees
      nodeTime = times !! (node - 1)
      subTreeTimes = sum $ (\(Node (_, nodetime, subtimes) _) -> nodetime + subtimes) <$> subTrees
   in
      Node (node, fromIntegral nodeTime, subTreeTimes / (fromIntegral $ length trees)) subTrees

-- | given (Nothing if starting from the beginning or Just acctime if it takes acctime to read questions up to this question, and
-- a tree from calculateTimeTreeSub1, calculates a tree of (question, time to read questions til no more questions available.1
calculateTimeTreeSub2 :: Maybe Double -> Tree (Int, Double, Double) -> Tree (Int, Double)
calculateTimeTreeSub2 maybeAcctime (Node (node, nodetime, subtimes) trees) =
  Node (node, acctime * factor1 + nodetime + subtimes * factor2) $
    calculateTimeTreeSub2 (Just $ acctime + nodetime) <$> trees
      where (factor1, factor2, acctime) = case maybeAcctime of
              Just act -> (1 / (1 + lensubtimes), lensubtimes / (1 + lensubtimes), act)
              Nothing -> (0, 1, 0)
            lensubtimes = fromIntegral $ length trees


calculateTimeTree times = calculateTimeTreeSub2 Nothing . calculateTimeTreeSub1 times

findMinimum :: (Ord b) => Tree (a, b) -> (a, b)
findMinimum (Node (node, time) []) = (node, time)
findMinimum (Node (node, time) trees) =
  let subMin@(subMinInd, subMinVal) = minimumBy (comparing snd) $ findMinimum <$> trees
   in
   if time < subMinVal then (node, time) else subMin
