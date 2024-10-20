import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow)
import Text.Printf (printf)

green = "\x1b[32m"
yellow = "\x1b[33m"
blue = "\x1b[34m"
reset = "\x1b[0m"

toStr :: Set Int -> String
toStr s =
    let ref = show s
     in fromMaybe ref (stripPrefix "fromList " ref)

{-

:cmd return $ unlines [":reload", ":!clear", "solve' 0 (Set.fromList [1..4]) Set.empty Set.empty"]

>>> solve (Set.fromList [1..4]) Set.empty Set.empty
2

>>> solve (Set.fromList [1..5]) Set.empty Set.empty
10

>>> solve (Set.fromList [1..6]) Set.empty Set.empty
4

>>> solve (Set.fromList [1..7]) Set.empty Set.empty
40

>>> solve (Set.fromList [1..8]) Set.empty Set.empty
92

 -}
solve' :: Int -> Set Int -> Set Int -> Set Int -> Int
solve' depth cols d1 d2 =
    if Set.null cols
        then 1
        else
            Set.foldl'
                ( \acc n ->
                    let cols' = Set.delete n cols
                        d1' = Set.map succ (Set.insert n d1)
                        d2' = Set.map pred (Set.insert n d2)

                        accDbg :: Int
                        accDbg =
                            trace
                                ( printf
                                    "%-6s %-7d cols: %-20s => %-25s d1: %-18s => %-25s d2: %-19s => %-20s"
                                    (replicate depth '*')
                                    n
                                    (yellow ++ toStr cols ++ reset)
                                    (green ++ toStr cols' ++ reset)
                                    (yellow ++ toStr d1 ++ reset)
                                    (green ++ toStr d1' ++ reset)
                                    (yellow ++ toStr d2 ++ reset)
                                    (green ++ toStr d2' ++ reset)
                                )
                                acc
                     in accDbg + solve' (depth + 1) cols' d1' d2'
                )
                0
                (Set.difference (Set.difference cols d1) d2)

-- solve2 :: Set Int -> Set Int -> Set Int -> (Int, [(Set Int, Set Int, Set Int)])
-- solve2 cols d1 d2 =
--     if Set.null cols
--         then (1, [(cols, d1, d2)]) -- Base case with final state
--         else
--             Set.foldl
--                 ( \acc n ->
--                     let newCols = Set.delete n cols
--                         newD1 = Set.map succ (Set.insert n d1)
--                         newD2 = Set.map pred (Set.insert n d2)
--                         (result, states) = solve newCols newD1 newD2
--                      in (fst acc + result, (cols, d1, d2) : snd acc ++ states)
--                 )
--                 (0, [])
--                 (Set.difference (Set.difference cols d1) d2)

{-

solve (Set.fromList [1..8]) Set.empty Set.empty

 -}
solve :: Set Int -> Set Int -> Set Int -> Int
solve cols d1 d2 =
    if Set.null cols
        then 1
        else
            Set.foldl'
                ( \acc n ->
                    acc
                        + solve
                            (Set.delete n cols)
                            (Set.map succ (Set.insert n d1))
                            (Set.map pred (Set.insert n d2))
                )
                0
                (Set.difference (Set.difference cols d1) d2)

{-

[2.14]$ ghc -O2 -o Main Main.hs
Loaded package environment from /home/benjamin/.ghc/x86_64-linux-9.4.8/environments/default
[1 of 2] Compiling Main             ( Main.hs, Main.o )
[2 of 2] Linking Main
[2.14]$ time ./Main
Result: 365596

real    0m15,526s
user    0m15,501s
sys     0m0,018s

 -}
main :: IO ()
main = putStrLn $ "Result: " <> show (solve (Set.fromList [1 .. 14]) Set.empty Set.empty)

{- FOURMOLU_DISABLE -}

{-

This one is a mind twister!


       1       cols: [1,2,3,4] => [2,3,4]          d1: []      => [2]              d2: []       => [0]
*      3       cols: [2,3,4]   => [2,4]            d1: [2]     => [3,4]            d2: [0]      => [-1,2]
*      4       cols: [2,3,4]   => [2,3]            d1: [2]     => [3,5]            d2: [0]      => [-1,3]
**     2       cols: [2,3]     => [3]              d1: [3,5]   => [3,4,6]          d2: [-1,3]   => [-2,1,2]
STOPS: 0

       2       cols: [1,2,3,4] => [1,3,4]          d1: []      => [3]              d2: []       => [1]
*      4       cols: [1,3,4]   => [1,3]            d1: [3]     => [4,5]            d2: [1]      => [0,3]
**     1       cols: [1,3]     => [3]              d1: [4,5]   => [2,5,6]          d2: [0,3]    => [-1,0,2]
***    3       cols: [3]       => []               d1: [2,5,6] => [3,4,6,7]        d2: [-1,0,2] => [-2,-1,1,2]
STOPS: 1

       3       cols: [1,2,3,4] => [1,2,4]          d1: []      => [4]              d2: []       => [2]
*      1       cols: [1,2,4]   => [2,4]            d1: [4]     => [2,5]            d2: [2]      => [0,1]
**     4       cols: [2,4]     => [2]              d1: [2,5]   => [3,5,6]          d2: [0,1]    => [-1,0,3]
***    2       cols: [2]       => []               d1: [3,5,6] => [3,4,6,7]        d2: [-1,0,3] => [-2,-1,1,2]
STOPS: 1

       4       cols: [1,2,3,4] => [1,2,3]          d1: []      => [5]              d2: []       => [3]
*      1       cols: [1,2,3]   => [2,3]            d1: [5]     => [2,6]            d2: [3]      => [0,2]
**     3       cols: [2,3]     => [2]              d1: [2,6]   => [3,4,7]          d2: [0,2]    => [-1,1,2]
STOPS: 0

*      2       cols: [1,2,3]   => [1,3]            d1: [5]     => [3,6]            d2: [3]      => [1,2]
STOPS 0

2


 -}