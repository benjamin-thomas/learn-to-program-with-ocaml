module Main where

import Control.Monad (unless)
import Data.Char (isDigit)
import Data.List (intercalate)
import System.Environment.Blank (getArgs)
import System.IO (isEOF)

{-

echo ./Main.hs | entr -c bash -rc 'runghc -Wall ./Main.hs 16 < <(echo -e "ABC\ndef\nXY2Z\nFF")'

 -}

red :: String
red = "\x1b[31m"

reset :: String
reset = "\x1b[0m"

toBase10 :: Char -> Maybe Int
toBase10 c
    | isDigit c = Just $ fromEnum c - fromEnum '0'
    | c >= 'A' && c <= 'F' = Just $ fromEnum c - fromEnum 'A' + 10
    | c >= 'a' && c <= 'f' = Just $ fromEnum c - fromEnum 'a' + 10
    | otherwise = Nothing

{-

Horner's method states that:
    A×16² + B×16¹ + C×16⁰

Is equivalent to:
    C + 16×(B + 16×(A + 16×0))

I'm using foldr to handle the list which is already in reverse order.

 -}
hornerRev :: Int -> [Int] -> Int
hornerRev base =
    foldr (\n acc -> n + base * acc) 0

mainLoop :: Int -> IO ()
mainLoop base = do
    eof <- isEOF
    unless eof loop
  where
    loop :: IO ()
    loop = do
        line <- getLine
        putStrLn $
            mconcat
                [ "["
                , intercalate "," $ map show line
                , "]"
                ]

        let (good, bad) = convertToBase10 line

        case (good, bad) of
            (vals, []) -> do
                print (reverse vals) -- just for clarity, for the user
                putStrLn $ " -> " <> show (hornerRev base vals)
            (_, chars) ->
                putStrLn $
                    mconcat
                        [ red
                        , " => Bad chars: " <> chars
                        , reset
                        ]
        putStrLn ""
        mainLoop base
      where
        convertToBase10 :: [Char] -> ([Int], [Char])
        convertToBase10 =
            foldl
                ( \(good, bad) c ->
                    case toBase10 c of
                        Just n -> (n : good, bad)
                        Nothing -> (good, c : bad)
                )
                ([], [])

main :: IO ()
main = do
    args <- getArgs
    case args of
        [n] -> do
            let base = read n
            putStrLn $ "Base is: " <> show base
            putStrLn "---\n"
            mainLoop base
            putStrLn "Done!"
        _ ->
            putStrLn "Usage ./Main.hs BASE"
