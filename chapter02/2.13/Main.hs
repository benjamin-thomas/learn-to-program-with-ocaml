{-# LANGUAGE LambdaCase #-}

import Graphics.Gloss (
    Display (InWindow),
    Picture,
    black,
    color,
    display,
    pictures,
    rectangleSolid,
    red,
    translate,
    white,
 )
import Graphics.Gloss.Interface.Environment (getScreenSize)

data Quad
    = White
    | Black
    | Node Quad Quad Quad Quad

checkerBoard :: Int -> Quad
checkerBoard = \case
    0 -> Black
    1 -> Node White Black White Black
    n ->
        let quad = checkerBoard (n - 1)
         in Node quad quad quad quad

quadToPicture :: Float -> Float -> Float -> Quad -> Picture
quadToPicture x y w = \case
    White -> translate x y $ color white $ rectangleSolid w w
    Black -> translate x y $ color black $ rectangleSolid w w
    Node q1 q2 q3 q4 ->
        f (w / 2)
      where
        f h =
            pictures
                [ quadToPicture (x + 0) (y + 0) h q1
                , quadToPicture (x + h) (y + 0) h q2
                , quadToPicture (x + h) (y + h) h q3
                , quadToPicture (x + 0) (y + h) h q4
                ]

main :: IO ()
main =
    let
        screenCenter :: (Int, Int)
        screenCenter = (761, 387)

        pow :: Int
        pow = 3

        gridSize :: Int
        gridSize = 2 ^ pow

        tileSize :: Int
        tileSize = 50

        boardSize :: Int
        boardSize = gridSize * tileSize

        picture =
            quadToPicture
                0
                0
                (fromIntegral boardSize)
                $ checkerBoard pow

        offset :: Float
        offset =
            fromIntegral boardSize / 2
                - fromIntegral tileSize / 2
     in
        do
            display
                (InWindow "Checkerboard" (boardSize, boardSize) screenCenter)
                red
                (translate (-offset) (-offset) picture)