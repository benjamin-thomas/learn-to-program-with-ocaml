{-# LANGUAGE TemplateHaskell #-}

import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Raylib.Core (clearBackground, closeWindow, initWindow, setTargetFPS, windowShouldClose)
import Raylib.Core.Shapes (drawCircle, drawPixel, drawRectangle)
import Raylib.Core.Text (drawText)
import Raylib.Util (WindowResources, drawing, raylibApplication)
import Raylib.Util.Colors (black, blue, lightGray, rayWhite, red)
import System.Random (randomRIO)

windowSize :: Int
windowSize = 800

data State = MkState
    { window :: WindowResources
    , inside :: IORef Int
    , outside :: IORef Int
    }

startup :: IO State
startup = do
    win <- initWindow windowSize windowSize "PI approximation"
    setTargetFPS 120
    inside <- newIORef 0
    outside <- newIORef 0
    return $ MkState win inside outside

isInside :: Int -> Int -> Bool
isInside x y =
    x * x + y * y <= windowSize * windowSize

piApprox :: Int -> Int -> Float
piApprox inside outside =
    4 * inside' / (inside' + outside')
  where
    inside' = fromIntegral inside
    outside' = fromIntegral outside

mainLoop :: State -> IO State
mainLoop state = do
    x <- randomRIO (0, windowSize) :: IO Int
    y <- randomRIO (0, windowSize) :: IO Int

    color <-
        if isInside x y
            then
                modifyIORef (inside state) (+ 1)
                    >> return red
            else
                modifyIORef (outside state) (+ 1)
                    >> return blue

    inside <- readIORef (inside state)
    outside <- readIORef (outside state)

    drawing
        ( do
            drawPixel x y color
            drawRectangle 0 0 145 60 black
            drawText ("PI = " ++ show (piApprox inside outside)) 10 10 20 rayWhite
            drawText ("Points: " ++ show (inside + outside)) 10 30 20 rayWhite
        )
    return state

shouldClose :: State -> IO Bool
shouldClose _ = windowShouldClose

teardown :: State -> IO ()
teardown = closeWindow . Just . window

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
