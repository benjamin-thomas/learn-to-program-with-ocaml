{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless, when)
import Foreign.C.Types (CInt)
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Core.Textures
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors
import Text.Printf (printf)

screenSize :: Int
screenSize = 800

doDrawTexture :: (Int, Int) -> Color -> IO ()
doDrawTexture (x, y) = drawPixel x y

newtype PiApprox = PiApprox Float
newtype Total = Total Int

doDraw :: RenderTexture -> PiApprox -> Total -> IO ()
doDraw texture (PiApprox piApprox) (Total total) = do
    clearBackground black
    drawTexture (renderTexture'texture texture) 0 0 white

    drawRectangle 0 0 145 60 black
    drawText (printf "PI = %.5f" piApprox) 10 10 20 white
    drawText (show total) 10 30 20 white

newtype Inside = Inside Int
newtype Outside = Outside Int

main :: IO ()
main = do
    windowResources <- initWindow screenSize screenSize "PI Approximation"
    texture <- loadRenderTexture screenSize screenSize
    -- setTargetFPS 120

    let mainLoop :: Inside -> Outside -> IO ()
        mainLoop (Inside inside) (Outside outside) = do
            shouldClose <- windowShouldClose
            unless shouldClose $ do
                x <- getRandomValue 0 screenSize
                y <- getRandomValue 0 screenSize

                let (inside', outside', color) =
                        if isInside
                            then (inside + 1, outside, blue)
                            else (inside, outside + 1, red)
                      where
                        isInside = x * x + y * y <= screenSize * screenSize
                    total = inside' + outside'
                    piApprox = (fromIntegral inside' / fromIntegral total) * 4 :: Float

                beginTextureMode texture
                doDrawTexture (x, y) color
                endTextureMode

                beginDrawing
                doDraw texture (PiApprox piApprox) (Total total)
                endDrawing

                mainLoop (Inside inside') (Outside outside')

    mainLoop (Inside 0) (Outside 0)

    -- Cleanup
    unloadRenderTexture texture windowResources
    closeWindow (Just windowResources)
