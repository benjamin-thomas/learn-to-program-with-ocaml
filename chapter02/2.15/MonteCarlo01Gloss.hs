-- {-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-

Compile (optimized) with:

    ghc --make MonteCarlo01.hs -O2

 -}

import Graphics.Gloss (
    Display (InWindow),
    Picture (Pictures, Text),
    arc,
    black,
    circleSolid,
    color,
    green,
    rectangleSolid,
    red,
    scale,
    simulate,
    translate,
    white,
    withAlpha,
    yellow,
 )
import Graphics.Gloss.Data.ViewPort (ViewPort)

import Control.Monad (forM)
import Data.Foldable qualified as F
import Data.Sequence (Seq, (<|))
import Data.Sequence qualified as S
import System.Random (Random (randomR), StdGen, getStdGen)

-- Text alignment is a bit quirky, so some other adjustments may be necessary unfortunately
width :: Int
width = 800

maxPoints :: Int
maxPoints = 400_000

batchSize = 1000

-- `length` is O(1) for the `Seq` type
-- It behaves similarly to a list
data Model = Model
    { stdGen :: StdGen
    , point :: (Float, Float)
    , inside :: Seq (Float, Float)
    , outside :: Seq (Float, Float)
    }

init' :: StdGen -> Model
init' stdGen =
    Model
        { stdGen = stdGen
        , point = (0, 0)
        , inside = S.empty
        , outside = S.empty
        }

randomPoint :: StdGen -> ((Float, Float), StdGen)
randomPoint gen =
    let (x, gen') = randomR (0, 1) gen
        (y, gen'') = randomR (0, 1) gen'
     in ((x, y), gen'')

view :: Model -> Picture
view model =
    let totalPoints = (S.length model.inside + S.length model.outside)
        (x, y) = model.point
        width' = fromIntegral width
        drawQuarterCircle =
            translate (-(width' / 2)) (-(width' / 2)) $
                color yellow $
                    arc 0 90 width'
        drawPi =
            color white $
                scale 0.3 0.3 $
                    Pictures
                        [ translate (-850) 100 $
                            Text $
                                "Points: " ++ show totalPoints
                        , translate (-850) (-100) $
                            Text $
                                "PI ~ " ++ show piApprox
                        ]
          where
            piApprox =
                4 * fromIntegral (S.length model.inside) / fromIntegral totalPoints
        drawPoints points color_ =
            mconcat
                ( F.toList $
                    fmap
                        ( \(x, y) ->
                            translate x y $
                                color color_ $
                                    circleSolid 1
                        )
                        points
                )
     in Pictures
            [ drawQuarterCircle
            , drawPoints model.inside green
            , drawPoints model.outside red
            , color (withAlpha 0.65 black) $ rectangleSolid (width' / 1.4) (width' / 4)
            , if 0 == totalPoints
                then
                    Text "" -- Don't show ugly NaN
                else drawPi
            ]

nextModel :: Model -> Model
nextModel model =
    let totalPoints = S.length model.inside + S.length model.outside
     in if totalPoints >= maxPoints
            then model
            else
                let
                    ((x, y), newStdGen) =
                        randomPoint model.stdGen
                    width' = fromIntegral width
                    newPoint =
                        ( x * width' - (width' / 2)
                        , y * width' - (width' / 2)
                        )
                    (newInside, newOutside) =
                        if x * x + y * y <= 1
                            then
                                ( newPoint <| model.inside
                                , model.outside
                                )
                            else
                                ( model.inside
                                , newPoint <| model.outside
                                )
                 in
                    model
                        { stdGen = newStdGen
                        , point = (x, y)
                        , inside = newInside
                        , outside = newOutside
                        }

nextModelBatch :: Int -> Model -> Model
nextModelBatch n model = iterate nextModel model !! n

update :: ViewPort -> Float -> Model -> Model
update _ _secs = nextModelBatch batchSize

main :: IO ()
main =
    let fps = 60
     in do
            g <- getStdGen
            simulate
                (InWindow "Monte Carlo" (width, width) (0, 0))
                black
                fps
                (init' g)
                view
                update