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
    red,
    scale,
    simulate,
    translate,
    white,
    yellow,
 )
import Graphics.Gloss.Data.ViewPort (ViewPort)

import Control.Monad (forM)
import Data.Foldable qualified as F
import Data.Sequence (Seq, (<|))
import Data.Sequence qualified as S
import System.Random (Random (randomR), StdGen, getStdGen)

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
        drawQuarterCircle =
            translate (-200) (-200) $
                color yellow $
                    arc 0 90 400
        drawPi =
            color white $
                scale 0.3 0.3 $
                    Pictures
                        [ translate (-100) 0 $
                            Text $
                                show piApprox
                        , translate (-100) 200 $
                            Text $
                                show totalPoints
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
            , if 0 == totalPoints
                then
                    Text "" -- Don't show ugly NaN
                else drawPi
            ]

nextModel :: Model -> Model
nextModel model =
    let totalPoints = S.length model.inside + S.length model.outside
     in if totalPoints >= 40_000
            then model
            else
                let
                    ((x, y), newStdGen) =
                        randomPoint model.stdGen
                    (newInside, newOutside) =
                        if x * x + y * y <= 1
                            then
                                ( (x * 400 - 200, y * 400 - 200) <| model.inside
                                , model.outside
                                )
                            else
                                ( model.inside
                                , (x * 400 - 200, y * 400 - 200) <| model.outside
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
update _ _secs = nextModelBatch 100

main :: IO ()
main =
    let fps = 60
     in do
            g <- getStdGen
            simulate
                (InWindow "Monte Carlo" (400, 400) (0, 0))
                black
                fps
                (init' g)
                view
                update