{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where


import Data.Time

import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine


main :: IO ()
main = do
  today <- fmap utctDay getCurrentTime

  let diff = diffDays today (fromGregorian 1989 03 12)
  let weeksAlive = weeks $ fromIntegral $ diff

  mainWith $ (lifeDiagram weeksAlive) # bgFrame 1 white


lifeDiagram :: Int -> Diagram B
lifeDiagram x =
    vsep sepDist $
        replicate (filledYears x) (hsep sepDist filledYear) ++
        [hsep sepDist (partialYear (mod x 52))] ++
        replicate (emptyYears x) (hsep sepDist emptyYear)

weeks :: Int -> Int
weeks x
  | remainder > 3 = divided + 1
  | otherwise     = divided
  where divided   = fst $ divMod x 7
        remainder = snd $ divMod x 7

sepDist :: Double
sepDist = 0.8

filledYears :: Int -> Int
filledYears x = div x 52

emptyYears :: Int -> Int
emptyYears x = div ((90 * 52) - x) 52

filledYear :: [Diagram B]
filledYear = replicate 52 filledSquare

emptyYear :: [Diagram B]
emptyYear = replicate 52 emptySquare

partialYear :: Int -> [Diagram B]
partialYear x = replicate x filledSquare ++ replicate (52-x) emptySquare

filledSquare :: Diagram B
filledSquare = emptySquare # fc black

emptySquare :: Diagram B
emptySquare = square 1 # lwG 0.2
