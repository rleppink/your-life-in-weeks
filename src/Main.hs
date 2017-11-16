{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Main where


import           Data.Time

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

import           Options.Generic

import           System.Environment


data Arguments w = Arguments
  { endDate      :: w ::: Maybe String <?> "Provide the end date to calculate the weeks from. [Default: weeks to be 90 years of age.]"
  , totalYears   :: w ::: Maybe Int    <?> "Provide the total number of years to calculate the weeks from. [Default: weeks to be 90 years of age.]"
  , totalWeeks   :: w ::: Maybe Int    <?> "Provide the total number of weeks to calculate the weeks from. [Default: weeks to be 90 years of age.]"
  , weeksPerLine :: w ::: Maybe Int    <?> "Set the amount of weeks to display per line. [Default: 52]"
  } deriving (Generic)

instance ParseRecord (Arguments Options.Generic.Wrapped)
deriving instance Show (Arguments Options.Generic.Unwrapped)


main :: IO ()
main = do
  today <- fmap utctDay getCurrentTime


  -- let dateArg =
        -- parseTimeOrError
            -- True
            -- defaultTimeLocale
            -- (iso8601DateFormat Nothing)
            -- (head args)
            -- :: Day


  -- let dayDiff = fromIntegral $ diffDays today dateArg

  args  <- unwrapRecord "Your Life In Weeks"
  print (args :: Arguments Options.Generic.Unwrapped)

  -- mainWith $
    -- lifeDiagram (weeks dayDiff) 90 # bgFrame 1 white


lifeDiagram :: Int -> Int -> Diagram B
lifeDiagram x y =
    vsep sepDist $
        replicate (yearsAlive x) (hsep sepDist filledYear) ++
        [hsep sepDist (partialYear (mod x 52))] ++
        replicate (yearsMax x y) (hsep sepDist emptyYear)

weeks :: Int -> Int
weeks x = div x 7

sepDist :: Double
sepDist = 0.8

yearsAlive :: Int -> Int
yearsAlive x = div x 52

yearsMax :: Int -> Int -> Int
yearsMax x y = div ((y * 52) - x) 52

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
