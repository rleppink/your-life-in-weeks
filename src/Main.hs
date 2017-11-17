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

import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude

import           Options.Generic


data Arguments w = Arguments
  { startDate    :: w ::: String       <?> "The start date to calculate the weeks from. [Mandatory. No default.]"
  , endDate      :: w ::: Maybe String <?> "The end date to calculate the weeks from. [Optional. Default: weeks to be 90 years of age.]"
  , years        :: w ::: Maybe Int    <?> "The total number of years to calculate the weeks from. [Optional. Default: weeks to be 90 years of age.]"
  , weeks        :: w ::: Maybe Int    <?> "The total number of weeks to calculate the weeks from. [Optional. Default: weeks to be 90 years of age.]"
  , lineLength   :: w ::: Maybe Int    <?> "The amount of weeks to display per line. [Optional. Default: 52, a year]"
  , yearStart    :: w ::: Bool         <?> "Start the diagram at the start of the start date's year, instead of at the start date. [Optional. Default: False]"
  } deriving (Generic)

modifiers :: Modifiers
modifiers = lispCaseModifiers { shortNameModifier = firstLetter }

instance ParseRecord (Arguments Options.Generic.Wrapped)
  where parseRecord = parseRecordWithModifiers modifiers
deriving instance Show (Arguments Options.Generic.Unwrapped)


main :: IO ()
main = do
  today <- fmap utctDay getCurrentTime
  args  <- unwrapRecord "Your Life In Weeks"

  let startDateArg =
        parseTimeOrError
             True
             defaultTimeLocale
             (iso8601DateFormat Nothing)
             (startDate args)
             :: Day

  let dayDiff = fromIntegral $ diffDays today startDateArg

  renderRasterific
    (diagramName (startDate args))
    (mkWidth 500)
    (lifeDiagram (div dayDiff 7) 90  # bgFrame 1 white)


lifeDiagram :: Int -> Int -> Diagram B
lifeDiagram x y =
    vsep sepDist $
        replicate (yearsAlive x) (hsep sepDist filledYear) ++
        [hsep sepDist (partialYear (mod x 52))] ++
        replicate (yearsMax x y) (hsep sepDist emptyYear)


-- | Generate a diagram filename from given arguments
diagramName :: String -> String
diagramName x = x ++ ".png"

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
