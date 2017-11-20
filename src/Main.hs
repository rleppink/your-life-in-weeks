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


import           Data.Maybe
import           Data.Time

import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude

import           Options.Generic


data Arguments w = Arguments
  { startDate     :: w ::: String       <?> "Start date to calculate the weeks from. [Mandatory. No default.]"
  , endDate       :: w ::: Maybe String <?> "End date to calculate the weeks from. [Optional. Default: weeks to be 90 years of age.]"
  , years         :: w ::: Maybe Int    <?> "Total number of years to calculate the weeks from. [Optional. Default: weeks to be 90 years of age.]"
  , weeks         :: w ::: Maybe Int    <?> "Total number of weeks to calculate the weeks from. [Optional. Default: weeks to be 90 years of age.]"
  , lineLength    :: w ::: Maybe Int    <?> "Amount of weeks to display per line. [Optional. Default: 52, a year]"
  , yearStart     :: w ::: Bool         <?> "Start the diagram at the start of the start date's year, instead of at the start date. [Optional. Default: False]"
  , diagramHeight :: w ::: Maybe Double <?> "Height of the diagram. [Optional. Default: 500]"
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
    (startDate args ++ ".png")
    (mkHeight (fromMaybe (500 :: Double) (diagramHeight args)))
    (lifeDiagram (div dayDiff 7) 90  # bgFrame 1 white)


