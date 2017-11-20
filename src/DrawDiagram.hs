module DrawDiagram where

import           Data.Time

import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude


data DiagramConfig = DiagramConfig
  { startDate    :: Day
  , endDate      :: Day
  , diagramStart :: Day
  , yearStart    :: Bool
  , lineLength   :: Int
  }


lifeInWeeksDiagram :: DiagramConfig -> Diagram B
lifeInWeeksDiagram x =
    vsep sepDist $
        if yearStart x then
          [hsep sepDist $
             partialEmptyRow
               (fromIntegral $ diffDays (diagramStart x) (startDate x))
               (lineLength x)
          ]
          ++
          replicate
            (div (lineLength x))
            (hsep sepDist $ replicate  filledRow)
          ++
          [hsep sepDist $

          ]
        else
          replicate (yearsAlive x) (hsep sepDist filledRow)
        ++
        replicate (yearsMax x y) (hsep sepDist emptyRow)


-- | The total weeks for this diagram config
weeks :: DiagramConfig -> Int
weeks x = fromIntegral $ diffDays (startDate x) (endDate x)

-- | The distance to separate squares and rows
sepDist :: Double
sepDist = 0.8

-- | Return the start of the year for the given Day
startOfYear :: Day -> Day
startOfYear x = fromGregorian (year $ toGregorian x) 01 01

-- | Return the year from a Gregorian triplet
year :: (Integer, Int, Int) -> Integer
year (x, _, _) = x

-- | Return a list of filled squares with length n
filledRow :: Int -> [Diagram B]
filledRow n = replicate n filledSquare

-- | Return a list of empty squares with length n
emptyRow :: Int -> [Diagram B]
emptyRow n = replicate n emptySquare

-- | Return a list starting with x empty squares, and finishing with n-x
-- | filled squares
partialEmptyRow :: Int -> Int -> [Diagram B]
partialEmptyRow x n = replicate x emptySquare ++ replicate (n-x) filledSquare

-- | Return a list starting with x filled squares, and finishing with n-x
-- | empty squares
partialFilledRow :: Int -> Int -> [Diagram B]
partialFilledRow x n = replicate x filledSquare ++ replicate (n-x) emptySquare

-- | Representation of a filled square
filledSquare :: Diagram B
filledSquare = emptySquare # fc black

-- | Representation of an empty square
emptySquare :: Diagram B
emptySquare = square 1 # lwG 0.2
