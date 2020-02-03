module GA.Individual
   ( Individual(..)
   , Population
   )
where

import           Data.List                      ( sortOn )

-- | Representation of an Individual
data Individual = Individual {
     fitness :: Double       -- ^ Fitness of the individual
   , fitnessArgs :: [String] -- ^ Arguments used to create the individual
} deriving (Show, Ord, Eq)

-- | Population is made up of N Individuals
type Population = [Individual]

-- individualSort :: Population -> Population
-- individualSort = sortOn fitness
