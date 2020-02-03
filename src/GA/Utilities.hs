-- |
-- Module      :  GA.Utilities
-- Copyright   :  Joris Sparreboom 2019
-- License     :  BSD3
--
-- Maintainer  :  jlssparreboom@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- A 'Genetic Algorithm' (GA)
--
--

module GA.Utilities
   ( validConfig
   , assignToIndex
   , runGA
   , newObjective
   , newChromosome
   , newBestIndividual
   , newPopulation
   , newPopulationCount
   , io
   )
where

import           GA.Types                       ( Config
                                                , Solution(..)
                                                , GA(..)
                                                , Individual(..)
                                                , Population
                                                , Chromosome(..)
                                                )
import           Control.Monad.Reader
import           Control.Monad.State

--------------------------------------------------------------------------------
-- General utility functions                                                  --
--------------------------------------------------------------------------------

-- TODO: Create verify config function, to check the boundaries for fields such
--       as the mutation rate (Should be between 0.0 && 1.0)
validConfig :: Config -> Either String Bool
validConfig = undefined

-- | Assign a value to an index
assignToIndex :: Int -> a -> [a] -> [a]
assignToIndex i y l = take i l <> [y] <> drop (i + 1) l

-- | Change the objective field of an Individual
newObjective :: Individual -> Double -> Individual
newObjective r obj = r { objective = obj }

-- | Change the chromosome of an Individual
newChromosome :: Individual -> Chromosome -> Individual
newChromosome r c = r { chromosome = c }

-- | Change the bestIndividual of a Solution
newBestIndividual :: Solution -> Maybe Individual -> Solution
newBestIndividual r i = r { bestIndividual = i }

-- | Change the population of a Solution
newPopulation :: Solution -> Population -> Solution
newPopulation r p = r { population = p, populationCount = length p }

-- | Change the population count of a solution
newPopulationCount :: Solution -> Int -> Solution
newPopulationCount r l = r { populationCount = l }

-- | Lift an 'IO' action into the 'GA' monad
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Run the 'GA' monad.
-- Return the result and final state
runGA :: Config -> Solution -> GA a -> IO (a, Solution)
runGA c s (GA a) = runStateT (runReaderT a c) s
