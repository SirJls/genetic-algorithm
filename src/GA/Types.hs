{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  GA.Types
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
module GA.Types
   ( Individual(..)
   , Children
   , Chromosome
   , Config(..)
   , Constraint(..)
   , GA(..)
   , Parents
   , Population
   , ProblemType(..)
   , Solution(..)
   )
where

import           Control.Monad.Reader
import           Control.Monad.State
import           System.Random

-- | Minimization or Maximization problem
data ProblemType = MIN | MAX deriving (Show, Enum, Eq)


-- | Number range constraint on the genes 
data Constraint = Constraint {
    lower :: !Double -- ^ Lower boundary constraint
  , upper :: !Double -- ^ Upper boundary constraint 
} deriving (Show)

-- | Configuration settings used by the GA
data Config = Config
  { generations    :: !Int             -- ^ The number of iterations to evolve the population
  , constraint     :: Maybe Constraint -- ^ The Constraint on the type and boundary
  , crossoverRate  :: !Double          -- ^ Crossover probability
  , elitism        :: !Bool            -- ^ Preserve the strongest individual of each generation
  , mutationRate   :: !Double          -- ^ Mutation probability
  , populationSize :: !Int             -- ^ The size of the population
  , chromLength    :: !Int             -- ^ Length of the chromosome
  , problemDesc    :: Maybe String     -- ^ The description of the problem
  , problemType    :: !ProblemType     -- ^ The type of problem in {minimization, maximization}
  } deriving (Show)

-- | Chromosome made up of genes
type Chromosome = [Double]

-- | Population is made up of N Individuals
type Population = [Individual]

-- | Type that represents the selected children after cross-over.
type Children = (Individual, Individual)

-- | Type that represent the parents that are selected for cross-over.
type Parents = (Individual, Individual)

-- | A measure of the performance, called cost for minimization problems, or
-- fitness for maximization problems.
type Objective = Double

-- | Data type for the representation of an Individual
data Individual = Individual
  { objective  :: !Double     -- ^ Fitness of the individual
  , chromosome :: !Chromosome
  } deriving (Show, Eq)

-- | Individuals are compared based on their objective data
instance Ord Individual where
   x <= y = objective x <= objective y

-- | Data for the solution of the 'GA'
data Solution = Solution
  { bestIndividual      :: Maybe Individual -- ^ The current best individual of the generation
  , population          :: ![Individual]    -- ^ The current population of the generation
  , populationCount     :: !Int             -- ^ The current amount of individuals that make up the population
  } deriving (Show, Eq)

-- | Solutions are compared based on their bestIndividual
instance Ord Solution where
   x <= y = bestIndividual x <= bestIndividual y

-- | The GA monad, 'ReaderT' and 'StateT' transformers over 'IO'
-- encapsulating the problem configuration and state, respectively.
newtype GA a =
  GA (ReaderT Config (StateT Solution IO) a)
  deriving (Functor, Monad, MonadIO, MonadState Solution, MonadReader Config)

instance Applicative GA where
   pure  = return
   (<*>) = ap

instance Semigroup a => Semigroup (GA a) where
   (<>) = liftM2 (<>)

instance Monoid a => Monoid (GA a) where
   mempty  = return mempty
   mappend = (<>)
