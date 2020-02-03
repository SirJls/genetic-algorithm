-- |
-- Module      :  GA.Core
-- Copyright   :  Joris Sparreboom 2019
-- License     :  BSD3
--
-- Maintainer  :  jlssparreboom@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- A 'Genetic Algorithm' (GA)
--
module GA.Core
   ( train
   , rouletteWheel
   , createPopulation
   , initialPopulation
   , mutate
   , maybePreserveBest
   , computeObjectives
   )
where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import           GA.Types
import           GA.Utilities
import           System.Random


--------------------------------------------------------------------------------
-- Core logic of the Genetic Algorithm                                        --
--------------------------------------------------------------------------------

-- | Produce two Parents
rouletteWheel :: GA Parents
rouletteWheel = do
   s <- get
   c <- ask
   let len = populationSize c
   let low = objective $ minimum $ population s
   let
      pop =
         [ newObjective i (objective i + (abs low * 0.01)) | i <- population s ]
   mother <- io $ spin (len - 1) 0.0 pop len
   father <- io $ spin (len - 1) 0.0 pop len
   let pop' = filter (/= mother) pop
   let len' = length pop'
   -- The father is the same as the mother?
   if mother == father
      then
      -- spin again
           ensureUniq mother (len' - 1) pop' len'
      else return (mother, father)
 where
  ensureUniq mother tim pop len = do
     father <- io $ spin tim 0.0 pop len
     return (mother, father)
  spin tim cummulative pop len = do
     rno <- randomRIO (0.0, 1.0) :: IO Double
     let ind          = pop !! tim
     let ts = foldl (\acc i -> objective i + acc) 0.0 pop
     let cummulative' = cummulative + (objective ind / ts)
     if tim == 0
        then return $ pop !! (len - 1)
        else if cummulative' >= rno
           then return ind
           else spin (tim - 1) cummulative' pop len

-- | Take a 'GA' monad and produce two children
crossover :: Parents -> GA Children
crossover (mother, father) = do
   s        <- get
   c        <- ask
   pc       <- io (randomRIO (0.0, 1.0) :: IO Double)
   children <- cross mother father
   if crossoverRate c > pc then return (father, mother) else return children
 where
  cross :: Individual -> Individual -> GA Children
  cross father mother = do
     c <- ask
     let m_c  = chromosome mother -- Chromosome of mother
     let f_c  = chromosome father -- Chromosome of father
     let nvar = length f_c
     n <- io (randomRIO (0.0, 1.0) :: IO Double)
     let a          = round (n * fromIntegral nvar)
     let (f_l, f_h) = splitAt a f_c
     let (m_l, m_h) = splitAt a m_c
     b <- io (randomRIO (0.0, 1.0) :: IO Double)
     if a == nvar
        then
      -- swap eveything left
           let (c1_c, c2_c) =
                  ( map (\(pma, pda) -> pma - b * (pma - pda)) (zip m_l f_l)
                  , map (\(pma, pda) -> pda + b * (pma - pda)) (zip m_l f_l)
                  )
           in
              return
                 ( newChromosome mother (c1_c <> f_h)
                 , newChromosome father (c2_c <> m_h)
                 )
        else
      -- swap eveything right
           let (c1_c, c2_c) =
                  ( map (\(pma, pda) -> pma - b * (pma - pda)) (zip m_h f_h)
                  , map (\(pma, pda) -> pda + b * (pma - pda)) (zip m_h f_h)
                  )
           in
              return
                 ( newChromosome mother (f_l <> c1_c)
                 , newChromosome father (m_l <> c2_c)
                 )

-- | Mutate a N number of chromosomes of N number of Individuals, where N =
--   mutationRate * populationSize * chromLength
mutate :: GA ()
mutate = do
   s  <- get
   c  <- ask
   rn <- io (randomRIO (0.0, 1.0) :: IO Double)
   let pop         = population s
   let mr          = mutationRate c
   let ps          = populationSize c
   let cl          = chromLength c
   let noMutations = round $ mr * fromIntegral (ps - 1) * fromIntegral cl
   mrow <- indices noMutations (ps - 1) []
   mcol <- indices noMutations (cl - 1) []
   mutate' mrow mcol
 where
  mutate' :: [Int] -> [Int] -> GA ()
  mutate' []       []       = return ()
  mutate' (x : xs) (y : ys) = do
     c <- ask
     s <- get
     let pop = population s
     let i   = pop !! x
     no <- case constraint c of
        Just r  -> io (randomRIO (lower r, upper r) :: IO Double)
        Nothing -> io (randomRIO (-1.0, 1.0) :: IO Double)
     let i_c  = newChromosome i (assignToIndex y no (chromosome i))
     let pop' = assignToIndex x i_c pop
     put $ newPopulation s pop'
     mutate' xs ys
  indices :: Int -> Int -> [Int] -> GA [Int]
  indices n len acc = do
     s     <- get
     index <- io (randomRIO (0, len) :: IO Int)
     if n == 0
        then return acc
        else case bestIndividual s of
           Nothing -> indices (n - 1) len (index : acc)
           Just i  -> if i == population s !! index
              then indices n len acc
              else indices (n - 1) len (index : acc)

-- | Preserve the best Individual.
maybePreserveBest :: GA Solution
maybePreserveBest = do
   s <- get
   c <- ask
   if elitism c && populationCount s > 0 then best c s else return s
 where
  best :: Config -> Solution -> GA Solution
  best c st = do
     let i = case problemType c of
            MIN -> minimum (population st)
            MAX -> maximum (population st)
     let s'   = newBestIndividual st (Just i)
     let s''  = newPopulation s' [i]
     let s''' = newPopulationCount s'' 1
     return s''

-- | Create a new Population
createPopulation :: Config -> Int -> Population -> GA Population
createPopulation _ 0 acc = return acc
createPopulation c n acc = do
   ind <- createIndividual (chromLength c) []
   createPopulation c (n - 1) (ind : acc)
 where
  createIndividual :: Int -> [Double] -> GA Individual
  createIndividual 0 acc = return $ Individual 0.0 acc
  createIndividual n acc = do
     no <- case constraint c of
        Just r  -> io (randomRIO (lower r, upper r) :: IO Double)
        Nothing -> io (randomRIO (-1.0, 1.0) :: IO Double)
     createIndividual (n - 1) (no : acc)

-- | Create a new initial population
initialPopulation :: GA ()
initialPopulation = do
   s   <- get
   c   <- ask
   pop <- createPopulation c (populationSize c) []
   let s' = newPopulation s pop
   put $ newPopulationCount s' (populationSize c)

-- | Compute the objective score for every Individual in the Solution
computeObjectives :: ([Double] -> Double) -> GA ()
computeObjectives f = do
   s <- get
   let pop  = population s
   let f'   = f . chromosome
   let rpop = map (\i -> newObjective i (f' i)) pop 
   put $ newPopulation s rpop


-- | Start training the population
train :: ([Double] -> Double) -> GA Individual
train f = do
   c <- ask
   initialPopulation
   evolve (generations c)
 where
  evolve gen = do
     c <- ask
     s <- maybePreserveBest
     nextPopulation (population s) (populationCount s)
     computeObjectives f
     case gen of
        0 ->
           let i = case problemType c of
                  MIN -> minimum (population s)
                  MAX -> maximum (population s)
           in  return i
        _ -> evolve (gen - 1)
   where
    nextPopulation pop size = do
       s                 <- get
       c                 <- ask
       parents           <- rouletteWheel
       (sister, brother) <- crossover parents
       mutate
       let thereIsroom = size < populationSize c
       let (size', pop') =
              if thereIsroom then (size + 1, sister : pop) else (size, pop)
       let thereIsMoreRoom = size' < populationSize c
       let (size'', pop'') = if thereIsMoreRoom
              then (size' + 1, brother : pop')
              else (size', pop')
       if size'' == populationSize c
          then put
             $ newPopulationCount (newPopulation s pop'') (populationSize c)
          else nextPopulation pop'' size''
