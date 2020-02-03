# HASKELL GENETIC ALGORITHM LIBRARY

*** A continous genetic algorithm (GA) in pure haskell ***

## Description

This is a continous genetic algorithm library written in pure Haskell, that offers an easy interface to start optimizing your solution.


## Dependencies

### Software
See the `genetic-algorithm.cabal` file.

### Tooling
* Cabal
	- Linux, use your favorite distro to install, or build cabal from [source](http://www.haskell.org/cabal/download.html)
	- Windows, install the [Haskell platform](https://www.haskell.org/platform/) or get a [pre-built binary](www.haskell.org/cabal/download.html)


## How to use

- Create/clone this library
- Link to it from your haskell project and then:
```haskell
import GA

-- Create an initial solution
initialSolution :: GA.Solution
initialSolution = Solution { GA.bestIndividual  = Nothing
                           , GA.population      = []
                           , GA.populationCount = 0
                           }

-- Create a configuration
config :: GA.Config
config = GA.Config { GA.generations    = 1500
                   , GA.constraint     = Just (Constraint (-0.5) 0.501)
                   , GA.crossoverRate  = 0.4
                   , GA.elitism        = True
                   , GA.mutationRate   = 0.1
                   , GA.populationSize = 30
                   , GA.chromLength    = 20
                   , GA.problemDesc    = Nothing
                   , GA.problemType    = MIN
                   }

-- create an objective function
objectiveFunction cs betas = cs `sse` betas

-- Start optimizing (bi = best individual, sol = complete solution (includes whole population from best indindividual)
(bi, sol) <- GA.runGA config initialSolution
	$ GA.train (objectiveFunction cs)
```
