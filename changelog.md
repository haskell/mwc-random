## Changes in 0.15.2.0

  * Support for `random-1.3`.


## Changes in 0.15.1.0

  * Additon of binomial sampler using the rejection sampling method in
    Kachitvichyanukul, V. and Schmeiser, B. W.  Binomial Random
    Variate Generation.  Communications of the ACM, 31, 2 (February,
    1988) 216. <https://dl.acm.org/doi/pdf/10.1145/42372.42381>. A more
    efficient basis for e.g. the beta binomial distribution:
	`beta a b g >>= \p -> binomial n p g`.

## Changes in 0.15.0.2

  * Doctests on 32-bit platforms are fixed. (#79)


## Changes in 0.15.0.1

  * Bug in generation of Int/Word in both uniform and uniformR is fixed. (#75)


## Changes in 0.15.0.0

  * `withSystemRandomST` and `createSystemSeed` are added.

  * `withSystemRandom` is deprecated.

  * `random>=1.2` is dependency of `mwc-random`.

  * Instances for type classes `StatefulGen` & `FrozenGen` defined in random-1.2
    are added for `Gen`.

  * Functions in `System.Random.MWC.Distributions` and
    `System.Random.MWC.CondensedTable` now work with arbitrary `StatefulGen`

  * `System.Random.MWC.uniformVector` now works with arbitrary `StatefulGen` as
    well and uses in-place initialization instead of `generateM`. It should be
    faster for anything but IO and ST (those shoud remain same).


## Changes in 0.14.0.0

  * Low level functions for acquiring random data for initialization
    of PRGN state is moved to `System.Random.MWC.SeedSource` module

  * Ensure that carry is always correct when restoring PRNG state from
    seed. Only affects users who create 258 element seed manually.
    (#63, #65)


## Changes in 0.13.6.0

  * `tablePoisson` now can handle λ>1923, see #59 for details.
    That required intoduction of dependency on math-functions.


## Changes in 0.13.5.0

  * `logCategorical` added

## Changes in 0.13.4.0

  * `withSystemRandom` uses RtlGenRandom for seeding generator on windows


## Changes in 0.13.3.1

  * primitive-0.6 compatibility


## Changes in 0.13.3.0

  * Monadic variant of vector shuffle added: `uniformShuffleM`

  * Context on `uniformShuffle` loosened


## Changes in 0.13.2.2

  * Fixed crash during gen. initialization on Windows when stderr
    is not available (#36).

## Changes in 0.13.2.0

  * Generators for beta, Bernoully, Dirichlet and categorical distributions
    added.

  * Functions for generating random shuffles added.


## Changes in 0.13.1.2

  * GHC 7.9 support


## Changes in 0.13.1.1

  * Long standing performance problem in normal distribution fixed (#16)


## Changes in 0.13.1.0

  * `createSystemRandom` added


## Changes in 0.13.0.0

  * Workaround for GHC bug 8072 (bug 25). GHC 7.6 on 32-bit platrofms is
    affected.

  * Generators for truncated exponential and geometric distributions
    added.


## Changes in 0.12.0.0

  * Fucntion `asGenIO` and `asGenST` added.

  * Generation of discrete random variates using condensed tables
    methed. Tables for Poisson and binomial distributions are
    provided.
