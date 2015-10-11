## DMatrix.hs

Convert a Haskell Data Frame (list of list of strings) to a DMatrix (list of list of floats), suitable for [XGBoost](http://www.github.com/robertzk/xgboost.hs).

```Haskell
import qualified DMatrix as D

D.column "Petal.Length" iris  -- Get the Petal Length column of iris
D.index 1 iris                -- Get the first column of iris
D.toDMatrix iris              -- Convert the Data Frame to a DMatrix
```

## Installation

```
cabal install --dependencies-only
cabal configure
```

## Demo

There is one provided demo to output a wide variety of (currently nonsensical) information about the iris data set.

```
cabal run iris
```
