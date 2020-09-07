# mlrintermbo: mlrMBO-mlr3-interface <img src="todo-files/mlrintermbo.png" width="300" align="right" />

[![Build Status](https://travis-ci.org/mb706/mlrintermbo.svg?branch=master)](https://travis-ci.org/mb706/mlrintermbo)
[![Coverage](https://codecov.io/github/mb706/mlrintermbo/branch/master/graphs/badge.svg)](https://codecov.io/github/mb706/mlrintermbo)
[![CRAN Status Badge](https://www.r-pkg.org/badges/version/mlrintermbo)](https://CRAN.R-project.org/package=mlrintermbo)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/mlrintermbo)](https://CRAN.R-project.org/package=mlrintermbo)

## What Is This?

Use [mlrMBO](https://github.com/mlr-org/mlrMBO) for tuning [mlr3](https://github.com/mlr-org/mlr3).

```r
tuner <- tnr("intermbo")
optimizer <- opt("intermbo")
```

The tuner / optimizer provide an *extensiv* `ParamSet` to configure the MBO method.

## Installation

```r
remotes::install_github("mb706/mlrintermbo", dependencies = TRUE)
```

## License

AGPL 
