# mlrintermbo: mlrMBO mlr3 Interface <img src="todo-files/mlrintermbo.png" width="300" align="right" />

[![check](https://github.com/mb706/mlrintermbo/actions/workflows/check.yml/badge.svg)](https://github.com/mb706/mlrintermbo/actions/workflows/check.yml)
[![Coverage](https://codecov.io/github/mb706/mlrintermbo/branch/master/graphs/badge.svg)](https://app.codecov.io/github/mb706/mlrintermbo)
[![CRAN Status Badge](https://www.r-pkg.org/badges/version/mlrintermbo)](https://CRAN.R-project.org/package=mlrintermbo)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/mlrintermbo)](https://CRAN.R-project.org/package=mlrintermbo)

## What Is This?

Currently, it is not possible to use [mlrMBO](https://github.com/mlr-org/mlrMBO) for tuning [mlr3](https://github.com/mlr-org/mlr3) and related packages directly, because of some disagreements between S3 (as used in mlrMBO) and R6 (used in mlr3). [mlr3mbo](https://github.com/mlr-org/mlr3mbo/) exists, but it is not yet as mature and feature-rich as mlrMBO. `mlrintermbo` provides the necessary interface to make mlrMBO accessible for mlr3.

To use `mlrintermbo`, one should **NOT** load `mlrMBO` as a library in the current R session. Instead, `mlrintermbo` will run mlrMBO on a different background R session to keep it sectioned off from the main process. Just load the tuner (for tuning mlr3 "Learners") or optimizer (for tuning bbotk "Objectives"):

```r
library("mlrintermbo")

# Tuning Learners:
library("mlr3tuning")
tuner <- tnr("intermbo")

# Tuning Objectives
library("bbotk")
optimizer <- opt("intermbo")
```

The tuner / optimizer provide an *extensive* `ParamSet` to configure the MBO method, covering practically everything that can usually be configured with an `MBOControl` object. To find out the specific function of each control parameter, read the [mlrMBO reference](https://mlrmbo.mlr-org.com/reference/index.html) entries of functions regarding "mlrMBO Control".

## Installing

When installing `mlrintermbo`, the required `mlrMBO` package is not installed automatically. It is therefore necessary to install `mlrMBO` manually:

```r
install.packages("mlrMBO")
install.packages("mlrintermbo")
```

## Known Issues
* **Assertion on 'xdt' failed**
    ```r
    Error in .__OptimInstance__eval_batch(self = self, private = private,  : 
      Assertion on 'xdt' failed: Must have at least 1 rows, but has 0 rows.
    ```
    
    This is caused by a [bug in the `callr` package](https://github.com/r-lib/callr/issues/184). The bug is fixed on CRAN, installing the current versions using `install.packages(c("callr", "processx"))` should fix the issue.
* **Most other errors**

   Some errors, for example
   
    ```r
    Error: Domains[,1] must be less than or equal to Domains[,2]
    ```
    
    are caused because the surrogate model is failing. (The error above happens when the objective function is giving constant values, which the surrogate learner does not handle well). Initialize the tuner with 
`on.surrogate.error` set to `"warn"` or `"quiet"` to ignore errors of the surrogate model. E.g.:

    ```r
    tuner <- tnr("intermbo", on.surrogate.error = "warn")  # alternatively "quiet"
    ```

## License

LGPL-3
