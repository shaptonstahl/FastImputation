pkgname <- "FastImputation"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('FastImputation')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("BoundNormalizedVariable")
### * BoundNormalizedVariable

flush(stderr()); flush(stdout())

### Name: BoundNormalizedVariable
### Title: Take a normalized variable and transform it back to a bounded
###   variable.
### Aliases: BoundNormalizedVariable

### ** Examples

  constraints=list(lower=5)           # lower bound when constrining to an interval
  constraints=list(upper=10)          # upper bound when constraining to an interval
  constraints=list(lower=5, upper=10) # both lower and upper bounds



cleanEx()
nameEx("FastImputation")
### * FastImputation

flush(stderr()); flush(stdout())

### Name: FastImputation
### Title: Use the pattern learned from the training data to impute (fill
###   in good guesses for) missing values.
### Aliases: FastImputation

### ** Examples

data(FItrain)   # provides FItrain dataset
patterns <- TrainFastImputation(FItrain)

data(FItest)
FItest          # note there is missing data
imputed.data <- FastImputation(FItest, patterns)
imputed.data    # good guesses for missing values are filled in

data(FItrue)
imputation.rmse <- sqrt(sum( (imputed.data - FItrue)^2 )/sum(is.na(FItest)))
imputation.rmse



cleanEx()
nameEx("LimitToSet")
### * LimitToSet

flush(stderr()); flush(stdout())

### Name: LimitToSet
### Title: Coerce numeric values into a given set.
### Aliases: LimitToSet

### ** Examples

x <- runif(100, min=0, max=10)
y <- LimitToSet(x, set=c(1:10))
plot(x, y)




cleanEx()
nameEx("NormalizeBoundedVariable")
### * NormalizeBoundedVariable

flush(stderr()); flush(stdout())

### Name: NormalizeBoundedVariable
### Title: Take a variable bounded above/below/both and return an unbounded
###   (normalized) variable.
### Aliases: NormalizeBoundedVariable

### ** Examples

  constraints=list(lower=5)           # lower bound when constrining to an interval
  constraints=list(upper=10)          # upper bound when constraining to an interval
  constraints=list(lower=5, upper=10) # both lower and upper bounds



cleanEx()
nameEx("TrainFastImputation")
### * TrainFastImputation

flush(stderr()); flush(stdout())

### Name: TrainFastImputation
### Title: Learn from the training data so that later you can fill in
###   missing data
### Aliases: TrainFastImputation

### ** Examples


data(FItrain)   # provides FItrain dataset
patterns <- TrainFastImputation(FItrain)

patterns.with.constraints <- TrainFastImputation(
  FItrain,
  constraints=list(list(1, list(set=0:1)),
                   list(2, list(lower=0)),
                   list(3, list(lower=0)),
                   list(4, list(lower=0)),
                   list(5, list(lower=0)),
                   list(6, list(lower=0, upper=1)),
                   list(7, list(lower=0)),
                   list(8, list(lower=0))))



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
