pkgname <- "FastImputation"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "FastImputation-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('FastImputation')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("BoundNormalizedVariable")
### * BoundNormalizedVariable

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BoundNormalizedVariable
### Title: Take a normalized variable and transform it back to a bounded
###   variable.
### Aliases: BoundNormalizedVariable

### ** Examples

  constraints=list(lower=5)           # lower bound when constrining to an interval
  constraints=list(upper=10)          # upper bound when constraining to an interval
  constraints=list(lower=5, upper=10) # both lower and upper bounds



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BoundNormalizedVariable", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("FastImputation")
### * FastImputation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: FastImputation
### Title: Use the pattern learned from the training data to impute (fill
###   in good guesses for) missing values.
### Aliases: FastImputation

### ** Examples

data(FI_train)   # provides FItrain dataset
patterns <- TrainFastImputation(
  FI_train,
  constraints=list(list(2, list(lower=0)),           # continuous var with lower bound
                   list(5, list(upper=0)),           # continuous var with only upper bound
                   list(6, list(lower=0, upper=1))   # bounded to a finite interval
                   ),
  idvars=1,  # user ids; also used for any variable not to be imputed
  categorical=9)

data(FI_test)
FI_test          # note there is missing data
## Not run: imputed.data <- FastImputation(FI_test, patterns)
## Not run: imputed.data    # good guesses for missing values are filled in

data(FI_true)
## Not run: imputation.rmse <- sqrt(sum( (imputed.data - FI_true)^2 )/sum(is.na(FI_test)))
## Not run: imputation.rmse

## Not run: library("caret")
## Not run: confusionMatrix(data=imputed.data$V9, reference=FI_true$V9)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("FastImputation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("LimitToSet")
### * LimitToSet

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: LimitToSet
### Title: Coerce numeric values into a given set.
### Aliases: LimitToSet

### ** Examples

x <- runif(100, min=0, max=10)
y <- LimitToSet(x, set=c(1:10))
plot(x, y)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("LimitToSet", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NormalizeBoundedVariable")
### * NormalizeBoundedVariable

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NormalizeBoundedVariable
### Title: Take a variable bounded above/below/both and return an unbounded
###   (normalized) variable.
### Aliases: NormalizeBoundedVariable

### ** Examples

  constraints=list(lower=5)           # lower bound when constrining to an interval
  constraints=list(upper=10)          # upper bound when constraining to an interval
  constraints=list(lower=5, upper=10) # both lower and upper bounds



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NormalizeBoundedVariable", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("TrainFastImputation")
### * TrainFastImputation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TrainFastImputation
### Title: Learn from the training data so that later you can fill in
###   missing data
### Aliases: TrainFastImputation

### ** Examples


data(FI_train)   # provides FI_train dataset

patterns_with_constraints <- TrainFastImputation(
  FI_train,
  constraints=list(list(2, list(lower=0)),           # continuous var with lower bound
                   list(5, list(upper=0)),           # continuous var with only upper bound
                   list(6, list(lower=0, upper=1))   # bounded to a finite interval
                   ),
  idvars=1,  # user ids; also used for any variable not to be imputed
  categorical=9)
  



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TrainFastImputation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
