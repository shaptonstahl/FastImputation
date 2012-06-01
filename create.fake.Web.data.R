# Generate fake data like real data

library(mvtnorm)            # provides rmvnorm for multivariate normal draws

# HapLap: setwd("C:/Documents and Settings/Steve/My Documents/Dropbox/FastImputation")
real.data <- read.table("rf.tsv", header=TRUE,sep="\t",na.strings = "?")[,1:10]

norm.real.data <- real.data
names(norm.real.data) <- c("cust.id",
  "order.id",
  "is.fraud",
  "customer.age.yrs",
  "spent.days.0to2",
  "spent.days.3to10",
  "spent.days.11to30",
  "geo.ip.fraud.rate",
  "account.age.days",
  "days.to.first.purchase"
)  
head(norm.real.data)

norm.real.data$cust.id <- sapply(norm.real.data$cust.id, function(real.id){
  which(real.id == unique(norm.real.data$cust.id))
})
norm.real.data$order.id <- 1:nrow(norm.real.data)
# norm.real.data$is.fraud  # just fine
norm.real.data$customer.age.yrs <- log(norm.real.data$customer.age.yrs)
norm.real.data$spent.days.0to2 <- log(pmax(round(norm.real.data$spent.days.0to2, 2), .01))
norm.real.data$spent.days.3to10 <- log(pmax(round(norm.real.data$spent.days.3to10, 2), .01))
norm.real.data$spent.days.11to30 <- log(pmax(round(norm.real.data$spent.days.11to30, 2), .01))
norm.real.data$geo.ip.fraud.rate <- qnorm(pmin(pmax(norm.real.data$geo.ip.fraud.rate, .01), .99))
norm.real.data$account.age.days <- log(pmax(norm.real.data$account.age.days, 1))
norm.real.data$days.to.first.purchase <- log(pmax(norm.real.data$days.to.first.purchase, 1))

norm.real.imp.data <- norm.real.data[,3:10]

real.norm.means <- colMeans(norm.real.imp.data)
real.norm.cov <- cov(norm.real.imp.data, use="complete.obs")

# Now, use this and rmvnorm to generate fake data
norm.fake.data <- as.data.frame(rmvnorm(nrow(norm.real.data), 
  mean=real.norm.means,
  sigma=real.norm.cov))
names(norm.fake.data) <- c("is.fraud",
  "customer.age.yrs",
  "spent.days.0to2",
  "spent.days.3to10",
  "spent.days.11to30",
  "geo.ip.fraud.rate",
  "account.age.days",
  "days.to.first.purchase")  
norm.fake.data <- data.frame(norm.real.data[,1:2], norm.fake.data)

LimitToSet <- 
function(x, 
  set
) {
  return( sapply(x, function(this.x) {
    index.winner <- which.min(abs(this.x - set))
    return( set[index.winner] )
  }) )
}

MakeSomeDataMissing <- function(x, prob.dropped=.1) {
  return( as.data.frame( 
    apply(x, c(1,2), function(this.cell) {
      return( ifelse(runif(1) < prob.dropped, NA, this.cell) )
    }) 
  ) )
}

fake.data <- norm.fake.data

# Transform back to un-normed data
fake.data$is.fraud <- LimitToSet(norm.real.data$is.fraud, set=c(0:1))
fake.data$customer.age.yrs <- exp(norm.fake.data$customer.age.yrs)
fake.data$spent.days.0to2 <- round(exp(norm.fake.data$spent.days.0to2), 2)
fake.data$spent.days.3to10 <- round(exp(norm.fake.data$spent.days.3to10), 2)
fake.data$spent.days.11to30 <- round(exp(norm.fake.data$spent.days.11to30), 2)
fake.data$geo.ip.fraud.rate <- pnorm(norm.fake.data$geo.ip.fraud.rate)
fake.data$account.age.days <- pmax(round(exp(norm.fake.data$account.age.days)), 1)
fake.data$days.to.first.purchase <- pmax(round(exp(norm.fake.data$days.to.first.purchase)), 1)

saveRDS(fake.data, file="fake.data.rds")
# load with: fake.data <- readRDS(file="fake.data.rds")

# Create data for package
FItrain <- fake.data[1:10000,]
FItest <- FItrue <- fake.data[10001:10010,]
# create some holes in the data
FItest <- cbind(FItrue[,1:3], MakeSomeDataMissing(FItrue[,4:10], .2))

save(FItrain, file="dev/FastImputation/data/FItrain.RData")
save(FItest, file="dev/FastImputation/data/FItest.RData")
save(FItrue, file="dev/FastImputation/data/FItrue.RData")
