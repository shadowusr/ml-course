library(MASS)
getPriorProbabilities <- function (dset, column) {
  dsetLength <- dim(dset)[1]
  return (table(dset[, column]) / dsetLength)
}

likelihood <- function(feature, expectation, deviation) {
  return ((1 / (deviation * sqrt(2 * pi))) * exp(-((feature - expectation) ^ 2) / (2 * deviation ^ 2)))
}

# returns a probability of a given point to be in a given class
getProbability <- function(dset, classesColumn, className, point) {
  classesCount <- 0
  featuresCount <- length(point)
  dsetLength <- dim(dset)[1]
  
  prior <- getPriorProbabilities(dset, classesColumn)[className]
  lambda <- 1 # rep(1, classesCount)
  
  expectations <- array(dim = featuresCount)
  for (i in 1:featuresCount) {
    expectations[i] <- mean(dset[which(dset[, classesColumn] == className), i])
  }
  
  matr <- matrix(0, featuresCount, featuresCount)
  for (i in 1:featuresCount) {
    matr[i, ] <- sum((dset[which(dset[, classesColumn] == className), i] - expectations[i]) ^ 2) / dsetLength
  }
  deviations <- array(dim = featuresCount)
  for (i in 1:featuresCount) {
    deviations[i] <- matr[i, i]
  }
  
  # print(log(lambda * prior))
  # print(expectations)
  # print('separator')
  # print(featuresCount)
  # print(deviations)
  # print(lambda * prior)
  # print(sum(log(likelihood(point, expectations, deviations))))
  
  logOfLikelihood <- sum(log(likelihood(point, expectations, deviations)))
  
  return (log(lambda * prior) + logOfLikelihood)
}

classify <- function(point) {
  classes <- unique(dset[, 3])
  classesCount <- length(classes)
  scores <- array(dim = classesCount)
  for (i in 1:classesCount) {
    scores[i] <- getProbability(dset, 3, classes[i], point)
  }
  return(classes[which.max(scores)])
}

mu1 <- c(0, 0)
mu2 <- c(4, 4)
sig1 <- matrix(c(2, 0.9, 0.9, 2), 2, 2)
sig2 <- matrix(c(0.5, 0, 0, 2), 2, 2)
dset1 <- mvrnorm(250, mu1, sig1)
dset2 <- mvrnorm(250, mu2, sig2)
dset <- rbind(cbind(dset1, 1),cbind(dset2, 2))
plot(dset[ ,1], dset[ ,2], pch=21, bg=c("green","blue")[dset[ ,3]])


# generating map
colors <- c("1" = "green", "2" = "blue")
xs <- seq(from = -5, to = 6.2, by = 0.1)
ys <- seq(from = -4.2, to = 8, by = 0.1)

all <- length(xs) * length(ys)
progress = 1
for (i in xs) {
  for (j in ys) {
    cat("\rProcessing point", progress, "of", all)
    progress <- progress + 1
    result <- classify(c(i, j))
    points(i, j, col = colors[result], pch = 21)
  }
}
cat("\rDone.                      \n")

