library(MASS)
getPriorProbabilities <- function (dset, column) {
  dsetLength <- dim(dset)[1]
  return (table(dset[, column]) / dsetLength)
}

getCovMatrix <- function(dset, expectation) {
  featureTypesCount <- dim(dset)[2]
  dsetLength <- dim(dset)[1]
  cov <- matrix(0, nrow = featureTypesCount - 1, ncol = featureTypesCount - 1)
  
  for (i in 1:dsetLength) {
    currentFeature <- as.matrix(dset[i, 1:featureTypesCount - 1], nrow = 1)
    tmp <- t(currentFeature - expectation) %*% (currentFeature - expectation)
    cov <- cov + tmp
  }
  cov <- cov / (dsetLength - 1)
  
  return(cov)
}

getLikelihood <- function(point, expectation, covMatrix) {
  pointMinusMean <- t(as.matrix(point - expectation))
  
  covMatrixInverse <- solve(covMatrix)
  nom <- exp(-(pointMinusMean %*% covMatrixInverse %*% t(pointMinusMean)) / 2)
  denom <- sqrt((2 * pi) ^ nrow(covMatrix) * det(covMatrix))
  
  return(nom / denom)
}

getPlugInProbability <- function(dset, classesColumn, className, point, lambda) {
  featuresCount <- length(point)
  croppedDset <- dset[which(dset[, classesColumn] == className), ]
  dsetLength <- dim(dset)[1]

  prior <- getPriorProbabilities(dset, classesColumn)[className]
  
  expectations <- array(dim = featuresCount)
  for (i in 1:featuresCount) {
    expectations[i] <- mean(croppedDset[, i])
  }
  
  
  # lambda = rep(1, featuresCount)
  covMatrix <- getCovMatrix(croppedDset, matrix(expectations, nrow=1))
  likelihood <- getLikelihood(point, expectations, covMatrix)
  
  probability <- lambda * prior * likelihood;
  
  return (probability)
}

plugInClassifier <- function(point, dset) {
  classes <- unique(dset[, 3])
  classesCount <- length(classes)
  scores <- array(dim = classesCount)
  for (i in 1:classesCount) {
    scores[i] <- getPlugInProbability(dset, 'class', classes[i], point, lambdas[i])
  }
  return(classes[which.max(scores)])
}

# mu1 <- c(0, 0)
# mu2 <- c(4, 4)
# sig1 <- matrix(c(2, 0.9, 0.9, 2), 2, 2)
# sig2 <- matrix(c(0.5, 0, 0, 2), 2, 2)
# dset1 <- mvrnorm(250, mu1, sig1)
# dset2 <- mvrnorm(250, mu2, sig2)
# dset <- rbind(cbind(dset1, 1),cbind(dset2, 2))

dset = data.frame(x = double(), y = double(), class = character())

dset <- rbind(dset, data.frame(x = 0, y = 2, class = 1))
dset <- rbind(dset, data.frame(x = 2, y = 0, class = 1))
dset <- rbind(dset, data.frame(x = 2.5, y = 2.5, class = 1))
dset <- rbind(dset, data.frame(x = 2.8, y = 2.8, class = 1))

dset <- rbind(dset, data.frame(x = 2, y = 4.5, class = 2))
dset <- rbind(dset, data.frame(x = 4.5, y = 2, class = 2))
dset <- rbind(dset, data.frame(x = 6, y = 6, class = 2))
dset <- rbind(dset, data.frame(x = 6, y = 5, class = 2))
dset <- rbind(dset, data.frame(x = 5, y = 6, class = 2))
dset <- rbind(dset, data.frame(x = 3, y = 5.5, class = 2))
dset <- rbind(dset, data.frame(x = 5.5, y = 3, class = 2))

plot(dset[ ,1], dset[ ,2], pch=21, bg=c("green","blue")[dset[ ,3]])

# print(plugInClassifier(c(0, 0), dset))
# # print(getPlugInProbability(dset, 3, 1, c(0, 0), 1))

## generating map
colors <- c("1" = "green", "2" = "blue")
xs <- seq(from = -5, to = 7, by = 0.2)
ys <- seq(from = -5, to = 7, by = 0.2)
## lambdas <- runif(2, min=1, max=100)
lambdas = c(1, 1)
print(lambdas)

all <- length(xs) * length(ys)
progress = 1
for (i in xs) {
  for (j in ys) {
    cat("\rProcessing point", progress, "of", all)
    progress <- progress + 1
    result <- plugInClassifier(c(i, j), dset)
    points(i, j, col = colors[result], pch = 21)
  }
}
cat("\rDone.                      \n")
