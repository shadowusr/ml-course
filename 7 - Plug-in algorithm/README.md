# 7 - Plug-in algorithm
### Assignment objective
- Implement the plug-in bayesian algorithm
- Generate a plot as a working demo of this method
- Consider cases, when the borderline is: parabola, ellipse, hyperbola

### Assignment implementation
###### Theory
The essence of plug-in algorithm is building estimates of the expectation vector and the covariance matrix and then "plugging" them in the likelihood function / optimal Bayesian classifier, hence the name. The estimates themselves are computed using the following formulas:

Expectation for the certain feature type, essentially a mean:

<a href="https://www.codecogs.com/eqnedit.php?latex=\mu=\frac{1}{m}\sum_{i&space;=&space;1}^{m}x_i;" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu=\frac{1}{m}\sum_{i&space;=&space;1}^{m}x_i;" title="\mu=\frac{1}{m}\sum_{i = 1}^{m}x_i;" /></a>

Covariance matrix estimate:

<a href="https://www.codecogs.com/eqnedit.php?latex=\Sigma=\frac{1}{1-m}\sum^m_{i&space;=&space;1}(x_i&space;-&space;\mu_{y_i})(x_i&space;-&space;\mu_{y_i})^T" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\Sigma=\frac{1}{1-m}\sum^m_{i&space;=&space;1}(x_i&space;-&space;\mu_{y_i})(x_i&space;-&space;\mu_{y_i})^T" title="\Sigma=\frac{1}{1-m}\sum^m_{i = 1}(x_i - \mu_{y_i})(x_i - \mu_{y_i})^T" /></a>

Here `m` is number of features in the input vector, `x_i` - certain feature value, and `^T` denotes matrix transposition.

###### Plots
| ![](https://i.imgur.com/QMgBm8t.png) | ![](https://i.imgur.com/tGkUcXv.png) |
| - | - |
| Normal distributions | Parabola-shaped borderline |
| ![](https://i.imgur.com/tk8cceE.png) | ![](https://i.imgur.com/PCQwiqv.png) |
| Ellipse-shaped borderline | Hyperbole-shaped borderline |

###### Code
```R
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
```