# 8 - Linear Fisher's discriminant
### Assignment objective
- Implement linear Fisher's discriminant
- Make a plot showing the algorithm in action
- Point on the method's advantages in comparison to the plug-in algorithm

### Assignment implementation
###### Theory
Before, we assumed that conditional probability density functions are normally distributed with mean and covariance parameters. Now we make the additional simplifying assumption that the class covariance matrices are all the same. Thus we increase the number of objects used to compute the covariance matrix, make it more stable along with simplifying the learning algorithm. The borderline of this method is always a straight line. It generalizes well on datasets where classes have clear borders and simple shape, but fails in cases when classes have some particular curvatures in their shape.

The "average" covariance matrix over the whole dataset is computed according to the following formulae: 
<a href="https://www.codecogs.com/eqnedit.php?latex=\Sigma=\frac{1}{l&space;-&space;|Y|}\sum^l_{i&space;=&space;1}(x_i&space;-&space;\mu_{y_i})(x_i&space;-&space;\mu_{y_i})^T" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\Sigma=\frac{1}{l&space;-&space;|Y|}\sum^l_{i&space;=&space;1}(x_i&space;-&space;\mu_{y_i})(x_i&space;-&space;\mu_{y_i})^T" title="\Sigma=\frac{1}{l - |Y|}\sum^l_{i = 1}(x_i - \mu_{y_i})(x_i - \mu_{y_i})^T" /></a>

###### Classification map examples
| ![](https://i.imgur.com/0I16ml8.png) | ![](https://i.imgur.com/ob6buHx.png) |
| - | - |

###### Comparison to the plug-in algorithm & method's advantages
| ![](https://i.imgur.com/g3s6ApT.png) | ![](https://i.imgur.com/tKVoRjC.png) |
| - | - |
| Linear discriminant of Fisher | Plug-in algorithm |

The fact that we compute the covariance matrix over the whole dataset makes the algorithm more stable, while keeping the learning algorithm relatively simple. It's well known that the simpler model is the better it generalizes on real world data, and LDF does exactly that.

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
  covMatrixInverse <- solve(covMatrix)
  a <- -(t(expectation) %*% covMatrixInverse %*% expectation) / 2
  b <- t(point) %*% covMatrixInverse %*% expectation
  
  return(a + b)
}

getLdfProbability <- function(dset, classesColumn, className, point, lambda, covMatrix, expectations) {
  featuresCount <- length(point)
  croppedDset <- dset[which(dset[, classesColumn] == className), ]
  dsetLength <- dim(dset)[1]
  
  prior <- getPriorProbabilities(dset, classesColumn)[className]

  # lambda = rep(1, featuresCount)
  likelihood <- getLikelihood(point, expectations, covMatrix)
  
  probability <- log(lambda * prior) + likelihood;
  return (probability)
}

ldfClassifier <- function(point, dset) {
  classes <- unique(dset[, 3])
  classesCount <- length(classes)
  scores <- array(dim = classesCount)
  
  expectations <- array(dim = featuresCount)
  for (i in 1:featuresCount) {
    expectations[i] <- mean(croppedDset[, i])
  }
  covMatrix <- getCovMatrix(dset, matrix(expectations, nrow=1))
  for (i in 1:classesCount) {
    scores[i] <- getPlugInProbability(dset, 'class', classes[i], point, lambdas[i], covMatrix, expectations)
  }
  # print(scores)
  return(classes[which.max(scores)])
}
```

###### Classification map & borderline
| <img src="https://i.imgur.com/ZT8V3hl.png" width="900"> | <img src="https://i.imgur.com/ZJRamxB.png" width="900"> |
| - | - |
| The LDF classification map. | The borderline. |