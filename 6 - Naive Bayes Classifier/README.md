# 6 - Naive normal Bayesian classifier
### Assignment objective
- Implement the naive normal Bayesian classifier
- Generate a plot, depicting the classifier in action

### Assignment implementation
###### Theory
The Bayesian approach to the classification problem is quite different from what we've studied before. This approach is based on an assumption that if the probability distribution of classes is known, then we can easily derive an optimal classification algorithm.

However, most commonly the probability distribution is not known and has to be restored based on the input data.

Abstractly, naive Bayes is a conditional probability model: given an instance to be classified - some vector consisting of n features, it assigns to this instance conditional probabilities <a href="https://www.codecogs.com/eqnedit.php?latex={\displaystyle&space;p(C_{k}\mid&space;x_{1},\ldots&space;,x_{n})\,}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?{\displaystyle&space;p(C_{k}\mid&space;x_{1},\ldots&space;,x_{n})\,}" title="{\displaystyle p(C_{k}\mid x_{1},\ldots ,x_{n})\,}" /></a> for each of possible classes (i.e. how likely is the class to be C_k given vector x?).
 
The problem of this formulation is that it works slow on large datasets. To mitigate that, we can decompose the probability above using Bayes' theorem:

<a href="https://www.codecogs.com/eqnedit.php?latex={\displaystyle&space;p(C_{k}\mid&space;\mathbf&space;{x}&space;)={\frac&space;{p(C_{k})\&space;p(\mathbf&space;{x}&space;\mid&space;C_{k})}{p(\mathbf&space;{x}&space;)}}\,}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?{\displaystyle&space;p(C_{k}\mid&space;\mathbf&space;{x}&space;)={\frac&space;{p(C_{k})\&space;p(\mathbf&space;{x}&space;\mid&space;C_{k})}{p(\mathbf&space;{x}&space;)}}\,}" title="{\displaystyle p(C_{k}\mid \mathbf {x} )={\frac {p(C_{k})\ p(\mathbf {x} \mid C_{k})}{p(\mathbf {x} )}}\,}" /></a>

We mostly care about the numerator only, since denominator doesn't depend on nether vector x nor class C_k and therefore is effectively a constant. The numerator is equivalent to the joint probability model <a href="https://www.codecogs.com/eqnedit.php?latex={\displaystyle&space;p(C_{k},x_{1},\ldots&space;,x_{n})\,}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?{\displaystyle&space;p(C_{k},x_{1},\ldots&space;,x_{n})\,}" title="{\displaystyle p(C_{k},x_{1},\ldots ,x_{n})\,}" /></a>.

This can be rewritten as follows, using the chain rule for repeated applications of the definition of conditional probability:

<a href="https://www.codecogs.com/eqnedit.php?latex={\displaystyle&space;{\begin{aligned}p(C_{k},x_{1},\ldots&space;,x_{n})&=p(x_{1},\ldots&space;,x_{n},C_{k})\\&=p(x_{1}\mid&space;x_{2},\ldots&space;,x_{n},C_{k})\&space;p(x_{2},\ldots&space;,x_{n},C_{k})\\&=p(x_{1}\mid&space;x_{2},\ldots&space;,x_{n},C_{k})\&space;p(x_{2}\mid&space;x_{3},\ldots&space;,x_{n},C_{k})\&space;p(x_{3},\ldots&space;,x_{n},C_{k})\\&=\cdots&space;\\&=p(x_{1}\mid&space;x_{2},\ldots&space;,x_{n},C_{k})\&space;p(x_{2}\mid&space;x_{3},\ldots&space;,x_{n},C_{k})\cdots&space;p(x_{n-1}\mid&space;x_{n},C_{k})\&space;p(x_{n}\mid&space;C_{k})\&space;p(C_{k})\\\end{aligned}}}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?{\displaystyle&space;{\begin{aligned}p(C_{k},x_{1},\ldots&space;,x_{n})&=p(x_{1},\ldots&space;,x_{n},C_{k})\\&=p(x_{1}\mid&space;x_{2},\ldots&space;,x_{n},C_{k})\&space;p(x_{2},\ldots&space;,x_{n},C_{k})\\&=p(x_{1}\mid&space;x_{2},\ldots&space;,x_{n},C_{k})\&space;p(x_{2}\mid&space;x_{3},\ldots&space;,x_{n},C_{k})\&space;p(x_{3},\ldots&space;,x_{n},C_{k})\\&=\cdots&space;\\&=p(x_{1}\mid&space;x_{2},\ldots&space;,x_{n},C_{k})\&space;p(x_{2}\mid&space;x_{3},\ldots&space;,x_{n},C_{k})\cdots&space;p(x_{n-1}\mid&space;x_{n},C_{k})\&space;p(x_{n}\mid&space;C_{k})\&space;p(C_{k})\\\end{aligned}}}" title="{\displaystyle {\begin{aligned}p(C_{k},x_{1},\ldots ,x_{n})&=p(x_{1},\ldots ,x_{n},C_{k})\\&=p(x_{1}\mid x_{2},\ldots ,x_{n},C_{k})\ p(x_{2},\ldots ,x_{n},C_{k})\\&=p(x_{1}\mid x_{2},\ldots ,x_{n},C_{k})\ p(x_{2}\mid x_{3},\ldots ,x_{n},C_{k})\ p(x_{3},\ldots ,x_{n},C_{k})\\&=\cdots \\&=p(x_{1}\mid x_{2},\ldots ,x_{n},C_{k})\ p(x_{2}\mid x_{3},\ldots ,x_{n},C_{k})\cdots p(x_{n-1}\mid x_{n},C_{k})\ p(x_{n}\mid C_{k})\ p(C_{k})\\\end{aligned}}}" /></a>

The "naive" assumption means that we consider all features in x to be independent. Under this assumption, <a href="https://www.codecogs.com/eqnedit.php?latex={\displaystyle&space;p(x_{i}\mid&space;x_{i&plus;1},\ldots&space;,x_{n},C_{k})=p(x_{i}\mid&space;C_{k})\,}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?{\displaystyle&space;p(x_{i}\mid&space;x_{i&plus;1},\ldots&space;,x_{n},C_{k})=p(x_{i}\mid&space;C_{k})\,}" title="{\displaystyle p(x_{i}\mid x_{i+1},\ldots ,x_{n},C_{k})=p(x_{i}\mid C_{k})\,}" /></a>.

This means that under the above independence assumptions, the conditional distribution over the class variable C is:

<a href="https://www.codecogs.com/eqnedit.php?latex={\displaystyle&space;p(C_{k}\mid&space;x_{1},\ldots&space;,x_{n})={\frac&space;{1}{Z}}p(C_{k})\prod&space;_{i=1}^{n}p(x_{i}\mid&space;C_{k})}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?{\displaystyle&space;p(C_{k}\mid&space;x_{1},\ldots&space;,x_{n})={\frac&space;{1}{Z}}p(C_{k})\prod&space;_{i=1}^{n}p(x_{i}\mid&space;C_{k})}" title="{\displaystyle p(C_{k}\mid x_{1},\ldots ,x_{n})={\frac {1}{Z}}p(C_{k})\prod _{i=1}^{n}p(x_{i}\mid C_{k})}" /></a>

Where evidence <a href="https://www.codecogs.com/eqnedit.php?latex={\displaystyle&space;Z=p(\mathbf&space;{x}&space;)=\sum&space;_{k}p(C_{k})\&space;p(\mathbf&space;{x}&space;\mid&space;C_{k})}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?{\displaystyle&space;Z=p(\mathbf&space;{x}&space;)=\sum&space;_{k}p(C_{k})\&space;p(\mathbf&space;{x}&space;\mid&space;C_{k})}" title="{\displaystyle Z=p(\mathbf {x} )=\sum _{k}p(C_{k})\ p(\mathbf {x} \mid C_{k})}" /></a>.

To make a classifier, we just have to pick a class with max probability:

<a href="https://www.codecogs.com/eqnedit.php?latex={\displaystyle&space;{\hat&space;{y}}={\underset&space;{k\in&space;\{1,\ldots&space;,K\}}{\operatorname&space;{argmax}&space;}}\&space;p(C_{k})\displaystyle&space;\prod&space;_{i=1}^{n}p(x_{i}\mid&space;C_{k}).}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?{\displaystyle&space;{\hat&space;{y}}={\underset&space;{k\in&space;\{1,\ldots&space;,K\}}{\operatorname&space;{argmax}&space;}}\&space;p(C_{k})\displaystyle&space;\prod&space;_{i=1}^{n}p(x_{i}\mid&space;C_{k}).}" title="{\displaystyle {\hat {y}}={\underset {k\in \{1,\ldots ,K\}}{\operatorname {argmax} }}\ p(C_{k})\displaystyle \prod _{i=1}^{n}p(x_{i}\mid C_{k}).}" /></a>

###### Implementation in R
Here's the code:
```R
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
```

Now let's build a classification map to see how it all works. We'll use a dataset of two classes generated by multivariate normal distribution with params listed below:
```R
mu1 <- c(0, 0)
mu2 <- c(4, 4)
sig1 <- matrix(c(2, 0.9, 0.9, 2), 2, 2)
sig2 <- matrix(c(0.5, 0, 0, 2), 2, 2)
```

| ![](https://i.imgur.com/tlLJ2K5.png) | ![](https://i.imgur.com/UDctOZs.png) |
| - | - |
| ![](https://i.imgur.com/fdsms6g.png) | ![](https://i.imgur.com/h1Xzmme.png) | 

Errors on this dataset: 16 (3,2%).