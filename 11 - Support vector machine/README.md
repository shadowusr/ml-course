# 11 - Support vector machine
### Assignment objective
- Using the kernlab library implement the SVM algorithm in R.
- Implement an algorithm for ROC curve plotting and computing AUC.
- Show how this model works for different cases:
    - linearly separable dataset;
    - linearly non-separable dataset with a small number of objects that "interfere" with linear separability;
    - linearly non-separable sample with an average number of objects that "interfere" with linear separability.
### Assignment implementation
###### Theory
Support vector machines are supervised learning models and algorithms that analyze data used for solving classification and regression problems. SVMs present one of the most robust prediction methods to day. Given a set of labeled training examples, each example belonging to one of two classes, an SVM training algorithm builds a model that assigns new examples to one of the given classes. It's a non-probabilistic binary linear classifier.

Apart from linear classification, SVMs are also capable of performing non-linear classification using the so-called kernel trick - a technique of implicitly mapping the inputs into high-dimensional feature spaces.

In case of support vector machines, a data point is viewed as a n-dimensional vector and we want to know if it's possible to separate a set of such points with a (n-1)-dimensional hyperplane. Typically, there are many hyperplanes that might do that. However, SVMs choose such hyperplane that represents the largest separation, i.e. the margin or distance between this plane and both classes is the greatest. The margin is usually computed between the hyperplane and some subset of the original dataset - a set of "support"-vectors.

An ROC curve (receiver operating characteristic curve) is a graph showing the performance of a classification model at all classification thresholds. This curve plots two parameters:
- True Positive Rate
- False Positive Rate

![](https://i.imgur.com/iOGLmnX.png)
An ROC curve plots TPR vs. FPR at different classification thresholds. Lowering the classification threshold classifies more items as positive, thus increasing both False Positives and True Positives. The following figure shows a typical ROC curve.

To compute the points in an ROC curve, we could evaluate a logistic regression model many times with different classification thresholds, but this would be inefficient. Fortunately, there's an efficient, sorting-based algorithm that can provide this information for us, called AUC.

AUC means Area Under the ROC Curve. AUC measures the entire two-dimensional area underneath the entire ROC curve from (0,0) to (1,1). AUC provides an aggregate measure of performance across all possible classification thresholds. One way of interpreting AUC is as the probability that the model ranks a random positive example more highly than a random negative example.

![](https://i.imgur.com/VeNToqz.png)

###### Implementation
We will use the kernlab library for R to implement and visualize SVM algorithm. Let's start with a simple example on how the kernlab svm model deals with normally distributed data. Here's the code:

```R
mu1 <- c(0, 0)
mu2 <- c(4, 4)
sig1 <- matrix(c(2, 0.9, 0.9, 2), 2, 2)
sig2 <- matrix(c(0.5, 0, 0, 2), 2, 2)
dset1 <- mvrnorm(50, mu1, sig1)
dset2 <- mvrnorm(50, mu2, sig2)
dset <- rbind(cbind(dset1, 1),cbind(dset2, -1))
dset <- cbind(dset, -1)

svp <- ksvm(dset[, 1:2], dset[, 3], type="C-svc", kernel = "vanilladot")
plot(svp,data=x)
svp
```

Changing the kernel we can achieve different border line shapes:
| ![](https://i.imgur.com/F8kMfic.png) | ![](https://i.imgur.com/V7jEIYP.png) | ![](https://i.imgur.com/YdJx4F1.png) |
| - | - | - |
| Linear kernel | Guassian kernel | Spline kernel |

Now let's consider 3 different cases.
1. When we have a linearly separable dataset.
![](https://i.imgur.com/c5YMdu6.png)

2. When we have a small number of points that are not linearly separable.
![](https://i.imgur.com/jCtnMNH.png)

3. When we have a linearly non-separable dataset.

| ![](https://i.imgur.com/8rRgLjW.png) | ![](https://i.imgur.com/twHxu5b.png) |
| - | - |
| Linear kernel | Guassian kernel |

###### How the C parameter affects the SVM
In a SVM you are searching for two things: a hyperplane with the largest minimum margin, and a hyperplane that correctly separates as many instances as possible. The problem is that you will not always be able to get both things. The c parameter determines how great your desire is for the latter.

| ![](https://i.imgur.com/eErCZK5.png) | ![](https://i.imgur.com/oacb0H7.png) |
| - | - |
| ![](https://i.imgur.com/2wYQ6Hb.png) | ![](https://i.imgur.com/wRnWrUw.png) |

###### ROC / AOC
In order to plot the ROC curve we will use the following function, which accepts the dataset and weights vector:
```R
plotROCCurve <- function(dset, w) {
  dsetLength <- dim(dset)[1]

  d <- matrix(NA, dsetLength, 2)
  
  for (i in 1:dsetLength) {
    d[i, ] <- c(i, sum(w * c(dset[i, 1:2], -1)))
  }
  
  sorted <- dset[order(d[ ,2], decreasing = TRUE), 3]
  
  truePositive <- 0
  falsePositive <- 0

  result <- matrix(0, dsetLength+1, 2)
  
  for (i in 1:dsetLength) {
    ans <- sorted[i]
    if (ans == 1) {
      truePositive <- truePositive + 1
    } else {
      falsePositive <- falsePositive + 1
    }
    result[i+1,] <- c(falsePositive, truePositive)
  }
  plot(result[, 1], result[, 2], type="l", xlab="False positive rate", ylab="True positive rate")
}
```
Results:
| ![](https://i.imgur.com/wy9rOY3.png) | ![](https://i.imgur.com/mY43XUV.png) |
| - | - |
| ![](https://i.imgur.com/cqNZNTo.png) | ![](https://i.imgur.com/YyPGcHu.png) |

###### Shiny
The SVM demo on shiny is available <a href="https://stre1ok.shinyapps.io/svm-demo/">here</a>.