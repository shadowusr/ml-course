# 10 - Logistic regression
### Assignment objective
- Train the logistic regression algorithm using SGD
- Draw a plot, depicting the method in action
- Depending on the value of the a posteriori probability, make a plot so that the higher the probability value, the brighter the color becomes
- Demonstrate the operation of the algorithms implemented above on a single graph
### Assignment implementation
###### Gradient descent
The gradient descent technique is a powerful way of optimising stuff. Specifically, let's introduce the concept of a loss function: it's a function that takes all points from our dataset and our predicted values and returns some numeric characteristic of how bad our prediction was.

Obviously, we want the loss to be the least possible, ideally zero. In some simple cases we can just take a derivative of a loss function, solve the equation and find the most optimal weights set right away. However, it's not always possible. That's when gradient descent comes in handy.
 
Instead of solving the equation we launch an iterative process, at each step optimising our weights further by calculating the loss function gradient and making step towards the most optimal point. The step size depends on the gradient and learning rate - a special number that usually changes over iterations. We end this process once the step becomes small enough or we've reached the limit of steps.

###### Stochastic gradient descent
Remember the loss function from the previous section? Well, at each step we had to plug our whole dataset in it. Now imagine we have some really huge dataset and it takes us a lot of steps to reach the optimal point. That's when the regular gradient descent becomes too slow.

The approach can be sped up if instead of considering the whole dataset on each step, we took only a single random point and calculated the loss based on it. That's exactly what stochastic gradient descent does.

###### Logistic regression
Logistic regression is used to model the probability of a certain class or event existing such as pass/fail, win/lost, etc. Logistic regression is a statistical model that in its basic form uses a logistic function to model a binary dependent variable. In this task, logistic regression is built on top of the SGD.

###### Logistic regression loss function and update rule
```R
sigmoid = function(z) 1 / (1 + exp(-z))

logisticRegressionLossFunction <- function(margin) log2(1 + exp(-margin))

logisticRegressionUpdateRule <- function(w, learningRate, sample, class) w + learningRate * sample * class * sigmoid(-sum(w * sample) * class) 
```

###### SGD Algorithm
1. Initialize weights with some random small numbers.
2. Calculate the initial loss.
3. Until the step size is not too small and the steps limit is not reached, do:
    1. Pick a random sample from the dataset.
    2. Calculate the algorithm loss.
    3. Update the weights.
    4. Calculate the current loss.
    
###### Code
```R
computeWeightsWithStochasticGradient <- function(dset, classes, lossFunction, updateRule) {
  dsetLength <- dim(dset)[1]
  featuresCount <- dim(dset)[2]
  
  w <- runif(featuresCount, -1 / (2 * featuresCount), 1 / (2 * featuresCount))
  lambda <- 1 / dsetLength
  
  currentLoss <- 0
  for (i in 1:dsetLength) {
    margin <- sum(w * dset[i, ]) * classes[i]
    currentLoss <- currentLoss + lossFunction(margin)
  }
  prevLoss <- currentLoss
  
  step <- 1
  repeat {
    margins <- array(0, dim = dsetLength)
    for (i in 1:dsetLength) {
      margins[i] <- sum(w * dset[i, ]) * classes[i]
    }
    errorIndices <- which(margins <= 0)
    
    if (length(errorIndices) == 0) break
    
    randomErrorSampleIndex <- sample(errorIndices, 1)
    x <- dset[randomErrorSampleIndex, ]
    class <- classes[randomErrorSampleIndex]
    
    margin <- sum(w * x) * class
    loss <- lossFunction(margin)
    
    learningRate <- 1 / step
    w <- updateRule(w, learningRate, x, class)
    
    currentLoss <- (1 - lambda) * currentLoss + lambda * loss
    
    if (abs(prevLoss - currentLoss) / abs(max(prevLoss, currentLoss)) < 1e-5) break
    
    if (step > 25000) break
    
    prevLoss <- currentLoss
    step <- step + 1
  }

  return(w)
}
```

![](https://i.imgur.com/9fMmFD7.png)


###### Dependency between loss and step count
![](https://i.imgur.com/1CeySFD.png)