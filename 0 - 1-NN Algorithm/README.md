# 0 - 1-NN Algorithm

### Algorithm abstract
**1-nearest-neighbours algorithm** is a method used for classification. The training examples are vectors in a multidimensional feature space, each with a class label. The training phase of the algorithm consists only of storing the feature vectors and class labels of the training samples. In the classification phase an unlabeled vector is classified by assigning the label of the nearest training sample.

### The assignment description
The task is to implement the 1-NN algorithm to classify arbitrary points on a 2D space, using the iris dataset. The 3 (Petal.Width) and 4 (Petal.Length) features engender this 2D space.

### Implementation description
The algorithm itself is implemented in the oneNN function. 5 random points are chosen each run to be categorized. The results of a sample run are depicted on the image below. Diamond shaped dots are from the original iris dataset, whereas squares are random points that were colored by the algorithm accordingly.

![Result of a sample algorithm run](https://i.ibb.co/m976yh5/Screenshot-2020-09-12-at-20-35-46.png)

