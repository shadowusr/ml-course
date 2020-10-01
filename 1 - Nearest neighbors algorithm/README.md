# 1 - Nearest neighbors algorithm

### Assignment objective

- Implement the k-NN algorithm
- Pick the optimal value k using leave-one-out cross validation and draw a corresponding plot
- Implement the weighted k-NN algorithm
- Pick the optimal values k and q utilizing LOO CV and draw a plot
- Compare accuracies of k-NN and weighted k-NN
- Build a classification maps
- Give an example proving the advantage of weighted k-NN over k-NN

### Assignment implementation
###### k-NN
k nearest neighbors algortihm implementation can be found in the `knn.r` file. A sample run of this program looks as follows.
![Result of a sample algorithm run](https://i.imgur.com/AOtvlBP.png)
The best K is 6. The plot on the left shows dependency between k and overall accuracy computed with use of leave-one-out cross validation. The diagram on the right exhibits practical method application: diamonds denote the dataset data, while squares were picked randomly and classified by the algorithm.

###### Weighted k-NN
The `kwnn-heatmap.r` showcases the algorithm implementation along with a heat map which shows a dependency between k, q and accuracy. Weight function used is q^i. The heat map is shown below.
![Result of a sample algorithm run](https://i.imgur.com/NDXGOUO.png)
Brighter color means better accuracy. White spots denote the best results. The best results were achieved with the following pairs of params: `K = 30, q = 0.952381`, `K = 32, q = 0.952381`, `K = 6, q = 1.000000`.

###### Classification maps
Classification maps for k-NN (k = 6) and weighted k-NN (k = 30, q = 0.952381), respectively:
![Result of a sample algorithm run](https://i.imgur.com/ngF5Nei.png)
![Result of a sample algorithm run](https://i.imgur.com/KYUCOyU.png)

###### Algorithms accuracy comparison
Code can be found in the `knn-vs-kwnn.r` file. Comparing these two algorithms on the iris dataset (features 1-4) revealed that the accuracies are identical (0.98).

###### Example of the advantage of weighted k-NN over k-NN
The main difference of the weighted k-NN algorithm from the plain k-NN is that the closer points are the more important they become. Because of this fact these algorithms can give different answers as depicted on the image below. The k-NN would put the grey dot in the blue class, but weighted k-NN would put it in the green one.
![Result of a sample algorithm run](https://i.imgur.com/cWwo992.png)