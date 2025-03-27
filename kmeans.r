euclidean_distance <- function(p1, p2) {
  sqrt(sum((p1 - p2) ^ 2))
}

k_means_clustering <- function(data,centroids, k, max_iter = 100) {
  for (iter in 1:max_iter) {
    cat(sprintf("\nIteration %d:\n", iter))
    
    # Assign each point to the nearest centroid
    labels <- apply(data, 1, function(point) {
      which.min(sapply(1:k, function(i) euclidean_distance(point, centroids[i, ])))
    })
    
    new_centroids <- matrix(NA, nrow = k, ncol = ncol(data))
    
    for (i in 1:k) {
      cluster_points <- data[labels == i, , drop = FALSE]  # Get points in cluster
      cat("cluster",i)
      print(cluster_points)
      if (nrow(cluster_points) > 0) {
        new_centroids[i, ] <- colMeans(cluster_points)  # Compute new centroid
      } else {
        new_centroids[i, ] <- centroids[i, ]  # Keep old centroid if cluster is empty
      }
    }
    
    cat("\nCluster Assignments:\n")
    print(labels)
    
    cat("\nCentroids:\n")
    print(new_centroids)
    
    if (all(new_centroids == centroids, na.rm = TRUE)) {
      cat("\nConverged at iteration", iter, "!\n")
      break
    }
    
    centroids <- new_centroids
  }
  
  return(list(labels = labels, centroids = centroids))
}


data <- read.csv("C:/prgms/kmm.csv", header = FALSE)

data <- as.matrix(data)
k <- as.numeric(readline(prompt = "Enter the number of centroids (k): "))

centroids <- data.frame(v1 = numeric(k), v2 = numeric(k))

# Manually input initial centroids
for(i in 1:k) {
  cat("Enter coordinates for centroid", i, ":\n")
  centroids$v1[i] <- as.numeric(readline(prompt = paste("v1 for centroid", i, ": ")))
  centroids$v2[i] <- as.numeric(readline(prompt = paste("v2 for centroid", i, ": ")))
}

result <- k_means_clustering(data,centroids, k)

cat("\nFinal clusters:\n")
for (i in 1:k) {
  cluster_points <- data[result$labels == i, , drop = FALSE]
  cat(sprintf("Cluster %d:\n", i))
  print(cluster_points)
}

cat("\nFinal centroids:\n")
print(result$centroids)
plot(data[, 1], data[, 2], col =result$labels, pch = 19, xlab = "x", ylab = "y", main = paste("K-means Clustering"))
points(new_centroids[, 1], new_centroids[, 2], col = "black", pch = 4, cex = 2)  # Plot centroids as black crosses
