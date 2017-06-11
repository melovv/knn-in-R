rm (list=ls())

set.seed(1)

# Procedural knn
###########################
knn = function (k, X_treino, Y_treino, X_teste){
  
  mode = function(x) names(which.max(table(x)))
  
  X_treino = t(X_treino) # Transpose to speedup the distance calculation
  
  pred = rep(NULL, length(X_teste))
  
  for (row in 1:nrow(X_teste)){
    dist = sqrt(colSums((X_treino - X_teste[row, ])^2))  # Euclidean distance
    nearest = order(dist)[1:k]
    classes = Y_treino[nearest]
    pred [row] = mode(classes)
  }

  pred
}

xmin_treino = -100
xmax_treino = 100

xmin_teste = -100
xmax_teste = 100

nr_treino = 100
nr_teste = 500

nc = 10

###########################
# Generate random data
###########################

X_treino = matrix ( runif(nr_treino * nc, xmin_treino, xmax_treino),
                    ncol = nc)

X_teste = matrix ( runif(nr_teste * nc, xmin_teste, xmax_teste),
                    ncol = nc)

nclasses = 2
Y_treino = sample(nclasses, nr_treino, replace=T)

Y_teste = sample(nclasses, nr_teste, replace=T)

###########################


plot(X_treino[,1], X_treino[,2], col=Y_treino, pch=Y_treino)

points(X_teste[,1], X_teste[,2], col=Y_treino+10, pch=Y_treino)

max_k = 10

# Evaluate all values of k
###########################
start = Sys.time()
resp_k = sapply(1:max_k, function(k) {
          pred_teste = knn (k, X_treino, Y_treino, X_teste)
          accuracy = sum(pred_teste == Y_teste) / length(Y_teste)
        })
print (Sys.time() - start)


# Which values showed the best accuracies
############################################
print (which.max(resp_k))

par(las=1)
plot(resp_k, type="l", log="y", lwd=4, xlab="k", ylab="Accuracy")
abline(v=which.max(resp_k), lty=2, lwd=2)


