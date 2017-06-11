rm (list=ls())

set.seed(1)

# Functional knn
###########################
knn = function (k, X_treino, Y_treino, X_teste){
  
  X_treino = t(X_treino)
  
  pred = rep(NULL, length(X_teste))
  
  for (row in 1:nrow(X_teste)){
    dist = sqrt(colSums((X_treino - X_teste[row, ])^2))  # Euclidean distance
    nearest = order(dist)[1:k]
    classes = Y_treino[nearest]
    pred [row] = mean(classes)
  }
  
  pred
  
}


xmin_treino = -100
xmax_treino = 100

xmin_teste = -50
xmax_teste = 50

nr_treino = 100
nr_teste = 500

nc = 100

###########################
# Generate random data
###########################

X_treino = matrix ( runif(nr_treino * nc, xmin_treino, xmax_treino),
                    ncol = nc)

X_teste = matrix ( rnorm(nr_teste * nc, 0, 10),
                    ncol = nc)

Y_treino = apply(X_treino, 1, function(x)
  (sum(x)/max(x))*sd(x))

Y_teste = apply(X_teste, 1, function(x)
  (sum(x)/max(x))*sd(x))

###########################

print (summary(Y_treino))
print (summary(Y_teste))

max_k = 10

# Evaluate all values of k
###########################
resp_k = sapply(1:max_k, function(k) {
          pred_teste = knn (k, X_treino, Y_treino, X_teste)
          erro = pred_teste - Y_teste
          mae = mean(abs(erro))
          mse = mean (erro^2)
          rmse = sqrt(mse)
          c(mae, mse, rmse)
        })

# Which values showed the lowest errors 
############################################
for (i in 1:nrow(resp_k))
  print (which.min(resp_k[i,]))

par(las=1)
matplot(t(resp_k), type="l", log="y", lwd=4, xlab="k", ylab="Error", xlim = c(1, max_k+4))
abline(v=apply(resp_k, 1, which.min), lty=1:3, col=1:3, lwd=1:3)
legend("topright", c("rmse", "mse", "mae"), col=1:3, lwd=3, lty=1:3)


