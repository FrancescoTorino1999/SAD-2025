Comandi Progetto Torino Francesco Maria (Id Progetto: 34)


################## Clustering ##################

#Calcolo random forest:

library(randomForest)
churn_rf <- randomForest(Churn ~ ., data = dataset)
print(churn_rf)
varImpPlot(churn_rf)

#Elbow method
library(ggplot2)

data <- dataset[, c("Complains", "Status", "Seconds.of.Use", "Frequency.of.use", "Subscription..Length")]

data_scaled <- scale(data)

wss <- sapply(1:10, function(k) {
  kmeans(data_scaled, centers = k, nstart = 10)$tot.withinss
})

ggplot(data.frame(k = 1:10, wss = wss), aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  ggtitle("Metodo dell'Elbow") +
  xlab("Numero di cluster (k)") +
  ylab("Somma delle distanze quadrate intra-cluster") +
  theme_minimal()

  #Clustering con kmeans

library(ggplot2)
library(cluster)

# Subset dei dati e scaling
data_subset <- dataset[, c("Complains", "Status", "Seconds.of.Use")]
data_scaled <- scale(data_subset)

# Clustering con k = 4
set.seed(123)
kmeans_model <- kmeans(data_scaled, centers = 4, nstart = 10)
dataset$Cluster <- as.factor(kmeans_model$cluster)

# Calcolo WSS, BSS, e CH Index

wss <- sum(kmeans_model$withinss)
bss <- sum(kmeans_model$betweenss)

n <- nrow(data_scaled)
k <- 4
s_t <- (1 / (n - 1)) * sum((data_scaled - colMeans(data_scaled))^2)
ch_index <- (bss / (k - 1)) / (wss / (n - k))
cat("Calinski-Harabasz Index:", ch_index, "\n")

# Tabella di confronto tra Churn e Cluster
table(dataset$Churn, dataset$Cluster)

# Tabella di confronto tra Complains e Cluster
table(Complains = dataset$Complains, Cluster = dataset$Cluster)

# Tabella di confronto tra Status e Cluster
table(Status = dataset$Status, Cluster = dataset$Cluster)

# Visualizzazione: Media di Seconds of Use per Cluster
ggplot(dataset, aes(x = Cluster, y = Seconds.of.Use, fill = Cluster)) +
    geom_bar(stat = "summary", fun = "mean", width = 0.6) +
    labs(title = "Media di Seconds of Use per Cluster",
         x = "Cluster",
         y = "Media Seconds of Use") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set3")

#Calcolo k con silouhette
library(cluster)
library(factoextra)
data_subset <- dataset[, c("Complains", "Status", "Seconds.of.Use")]
data_scaled <- scale(data_subset) # Normalizzare i dati
silhouette_scores <- c()
for (k in 2:10) {
  kmeans_model <- kmeans(data_scaled, centers = k, nstart = 10)
  sil_score <- mean(silhouette(kmeans_model$cluster, dist(data_scaled))[, 3])
  silhouette_scores <- c(silhouette_scores, sil_score)
}
print(silhouette_scores)
k_values <- 2:10
plot(k_values, silhouette_scores, type = "b", pch = 19, col = "blue",
     xlab = "Numero di cluster (k)", ylab = "Punteggio silhouette",
     main = "Punteggi silhouette per diversi k")


########Clustering senza outliers con kmeans++#############

library(ClusterR)
library(ggplot2)
library(cluster)
library(factoextra)

# Subset dei dati e scaling
data_subset <- dataset[, c("Complains", "Status", "Seconds.of.Use")]
data_scaled <- scale(data_subset)

# Clustering con K-Means++ (k = 4)
set.seed(123)
kmeans_model <- KMeans_rcpp(data_scaled, clusters = 4, num_init = 10, initializer = "kmeans++")

# Aggiungo i cluster al dataset
dataset$Cluster <- as.factor(kmeans_model$clusters)

# Stampa WSS, BSS, e CH Index
kmeans_model

# Numero di osservazioni
n <- nrow(data_scaled)  

# Numero di cluster
k <- 4  

# Valori di BSS e WSS dal modello KMeans++
bss <- 8353.02  
wss <- 1093.98  

# Calcolo dell'Indice di Calinski-Harabasz
ch_index <- (bss / (k - 1)) / (wss / (n - k))

# Stampa del risultato
cat("Calinski-Harabasz Index:", ch_index, "\n")cat("Calinski-Harabasz Index:", ch_index, "\n")

# Tabella di confronto tra Churn e Cluster
table(dataset$Churn, dataset$Cluster)

# Visualizzazione: Media di Seconds of Use per Cluster
ggplot(dataset, aes(x = Cluster, y = Seconds.of.Use, fill = Cluster)) +
    geom_bar(stat = "summary", fun = "mean", width = 0.6) +
    labs(title = "Media di Seconds of Use per Cluster",
         x = "Cluster",
         y = "Media Seconds of Use") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set3")
