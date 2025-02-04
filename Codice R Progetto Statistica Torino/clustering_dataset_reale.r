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

#Creazione dataset senza outliers su Seconds of use

Q1 <- quantile(dataset$Seconds.of.Use, 0.25)
Q3 <- quantile(dataset$Seconds.of.Use, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
median_value <- median(dataset$Seconds.of.Use, na.rm = TRUE)
dataset$Seconds.of.Use <- ifelse(
  dataset$Seconds.of.Use < lower_limit | dataset$Seconds.of.Use > upper_limit,
  median_value,
  dataset$Seconds.of.Use
)
write.csv(dataset, file = "Customer_Churn_without_outliers.csv", row.names = FALSE)
dataset <- read.csv("Customer_Churn_without_outliers.csv")
