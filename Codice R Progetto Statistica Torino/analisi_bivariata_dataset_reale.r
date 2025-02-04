Comandi Progetto Torino Francesco Maria (Id Progetto: 34)


################## Analisi bivariata tra Customer Value e Frequency of SMS ##################

#Calcolo covarianza campionaria della relazione tra Customer Value e Frequency of SMS
cov(dataset$Frequency.of.SMS, dataset$Customer.Value)

#Calcolo Calcola il coefficiente di correlazione della relazione tra Customer Value e Frequency of SMS
cor(dataset$Frequency.of.SMS, dataset$Customer.Value)

# Plot della relazione tra Customer Value e Frequency of SMS
plot(dataset$Frequency.of.SMS, dataset$Customer.Value, xlab="Frequency of SMS", ylab="Customer Value")
abline(lm(dataset$Customer.Value ~ dataset$Frequency.of.SMS), col="red")

# Calcolo della regressione lineare
model <- lm(Customer.Value ~ Frequency.of.SMS, data = dataset)

# Calcolo coefficiente di dererminazione
summary(model)$r.squared

# Riassunto della regressione
summary(model)

# Ottieni i residui
residui <- residuals(model)

# Calcola la media campionaria dei residui
media_residui <- mean(residui)
cat("Media campionaria dei residui:", media_residui, "\n")

# Calcola la varianza campionaria dei residui
varianza_residui <- var(residui)
cat("Varianza campionaria dei residui:", varianza_residui, "\n")

#Calcolo covarianza campionaria della relazione tra Customer Value e Frequency of use
cov(dataset$Frequency.of.use, dataset$Customer.Value)

#Calcolo Calcola il coefficiente di correlazione della relazione tra Customer Value e Frequency of use
cor(dataset$Frequency.of., dataset$Customer.Value)

# Plot della relazione tra Customer Value e Frequency of use
plot(dataset$Frequency.of.use, dataset$Customer.Value, xlab="Frequency of use", ylab="Customer Value")
abline(lm(dataset$Customer.Value ~ dataset$Frequency.of.use), col="red")

# Calcolo della regressione lineare
model <- lm(Customer.Value ~ Frequency.of.use, data = dataset)

# Riassunto della regressione
summary(model)

# Relazione tra Customer Value e Frequency of use e Frequency of SMS#####

# Calcolo della regressione lineare
model <- lm(dataset$Customer.Value ~ dataset$Frequency.of.use + dataset$Frequency.of.SMS)

# Riassunto della regressione
summary(model)

# Ottieni i residui
residui <- residuals(model)

# Calcola la media campionaria dei residui
media_residui <- mean(residui)
cat("Media campionaria dei residui:", media_residui, "\n")

# Calcola la varianza campionaria dei residui
varianza_residui <- var(residui)
cat("Varianza campionaria dei residui:", varianza_residui, "\n")

# Carica la libreria per grafici 3D
library(scatterplot3d)

# Crea il grafico 3D con Customer Value in funzione di Frequency of use e Frequency of SMS
scatterplot3d(dataset$Frequency.of.use, dataset$Frequency.of.SMS, dataset$Customer.Value,
              xlab = "Frequency of Use", ylab = "Frequency of SMS", zlab = "Customer Value",
              main = "Regressione multipla 3D",
              pch = 16, color = "blue")

# Adatta il modello di regressione lineare multipla
model <- lm(Customer.Value ~ Frequency.of.use + Frequency.of.SMS, data = dataset)

# Aggiungi il piano di regressione 3D
s3d <- scatterplot3d(dataset$Frequency.of.use, dataset$Frequency.of.SMS, dataset$Customer.Value)
s3d$plane3d(model, col = "red")

# Relazione tra Frequency of use e Seconds of use#####

#Calcolo covarianza campionaria della relazione tra Frequency of use e Seconds of use
cov(dataset$Seconds.of.Use, dataset$Frequency.of.use)

#Calcolo Calcola il coefficiente di correlazione della relazione tra Frequency of use e Seconds of use
cor(dataset$Seconds.of.Use, dataset$Frequency.of.use)

plot(dataset$Frequency.of.use, dataset$Seconds.of.Use, xlab="Frequency of use", ylab="Seconds of use")
abline(lm(dataset$Seconds.of.Use ~ dataset$Frequency.of.use), col="red")


# Calcolo della regressione lineare
model <- lm(dataset$Seconds.of.Use ~ dataset$Frequency.of.use)

# Riassunto della regressione
summary(model)

# Ottieni i residui
residui <- residuals(model)

# Calcola la media campionaria dei residui
media_residui <- mean(residui)
cat("Media campionaria dei residui:", media_residui, "\n")

# Calcola la varianza campionaria dei residui
varianza_residui <- var(residui)
cat("Varianza campionaria dei residui:", varianza_residui, "\n")

