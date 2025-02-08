Comandi Progetto Torino Francesco Maria (Id Progetto: 34)


######################################################  Studio popolazioni  ###########################################################################

#############De moivre - la place#########
# Numero totale di utenti
n <- nrow(dataset)

# Probabilità stimata di churn
p_hat <- mean(dataset$Churn)

# Parametri della normale approssimata
mu <- n * p_hat                    # Media
sigma <- sqrt(n * p_hat * (1 - p_hat))  # Deviazione standard
# Standardizzazione della binomiale
dataset$Z <- (dataset$Churn - mu) / sigma
# Asse X per la normale standard
x <- seq(-4, 4, length = 100)

# Funzione di densità della normale standard
y <- dnorm(x, mean = 0, sd = 1)

# Grafico
plot(x, y, type = "l", col = "blue", lwd = 2,
     xlab = "Z", ylab = "Densità",
     main = "Normale Standardizzata (De Moivre-Laplace)")

# Aggiungiamo una linea verticale sulla media (Z=0)
abline(v = 0, col = "red", lwd = 2, lty = 2)

###############Stima intervallare per p cappello######

# Parametri del campione
n <- 3150  # Numero totale di utenti nel dataset
k <- sum(dataset$Churn)  # Numero di utenti che hanno effettuato churn
p_hat <- k / n  # Stima della probabilità di churn

# Livello di confidenza 99%
alpha <- 0.01
z <- qnorm(1 - alpha / 2)  # Valore critico della normale standard

# Calcolo dell'intervallo di confidenza
se <- sqrt((p_hat * (1 - p_hat)) / n)  # Errore standard
ci_lower <- p_hat - z * se
ci_upper <- p_hat + z * se

# Creazione della curva normale
x <- seq(p_hat - 4 * se, p_hat + 4 * se, length.out = 1000)
y <- dnorm(x, mean = p_hat, sd = se)

# Plot della distribuzione normale
plot(x, y, type = "l", lwd = 2, col = "blue", 
     xlab = "Probabilità di churn", ylab = "Densità",
     main = "Distribuzione Normale con Intervallo di Confidenza al 99%")

# Evidenzia l'intervallo di confidenza
polygon(c(x[x >= ci_lower & x <= ci_upper], rev(x[x >= ci_lower & x <= ci_upper])),
        c(y[x >= ci_lower & x <= ci_upper], rep(0, sum(x >= ci_lower & x <= ci_upper))),
        col = rgb(1, 0, 0, 0.3), border = NA)

# Linee per la media e i limiti di confidenza
abline(v = p_hat, col = "black", lwd = 2, lty = 2)  # Media
abline(v = ci_lower, col = "red", lwd = 2, lty = 2)  # Limite inferiore
abline(v = ci_upper, col = "red", lwd = 2, lty = 2)  # Limite superiore

# Mostra i valori degli intervalli
text(ci_lower, max(y) * 0.8, round(ci_lower, 4), col = "red", pos = 2)
text(ci_upper, max(y) * 0.8, round(ci_upper, 4), col = "red", pos = 4)
text(p_hat, max(y) * 0.9, round(p_hat, 4), col = "black", pos = 3)


############Test unilaterali##############

# Dati
p_hat <- 0.1571   # Tasso di churn osservato (15.71%)
n <- 100          # Numero di osservazioni

# Definiamo p0 (valore minimo) e p1 (valore massimo)
p0 <- 0.10        # Valore minimo della categoria (10%)
p1 <- 0.20        # Valore massimo della categoria (20%)

alpha <- 0.01

z_critico = qnorm(alpha, mean= 0, sd =1)

# Calcoliamo la deviazione standard per ciascun valore p0 e p1
std_error_p0 <- sqrt(p0 * (1 - p0) / n)
std_error_p1 <- sqrt(p1 * (1 - p1) / n)

# Calcoliamo Z per p0 (limite inferiore)
Z_p0 <- (p_hat - p0) / std_error_p0
# Calcoliamo Z per p1 (limite superiore)
Z_p1 <- (p_hat - p1) / std_error_p1

Z_p0
Z_p1

