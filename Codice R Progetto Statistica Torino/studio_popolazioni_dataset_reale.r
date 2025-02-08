Comandi Progetto Torino Francesco Maria (Id Progetto: 34)


######################################################  Studio popolazioni  ###########################################################################

######Test chiquadro per variabile complains#########

observed <- table(dataset$Complains)
n_trials <- sum(observed)
successes <- observed["1"]
failures <- observed["0"]
p_estimated <- successes / n_trials  
k_values <- names(observed)
expected_prob <- dbinom(as.numeric(k_values), size = 1, prob = p_estimated)
expected_freq <- expected_prob * n_trials
chisq.test(x = observed, p = expected_prob, rescale.p = TRUE)

#Divisione churn per complains

# Suddivisione del dataset in base alla variabile 'complains'
utenti_con_lamentele <- dataset[dataset$Complains == 1, ]
utenti_senza_lamentele <- dataset[dataset$Complains == 0, ]

#Popolazione con lamentele
observed <- table(utenti_con_lamentele$Churn)
####Approssimazione alla normale############

x1 <- sum(utenti_con_lamentele$Churn) / length(utenti_con_lamentele)

mu = length(utenti_con_lamentele) * x1
n <- length(utenti_con_lamentele)
p <- x1
sigma <- sqrt(n * p * (1 - p))
sigma

##Rappresentazione normale###
x <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 1000)
y <- dnorm(x, mean = mu, sd = sigma)

plot(x, y, type = "l", col = "blue", lwd = 2, main = "Distribuzione Normale", 
     xlab = "Valori", ylab = "Densità", xlim = c(mu - 4 * sigma, mu + 4 * sigma))

abline(v = mu, col = "red", lwd = 2, lty = 2)
abline(v = c(mu - sigma, mu + sigma), col = "green", lwd = 2, lty = 2)

text(mu, 0.003, labels = paste("mu =", round(mu, 2)), pos = 4, col = "red")
text(mu - sigma, 0.003, labels = paste("mu - sigma =", round(mu - sigma, 2)), pos = 4, col = "green")
text(mu + sigma, 0.003, labels = paste("mu + sigma =", round(mu + sigma, 2)), pos = 4, col = "green")



# Generazione della distribuzione normale
x <- seq(meancamp - 4 * sigma, meancamp + 4 * sigma, length.out = 500)
y <- dnorm(x, mean = meancamp, sd = sigma)

plot(x, y, type = "l", col = "blue", lwd = 2, main = "Distribuzione Gaussiana e Intervalli di Confidenza",
     xlab = "Valore di Churn", ylab = "Densità di Probabilità")
abline(v = meancamp, col = "red", lty = 2, lwd = 2) # Media
abline(v = ci_lower, col = "green", lty = 2, lwd = 2) # Limite inferiore
abline(v = ci_upper, col = "green", lty = 2, lwd = 2) # Limite superiore
polygon(c(ci_lower, seq(ci_lower, ci_upper, length.out = 100), ci_upper), 
        c(0, dnorm(seq(ci_lower, ci_upper, length.out = 100), meancamp, sigma), 0), 
        col = rgb(0, 1, 0, 0.2), border = NA) # Area intervallo di confidenza
legend("topright", legend = c("Media", "Limiti CI", "Area CI"),
       col = c("red", "green", rgb(0, 1, 0, 0.2)), lty = c(2, 2, NA), lwd = c(2, 2, NA), pch = c(NA, NA, 15))


#Popolazione con lamentele
observed <- table(utenti_senza_lamentele$Churn)

x1 <- sum(utenti_senza_lamentele$Churn) / length(utenti_senza_lamentele)

mu = length(utenti_senza_lamentele) * x1
n <- length(utenti_senza_lamentele)
p <- 295/2909
sigma <- sqrt(n * p * (1 - p))
sigma

##Rappresentazione normale###
x <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 1000)
y <- dnorm(x, mean = mu, sd = sigma)

plot(x, y, type = "l", col = "blue", lwd = 2, main = "Distribuzione Normale", 
     xlab = "Valori", ylab = "Densità", xlim = c(mu - 4 * sigma, mu + 4 * sigma))

abline(v = mu, col = "red", lwd = 2, lty = 2)
abline(v = c(mu - sigma, mu + sigma), col = "green", lwd = 2, lty = 2)

text(mu, 0.003, labels = paste("mu =", round(mu, 2)), pos = 4, col = "red")
text(mu - sigma, 0.003, labels = paste("mu - sigma =", round(mu - sigma, 2)), pos = 4, col = "green")
text(mu + sigma, 0.003, labels = paste("mu + sigma =", round(mu + sigma, 2)), pos = 4, col = "green")



# Generazione della distribuzione normale
x <- seq(meancamp - 4 * sigma, meancamp + 4 * sigma, length.out = 500)
y <- dnorm(x, mean = meancamp, sd = sigma)

plot(x, y, type = "l", col = "blue", lwd = 2, main = "Distribuzione Gaussiana e Intervalli di Confidenza",
     xlab = "Valore di Churn", ylab = "Densità di Probabilità")
abline(v = meancamp, col = "red", lty = 2, lwd = 2) # Media
abline(v = ci_lower, col = "green", lty = 2, lwd = 2) # Limite inferiore
abline(v = ci_upper, col = "green", lty = 2, lwd = 2) # Limite superiore
polygon(c(ci_lower, seq(ci_lower, ci_upper, length.out = 100), ci_upper), 
        c(0, dnorm(seq(ci_lower, ci_upper, length.out = 100), meancamp, sigma), 0), 
        col = rgb(0, 1, 0, 0.2), border = NA) # Area intervallo di confidenza
legend("topright", legend = c("Media", "Limiti CI", "Area CI"),
       col = c("red", "green", rgb(0, 1, 0, 0.2)), lty = c(2, 2, NA), lwd = c(2, 2, NA), pch = c(NA, NA, 15))

####Confronto tra popolazioni##########

utenti_con_lamentele <- data[data$Complains == 1, ]
utenti_senza_lamentele <- data[data$Complains == 0, ]

n1 <- length(utenti_con_lamentele$Churn)
n2 <- length(utenti_senza_lamentele$Churn)

# Numero di successi (utenti che hanno churnato in ciascun gruppo)
x1 <- sum(utenti_con_lamentele$Churn)
x2 <- sum(utenti_senza_lamentele$Churn)

p1 <- x1 / n1
p2 <- x2 / n2

se <- sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))

# Livello di confidenza (95%)
z_95 <- qnorm(0.975)  # Quantile della normale standard
ci_lower <- (p1 - p2) - z_95 * se
ci_upper <- (p1 - p2) + z_95 * se

# Stampa intervallo di confidenza
cat("Intervallo di confidenza al 95% per p1 - p2:", ci_lower, "-", ci_upper, "\n") 


###Chi quadro per l'indipendenza#####
# Creazione della tabella di contingenza
tabella_contingenza <- table(data$Complains, data$Churn)
print(tabella_contingenza)

chi_test <- chisq.test(tabella_contingenza)

print(chi_test)
cat("Chi-squared statistic:", chi_test$statistic, "\n")
cat("Degrees of freedom:", chi_test$parameter, "\n")
cat("P-value:", chi_test$p.value, "\n")

########Confronto tra popolazioni

mu = -95
sigma <- 2.54

##Rappresentazione normale###
x <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 1000)
y <- dnorm(x, mean = mu, sd = sigma)

plot(x, y, type = "l", col = "blue", lwd = 2, main = "Distribuzione Normale", 
     xlab = "Valori", ylab = "Densità", xlim = c(mu - 4 * sigma, mu + 4 * sigma))

abline(v = mu, col = "red", lwd = 2, lty = 2)
abline(v = c(mu - sigma, mu + sigma), col = "green", lwd = 2, lty = 2)

text(mu, 0.003, labels = paste("mu =", round(mu, 2)), pos = 4, col = "red")
text(mu - sigma, 0.003, labels = paste("mu - sigma =", round(mu - sigma, 2)), pos = 4, col = "green")
text(mu + sigma, 0.003, labels = paste("mu + sigma =", round(mu + sigma, 2)), pos = 4, col = "green")




#########################Studio proiezione churn su scala globale######################

# Numero totale di utenti
n <- nrow(dataset)

# Probabilità stimata di churn (p cappello)
p_hat <- mean(dataset$Churn)

observed <- table(dataset$Churn)
n_trials <- sum(observed)
successes <- observed["1"]
failures <- observed["0"]
p_estimated <- successes / n_trials  
k_values <- names(observed)
expected_prob <- dbinom(as.numeric(k_values), size = 1, prob = p_estimated)
expected_freq <- expected_prob * n_trials
chisq.test(x = observed, p = expected_prob, rescale.p = TRUE)

####Approssimazione alla normale############

x1 <- sum(utenti_con_lamentele$Churn) / length(utenti_con_lamentele)
p = 41/241
mu = length(utenti_con_lamentele) * x1
n <- length(utenti_con_lamentele)
sigma <- sqrt(n * p * (1 - p))
sigma

##Rappresentazione normale###
x <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 1000)
y <- dnorm(x, mean = mu, sd = sigma)

plot(x, y, type = "l", col = "blue", lwd = 2, main = "Distribuzione Normale", 
     xlab = "Valori", ylab = "Densità", xlim = c(mu - 4 * sigma, mu + 4 * sigma))

abline(v = mu, col = "red", lwd = 2, lty = 2)
abline(v = c(mu - sigma, mu + sigma), col = "green", lwd = 2, lty = 2)

text(mu, 0.003, labels = paste("mu =", round(mu, 2)), pos = 4, col = "red")
text(mu - sigma, 0.003, labels = paste("mu - sigma =", round(mu - sigma, 2)), pos = 4, col = "green")
text(mu + sigma, 0.003, labels = paste("mu + sigma =", round(mu + sigma, 2)), pos = 4, col = "green")


########Normale standard per calcolo mu###########
alpha <- 0.1
z1 <- qnorm(1 - alpha / 2) 
x <- seq(-4, 4, length = 1000)
y <- dnorm(x)
plot(x, y, type = "l", col = "blue", lwd = 2, main = "Normale Standard con Intervallo di Confidenza (alpha = 0.1)",
     xlab = "Valori di X", ylab = "Densità", xlim = c(-4, 4), ylim = c(0, 0.4))
abline(v = c(-z1, z1), col = "red", lwd = 2, lty = 2)
text(-z1, 0.02, paste("-z1 = ", round(-z1, 2)), col = "red", pos = 4)
text(z1, 0.02, paste("z1 = ", round(z1, 2)), col = "red", pos = 4)


##########calcolo intervallo confidenza per mu

mu_old = 494.55
z1 = 1.645
minus_z1 = -1.645
sigma = 20.82
n = 3150

q1 = mu_old + (minus_z1 * (sigma/(sqrt(n))))
q2 = mu_old + (z1 * (sigma/(sqrt(n))))

p1 = q1/n
p2 = q2/n


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