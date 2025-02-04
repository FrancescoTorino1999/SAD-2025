Comandi Progetto Torino Francesco Maria (Id Progetto: 34)


################## feature Call Failures ##################

# Calcolo della media per la feature Call Failures
mean(dataset$Call..Failure)

# Calcolo della mediana per la feature Call Failures
median(dataset$Call..Failure)

# Definizione funzione per calcolare la moda
moda <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Calcolo della moda per la feature Call Failures
moda(dataset$Call..Failure)

# Boxplot con baffo per la feature Call Failures
boxplot(dataset$Call..Failure, xlab="Call Failures", col=c("red"))

# Calcolo degli outliers per la feature Call Failures
# Calcola Q1 e Q3
Q1 <- quantile(dataset$Call..Failure, 0.25, na.rm = TRUE)
> Q3 <- quantile(dataset$Call..Failure, 0.75, na.rm = TRUE)
# Calcola Scarto interquartile
IQR <- Q3 - Q1
# Definisce i limiti
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Trova gli outlier
outliers <- dataset$Call..Failure[dataset$Call..Failure < lower_bound | dataset$Call..Failure > upper_bound]
# Visualizza gli outlier
outliers

#Istogramma Frequeze assolute per la feature Call Failures
plot(table(dataset$Call..Failure), col=c("red"), xlab = "Call Failures", ylab = "Numero fruitori del servizio")

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. per la feature Call Failures
summary(dataset$Call..Failure)

# Pie per frequenza relative per la feature Call Failures
pie(table(dataset$Call..Failure), col=rainbow(length(table(dataset$Call..Failure))), labels=NA, xlab="Call Failures")
legend("bottomleft", legend=names(table(dataset$Call..Failure)), fill=rainbow(length(table(dataset$Call..Failure))), ncol=2)

# Caloclo varianza  per la feature Call Failures
var(dataset$Call..Failure, na.rm = TRUE)

# Caloclo deviazione standard per la feature Call Failures
sd(dataset$Call..Failure, na.rm = TRUE)

# Caloclo coefficiente di variazione per la feature Call Failures
sd(dataset$Call..Failure, na.rm = TRUE)/mean(dataset$Call..Failure)*100

# Caloclo Funzione di ditribuzione empirica (discreta) per la feature Call Failures
FDE_DISCRETA_CALL_FAILURES <- ecdf(dataset$Call..Failure)
plot(FDE_DISCRETA_CALL_FAILURES, main = "Funzione di ditribuzione empirica (discreta)", xlab="Call Failures", ylab = "FDE", verticals = TRUE, do.points = TRUE)

# Caloclo diagramma di Pareto  per la feature Call Failures
table_call_failures <- table(dataset$Call..Failure)
ordered_table_call_failures <- sort(table_call_failures, decreasing = TRUE)
propOrdCallOfFailures <- prop.table(ordered_table_call_failures)
plotCallOfFailures <- barplot(propOrdCallOfFailures, ylim = c(0, 1.05), main = "Diagramma di pareto Call Failures", col=length(dataset$Call..Failure), las = 2)
lines(plotCallOfFailures, cumsum(propOrdCallOfFailures))
points(plotCallOfFailures, cumsum(propOrdCallOfFailures), pch = 16)

# Calcolo skewness per la feature Call Failures
skewness_value <- skewness(dataset$Call..Failure)

# Calcolo curtosi per la feature Call Failures
kurtosis_value <- kurtosis(dataset$Call..Failure)

# Calcolo plot per distribuzioni di Frequenza
library(ggplot2)
ggplot(dataset, aes(x = Call..Failure)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +  # Usa after_stat(count) e linewidth
    labs(title = "Distribuzione di Frequenza di Call Failures",
         x = "Numero di Call Failures",
         y = "Frequenza") +
    theme_minimal()

################## feature Complaints ##################

# Calcolo frequenze assolute per feature Complains
freq_assolute <- table(dataset$Complains)

# Calcolo frequenze relative per feature Complains
freq_relative <- prop.table(freq_assolute)

# Pie per frequenza relative per la feature Complains e Funzione di distribuzione empirica (discreta) per la feature Complains
# Imposta i grafici affiancati
par(mfrow = c(1, 2)) 
pie(table(dataset$Complains), 
    col = rainbow(length(table(dataset$Complains))), 
    labels = NA, 
    main = "Distribuzione Complains")
legend("bottomleft", 
       legend = names(table(dataset$Complains)), 
       fill = rainbow(length(table(dataset$Complains))), 
       ncol = 2)
FDE_DISCRETA_COMPLAINS <- ecdf(dataset$Complains)
plot(FDE_DISCRETA_COMPLAINS, 
     main = "Funzione di distribuzione empirica (discreta)", 
     xlab = "Complains", 
     ylab = "FDE", 
     verticals = TRUE, 
     do.points = TRUE)

################## feature Subscription Length ##################

# Calcolo della media per la feature Subscription Length
mean(dataset$Subscription..Length)

# Calcolo della mediana per la feature Subscription Length
median(dataset$Subscription..Length)

# Definizione funzione per calcolare la moda
moda <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Calcolo della moda per la feature Subscription Length
moda(dataset$Subscription..Length)

# Boxplot con baffo per la feature Subscription Length
boxplot(dataset$Subscription..Length, xlab="Subscription Length", col=c("blue"))

# Calcolo degli outliers per la feature Subscription Length
# Calcola Q1 e Q3
Q1 <- quantile(dataset$Subscription..Length, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$Subscription..Length, 0.75, na.rm = TRUE)
# Calcola Scarto interquartile
IQR <- Q3 - Q1
# Definisce i limiti
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Trova gli outlier
outliers <- dataset$Subscription..Length[dataset$Subscription..Length < lower_bound | dataset$Subscription..Length > upper_bound]
# Visualizza gli outlier
outliers

# Istogramma Frequenze assolute per la feature Subscription Length
plot(table(dataset$Subscription..Length), col=c("blue"), xlab = "Subscription Length", ylab = "Numero fruitori del servizio")

# Min. 1st Qu. Median Mean 3rd Qu. Max. per la feature Subscription Length
summary(dataset$Subscription..Length)

# Pie per frequenza relative per la feature Subscription Length
pie(table(dataset$Subscription..Length), col=rainbow(length(table(dataset$Subscription..Length))), labels=NA, xlab="Subscription Length")
legend("bottomleft", legend=names(table(dataset$Subscription..Length)), fill=rainbow(length(table(dataset$Subscription..Length))), ncol=3)

# Calcolo varianza per la feature Subscription Length
var(dataset$Subscription..Length, na.rm = TRUE)

# Calcolo deviazione standard per la feature Subscription Length
sd(dataset$Subscription..Length, na.rm = TRUE)

# Calcolo coefficiente di variazione per la feature Subscription Length
sd(dataset$Subscription..Length, na.rm = TRUE)/mean(dataset$Subscription..Length)*100

# Calcolo Funzione di distribuzione empirica (discreta) per la feature Subscription Length
FDE_DISCRETA_SUBSCRIPTION_LENGTH <- ecdf(dataset$Subscription..Length)
plot(FDE_DISCRETA_SUBSCRIPTION_LENGTH, main = "Funzione di distribuzione empirica (discreta)", xlab="Subscription Length", ylab = "FDE", verticals = TRUE, do.points = TRUE)

# Calcolo diagramma di Pareto per la feature Subscription Length
table_subscription_length <- table(dataset$Subscription..Length)
ordered_table_subscription_length <- sort(table_subscription_length, decreasing = TRUE)
propOrdSubscriptionLength <- prop.table(ordered_table_subscription_length)
plotSubscriptionLength <- barplot(propOrdSubscriptionLength, ylim = c(0, 1.05), main = "Diagramma di Pareto Subscription Length", col=length(dataset$Subscription..Length), las = 2)
lines(plotSubscriptionLength, cumsum(propOrdSubscriptionLength))
points(plotSubscriptionLength, cumsum(propOrdSubscriptionLength), pch = 16)

# Calcolo skewness per la feature Subscription Length
skewness_value <- skewness(dataset$Subscription..Length)

# Calcolo curtosi per la feature Subscription Length
kurtosis_value <- kurtosis(dataset$Subscription..Length)

# Calcolo plot per distribuzioni di Frequenza
library(ggplot2)
ggplot(dataset, aes(x = Subscription..Length)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +  # Usa after_stat(count) e linewidth
    labs(title = "Distribuzione di Frequenza di Subscription Length",
         x = "Subscription Length",
         y = "Frequenza") +
    theme_minimal()

################## feature Charge Amount ##################

# Calcolo della media per la feature Charge Amount
mean(dataset$Charge..Amount)

# Calcolo della mediana per la feature Charge Amount
median(dataset$Charge..Amount)

# Definizione funzione per calcolare la moda
moda <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Calcolo della moda per la feature Charge Amount
moda(dataset$Charge..Amount)

# Boxplot per la feature Charge Amount
boxplot(dataset$Charge..Amount, xlab="Charge Amount", col=c("green"))

# Calcolo degli outliers per la feature Charge Amount
# Calcola Q1 e Q3
Q1 <- quantile(dataset$Charge..Amount, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$Charge..Amount, 0.75, na.rm = TRUE)
# Calcola Scarto interquartile
IQR <- Q3 - Q1
# Definisce i limiti
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Trova gli outlier
outliers <- dataset$Charge..Amount[dataset$Charge..Amount < lower_bound | dataset$Charge..Amount > upper_bound]
# Visualizza gli outlier
outliers

# Istogramma Frequenze assolute per la feature Charge Amount
plot(table(dataset$Charge..Amount), col=c("green"), xlab = "Charge Amount", ylab = "Numero fruitori del servizio")

# Min. 1st Qu. Median Mean 3rd Qu. Max. per la feature Charge Amount
summary(dataset$Charge..Amount)

# Pie per frequenza relative per la feature Charge Amount
pie(table(dataset$Charge..Amount), col=rainbow(length(table(dataset$Charge..Amount))), labels=NA, xlab="Charge Amount")
legend("bottomleft", legend=names(table(dataset$Charge..Amount)), fill=rainbow(length(table(dataset$Charge..Amount))), ncol=3)

# Calcolo varianza per la feature Charge Amount
var(dataset$Charge..Amount, na.rm = TRUE)

# Calcolo deviazione standard per la feature Charge Amount
sd(dataset$Charge..Amount, na.rm = TRUE)

# Calcolo coefficiente di variazione per la feature Charge Amount
sd(dataset$Charge..Amount, na.rm = TRUE)/mean(dataset$Charge..Amount)*100

# Calcolo Funzione di distribuzione empirica (discreta) per la feature Charge Amount
FDE_DISCRETA_CHARGE_AMOUNT <- ecdf(dataset$Charge..Amount)
plot(FDE_DISCRETA_CHARGE_AMOUNT, main = "Funzione di distribuzione empirica (discreta)", xlab="Charge Amount", ylab = "FDE", verticals = TRUE, do.points = TRUE)

# Calcolo diagramma di Pareto per la feature Charge Amount
table_charge_amount <- table(dataset$Charge..Amount)
ordered_table_charge_amount <- sort(table_charge_amount, decreasing = TRUE)
propOrdChargeAmount <- prop.table(ordered_table_charge_amount)
plotChargeAmount <- barplot(propOrdChargeAmount, ylim = c(0, 1.05), main = "Diagramma di Pareto Charge Amount", col=length(dataset$Charge..Amount), las = 2)
lines(plotChargeAmount, cumsum(propOrdChargeAmount))
points(plotChargeAmount, cumsum(propOrdChargeAmount), pch = 16)

# Calcolo skewness per la feature Charge Amount
skewness_value <- skewness(dataset$Charge..Amount)

# Calcolo curtosi per la feature Charge Amount
kurtosis_value <- kurtosis(dataset$Charge..Amount)

# Calcolo plot per distribuzioni di Frequenza
library(ggplot2)
ggplot(dataset, aes(x = Charge..Amount)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +  # Usa after_stat(count) e linewidth
    labs(title = "Distribuzione di Frequenza di Charge Amount",
         x = "Charge Amount",
         y = "Frequenza") +
    theme_minimal()

################## feature Seconds of Use ##################

# Calcolo della media per la feature Seconds of Use
mean(dataset$Seconds.of.Use)

# Calcolo della mediana per la feature Seconds of Use
median(dataset$Seconds.of.Use)

# Definizione funzione per calcolare la moda
moda <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Calcolo della moda per la feature Seconds of Use
moda(dataset$Seconds.of.Use)

# Boxplot con baffo per la feature Seconds of Use
boxplot(dataset$Seconds.of.Use, xlab="Seconds of Use", col=c("blue"))

# Calcolo degli outliers per la feature Seconds of Use
# Calcola Q1 e Q3
Q1 <- quantile(dataset$Seconds.of.Use, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$Seconds.of.Use, 0.75, na.rm = TRUE)
# Calcola Scarto interquartile
IQR <- Q3 - Q1
# Definisce i limiti
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Trova gli outlier
outliers <- dataset$Seconds.of.Use[dataset$Seconds.of.Use < lower_bound | dataset$Seconds.of.Use > upper_bound]
# Visualizza gli outlier
outliers

# Istogramma Frequenze assolute per la feature Seconds of Use
plot(table(dataset$Seconds.of.Use), col=c("blue"), xlab = "Seconds of Use", ylab = "Numero fruitori del servizio")

# Min. 1st Qu. Median Mean 3rd Qu. Max. per la feature Seconds of Use
summary(dataset$Seconds.of.Use)

# Pie per frequenza relative per la feature Seconds of Use
pie(table(dataset$Seconds.of.Use), col=rainbow(length(table(dataset$Seconds.of.Use))), labels=NA, xlab="Seconds of Use")
legend("bottomleft", legend=names(table(dataset$Seconds.of.Use)), fill=rainbow(length(table(dataset$Seconds.of.Use))), ncol=2)

# Calcolo varianza per la feature Seconds of Use
var(dataset$Seconds.of.Use, na.rm = TRUE)

# Calcolo deviazione standard per la feature Seconds of Use
sd(dataset$Seconds.of.Use, na.rm = TRUE)

# Calcolo coefficiente di variazione per la feature Seconds of Use
sd(dataset$Seconds.of.Use, na.rm = TRUE)/mean(dataset$Seconds.of.Use)*100

# Calcolo Funzione di distribuzione empirica (discreta) per la feature Seconds of Use
FDE_DISCRETA_SECONDS_OF_USE <- ecdf(dataset$Seconds.of.Use)
plot(FDE_DISCRETA_SECONDS_OF_USE, main = "Funzione di distribuzione empirica (discreta)", xlab="Seconds of Use", ylab = "FDE", verticals = TRUE, do.points = TRUE)

# Calcolo diagramma di Pareto per la feature Seconds of Use
table_seconds_of_use <- table(dataset$Seconds.of.Use)
ordered_table_seconds_of_use <- sort(table_seconds_of_use, decreasing = TRUE)
propOrdSecondsOfUse <- prop.table(ordered_table_seconds_of_use)
plotSecondsOfUse <- barplot(propOrdSecondsOfUse, ylim = c(0, 1.05), main = "Diagramma di Pareto Seconds of Use", col=length(dataset$Seconds.of.Use), las = 2)
lines(plotSecondsOfUse, cumsum(propOrdSecondsOfUse))
points(plotSecondsOfUse, cumsum(propOrdSecondsOfUse), pch = 16)

# Calcolo skewness per la feature Seconds of Use
skewness_value <- skewness(dataset$Seconds.of.Use)

# Calcolo curtosi per la feature Seconds of Use
kurtosis_value <- kurtosis(dataset$Seconds.of.Use)

# Calcolo plot per distribuzioni di Frequenza
library(ggplot2)
ggplot(dataset, aes(x = Seconds.of.Use)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +  # Usa after_stat(count) e linewidth
    labs(title = "Distribuzione di Frequenza di Seconds of Use",
         x = "Seconds of Use",
         y = "Frequenza") +
    theme_minimal()



################## feature Frequency of Use ##################

# Calcolo della media per la feature Frequency of Use
mean(dataset$Frequency.of.use)

# Calcolo della mediana per la feature Frequency of Use
median(dataset$Frequency.of.use)

# Definizione funzione per calcolare la moda
moda <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Calcolo della moda per la feature Frequency of Use
moda(dataset$Frequency.of.use)

# Boxplot con baffo per la feature Frequency of Use
boxplot(dataset$Frequency.of.use, xlab="Frequency of Use", col=c("blue"))

# Calcolo degli outliers per la feature Frequency of Use
# Calcola Q1 e Q3
Q1 <- quantile(dataset$Frequency.of.use, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$Frequency.of.use, 0.75, na.rm = TRUE)
# Calcola Scarto interquartile
IQR <- Q3 - Q1
# Definisce i limiti
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Trova gli outlier
outliers <- dataset$Frequency.of.use[dataset$Frequency.of.use < lower_bound | dataset$Frequency.of.use > upper_bound]
# Visualizza gli outlier
outliers

# Istogramma Frequeze assolute per la feature Frequency of Use
plot(table(dataset$Frequency.of.use), col=c("blue"), xlab = "Frequency of Use", ylab = "Numero fruitori del servizio")

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. per la feature Frequency of Use
summary(dataset$Frequency.of.use)


# Calcolo varianza per la feature Frequency of Use
var(dataset$Frequency.of.use, na.rm = TRUE)

# Calcolo deviazione standard per la feature Frequency of Use
sd(dataset$Frequency.of.use, na.rm = TRUE)

# Calcolo coefficiente di variazione per la feature Frequency of Use
sd(dataset$Frequency.of.use, na.rm = TRUE)/mean(dataset$Frequency.of.use)*100

# Calcolo Funzione di distribuzione empirica (discreta) per la feature Frequency of Use
FDE_DISCRETA_FREQUENCY_OF_USE <- ecdf(dataset$Frequency.of.use)
plot(FDE_DISCRETA_FREQUENCY_OF_USE, main = "Funzione di distribuzione empirica (discreta)", xlab="Frequency of Use", ylab = "FDE", verticals = TRUE, do.points = TRUE)

# Calcolo diagramma di Pareto per la feature Frequency of Use
table_frequency_of_use <- table(dataset$Frequency.of.use)
ordered_table_frequency_of_use <- sort(table_frequency_of_use, decreasing = TRUE)
propOrdFrequencyOfUse <- prop.table(ordered_table_frequency_of_use)
plotFrequencyOfUse <- barplot(propOrdFrequencyOfUse, ylim = c(0, 1.05), main = "Diagramma di Pareto Frequency of Use", col=length(dataset$Frequency.of.use), las = 2)
lines(plotFrequencyOfUse, cumsum(propOrdFrequencyOfUse))
points(plotFrequencyOfUse, cumsum(propOrdFrequencyOfUse), pch = 16)

# Calcolo skewness per la feature Frequency of Use
skewness_value <- skewness(dataset$Frequency.of.use)

# Calcolo curtosi per la feature Frequency of Use
kurtosis_value <- kurtosis(dataset$Frequency.of.use)

# Calcolo plot per distribuzioni di Frequenza
library(ggplot2)
ggplot(dataset, aes(x = Frequency.of.use)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +
    labs(title = "Distribuzione di Frequenza di Frequency of Use",
         x = "Numero di Frequency of Use",
         y = "Frequenza") +
    theme_minimal()


################## feature Frequency of SMS ##################

# Calcolo della media per la feature Frequency of SMS
mean(dataset$Frequency.of.SMS)

# Calcolo della mediana per la feature Frequency of SMS
median(dataset$Frequency.of.SMS)

# Definizione funzione per calcolare la moda
moda <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Calcolo della moda per la feature Frequency of SMS
moda(dataset$Frequency.of.SMS)

# Boxplot con baffo per la feature Frequency of SMS
boxplot(dataset$Frequency.of.SMS, xlab="Frequency of SMS", col=c("blue"))

# Calcolo degli outliers per la feature Frequency of SMS
# Calcola Q1 e Q3
Q1 <- quantile(dataset$Frequency.of.SMS, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$Frequency.of.SMS, 0.75, na.rm = TRUE)
# Calcola Scarto interquartile
IQR <- Q3 - Q1
# Definisce i limiti
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Trova gli outlier
outliers <- dataset$Frequency.of.SMS[dataset$Frequency.of.SMS < lower_bound | dataset$Frequency.of.SMS > upper_bound]
# Visualizza gli outlier
outliers

# Istogramma Frequeze assolute per la feature Frequency of SMS
plot(table(dataset$Frequency.of.SMS), col=c("blue"), xlab = "Frequency of SMS", ylab = "Numero fruitori del servizio")

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. per la feature Frequency of SMS
summary(dataset$Frequency.of.SMS)

# Pie per frequenza relative per la feature Frequency of SMS
pie(table(dataset$Frequency.of.SMS), col=rainbow(length(table(dataset$Frequency.of.SMS))), labels=NA, xlab="Frequency of SMS")
legend("bottomleft", legend=names(table(dataset$Frequency.of.SMS)), fill=rainbow(length(table(dataset$Frequency.of.SMS))), ncol=2)

# Calcolo varianza per la feature Frequency of SMS
var(dataset$Frequency.of.SMS, na.rm = TRUE)

# Calcolo deviazione standard per la feature Frequency of SMS
sd(dataset$Frequency.of.SMS, na.rm = TRUE)

# Calcolo coefficiente di variazione per la feature Frequency of SMS
sd(dataset$Frequency.of.SMS, na.rm = TRUE)/mean(dataset$Frequency.of.SMS)*100

# Calcolo Funzione di distribuzione empirica (discreta) per la feature Frequency of SMS
FDE_DISCRETA_FREQUENCY_OF_SMS <- ecdf(dataset$Frequency.of.SMS)
plot(FDE_DISCRETA_FREQUENCY_OF_SMS, main = "Funzione di distribuzione empirica (discreta)", xlab="Frequency of SMS", ylab = "FDE", verticals = TRUE, do.points = TRUE)

# Calcolo diagramma di Pareto per la feature Frequency of SMS
table_frequency_of_sms <- table(dataset$Frequency.of.SMS)
ordered_table_frequency_of_sms <- sort(table_frequency_of_sms, decreasing = TRUE)
propOrdFrequencyOfSMS <- prop.table(ordered_table_frequency_of_sms)
plotFrequencyOfSMS <- barplot(propOrdFrequencyOfSMS, ylim = c(0, 1.05), main = "Diagramma di Pareto Frequency of SMS", col=length(dataset$Frequency.of.SMS), las = 2)
lines(plotFrequencyOfSMS, cumsum(propOrdFrequencyOfSMS))
points(plotFrequencyOfSMS, cumsum(propOrdFrequencyOfSMS), pch = 16)

# Calcolo skewness per la feature Frequency of SMS
skewness_value <- skewness(dataset$Frequency.of.SMS)

# Calcolo curtosi per la feature Frequency of SMS
kurtosis_value <- kurtosis(dataset$Frequency.of.SMS)

# Calcolo plot per distribuzioni di Frequenza
library(ggplot2)
ggplot(dataset, aes(x = Frequency.of.SMS)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +
    labs(title = "Distribuzione di Frequenza di Frequency of SMS",
         x = "Numero di Frequency of SMS",
         y = "Frequenza") +
    theme_minimal()


################## feature Distinct Called Numbers ##################

# Calcolo della media per la feature Distinct Called Numbers
mean(dataset$Distinct.Called.Numbers)

# Calcolo della mediana per la feature Distinct Called Numbers
median(dataset$Distinct.Called.Numbers)

# Definizione funzione per calcolare la moda
moda <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Calcolo della moda per la feature Distinct Called Numbers
moda(dataset$Distinct.Called.Numbers)

# Boxplot con baffo per la feature Distinct Called Numbers
boxplot(dataset$Distinct.Called.Numbers, xlab="Distinct Called Numbers", col=c("green"))

# Calcolo degli outliers per la feature Distinct Called Numbers
# Calcola Q1 e Q3
Q1 <- quantile(dataset$Distinct.Called.Numbers, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$Distinct.Called.Numbers, 0.75, na.rm = TRUE)
# Calcola Scarto interquartile
IQR <- Q3 - Q1
# Definisce i limiti
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Trova gli outlier
outliers <- dataset$Distinct.Called.Numbers[dataset$Distinct.Called.Numbers < lower_bound | dataset$Distinct.Called.Numbers > upper_bound]
# Visualizza gli outlier
outliers

# Istogramma Frequeze assolute per la feature Distinct Called Numbers
plot(table(dataset$Distinct.Called.Numbers), col=c("green"), xlab = "Distinct Called Numbers", ylab = "Numero fruitori del servizio")

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. per la feature Distinct Called Numbers
summary(dataset$Distinct.Called.Numbers)

# Pie per frequenza relative per la feature Distinct Called Numbers
pie(table(dataset$Distinct.Called.Numbers), col=rainbow(length(table(dataset$Distinct.Called.Numbers))), labels=NA, xlab="Distinct Called Numbers")
legend("bottomleft", legend=names(table(dataset$Distinct.Called.Numbers)), fill=rainbow(length(table(dataset$Distinct.Called.Numbers))), ncol=2)

# Calcolo varianza per la feature Distinct Called Numbers
var(dataset$Distinct.Called.Numbers, na.rm = TRUE)

# Calcolo deviazione standard per la feature Distinct Called Numbers
sd(dataset$Distinct.Called.Numbers, na.rm = TRUE)

# Calcolo coefficiente di variazione per la feature Distinct Called Numbers
sd(dataset$Distinct.Called.Numbers, na.rm = TRUE)/mean(dataset$Distinct.Called.Numbers)*100

# Calcolo Funzione di distribuzione empirica (discreta) per la feature Distinct Called Numbers
FDE_DISCRETA_DISTINCT_CALLED_NUMBERS <- ecdf(dataset$Distinct.Called.Numbers)
plot(FDE_DISCRETA_DISTINCT_CALLED_NUMBERS, main = "Funzione di distribuzione empirica (discreta)", xlab="Distinct Called Numbers", ylab = "FDE", verticals = TRUE, do.points = TRUE)

# Calcolo diagramma di Pareto per la feature Distinct Called Numbers
table_distinct_called_numbers <- table(dataset$Distinct.Called.Numbers)
ordered_table_distinct_called_numbers <- sort(table_distinct_called_numbers, decreasing = TRUE)
propOrdDistinctCalledNumbers <- prop.table(ordered_table_distinct_called_numbers)
plotDistinctCalledNumbers <- barplot(propOrdDistinctCalledNumbers, ylim = c(0, 1.05), main = "Diagramma di Pareto Distinct Called Numbers", col=length(dataset$Distinct.Called.Numbers), las = 2)
lines(plotDistinctCalledNumbers, cumsum(propOrdDistinctCalledNumbers))
points(plotDistinctCalledNumbers, cumsum(propOrdDistinctCalledNumbers), pch = 16)

# Calcolo skewness per la feature Distinct Called Numbers
skewness_value <- skewness(dataset$Distinct.Called.Numbers)

# Calcolo curtosi per la feature Distinct Called Numbers
kurtosis_value <- kurtosis(dataset$Distinct.Called.Numbers)

# Calcolo plot per distribuzioni di Frequenza
library(ggplot2)
ggplot(dataset, aes(x = Distinct.Called.Numbers)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +
    labs(title = "Distribuzione di Frequenza di Distinct Called Numbers",
         x = "Numero di Distinct Called Numbers",
         y = "Frequenza") +
    theme_minimal()

################## feature Age Group ##################

# Calcolo della media per la feature Age Group
mean(dataset$Age.Group)

# Calcolo della mediana per la feature Age Group
median(dataset$Age.Group)

# Definizione funzione per calcolare la moda
moda <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Calcolo della moda per la feature Age Group
moda(dataset$Age.Group)

# Boxplot con baffo per la feature Age Group
boxplot(dataset$Age.Group, xlab="Age Group", col=c("blue"))

# Calcolo degli outliers per la feature Age Group
# Calcola Q1 e Q3
Q1 <- quantile(dataset$Age.Group, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$Age.Group, 0.75, na.rm = TRUE)
# Calcola Scarto interquartile
IQR <- Q3 - Q1
# Definisce i limiti
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Trova gli outlier
outliers <- dataset$Age.Group[dataset$Age.Group < lower_bound | dataset$Age.Group > upper_bound]
# Visualizza gli outlier
outliers

# Istogramma Frequeze assolute per la feature Age Group
plot(table(dataset$Age.Group), col=c("blue"), xlab = "Age Group", ylab = "Numero fruitori del servizio")

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. per la feature Age Group
summary(dataset$Age.Group)

# Pie per frequenza relative per la feature Age Group
pie(table(dataset$Age.Group), col=rainbow(length(table(dataset$Age.Group))), labels=NA, xlab="Age Group")
legend("bottomleft", legend=names(table(dataset$Age.Group)), fill=rainbow(length(table(dataset$Age.Group))), ncol=2)

# Calcolo varianza per la feature Age Group
var(dataset$Age.Group, na.rm = TRUE)

# Calcolo deviazione standard per la feature Age Group
sd(dataset$Age.Group, na.rm = TRUE)

# Calcolo coefficiente di variazione per la feature Age Group
sd(dataset$Age.Group, na.rm = TRUE)/mean(dataset$Age.Group)*100

# Calcolo Funzione di distribuzione empirica (discreta) per la feature Age Group
FDE_DISCRETA_AGE_GROUP <- ecdf(dataset$Age.Group)
plot(FDE_DISCRETA_AGE_GROUP, main = "Funzione di distribuzione empirica (discreta)", xlab="Age Group", ylab = "FDE", verticals = TRUE, do.points = TRUE)

# Calcolo diagramma di Pareto per la feature Age Group
table_age_group <- table(dataset$Age.Group)
ordered_table_age_group <- sort(table_age_group, decreasing = TRUE)
propOrdAgeGroup <- prop.table(ordered_table_age_group)
plotAgeGroup <- barplot(propOrdAgeGroup, ylim = c(0, 1.05), main = "Diagramma di Pareto Age Group", col=length(dataset$Age.Group), las = 2)
lines(plotAgeGroup, cumsum(propOrdAgeGroup))
points(plotAgeGroup, cumsum(propOrdAgeGroup), pch = 16)

# Calcolo skewness per la feature Age Group
skewness_value <- skewness(dataset$Age.Group)

# Calcolo curtosi per la feature Age Group
kurtosis_value <- kurtosis(dataset$Age.Group)

# Calcolo plot per distribuzioni di Frequenza
library(ggplot2)
ggplot(dataset, aes(x = Age.Group)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +
    labs(title = "Distribuzione di Frequenza di Age Group",
         x = "Numero di Age Group",
         y = "Frequenza") +
    theme_minimal()

################## feature Tariff Plan ##################

# Calcolo frequenze assolute per feature Tariff Plan
freq_assolute <- table(dataset$Tariff.Plan)

# Calcolo frequenze relative per feature Tariff Plan
freq_relative <- prop.table(freq_assolute)

# Pie per frequenza relative per la feature Tariff Plan e Funzione di distribuzione empirica (discreta) per la feature Tariff Plan
# Imposta i grafici affiancati
par(mfrow = c(1, 2)) 
pie(table(dataset$Tariff.Plan), 
    col = rainbow(length(table(dataset$Tariff.Plan))), 
    labels = NA, 
    main = "Distribuzione Tariff Plan")
legend("bottomleft", 
       legend = names(table(dataset$Tariff.Plan)), 
       fill = rainbow(length(table(dataset$Tariff.Plan))), 
       ncol = 2)

# Funzione di distribuzione empirica (discreta)
FDE_DISCRETA_TARIFF_PLAN <- ecdf(dataset$Tariff.Plan)
plot(FDE_DISCRETA_TARIFF_PLAN, 
     main = "Funzione di distribuzione empirica (discreta)", 
     xlab = "Tariff Plan", 
     ylab = "FDE", 
     verticals = TRUE, 
     do.points = TRUE)

################## feature Status ##################

# Calcolo frequenze assolute per feature Status
freq_assolute <- table(dataset$Status)

# Calcolo frequenze relative per feature Status
freq_relative <- prop.table(freq_assolute)

# Pie per frequenza relative per la feature Status e Funzione di distribuzione empirica (discreta) per la feature Status
# Imposta i grafici affiancati
par(mfrow = c(1, 2)) 
pie(table(dataset$Status), 
    col = rainbow(length(table(dataset$Status))), 
    labels = NA, 
    main = "Distribuzione Status")
legend("bottomleft", 
       legend = names(table(dataset$Status)), 
       fill = rainbow(length(table(dataset$Status))), 
       ncol = 2)

# Funzione di distribuzione empirica (discreta)
FDE_DISCRETA_STATUS <- ecdf(dataset$Status)
plot(FDE_DISCRETA_STATUS, 
     main = "Funzione di distribuzione empirica (discreta)", 
     xlab = "Status", 
     ylab = "FDE", 
     verticals = TRUE, 
     do.points = TRUE)

################## feature Age ##################

# Calcolo della media per la feature Age
mean(dataset$Age)

# Calcolo della mediana per la feature Age
median(dataset$Age)

# Definizione funzione per calcolare la moda
moda <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Calcolo della moda per la feature Age
moda(dataset$Age)

# Boxplot con baffo per la feature Age
boxplot(dataset$Age, xlab="Age", col=c("blue"))

# Calcolo degli outliers per la feature Age
# Calcola Q1 e Q3
Q1 <- quantile(dataset$Age, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$Age, 0.75, na.rm = TRUE)
# Calcola Scarto interquartile
IQR <- Q3 - Q1
# Definisce i limiti
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Trova gli outlier
outliers <- dataset$Age[dataset$Age < lower_bound | dataset$Age > upper_bound]
# Visualizza gli outlier
outliers

# Istogramma Frequeze assolute per la feature Age
plot(table(dataset$Age), col=c("blue"), xlab = "Age", ylab = "Numero fruitori del servizio")

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. per la feature Age
summary(dataset$Age)

# Pie per frequenza relative per la feature Age
pie(table(dataset$Age), col=rainbow(length(table(dataset$Age))), labels=NA, xlab="Age")
legend("bottomleft", legend=names(table(dataset$Age)), fill=rainbow(length(table(dataset$Age))), ncol=2)

# Calcolo varianza per la feature Age
var(dataset$Age, na.rm = TRUE)

# Calcolo deviazione standard per la feature Age
sd(dataset$Age, na.rm = TRUE)

# Calcolo coefficiente di variazione per la feature Age
sd(dataset$Age, na.rm = TRUE)/mean(dataset$Age)*100

# Calcolo Funzione di distribuzione empirica (discreta) per la feature Age
FDE_DISCRETA_AGE <- ecdf(dataset$Age)
plot(FDE_DISCRETA_AGE, main = "Funzione di distribuzione empirica (discreta)", xlab="Age", ylab = "FDE", verticals = TRUE, do.points = TRUE)

# Calcolo diagramma di Pareto per la feature Age
table_age <- table(dataset$Age)
ordered_table_age <- sort(table_age, decreasing = TRUE)
propOrdAge <- prop.table(ordered_table_age)
plotAge <- barplot(propOrdAge, ylim = c(0, 1.05), main = "Diagramma di Pareto Age", col=length(dataset$Age), las = 2)
lines(plotAge, cumsum(propOrdAge))
points(plotAge, cumsum(propOrdAge), pch = 16)

# Calcolo skewness per la feature Age
skewness_value <- skewness(dataset$Age)

# Calcolo curtosi per la feature Age
kurtosis_value <- kurtosis(dataset$Age)

# Calcolo plot per distribuzioni di Frequenza
library(ggplot2)
ggplot(dataset, aes(x = Age)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +
    labs(title = "Distribuzione di Frequenza di Age",
         x = "Età",
         y = "Frequenza") +
    theme_minimal()

################## feature Churn ##################

# Calcolo frequenze assolute per feature Churn
freq_assolute <- table(dataset$Churn)

# Calcolo frequenze relative per feature Churn
freq_relative <- prop.table(freq_assolute)

# Pie per frequenza relative per la feature Churn e Funzione di distribuzione empirica (discreta) per la feature Churn
# Imposta i grafici affiancati
par(mfrow = c(1, 2)) 
pie(table(dataset$Churn), 
    col = rainbow(length(table(dataset$Churn))), 
    labels = NA, 
    main = "Distribuzione Churn")
legend("bottomleft", 
       legend = names(table(dataset$Churn)), 
       fill = rainbow(length(table(dataset$Churn))), 
       ncol = 2)

# Funzione di distribuzione empirica (discreta)
FDE_DISCRETA_CHURN <- ecdf(dataset$Churn)
plot(FDE_DISCRETA_CHURN, 
     main = "Funzione di distribuzione empirica (discreta)", 
     xlab = "Churn", 
     ylab = "FDE", 
     verticals = TRUE, 
     do.points = TRUE)

################## feature Customer Value ##################

# Calcolo della media per la feature Customer Value
mean(dataset$Customer.Value, na.rm = TRUE)

# Calcolo della mediana per la feature Customer Value
median(dataset$Customer.Value, na.rm = TRUE)

# Definizione funzione per calcolare la moda
moda <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Calcolo della moda per la feature Customer Value
moda(dataset$Customer.Value)

# Boxplot con baffo per la feature Customer Value
boxplot(dataset$Customer.Value, xlab="Customer Value", col=c("blue"))

# Calcolo degli outliers per la feature Customer Value
# Calcola Q1 e Q3
Q1 <- quantile(dataset$Customer.Value, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$Customer.Value, 0.75, na.rm = TRUE)
# Calcola Scarto interquartile
IQR <- Q3 - Q1
# Definisce i limiti
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Trova gli outlier
outliers <- dataset$Customer.Value[dataset$Customer.Value < lower_bound | dataset$Customer.Value > upper_bound]
# Visualizza gli outlier
outliers

# Istogramma Frequenze assolute per la feature Customer Value
plot(table(dataset$Customer.Value), col=c("blue"), xlab = "Customer Value", ylab = "Numero fruitori del servizio")

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. per la feature Customer Value
summary(dataset$Customer.Value)

# Pie per frequenze relative per la feature Customer Value
pie(table(dataset$Customer.Value), col=rainbow(length(table(dataset$Customer.Value))), labels=NA, xlab="Customer Value")
legend("bottomleft", legend=names(table(dataset$Customer.Value)), fill=rainbow(length(table(dataset$Customer.Value))), ncol=2)

# Calcolo varianza per la feature Customer Value
var(dataset$Customer.Value, na.rm = TRUE)

# Calcolo deviazione standard per la feature Customer Value
sd(dataset$Customer.Value, na.rm = TRUE)

# Calcolo coefficiente di variazione per la feature Customer Value
sd(dataset$Customer.Value, na.rm = TRUE)/mean(dataset$Customer.Value, na.rm = TRUE)*100

# Calcolo Funzione di distribuzione empirica (discreta) per la feature Customer Value
FDE_DISCRETA_CUSTOMER_VALUE <- ecdf(dataset$Customer.Value)
plot(FDE_DISCRETA_CUSTOMER_VALUE, main = "Funzione di distribuzione empirica (discreta)", xlab="Customer Value", ylab = "FDE", verticals = TRUE, do.points = TRUE)

# Calcolo diagramma di Pareto per la feature Customer Value
table_customer_value <- table(dataset$Customer.Value)
ordered_table_customer_value <- sort(table_customer_value, decreasing = TRUE)
propOrdCustomerValue <- prop.table(ordered_table_customer_value)
plotCustomerValue <- barplot(propOrdCustomerValue, ylim = c(0, 1.05), main = "Diagramma di pareto Customer Value", col=length(dataset$Customer.Value), las = 2)
lines(plotCustomerValue, cumsum(propOrdCustomerValue))
points(plotCustomerValue, cumsum(propOrdCustomerValue), pch = 16)

# Calcolo skewness per la feature Customer Value
library(moments)
skewness_value <- skewness(dataset$Customer.Value, na.rm = TRUE)

# Calcolo curtosi per la feature Customer Value
kurtosis_value <- kurtosis(dataset$Customer.Value, na.rm = TRUE)

# Calcolo plot per distribuzioni di Frequenza
library(ggplot2)
ggplot(dataset, aes(x = Customer.Value)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +
    labs(title = "Distribuzione di Frequenza di Customer Value",
         x = "Customer Value",
         y = "Frequenza")