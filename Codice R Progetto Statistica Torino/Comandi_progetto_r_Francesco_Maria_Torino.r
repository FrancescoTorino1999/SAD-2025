Comandi Progetto Torino Francesco Maria (Id Progetto: 34)

setwd('/Users/francescomariatorino/Desktop/Università - 0522501879/Corsi/Statistica/Progetto')
#Dataset reale
dataset <- read.csv("Customer Churn.csv")
#Dataset con aggiunta la feature Second of use intervals
dataset <- read.csv("Customer Churn with Intervals.csv")
#Dataset Sintetico
dataset <- read.csv("synthetic_iranian_churn_dataset_with_outliers.csv")

# Count delle entry

numero_entry <- nrow(dataset)
print(numero_entry)



# Controllo dei nomi delle colonne e tipo di dato per ogni feature

head(dataset)

################## Creazione feature Seconds of use Interval ##################

# Determina il valore massimo per la feature Seconds.of.Use
max_value <- max(dataset$Seconds.of.Use, na.rm = TRUE)

# Creare intervalli da 0 a 17090 con ampiezza uguale per ogni intervallo
dataset$Seconds.of.Use.Interval <- cut(
  dataset$Seconds.of.Use,
  breaks = seq(0, max_value, length.out = 51), # 50 intervalli tra 0 e 17090
  include.lowest = TRUE,
  right = TRUE
)

# Verifica della feature aggiunta
head(dataset$Seconds.of.Use.Interval)


write.csv(dataset, "Customer Churn with Intervals Outliers.csv", row.names = FALSE)

# Calcolo plot per distribuzioni di Frequenza
library(ggplot2)
ggplot(dataset, aes(x = Seconds.of.Use.Interval)) +
    geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = "Distribuzione di Frequenza di Seconds of Use Interval",
         x = "Seconds of Use Interval",
         y = "Frequenza") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


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

############################ Analisi univariata ############################



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


############################ Analisi univariata dataset sintetico ############################



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

############################ Analisi bivariata ############################ 


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

############################ Clustering dataset reale ############################


#Calcolo random forest:

library(randomForest)
churn_rf <- randomForest(Churn ~ ., data = dataset)
print(churn_rf)
varImpPlot(churn_rf)

#Elbow method
library(ggplot2)

data <- dataset[, c("Complains", "Status", "Seconds.of.Use")]

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


############################ Studio popolazioni ############################


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

