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