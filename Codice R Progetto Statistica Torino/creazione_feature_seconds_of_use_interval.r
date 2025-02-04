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