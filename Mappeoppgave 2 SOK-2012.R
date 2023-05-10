n <- 100
reps <- c(10, 100, 1000, 10000, 100000)
set.seed(10)
data <- array(sample(c(0,1), max(reps)*n, replace=TRUE), c(max(reps),n))
intervaller <- seq(0, 99, by=1)
gjennomsnitt <- matrix(nrow=length(intervaller), ncol=length(reps))
for (k in 1:length(reps)) {
  rep <- reps[k]
  pb <- txtProgressBar(min = 0, max = length(intervaller), style = 3,
                       label = sprintf("rep = %d", rep))
  prob <- rep(NA, rep)
  for (j in 1:length(intervaller)) {
    for (i in 1:rep){
      heads1 <- data[i,1:(n-1)][1:intervaller[j]]==1
      heads2 <- data[i,2:n][1:intervaller[j]]==1
      n_heads1 <- sum(heads1)
      if (n_heads1 > 0) {
        prob[i] <- sum(heads1 & heads2, na.rm=TRUE)/n_heads1
      }
    }
    gjennomsnitt[j,k] <- mean(prob, na.rm=TRUE)
    setTxtProgressBar(pb, j)
  }
  close(pb)
}


hot_hand <- rep(NA, n)
for (i in 2:n) {
  hot_hand[i] <- sum(data[, i] == 1 & data[, i-1] == 1) / sum(data[, i-1] == 1)
}


cold_hand <- rep(NA, n)
for (i in 2:n) {
  cold_hand[i] <- sum(data[, i] == 1 & data[, i-1] == 0) / sum(data[, i-1] == 0)
}
result_df <- data.frame(intervaller = intervaller)
for (k in 1:length(reps)) {
  rep <- reps[k]
  col_name_hot <- paste0("Hot Hand (", rep, ")")
  col_name_cold <- paste0("Cold Hand (", rep, ")")
  result_df[[col_name_hot]] <- gjennomsnitt[,k]
  result_df[[col_name_cold]] <- 1 - gjennomsnitt[,k]
}


#oppgave 3



mean_hot_hand <- sapply(hot_hand, mean)
mean_cold_hand <- sapply(cold_hand, mean)

gvt_estimat <- mean_hot_hand - mean_cold_hand



library(tidyverse)
gvt_estimat <- mean(hot_hand - cold_hand, na.rm=TRUE)
cat("GVT estimate:", gvt_estimat, "\n")


ggplot(result_df, aes(x = intervaller)) +
  geom_line(aes(y = gjennomsnitt[,1], color = "M = 10")) +
  geom_line(aes(y = gjennomsnitt[,2], color = "M = 100")) +
  geom_line(aes(y = gjennomsnitt[,3], color = "M = 1000")) +
  geom_line(aes(y = gjennomsnitt[,4], color = "M = 10000")) +
  geom_line(aes(y = gjennomsnitt[,5], color = "M = 100000")) +
  scale_color_manual(name = "M", values = c("M = 10" = "red", "M = 100" = "green", "M = 1000" = "blue", "M = 10000" = "orange", "M = 100000" = "pink")) +
  labs(x = "Antall kast", y = "Sannsynlighet", title = "GVT-estimat for ulike verdier av M")
