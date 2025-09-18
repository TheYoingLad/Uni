data <- c(180, 163, 150, 157, 165, 165, 174, 191, 172, 165, 168, 186)

atlag <- mean(data)
szoras <- sd(data)

szoras_eh <- szoras / atlag

boxplot(data, horizontal = TRUE, main = "Magasságok boxplot")$stats

plot(ecdf(data), do.points = FALSE, main = "Magasságok eloszlása")