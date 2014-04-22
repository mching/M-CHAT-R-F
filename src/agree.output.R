agree.output <- function(x1, x2, title1, title2) {
  par(mfrow = c(1,2))
  plot(x1, main = title1)
  plot(x2, main = title2)
  par(mfrow = c(1,1))
  
  print(summary(x1))
  table1 <- table(x1 == "Agree" | x1 == "Strongly Agree") # number of agree or strongly agree
  print(binom.confint(table1[2], n = sum(table1), method = "exact"))
  
  print(summary(x2))
  table2 <- table(x2 == "Agree" | x2 == "Strongly Agree") # number of agree or strongly agree
  print(binom.confint(table2[2], n = sum(table2), method = "exact"))
  
  mean(as.numeric(x1), na.rm = T)
  sd(as.numeric(x1), na.rm = T)
  
  mean(as.numeric(x2), na.rm = T)
  sd(as.numeric(x2), na.rm = T)
  
  print(t.test(as.numeric(x1), as.numeric(x2), na.rm = T)) # t-test on likert scale values
  print(fisher.test(matrix(c(table1, table2), nrow = 2))) # Fisher test for ratios
}
