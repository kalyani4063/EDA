data <- read.csv("CO2.csv")
result <- t.test(data$conc,mu=550,alternative = "less")
print(result)
data$Treatment <- as.factor(data$Treatment)
data$Type <- as.factor(data$Type)
boxplot(uptake~Treatment,data=data,
 main="Uptake by Treatment",
 xlab="Treatment",
 ylab="Uptake")
result2 <- t.test(uptake~Treatment,data=data)
print(result2)
22CSEB11 KALYANI S Pg No:
result3 <- t.test(uptake~Treatment,data=data,var.equal = TRUE)
print(result3)
chill <- data$uptake[data$Treatment=="chilled"]
print(chill)
nchill <- data$uptake[data$Treatment=="nonchilled"]
print(nchill)
result4=t.test(chill,nchill,paired=TRUE)
print(result4)
result5 <- t.test(uptake~Type,data=data)
print(result5)
