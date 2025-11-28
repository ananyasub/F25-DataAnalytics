library(readr)
library(EnvStats)
library(nortest)

epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

PAR <- epi.data$PAR.new
SPI <- epi.data$SPI.new

summary(PAR)
summary(SPI)

boxplot(PAR, SPI, names = c("PAR","SPI"))

NAs <- is.na(PAR)
PAR.noNA <- PAR[!NAs]

hist(PAR, probability = TRUE)
lines(density(PAR,na.rm=TRUE,bw="SJ")) # or try bw=“SJ”

x1<-seq(20,80,1)
d1 <- dnorm(x1,mean=41, sd=26,log=FALSE)
lines(x1,d1, col="red")

NAs <- is.na(SPI)
SPI.noNA <- SPI[!NAs]

hist(SPI, probability = TRUE)
lines(density(SPI,na.rm=TRUE,bw="SJ")) # or try bw=“SJ”

x1<-seq(20,80,1)
d1 <- dnorm(x1,mean=49, sd=28,log=FALSE)
lines(x1,d1, col="red")

plot(ecdf(PAR), do.points=FALSE, verticals=TRUE) 

plot(ecdf(SPI), do.points=FALSE, verticals=TRUE)


qqnorm(PAR, main = "Q-Q Plot of PAR vs Normal")
qqline(PAR, col = "red")

qqnorm(SPI, main = "Q-Q Plot of SPI vs Normal")
qqline(SPI, col = "red")


qqplot(PAR, SPI, main = "Q-Q Plot of PAR vs SPI",
       xlab = "PAR Quantiles", ylab = "SPI Quantiles")
abline(0, 1, col = "blue")


shapiro.test(PAR)
shapiro.test(SPI)

ad.test(PAR)
ad.test(SPI)


ks.test(PAR,SPI)

wilcox.test(PAR,SPI)

var.test(PAR,SPI)
t.test(PAR,SPI)
