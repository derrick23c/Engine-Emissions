setwd("C:/Users/derri/Onedrive/r")
data <- read.csv('emission.csv',header=TRUE)

summary(x)
summary(y)
summary(z)
x=data$hc
y=data$co1
z=data$co2

x1 = data$dev

legend = rownames(VADeaths)

fit1 <- aov(co1 ~ day*rep, data=data)
fit2 <- aov(co2 ~ day*rep, data=data)
fit3 <- aov(hc ~ day*rep, data=data) aov(co2 ~ day, data = data)


summary(fit1)
summary(fit2)
summary(fit3)

model11 <- aov(co1 ~ day, data=data)
model12 <- aov(co1 ~ rep, data=data)
summary(model11)
summary(model12)

model21 <- aov(co2 ~ day, data=data)
model22 <- aov(co2 ~ rep, data=data)
summary(model21)
summary(model22)

model31 <- aov(hc ~ day, data=data)
model32 <- aov(hc ~ rep, data=data)
summary(model31)
summary(model32)

modela <- aov(hc ~ dev, data=data)
modelb <- aov(co1 ~ dev, data=data)
modelc <- aov(co2 ~ dev, data=data)
summary(modela)
summary(modelb)
summary(modelc)

hc_a <- c(8.24,20.2,12.48,20.27,7.73,13.1,12.13,10.03,18.08,11.16,10.76,5.22,13.63,14,14.04,12.5)
shapiro.test(hc_a)

ee <-as.matrix(data) 
dd <- as.vector(ee)

data <- read.table('1.txt',header=TRUE)
ee <-as.matrix(data) 
co2_a <- as.vector(ee)
co2_a
shapiro.test(co2_a)

data <- read.table('2.txt',header=TRUE)
ee <-as.matrix(data) 
co2_b <- as.vector(ee)
co2_b
shapiro.test(co2_b)

data <- read.table('1.txt',header=TRUE)
ee <-as.matrix(data) 
co1_a <- as.vector(ee)
co1_a
shapiro.test(co1_a)

data <- read.table('2.txt',header=TRUE)
ee <-as.matrix(data) 
co1_b <- as.vector(ee)
co1_b
shapiro.test(co1_b)

data <- read.csv('emission.csv',header=TRUE)
bartlett.test(hc~dev, data)
bartlett.test(co2~dev, data)
bartlett.test(co1~dev, data)



t.test(hc ~ co3, var.equal = TRUE, data)
t.test(co1 ~ co3, var.equal = TRUE, data)
t.test(co2 ~ co3, var.equal = TRUE, data)

data <- read.csv('4.csv',header=TRUE)
attach(data)
t.test(hc ~ co3, var.equal = TRUE, data)
t.test(co1 ~ co3, var.equal = TRUE, data)
t.test(co2 ~ co3, var.equal = TRUE, data)

