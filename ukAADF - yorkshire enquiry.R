#Assignment

#The UK government also like to look at miles driven. 
#You can do this by multiplying the AADF by the corresponding length of road 
#(link length) and by the number of days in the years. 


setwd("~/MSc Module 3/CETM72")
library(factoextra) 
library(caret)
library(e1071)
library(ggplot2)
library(graphics)
library(grDevices)
library(stats)
library(tibble)
library(utils)


roadusers <- read.csv("YorkshireAADF.csv",header=TRUE, stringsAsFactors = FALSE)
rothroadusers<- read.csv("RotherhamAADF.csv",header=TRUE, stringsAsFactors = FALSE)
attach(roadusers) 
head(roadusers)
tail(roadusers)

attach(rothroadusers)
head(rothroadusers)
anyNA(roadusers)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

roadusers$AADFYear_norm<-normalize(roadusers$AADFYear)
roadusers$AllHGVs_norm<-normalize(roadusers$AllHGVs)
roadusers$CarsTaxis_norm<-normalize(roadusers$CarsTaxis)
roadusers$BusesCoaches_norm<-normalize(roadusers$BusesCoaches)
roadusers$Motorcycles_norm<-normalize(roadusers$Motorcycles)
roadusers$AllMotorVehicles_norm<-normalize(roadusers$AllMotorVehicles)

view(roadusers)

pairs(~RoadCat+AADFYear_norm+BusesCoaches_norm+AllHGVs_norm
      +CarsTaxis_norm+Motorcycles_norm, data = roadusers,
      main = "Scatterplot Matrix")

#correlation plots
cor(roadusers$AllHGVs_norm, roadusers$Motorcycles_norm,  method = "pearson")
cor(roadusers$AllHGVs_norm, roadusers$Motorcycles_norm,  method = "spearman")

plot(roadusers$AllHGVs_norm, roadusers$Motorcycles_norm,
     main = "Correlation Plot of Normalised HGVs against Motorcycles",
     xlab = "All HGVs normalised", 
     ylab = "Motorcycles normalised",
     pch = 19, col = 1:2, frame = TRUE, font.main= 1)
legend("top", legend = paste("Vehicle Class", 1:2), col = 1:2, pch = 19, bty = "n")

cor(roadusers$CarsTaxis_norm, roadusers$Motorcycles_norm, method = "pearson")
cor(roadusers$CarsTaxis_norm, roadusers$Motorcycles_norm, method = "spearman")

plot(roadusers$CarsTaxis_norm, roadusers$Motorcycles_norm,
     main = "Correlation Plot of Normalised Cars/Taxis against Motorcycles",
     xlab = "Cars/Taxis normalised", 
     ylab = "Motorcycles normalised",
     pch = 19, col = 1:2, frame = TRUE, font.main= 1)
legend("topleft", legend = paste("Vehicle Class", 1:2), col = 1:2, pch = 19, bty = "n")


cor(roadusers$RoadCat, roadusers$Motorcycles_norm, method = "pearson")
cor(roadusers$RoadCat, roadusers$Motorcycles_norm, method = "spearman")

plot(roadusers$RoadCat, roadusers$Motorcycles_norm,
     main = "Correlation Plot of Normalised Motorcycles against Road Category", 
     xlab = "RoadCat normalised", 
     ylab = "Motorcycles normalised",
     pch = 19, frame = TRUE, font.main= 1)

# Elbow method kmeans HGVs + Cars
hgvcar <- matrix(roadusers$AADFYear+roadusers$AllHGVs_norm
                   +roadusers$CarsTaxis_norm, nrow=700, ncol=3, byrow=TRUE)

fviz_nbclust(hgvcar, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) + 
  labs(subtitle = "Elbow method kmeans hgv + car")

# Elbow method kmeans HGVs + Bus
hgvbus <- matrix(roadusers$AADFYear+roadusers$AllHGVs_norm
                 +roadusers$BusesCoaches_norm, nrow=700, ncol=3, byrow=TRUE)

fviz_nbclust(hgvbus, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) + 
  labs(subtitle = "Elbow method kmeans hgv + bus")

# Elbow method kmeans Cars + Buses
carbus <- matrix(roadusers$AADFYear+roadusers$CarsTaxis_norm
                   +roadusers$BusesCoaches_norm, nrow=700, ncol=3, byrow=TRUE)

fviz_nbclust(carbus, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) + 
  labs(subtitle = "Elbow method kmeans car + bus")

## Elbow method kmeans HGVs + Motorcycles
hgvmbike <- matrix(roadusers$AADFYear+roadusers$AllHGVs_norm+
                     roadusers$Motorcycles_norm, nrow=700, ncol=3, byrow=TRUE)

fviz_nbclust(hgvmbike, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) + 
  labs(subtitle = "Elbow method kmeans hgv + motorcycles")

#Elbow method kmeans BusesCoaches + RoadCat
bus <- matrix(roadusers$BusesCoaches_norm+
                      roadusers$RoadCat, nrow=700, ncol=2, byrow=TRUE)

fviz_nbclust(bus, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) + 
  labs(subtitle = "Elbow method kmeans bus + Road Catergory Variable")

#Kmeans test AllHGVs and road category
cl <- kmeans(roadusers$RoadCat + roadusers$AllHGVs_norm, 5, nstart = 70) 
plot(roadusers$RoadCat + roadusers$AllHGVs_norm, col = cl$cluster)

#car vs road category
cl2 <- kmeans(roadusers$RoadCat+roadusers$CarsTaxis_norm, 5, nstart = 70) 
plot(roadusers$RoadCat+roadusers$CarsTaxis_norm, col = cl2$cluster)

# motorcycles vs road category
cl3 <- kmeans(roadusers$RoadCat
              +roadusers$Motorcycles_norm, 5, nstart = 70) 
plot(roadusers$RoadCat
     +roadusers$Motorcycles_norm, col = cl3$cluster)

#buses and coaches vs road category
cl4 <- kmeans(roadusers$RoadCat
              +roadusers$BusesCoaches_norm, 5, nstart = 70) 

plot(roadusers$RoadCat
     +roadusers$BusesCoaches_norm, col = cl4$cluster,
     main = "Cluster plot of Buses and Coaches by Road Category", 
     xlab = "Dataset Row", 
     ylab = "Road Category",
     font.main= 1)

################

#fitting the linear model or Support Vector Modelling
linear_modelHGV <- lm(AllMotorVehicles ~ AllHGVs, data = roadusers)
linear_modelcar <- lm(AllMotorVehicles ~ CarsTaxis, data = roadusers)

lm_All <- lm(RoadCat ~ AllMotorVehicles, data = roadusers)
svm_All <- svm(RoadCat ~ AllMotorVehicles, data = roadusers)
lm_Bus <- lm(RoadCat ~ BusesCoaches, data = roadusers)
svm_Bus <- svm(RoadCat ~ BusesCoaches, data = roadusers)
lm_Car <- lm(RoadCat ~ CarsTaxis, data = roadusers)
svm_Car <- svm(RoadCat ~ CarsTaxis, data = roadusers)
lm_HGV <- lm(RoadCat ~ AllHGVs, data = roadusers)
svm_HGV <- svm(RoadCat ~ AllHGVs, data = roadusers)
lm_mcycle <- lm(RoadCat ~ Motorcycles, data = roadusers)
svm_mcycle <- svm(RoadCat ~ Motorcycles, data = roadusers)


##########

#linear model summaries
summary(linear_modelcar)
summary(linear_modelHGV)
summary(lm_All)
summary(lm_Bus)
summary(lm_Car)
summary(lm_HGV)
summary(lm_mcycle)


#######
#plots
plot(linear_modelcar)
plot(linear_modelHGV)
plot(lm_All)
plot(lm_Bus)
plot(lm_Car)
plot(lm_HGV)
plot(lm_mcycle)


#make data frames
rothRU <- data.frame(rothroadusers$AllHGVs,
                   rothroadusers$CarsTaxis,
                   rothroadusers$Motorcycles,
                   rothroadusers$BusesCoaches,
                   rothroadusers$AllMotorVehicles)
                

#predicts the future values for cars and taxis
predictCars <- predict(lm_Car, newdata = rothRU, interval= "prediction")
predictCarssvm <- predict(svm_Car, newdata = rothRU, interval= "prediction")
predictCars2 <- predict(linear_modelcar, newdata = rothRU, interval = "prediction")

RMSE(rothroadusers$CarsTaxis, predictCars)
RMSE(rothroadusers$CarsTaxis, predictCarssvm)
RMSE(rothroadusers$CarsTaxis, predictCars2)

#predict the future values for All HGVs
predictHGV <- predict(lm_HGV, newdata = rothRU, interval= "prediction")
predictHGVsvm <- predict(svm_HGV, newdata = rothRU, interval= "prediction")
predictHGV2 <- predict(linear_modelHGV, newdata = rothRU, interval = "prediction")

RMSE(rothroadusers$AllHGVs, predictHGV)
RMSE(rothroadusers$AllHGVs, predictHGVsvm)
RMSE(rothroadusers$AllHGVs, predictHGV2)

#predict the future values for Bus and Coach
predictBus <- predict(lm_Bus, newdata = rothRU, interval= "prediction")
predictBussvm <- predict(svm_Bus, newdata = rothRU, interval= "prediction")

RMSE(rothroadusers$BusesCoaches, predictBus)
RMSE(rothroadusers$BusesCoaches, predictBussvm)

#predict the future values for Motorcycles
predictMcycle <- predict(lm_mcycle, newdata = rothRU, interval= "prediction")
predictMcyclesvm <- predict(svm_mcycle, newdata = rothRU, interval= "prediction")

RMSE(rothroadusers$Motorcycles, predictMcycle)
RMSE(rothroadusers$Motorcycles, predictMcyclesvm)
#predict the future values for All Motor vehicles

predictAllMVs <- predict(lm_All, newdata = rothRU, interval= "prediction")
predictAllMVssvm <- predict(svm_All, newdata = rothRU, interval= "prediction")

RMSE(rothroadusers$AllMotorVehicles, predictAllMVs)
RMSE(rothroadusers$AllMotorVehicles, predictAllMVssvm)

########
#rmse gathered

RMSE(rothroadusers$CarsTaxis, predictCars)
RMSE(rothroadusers$BusesCoaches, predictBus)
RMSE(rothroadusers$AllHGVs, predictHGV)
RMSE(rothroadusers$Motorcycles, predictMcycle)
RMSE(rothroadusers$AllMotorVehicles, predictAllMVs)

RMSE(rothroadusers$CarsTaxis, predictCars2)
RMSE(rothroadusers$AllHGVs, predictHGV2)

RMSE(rothroadusers$CarsTaxis, predictCarssvm)
RMSE(rothroadusers$BusesCoaches, predictBussvm)
RMSE(rothroadusers$AllHGVs, predictHGVsvm)
RMSE(rothroadusers$Motorcycles, predictMcyclesvm)
RMSE(rothroadusers$AllMotorVehicles, predictAllMVssvm)

###########
RMS1 <- data.frame(name = c("RMSE Cars",
                  "RMSE Bus",
                  "RMSE HGVs",
                  "RMSE Mcycle",
                  "RMSE AllMVs"),
                  value=c(28374.22, 271.3043, 5089.934, 145.6151, 38245.03)
                )
          
ggplot(RMS1, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")

RMS2 <- data.frame(name = c("RMSE Cars",
                            "RMSE HGVs"),
                   value=c(12062.01, 28566.44)
)

ggplot(RMS2, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")


RMS3 <- data.frame(name = c("RMSE Cars",
                            "RMSE Bus",
                            "RMSE HGVs",
                            "RMSE Mcycle",
                            "RMSE AllMVs"),
                   value=c(23638, 447.6222, 2748.621, 474.1999, 30185.16)
)

ggplot(RMS3, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")

##########

dev.off()
#plots

ggplot(lm_All,aes(x = RoadCat, y = AllMotorVehicles),ylim = c(0,700),
       xlim = c(1,5),) + geom_point() +geom_smooth(method = "lm")


ggplot(lm_Bus,aes(x = RoadCat, y = BusesCoaches)) + geom_point() +geom_smooth(method = "lm")

ggplot(lm_Car,aes(x = RoadCat, y = CarsTaxis)) + geom_point() +geom_smooth(method = "lm")

ggplot(lm_HGV,aes(x = RoadCat, y = AllHGVs)) + geom_point() +geom_smooth(method = "lm")

ggplot(lm_mcycle,aes(x = RoadCat, y = Motorcycles)) + geom_point() +geom_smooth(method = "lm")


###############

library(NCmisc)

l<-list.functions.in.file("Yorkshire Assignment.R")

summary(l)