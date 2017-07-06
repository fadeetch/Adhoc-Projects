rm(list = ls())
getwd()


#Load libraries
library(ggplot2)
library(plyr)
library(data.table)
library(tseries)


#Load data, providing relative path
target1 <- read.table("./lalaland.txt", sep = "\t", quote = "", header = T)
peer1 <- read.table("./revenant.txt", sep = "\t", quote = "", header = T)

#Convert to date
target1$date <- as.Date(target1$date, format = "%Y-%m-%d")
peer1$date <- as.Date(peer1$date, format = "%Y-%m-%d")

#Subset HD EST only
est_target1 <- target1[ which(target1$asin %in% c("B01MZ82VFO")),]
est_peer1 <- peer1[which(peer1$asin %in% c("B01AN69UP6")),]

#Release dates
#Lala land 2017-05-16
#Revenant 2016-05-05
#Passengers 2017-05-08
#Gravity 2014-02-07

cutoff_start_target1 <- as.Date("2017-05-16")
cutoff_start_peer1 <- as.Date("2016-05-05")

#Subset by date to match release date
target_time_HD <- est_target1[est_target1$date >= cutoff_start_target1 & est_target1$date <= cutoff_start_target1+13, ]
peer_time_HD <- est_peer1[est_peer1$date >= cutoff_start_peer1 & est_peer1$date <= cutoff_start_peer1+13, ]
merged <- as.data.frame(cbind(peer_time_HD$units, target_time_HD$units))
names(merged) <- c("Revenant","LaLaLand")
merged$index <- seq(1,nrow(merged))

#Visualize the stuff
ggplot(data = merged, aes(merged$index)) +
  geom_line(aes(y = merged$Revenant, colour = "peer")) + 
  geom_line(aes(y = merged$LaLaLand, colour = "target")) + ggtitle("Unit sold during first 2 weeks")

#Define 14 day period and calculate the average number of units sold per day for peer and target
sum(merged$Revenant / nrow(merged)) #units sold for Revenant
sum(merged$LaLaLand / nrow(merged)) #units sold for LaLaLand

#HD est costs for LaLaLand 13.99, Revenant costs 6.98
sum(merged$Revenant / nrow(merged)) * 7 * 3
sum(merged$LaLaLand / nrow(merged)) * 14 * 3



#Take another example
#Load data, providing relative path
target2 <- read.table("./passengers.txt", sep = "\t", quote = "", header = T)
peer2 <- read.table("./interstellar.txt", sep = "\t", quote = "", header = T)


#Convert to date
target2$date <- as.Date(target2$date, format = "%Y-%m-%d")
peer2$date <- as.Date(peer2$date, format = "%Y-%m-%d")

#Subset HD EST only
est_target2 <- target2[ which(target2$asin %in% c("B01N30KZNI")),]
est_peer2 <- peer2[which(peer2$asin %in% c("B00US3KAPE")),]



cutoff_start_target2 <- as.Date("2017-05-08")
cutoff_start_peer2 <- as.Date("2015-05-17")

#Subset by date to match release date
target_time_HD2 <- est_target2[est_target2$date >= cutoff_start_target2 & est_target2$date <= cutoff_start_target2+13, ]
peer_time_HD2 <- est_peer2[est_peer2$date >= cutoff_start_peer2 & est_peer2$date <= cutoff_start_peer2+13, ]
merged2 <- as.data.frame(cbind(peer_time_HD2$units, target_time_HD2$units))
names(merged2) <- c("Passengers","Interstellar")
merged2$index <- seq(1,nrow(merged2))

#Visualize the stuff
ggplot(data = merged2, aes(merged2$index)) +
  geom_line(aes(y = merged2$Interstellar, colour = "peer")) + 
  geom_line(aes(y = merged2$Passengers, colour = "target"))

#Define 14 day period and calculate the average number of units sold per day for peer and target
sum(merged2$Interstellar / nrow(merged2)) #units sold for Revenant
sum(merged2$Passengers / nrow(merged2)) #units sold for LaLaLand

#HD est costs for LaLaLand 16.99, Interstellar costs 9.99
sum(merged2$Interstellar / nrow(merged2)) * 10
sum(merged2$Passengers / nrow(merged2)) * 17 
