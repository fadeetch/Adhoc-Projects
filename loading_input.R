rm(list = ls())

setwd("C:/Users/dfadeeff/Desktop/Catalog/Analyses/Ad_hoc/Rentral_curve/Rcode/")

library(data.table)
library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)

#Load datasets
MOS <- read.table("C:/Users/dfadeeff/Desktop/Catalog/Analyses/Ad_hoc/Rentral_curve/Rcode/data/input/master_of_sex_dirty.txt", quote = "", sep = "\t", header = T)
HOC <- read.table("C:/Users/dfadeeff/Desktop/Catalog/Analyses/Ad_hoc/Rentral_curve/Rcode/data/input/house_of_cards_S1_dirty.txt", quote = "", sep = "\t", header = T)
BBT <- read.table("C:/Users/dfadeeff/Desktop/Catalog/Analyses/Ad_hoc/Rentral_curve/Rcode/data/input/big_bang_theory_S10_dirty.txt", quote = "", sep = "\t", header = T)
PB <- read.table("C:/Users/dfadeeff/Desktop/Catalog/Analyses/Ad_hoc/Rentral_curve/Rcode/data/input/prison_break_S4_dirty.txt", quote = "", sep = "\t", header = T)



#FEF for House of Cards and Master of Sex

#Create a function to do all the preprocessing
preprocess <- function(x)
  { 
  #Convert to data table
  x <- data.table(x)
  
  #Move from long to wide format, i.e. unique users as rows vs episodes as cols
  d <- dcast(x, encrypted_customer_id ~ episode_number, value.var = "episode_number")
  
  #To replace numbers with 0 and 1s, first convert to matrix
  m <- as.matrix(d[0:nrow(d),2:ncol(d)])
  #Then replace
  m[m>0] <- 1
  
  #Convert to data.frame
  num_set <- as.data.frame(m)
  
  df <- num_set 
  
  #Create row Sum column
  num_set$rowSum <- rowSums(num_set)
  
  #Most importantly, build cumulaltive distribution
  cum_dist <- as.data.frame(sapply(seq_along(df), function(x)  sum(rowSums(df[1:x]) == x)))
  names(cum_dist) <- 'cum_dist'
  
  #Provide % for each step
  cum_dist$percentage <- tail(cum_dist$cum_dist, n = 1) / cum_dist$cum_dist 
  cum_dist$index <- seq(1, nrow(cum_dist))
  
  #Return output
  return(cum_dist)
  }

#Apply function to datasets
master_of_sex <- preprocess(MOS)
house_of_cards <- preprocess(HOC)
big_bang_theory <- preprocess(BBT)
prison_break <- preprocess(PB)


#Prepare df with all the datasets
final_df <- merge(x = master_of_sex[c("percentage","index")], y = house_of_cards[c("percentage","index")], by = 'index', all = TRUE)
final_df <- merge(x = final_df, y = big_bang_theory[c("percentage","index")], by = "index", all = TRUE)
final_df <- merge(x = final_df, y = prison_break[c("percentage","index")], by = "index", all = TRUE)
names(final_df) <- c("index", "master_of_sex","house_of_cards","big_bang_theory","prison_break")

#Melt that dataframe
final_df_melted <- melt(final_df, id = 'index')

#Visualize by one
#ggplot(data = master_of_sex, aes(x = seq(1,length(cum_dist)), y = percentage)) + geom_line() + geom_point()

ggplot(final_df_melted, aes(x = index, y = value, colour = variable)) + 
  geom_line() + geom_point() + 
  ylab(label="Cum dist") + 
  xlab("episodes") + 
  scale_colour_manual(values=c("grey", "blue","green","red")) + ggtitle("What`s % of those who watched N episodes also watched entire series ")
