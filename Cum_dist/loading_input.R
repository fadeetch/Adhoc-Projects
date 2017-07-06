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
GA <- read.table("C:/Users/dfadeeff/Desktop/Catalog/Analyses/Ad_hoc/Rentral_curve/Rcode/data/input/greys_anatomy_S2_dirty.txt", quote = "", sep = "\t", header = T)
BL <- read.table("C:/Users/dfadeeff/Desktop/Catalog/Analyses/Ad_hoc/Rentral_curve/Rcode/data/input/blacklist_S1_dirty.txt", quote = "", sep = "\t", header = T)
GOT <- read.table("C:/Users/dfadeeff/Desktop/Catalog/Analyses/Ad_hoc/Rentral_curve/Rcode/data/input/game_of_thrones_S4_dirty.txt", quote = "", sep = "\t", header = T)


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
grey_anatomy <- preprocess(GA)
blacklist <- preprocess(BL)
game_of_thrones <- preprocess(GOT)

#Prepare df with all the datasets
final_df <- merge(x = master_of_sex[c("percentage","index")], y = house_of_cards[c("percentage","index")], by = 'index', all = TRUE)
final_df <- merge(x = final_df, y = big_bang_theory[c("percentage","index")], by = "index", all = TRUE)
final_df <- merge(x = final_df, y = prison_break[c("percentage","index")], by = "index", all = TRUE)
final_df <- merge(x = final_df, y = grey_anatomy[c("percentage","index")], by = "index", all = TRUE)
final_df <- merge(x = final_df, y = blacklist[c("percentage","index")], by = "index", all = TRUE)
final_df <- merge(x = final_df, y = game_of_thrones[c("percentage","index")], by = "index", all = TRUE)

names(final_df) <- c("index", "master_of_sex_S1_FEF","house_of_cards_S1_FEF","big_bang_theory_S10","prison_break_S4", "grey_anatomy_S2", "blacklist_S1_FEF", "game_of_thrones_S4")

#Melt that dataframe
final_df_melted <- melt(final_df, id = 'index')

#Visualize by one
#ggplot(data = master_of_sex, aes(x = seq(1,length(cum_dist)), y = percentage)) + geom_line() + geom_point()

ggplot(final_df_melted, aes(x = index, y = value, colour = variable)) + 
  geom_line() + geom_point() + 
  ylab(label="Cum dist") + 
  xlab("episodes") + 
  scale_colour_manual(values=c("grey", "blue","green","red","purple", "navyblue", "cyan")) + ggtitle("What`s % of those who watched N episodes also watched entire series ")




#Check FEF
#Convert example dataframe to datatable
BL <- data.table(BL)

#Separate into two dataframes creating pattern
ptn <- "*FEF*" 

#Get those who used FEF

BL_customers_FEF <- BL[grep(ptn, BL$upper),]
BL_FEF <- BL[BL$encrypted_customer_id %in% unique(BL_customers_FEF$encrypted_customer_id),]
BL_FEF <- preprocess(BL_FEF)
BL_nonFEF <- BL[!(BL$encrypted_customer_id %in% unique(BL_customers_FEF$encrypted_customer_id)), ]
BL_nonFEF <- preprocess(BL_nonFEF)
df_forFEF_split <- merge(x = BL_FEF[c("percentage","index")], y = BL_nonFEF[c("percentage","index")], by = "index", all = TRUE)
names(df_forFEF_split) <- c("index","used_FEF","not_used_FEF")


#Do melt
df_forFEF_split_melted <- melt(df_forFEF_split, id.vars = "index")

ggplot(data = df_forFEF_split_melted, aes(x = index, y = value, colour = variable )) + 
         geom_line() + geom_point() +
         ylab(label = "cumulative distribution") + 
         xlab(label = "episodes") + 
         scale_color_manual(values = c("green","navy"))  + ggtitle("Those who used FEF vs those who did not")

HOC <- data.table(HOC)

HOC_customers_FEF <- HOC[grep(ptn,HOC$upper),]
HOC_FEF <- HOC[HOC$encrypted_customer_id %in% unique(HOC_customers_FEF$encrypted_customer_id),]
HOC_FEF <- preprocess(HOC_FEF)
HOC_nonFEF <- HOC[!(HOC$encrypted_customer_id %in% unique(HOC_customers_FEF$encrypted_customer_id)),]
HOC_nonFEF <- preprocess(HOC_nonFEF)
df_forFEF_split2 <- merge(x = HOC_FEF[c("percentage","index")], y = HOC_nonFEF[c("percentage","index")], by = "index", all = TRUE)
names(df_forFEF_split2) <- c("index","used_FEF", "not_used_FEF")

df_forFEF_split_melted2 <- melt(df_forFEF_split2, id.vars = "index")

ggplot(data = df_forFEF_split_melted2, aes(x = index, y = value, colour = variable)) + 
  geom_line() + geom_point() + 
  ylab(label = "cumulative distribution") + 
  xlab(label = "episodes") + 
  scale_color_manual(values = c("green", "navy")) + ggtitle("Those who used FEF vs those who did not ")