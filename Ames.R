library(dplyr)
library(ggplot2)
library(modelr)
library(EnvStats)
#set your wd
setwd('C:/Users/nates/Documents/Projects/KaggleAmesIowaCompetition')

#read in your training set
train_df = read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)

#check missing values 
colSums(is.na(train_df) == TRUE)

#missing values in LotFrontage, MasVnrType, MasVnrArea, Electrical
#Replace NA's in LotFrontage with 0's
#https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/97087
train_df$LotFrontage[is.na(train_df$LotFrontage)] = 0

#impute mean in MasVnrArea
train_df$MasVnrArea[is.na(train_df$MasVnrArea)] = mean(train_df$MasVnrArea, na.rm = TRUE)

#generate random distribution for MasVnrType
set.seed(0)
train_df$MasVnrType[is.na(train_df$MasVnrType)] = as.integer(runif(sum(is.na(train_df$MasVnrType)==TRUE), min=0, max = 5))
train_df$MasVnrType[grep(0, train_df$MasVnrType)] = "BrkCmn"
train_df$MasVnrType[grep(1, train_df$MasVnrType)] = "BrkFace"
train_df$MasVnrType[grep(2, train_df$MasVnrType)] = "CBlock"
train_df$MasVnrType[grep(3, train_df$MasVnrType)] = "None"
train_df$MasVnrType[grep(4, train_df$MasVnrType)] = "Stone"

#create numerical score for ExterQual value. Yes, RStudio's autoformat is a little ideosyncratic
train_df = train_df %>% mutate(ExterQualScore = ifelse(ExterQual == "Po",
                                            0,
                                            ifelse(
                                              ExterQual == "Fa",
                                              1,
                                              ifelse(ExterQual == "TA",
                                                     2,
                                                     ifelse(
                                                       ExterQual == "Gd",
                                                       3,
                                                       ifelse(ExterQual == "Ex",
                                                              4,
                                                              ExterQual)
                                                     ))
                                            )))

#create numerical score for BsmtQual value. Yes, RStudio's autoformat is a little ideosyncratic
train_df = train_df %>% mutate(ExterCondScore = ifelse(ExterCond == "Po",
                                                       0,
                                                       ifelse(
                                                         ExterCond == "Fa",
                                                         1,
                                                         ifelse(ExterCond == "TA",
                                                                2,
                                                                ifelse(
                                                                  ExterCond == "Gd",
                                                                  3,
                                                                  ifelse(ExterCond == "Ex",
                                                                         4,
                                                                         ExterCond)
                                                                ))
                                                       )))

#create numerical score for BsmtQual value. Yes, RStudio's autoformat is a little ideosyncratic
train_df = train_df %>% mutate(BsmtQualScore = ifelse(BsmtQual == "Po",
                                                       0,
                                                       ifelse(
                                                         BsmtQual == "Fa",
                                                         1,
                                                         ifelse(BsmtQual == "TA",
                                                                2,
                                                                ifelse(
                                                                  BsmtQual == "Gd",
                                                                  3,
                                                                  ifelse(BsmtQual == "Ex",
                                                                         4,
                                                                         BsmtQual)
                                                                ))
                                                       )))
train_df %>%
  ggplot(aes(x = SalePrice)) + geom_boxplot()

write.csv(train_df, file = 'post_process_train.csv')
#co-linearity check
train_df %>%
  select(LotFrontage, LotArea, OverallQual, OverallCond, YearBuilt, YearRemodAdd, 
         MasVnrArea, GarageArea, SalePrice) %>%
  #influencePlot()
  cor()

ggplot(data = train_df, aes(x = boxcox(SalePrice))) + geom_histogram()

str(boxcox(train_df$SalePrice))
train_df %>%
  filter(YearBuilt != YearRemodAdd) %>%
  mutate(YearDiff = YearBuilt -  YearRemodAdd) %>%
  ggplot(aes(x = YearDiff, y = OverallQual)) + geom_point() #+ xlim(-20,0)
  #summarise(yearDiff = mean(YearBuilt -  YearRemodAdd))