library(dplyr)
library(ggplot2)
library(modelr)
library(EnvStats)
library(corrplot)
library(shiny)
library(car)
#set your wd
setwd('C:/Users/nates/Documents/Projects/KaggleAmesIowaCompetition')

#read in your training set
train_df = read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)

#check missing values 
colSums(is.na(train_df) == TRUE)

#Missingness Imputation and Relabeling
#Replace NA's in LotFrontage with 0's
#https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/97087
train_df$LotFrontage[is.na(train_df$LotFrontage)] = 0

#replace NA's in GarageYrBlt with the year the home was built
train_df$GarageYrBlt[which(is.na(train_df$GarageYrBlt) == TRUE)] = train_df$YearBuilt[which(is.na(train_df$GarageYrBlt) == TRUE)]

#Replace NA's with None in Categorical Values where NA indicates that the feature(wc?) is missing (no basement, etc)
train_df[c("Alley","BsmtQual", "BsmtCond", 
           "BsmtExposure", "BsmtFinType1",
           "BsmtFinType2","KitchenQual", "FireplaceQu",
           "GarageType", "GarageFinish",
           "GarageQual", "GarageCond",
           "PoolQC", "Fence", "MiscFeature")][is.na(train_df[c("Alley","BsmtQual", "BsmtCond", 
                 "BsmtExposure", "BsmtFinType1",
                 "BsmtFinType2","KitchenQual", "FireplaceQu",
                 "GarageType", "GarageFinish",
                 "GarageQual", "GarageCond",
                 "PoolQC", "Fence", "MiscFeature")])] <- "None"
#Assign most common value to NA in electrical
train_df %>%
  ggplot(aes(x = Electrical)) + geom_bar()

train_df$Electrical[is.na(train_df$Electrical)] = "SBrkr"

#Rescore nominal categorical values as numerical
must_convert = c("ExterQual","ExterCond", "BsmtQual", "BsmtCond",
                "BsmtExposure", "BsmtFinType1",
                "BsmtFinType2", "FireplaceQu", "GarageQual",
                "GarageCond", "PoolQC")
train_df[must_convert][train_df[must_convert] == "None"] <- "0"
train_df[must_convert][train_df[must_convert] == "Po"] <- "1"
train_df[must_convert][train_df[must_convert] == "Fa"] <- "2"
train_df[must_convert][train_df[must_convert] == "TA"] <- "3"
train_df[must_convert][train_df[must_convert] == "Gd"] <- "4"
train_df[must_convert][train_df[must_convert] == "Ex"] <- "5"

#Visualize MasVnrArea. Data is highly skewed, so impute median
#ggplot(data = train_df,aes(x = MasVnrArea)) + geom_histogram() + xlim(0,500)
train_df$MasVnrArea[is.na(train_df$MasVnrArea)] = median(train_df$MasVnrArea, na.rm = TRUE)

#generate random distribution for MasVnrType
set.seed(0)
train_df$MasVnrType[is.na(train_df$MasVnrType)] = as.integer(runif(sum(is.na(train_df$MasVnrType)==TRUE), min=0, max = 5))
train_df$MasVnrType[grep(0, train_df$MasVnrType)] = "BrkCmn"
train_df$MasVnrType[grep(1, train_df$MasVnrType)] = "BrkFace"
train_df$MasVnrType[grep(2, train_df$MasVnrType)] = "CBlock"
train_df$MasVnrType[grep(3, train_df$MasVnrType)] = "None"
train_df$MasVnrType[grep(4, train_df$MasVnrType)] = "Stone"

#Remove Outliers from GrLvArea
train_df = train_df%>%
  filter(GrLivArea < 3000)



#Is sale month related to sale price?Not really...
train_df %>%
  ggplot(aes(x = MoSold)) + geom_bar()

train_df %>%
  ggplot(aes(x = MoSold, y = SalePrice)) + geom_smooth(method = "loess") +
  geom_point() 

#mean price over time
train_df %>%
  ggplot(aes(x = YrSold, y = SalePrice)) + geom_smooth(method = "loess") +
  geom_point(position = "jitter", aes(color = Neighborhood)) +
  facet_wrap( ~ Neighborhood)

train_df %>%
  ggplot(aes(x = SalePrice)) + geom_histogram() + 
  facet_wrap( ~ Neighborhood)


#Yearbuilt has an interesting relationship with GarageYrBlt.
#Their mean difference is 5.24 years
#Built, when you eliminate cases in which YearBuilt == GarageYrBlt
#Their mean difference jumps to ~ 26 years
train_df %>%
  ggplot(aes(x = GarageYrBlt - YearBuilt)) + geom_histogram() + 
  xlim(1,120)

train_df %>%
  filter(GarageYrBlt != YearBuilt) %>%
  summarise(mean(GarageYrBlt - YearBuilt))


#Colinearity Check
cors = data.frame(train_df %>%
  select_if(is.numeric)%>%
  #select(LotFrontage, LotArea, OverallQual, OverallCond, YearBuilt, YearRemodAdd, 
      #   MasVnrArea, GarageArea, SalePrice) %>%
  cor()) 

train_df %>%
  select_if(is.numeric) %>%
  influencePlot()

cors = cors %>%
  arrange(SalePrice, desc()) 

corrplot(train_df %>%
           select_if(is.numeric)%>%
           cor()>.7, order="hclust")

#Garage Area and Garage Cars are highly correlated
#Garage Cars looks a lot more normally distributed, so we'll try eliminating Garage Area
train_df%>%
  ggplot(aes(x = GarageCars)) + geom_histogram()

#Garage Cars and GarageYrBlt are also highly correlated (.6>)
#Garage cars still his a higher correlation with sale price, so we'll try removing GarageYrBlt
train_df %>%
  ggplot(aes(y= GarageCars, x = GarageYrBlt)) + 
  geom_point(position = "jitter") + 
  geom_smooth()

#Full Bath is correlated with GrLivArea (.6>), but not highly correlated w/ sale price
#It also has some logy bumps at around 1300 and 2300 SF
#We'll remove FullBath for now
train_df %>%
  ggplot(aes(x= GrLivArea, y = FullBath)) + 
  geom_point(position = "jitter") + 
  geom_smooth()

#GrLivArea and TotRmsAbvGrd are pretty correlated.
#GrLivArea is more highly correlated with SalePrice
#BUT, it's got more skew
#So we'll eliminate TotRmsAbvGrd AND we'll get rid of some outliers from GrLivArea
train_df %>%
  ggplot(aes(x= GrLivArea)) + 
  geom_histogram()

train_df %>%
  ggplot(aes(x= GrLivArea, y = TotRmsAbvGrd)) + 
  geom_point(position = "jitter") + 
  geom_smooth()

#Also remove X2ndFlrSF and BsmtFullBath for similar reasons
train_df %>%
  ggplot(aes(x= BsmtFullBath)) + 
  geom_histogram()

train_df %>%
  ggplot(aes(x= BsmtFinSF1, y =BsmtFullBath )) + 
  geom_point(position = "jitter") + 
  geom_smooth()

#
train_df %>%
  ggplot(aes(x = Neighborhood, y = SalePrice)) + geom_boxplot()

train_df%>%
  ggplot(aes(x= Neighborhood)) + geom_bar() #+ ylim(30, 300)
#Output train data for Advanced Models ()
write.csv(train_df, file = 'post_process_train.csv')







#co-linearity check


ggplot(data = train_df, aes(x = boxcox(SalePrice))) + geom_histogram()

str(boxcox(train_df$SalePrice))
train_df %>%
  filter(YearBuilt != YearRemodAdd) %>%
  mutate(YearDiff = YearBuilt -  YearRemodAdd) %>%
  ggplot(aes(x = YearDiff, y = OverallQual)) + geom_point() #+ xlim(-20,0)
  #summarise(yearDiff = mean(YearBuilt -  YearRemodAdd))