library(dplyr)
library(ggplot2)
library(modelr)
library(EnvStats)
library(corrplot)
library(shiny)
library(car)
library(plotly)
#set your wd
setwd('C:/Users/nates/Documents/Projects/KaggleAmesIowaCompetition')

#read in your training set
train_df = read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)

#check missing values 
missing_df = data.frame(colSums(is.na(train_df) == TRUE)/nrow(train_df), stringsAsFactors = FALSE)
colnames(missing_df) = c("Vars")
rownames = c(rownames(missing_df))
missing_df = cbind(missing_df,rownames)
missing_df %>%
  filter(Vars != 0)%>%
  ggplot(aes(x = reorder(rownames, -Vars) , y =Vars )) + 
  geom_bar(stat="identity")
#The columns that have a high proportion of missing variables seem
#to be ones where NA indicates 0, so we won't really drop them

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


#generate random distribution for MasVnrType
ggplot(data = train_df, aes(x= MasVnrType)) +geom_bar()
sum(is.na(train_df$MasVnrType))
set.seed(0)
train_df$MasVnrType[is.na(train_df$MasVnrType)] = as.integer(runif(sum(is.na(train_df$MasVnrType)==TRUE), min=0, max = 5))
train_df$MasVnrType[grep(0, train_df$MasVnrType)] = "BrkCmn"
train_df$MasVnrType[grep(1, train_df$MasVnrType)] = "BrkFace"
train_df$MasVnrType[grep(2, train_df$MasVnrType)] = "CBlock"
train_df$MasVnrType[grep(3, train_df$MasVnrType)] = "None"
train_df$MasVnrType[grep(4, train_df$MasVnrType)] = "Stone"

#Visualize MasVnrArea. Data is highly skewed, so impute median
ggplot(data = train_df,aes(x = MasVnrArea)) + geom_histogram() #+ xlim(0,500)
train_df$MasVnrArea[is.na(train_df$MasVnrArea)] = if_else(train_df$MasVnrType == 'None', 0, median(train_df$MasVnrArea, na.rm = TRUE))

#Remove odd cases where MasVnrType is listed as none, but MasVnrArea isn't zero
train_df %>%
  filter(MasVnrType == 'None', MasVnrArea !=0) %>%
  summarise(Id, MasVnrType, MasVnrArea)

train_df = train_df %>%
  filter(Id !=625, Id != 774, Id != 1231, Id!=1301, Id!=1335)

train_df = train_df %>%
  filter(Id!=186 & Id!=1183& Id!=1170 & Id!=692 & Neighborhood!= "Blueste" &
           Neighborhood!= "NPkVill" & Neighborhood != "Veenker") 

#Rescore nominal categorical values as numerical
must_convert = c("ExterQual","ExterCond", "BsmtQual", "BsmtCond",
                "BsmtFinType1", "HeatingQC",
                "BsmtFinType2", "FireplaceQu", "GarageQual",
                "GarageCond", "PoolQC", "KitchenQual")
train_df[must_convert][train_df[must_convert] == "None"] <- "0"
train_df[must_convert][train_df[must_convert] == "Po"] <- "1"
train_df[must_convert][train_df[must_convert] == "Fa"] <- "2"
train_df[must_convert][train_df[must_convert] == "TA"] <- "3"
train_df[must_convert][train_df[must_convert] == "Gd"] <- "4"
train_df[must_convert][train_df[must_convert] == "Ex"] <- "5"

#BsmtExposure has it's own nomenclature, apparently
train_df["BsmtExposure"][train_df["BsmtExposure"] == "None"] <- "0"
train_df["BsmtExposure"][train_df["BsmtExposure"] == "No"] <- "1"
train_df["BsmtExposure"][train_df["BsmtExposure"] == "Mn"] <- "2"
train_df["BsmtExposure"][train_df["BsmtExposure"] == "Av"] <- "3"
train_df["BsmtExposure"][train_df["BsmtExposure"] == "Gd"] <- "4"


#Outlier scrub

#OverallQual
train_df %>%
  ggplot(aes(x = OverallQual, y = SalePrice)) + geom_point(position = "jitter") 

#Remove Outliers from GrLvArea
train_df = train_df%>%
  filter(GrLivArea < 3000)

#GarageCars
train_df %>%
  ggplot(aes(x = GarageCars, y = SalePrice)) + geom_point(position = "jitter") 

#mean price over time by neighborhood
train_df %>%
  ggplot(aes(x = YrSold, y = SalePrice)) + geom_smooth(method = "loess") +
  geom_point(position = "jitter", aes(color = Neighborhood)) #+
# facet_wrap( ~ Neighborhood)

train_df %>%
  ggplot(aes(x = SalePrice)) + geom_histogram() + 
  facet_wrap( ~ Neighborhood)

#Removed outliers & neighborhoods that appear to be missing data (Veenker missing points for 2009/2010?)
#But found that doing so increased overfit dramatically in MLR, Lasso, and RandomForest
#Kept it to demonstrate knowledge level
train_df = train_df %>%
  filter(Id!=186 & Id!=1183& Id!=1170 & Id!=692 & Neighborhood!= "Blueste" &
           Neighborhood!= "NPkVill" & Neighborhood != "Veenker") 

#Removing outliers in saleprice
train_df = train_df%>%
  filter(SalePrice <= 480000)


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
  arrange(descSalePrice) 

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

#With the exception of BrkSide, Somerst, and maybe CollgeCr,
#each neighborhood basically has the same zoning
#Remove MSZoning for now since it has less variance than Neighborhood

train_df%>%
  ggplot(aes(y=MSZoning)) + geom_bar() #+ facet_wrap(~Neighborhood)

train_df%>%
  ggplot(aes(y=SalePrice, x= Neighborhood, col=MSZoning)) + geom_point(position = "jitter")


#
train_df %>%
  ggplot(aes(x = MSZoning, y = SalePrice)) + geom_boxplot()

#Is sale month related to sale price?Not really...
#This is kinda interesting since you tend to assume your home will sell better
#over the summer
train_df %>%
  ggplot(aes(x = MoSold)) + geom_bar()

train_df %>%
  ggplot(aes(x = MoSold, y = SalePrice)) + geom_smooth(method = "loess") +
  geom_point() 

#Yearbuilt has an interesting relationship with GarageYrBlt.
#Their mean difference is 5.24 years
#Built, when you eliminate cases in which YearBuilt == GarageYrBlt
#Their mean difference jumps to ~ 26 years
train_df %>%
  ggplot(aes(x = GarageYrBlt - YearBuilt)) + geom_histogram() + 
  xlim(1,120)

train_df %>%
  filter(GarageYrBlt != YearBuilt) %>%
  summarise(mean(GarageYrBlt - YearBuilt), mean(GarageYrBlt), mean(YearBuilt))

train_df = train_df %>%
  mutate(Last_Build = if_else(YearRemodAdd >YearBuilt, YearRemodAdd, YearBuilt))
plot(Last_Build, train_df$SalePrice)

#Well, worth a try anyway
cor(train_df$Last_Build, train_df$SalePrice)
cor(train_df$YearBuilt, train_df$SalePrice)
cor(train_df$YearRemodAdd, train_df$SalePrice)


#variance check (Abandoned this because Python does it for me)

train_df%>%
  ggplot(aes(x= Neighborhood)) + geom_bar() #+ ylim(30, 300)

variances = sapply(train_df, var)
which(variances <= 1)

#
Anova(train_df)

ggplot(data = train_df, aes(x = boxcox(SalePrice))) + geom_histogram()

str(boxcox(train_df$SalePrice))
train_df %>%
  filter(YearBuilt != YearRemodAdd) %>%
  mutate(YearDiff = YearBuilt -  YearRemodAdd) %>%
  ggplot(aes(x = YearDiff, y = OverallQual)) + geom_point() #+ xlim(-20,0)
  #summarise(yearDiff = mean(YearBuilt -  YearRemodAdd))



#Output train data for Advanced Models 
write.csv(train_df, file = 'post_process_train.csv')