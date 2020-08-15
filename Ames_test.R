###########################All the Same Craziness with Test Data##############################

#read in your test set
test_df = read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)

#check missing values 
colSums(is.na(test_df) == TRUE)

#Missingness Imputation and Relabeling
#Replace NA's in LotFrontage with 0's
#https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/97087
test_df$LotFrontage[is.na(test_df$LotFrontage)] = 0

#replace NA's in GarageYrBlt with the year the home was built
test_df$GarageYrBlt[which(is.na(test_df$GarageYrBlt) == TRUE)] = test_df$YearBuilt[which(is.na(test_df$GarageYrBlt) == TRUE)]

#Replace NA's with None in Categorical Values where NA indicates that the feature(wc?) is missing (no basement, etc)
test_df[c("Alley","BsmtQual", "BsmtCond", 
          "BsmtExposure", "BsmtFinType1",
          "BsmtFinType2", "KitchenQual", "FireplaceQu",
          "GarageType", "GarageFinish",
          "GarageQual", "GarageCond",
          "PoolQC", "Fence", "MiscFeature")][is.na(test_df[c("Alley","BsmtQual", "BsmtCond", 
                                                             "BsmtExposure", "BsmtFinType1",
                                                             "BsmtFinType2","KitchenQual", "FireplaceQu",
                                                             "GarageType", "GarageFinish",
                                                             "GarageQual", "GarageCond",
                                                             "PoolQC", "Fence", "MiscFeature")])] <- "None"
#Assign most common value to NA in electrical
test_df %>%
  ggplot(aes(x = Electrical)) + geom_bar()

test_df$Electrical[is.na(test_df$Electrical)] = "SBrkr"

#Rescore nominal categorical values as numerical
must_convert = c("ExterQual","ExterCond", "BsmtQual", "BsmtCond",
                 "BsmtExposure", "BsmtFinType1",
                 "BsmtFinType2", "FireplaceQu", "GarageQual",
                 "GarageCond", "PoolQC")
test_df[must_convert][test_df[must_convert] == "None"] <- "0"
test_df[must_convert][test_df[must_convert] == "Po"] <- "1"
test_df[must_convert][test_df[must_convert] == "Fa"] <- "2"
test_df[must_convert][test_df[must_convert] == "TA"] <- "3"
test_df[must_convert][test_df[must_convert] == "Gd"] <- "4"
test_df[must_convert][test_df[must_convert] == "Ex"] <- "5"

#Visualize MasVnrArea. Data is highly skewed, so impute median
#ggplot(data = test_df,aes(x = MasVnrArea)) + geom_histogram() + xlim(0,500)
test_df$MasVnrArea[is.na(test_df$MasVnrArea)] = median(test_df$MasVnrArea, na.rm = TRUE)

#generate random distribution for MasVnrType
set.seed(0)
test_df$MasVnrType[is.na(test_df$MasVnrType)] = as.integer(runif(sum(is.na(test_df$MasVnrType)==TRUE), min=0, max = 5))
test_df$MasVnrType[grep(0, test_df$MasVnrType)] = "BrkCmn"
test_df$MasVnrType[grep(1, test_df$MasVnrType)] = "BrkFace"
test_df$MasVnrType[grep(2, test_df$MasVnrType)] = "CBlock"
test_df$MasVnrType[grep(3, test_df$MasVnrType)] = "None"
test_df$MasVnrType[grep(4, test_df$MasVnrType)] = "Stone"



cors = cors %>%
  arrange(SalePrice, desc())

#Output train data for Advanced Models ()
write.csv(test_df, file = 'post_process_test.csv')