# Data Preparation

# read the dataset
file_path = "E:/18 Data Analysis/02 Kaggle/01 House Price/train.csv"

# can't use '\', it's a special symbol. 
# file_path = "E:\18 Data Analysis\02 Kaggle\01 House Price\train.csv"

HousePriceDF_original <- read.csv(file = file_path, header = TRUE, sep = ',')

str(HousePriceDF_original)

#save the original data, use the backup to analyse
HousePriceDF <- HousePriceDF_original

print(HousePriceDF)
head(HousePriceDF)
colnames(HousePriceDF)
length(HousePriceDF)
attach(HousePriceDF)

# lots of data outier, means higher price
boxplot(SalePrice)

library(ggplot2)
# the price of WD type house is abnormal
ggplot(HousePriceDF, mapping=aes(x=SaleType, y=SalePrice)) + geom_boxplot()
rlang::last_error()
rlang::last_trace()

str(HousePriceDF)
#the price of 1Fam,TwinsE are abnormal
ggplot(HousePriceDF, aes(x=BldgType, y=SalePrice)) + geom_boxplot()
#same result with boxplot()
boxplot(SalePrice ~ BldgType, data = HousePriceDF,
        ylab = "price (USD)", las = 1)
#the price of 1.5Fin, 1Story, 2 Story, SFoyer, Slv1 are abnormal
ggplot(HousePriceDF, aes(x=HouseStyle, y=SalePrice)) + geom_boxplot()

# the price of garage type alse abnormal: Attchd, BuiltIn, Detchd
ggplot(HousePriceDF, aes(x=GarageType, y=SalePrice)) + geom_boxplot()

# the price of lotarea alse abnormal
ggplot(HousePriceDF, aes(x=mean(LotArea), y=SalePrice)) + geom_boxplot()

#lots of col are chr, can't pairs
var <- c(
  "LotFrontage",   "LotArea"      ,
  "Condition1"  ,  "Condition2"   ,
  "BldgType"   ,   "HouseStyle"  ,  "YearBuilt"    ,
  "YearRemodAdd"  ,
  "MasVnrType" ,   "MasVnrArea"    ,
  "BsmtQual"  ,    "BsmtCond" ,     "BsmtExposure" , "BsmtFinType1" , "BsmtFinSF1"   ,
  "BsmtFinType2",  "BsmtFinSF2" ,   "BsmtUnfSF"  ,   "TotalBsmtSF"  , "Heating"      ,
  "HeatingQC" ,    "CentralAir" ,   "Electrical",    "X1stFlrSF" ,    "X2ndFlrSF",    
  "LowQualFinSF"  ,
  "HalfBath" ,     "BedroomAbvGr",  "KitchenAbvGr" ,
  "Functional",    "Fireplaces",    "FireplaceQu",   "GarageType",    "GarageYrBlt"  ,
  "GarageFinish",  "GarageCars",    "GarageArea",    "GarageQual",    "GarageCond",   
  "PoolArea",      "PoolQC"      ,
  "MoSold",        "YrSold",        "SaleType",      "SaleCondition",
  "SalePrice")


#the whole numeric data fields saved as original var
var_org <- c("LotFrontage", "LotArea",
          "OverallQual","OverallCond",
          "YearBuilt","YearRemodAdd",
          "MasVnrArea","BsmtFinSF1",
          "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF",
          "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", "GrLivArea",
          "BsmtFullBath",  "BsmtHalfBath", "FullBath", 
          "HalfBath", "BedroomAbvGr" , "KitchenAbvGr", "TotRmsAbvGrd", 
          "Fireplaces", "GarageYrBlt", "GarageCars", "GarageYrBlt", "GarageCars",
          "GarageArea","WoodDeckSF", "OpenPorchSF",
          "EnclosedPorch", "X3SsnPorch",  "ScreenPorch" ,  "PoolArea",
          "MiscVal", "MoSold", "YrSold",
                    "SalePrice")
#seperate for 3 parts to analyse the relation with saleprice
var_1 <- c("LotFrontage", "LotArea",
             "OverallQual","OverallCond",
             "YearBuilt","YearRemodAdd",
             "MasVnrArea","BsmtFinSF1",
             "SalePrice")
pairs(HousePriceDF[var_1])

#saleprice relate to LotFrontage, LotArea, OveralQual, OveralCond, BsmtFinSF1)


var_2 <- c( "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF",
             "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", "GrLivArea",
             "BsmtFullBath",  "BsmtHalfBath", "FullBath", 
             "HalfBath", "BedroomAbvGr" , "KitchenAbvGr", "TotRmsAbvGrd", 
         
             "SalePrice")
pairs(HousePriceDF[var_2])
# price relate to "TotalBsmtSF","X1stFlrSF","LowQualFinSF","BsmtFullBath","BsmtHalfBath", "FullBath","KitchenAbvGr"


var_3 <- c("Fireplaces", "GarageYrBlt", "GarageCars", "GarageYrBlt", "GarageCars",
           "GarageArea","WoodDeckSF", "OpenPorchSF",
           "EnclosedPorch", "X3SsnPorch",  "ScreenPorch" ,  "PoolArea",
           "MiscVal", "MoSold", "YrSold",
           "SalePrice")
pairs(HousePriceDF[var_3])

#price: "Fireplaces", "GarageCars", "X3SsnPorch",    "PoolArea",

#the price components are:
var_4 <- c("LotArea", "OverallQual","OverallCond","BsmtFinSF1",
           "TotalBsmtSF","X1stFlrSF", "LowQualFinSF","BsmtFullBath",
           "BsmtHalfBath", "FullBath","KitchenAbvGr",
           "Fireplaces", "GarageCars", "X3SsnPorch","PoolArea",
           "SalePrice")
pairs(HousePriceDF[var_4])

par(mfrow=c(4,4))
hist(x = na.omit(LotArea))
hist(OverallQual)
hist(OverallCond)
hist(BsmtFinSF1)
hist(BsmtHalfBath)
hist(FullBath)
hist(KitchenAbvGr)
hist(Fireplaces)
hist(TotalBsmtSF)
hist(X1stFlrSF)
hist(LowQualFinSF)
hist(BsmtFullBath)
hist(GarageCars)
hist(X3SsnPorch)
hist(PoolArea)
hist(SalePrice)

str(HousePriceDF)

colnames(HousePriceDF)

str(HousePriceDF)

# factor(x = ["Normal" "Normal" "Normal" "Abnorml"],)



# detach(HousePriceDF,)
