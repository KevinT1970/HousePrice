{"metadata":{"language_info":{"name":"R","codemirror_mode":"r","pygments_lexer":"r","mimetype":"text/x-r-source","file_extension":".r","version":"4.0.5"},"kernelspec":{"name":"ir","display_name":"R","language":"R"}},"nbformat_minor":4,"nbformat":4,"cells":[{"cell_type":"code","source":"---\ntitle: \"Fun with Real Estate\"\noutput: html_document\n---\n## Data Driven Real Estate Analysis ##\n\nReality tv shows about real estate are all the rage lately, and they drive me nuts because of how unscientific and illogical they are. (Well, that's not the only reason, but it's the relevant one.) People with more money than they know what to do with going 10 rounds about the kind of drawer pulls they want to have in their kitchen and where exactly their imported bathroom tile should come from. Blech.\n\nThis dataset gives us a chance to look into the data on what really influences the value of a house without the garbage impulses of reality tv getting in the way, so I am excited to take a look!\n\n\n```{r Libraries, message=FALSE, warning=FALSE, echo=FALSE}\nlibrary(data.table)\nlibrary(FeatureHashing)\nlibrary(Matrix)\nlibrary(xgboost)\nrequire(randomForest)\nrequire(caret)\nrequire(dplyr)\nrequire(ggplot2)\nlibrary(pROC)\nlibrary(stringr)\nlibrary(dummies)\nlibrary(Metrics)\nlibrary(kernlab)\nlibrary(mlbench)\n```\n\n###Plan\n\n* Assemble the data and explore it\n* Clean variables, build what is needed\n* Three Models: Linear, randomForest, and xgboost\n* Choose the best model and make the prediction for entry\n\n\n###Clean the Data\n\nSo, what do we have here?\n```{r load, echo=FALSE}\ntrain <- read.csv(\"../input/train.csv\", stringsAsFactors=FALSE)\ntest <- read.csv(\"../input/test.csv\", stringsAsFactors=FALSE)\n\n#train <- read.csv(\"U:/Programming/R/Kaggle/houses/train.csv\", stringsAsFactors=FALSE)\n#test <- read.csv(\"U:/Programming/R/Kaggle/houses/test.csv\", stringsAsFactors=FALSE)\n\nnames(train)\n\n```\n\nI think the best steps to start with would be reformatting some character variables that we can easily convert to numeric. What's the street type about?\n\n```{r formatting_street}\ntable(train$Street)\n```\nNot exactly fancy, let's just make that paved or not. What about Lot Shape?\n\n```{r formatting_street2}\ntrain$paved[train$Street == \"Pave\"] <- 1\ntrain$paved[train$Street != \"Pave\"] <- 0\n\ntable(train$LotShape)\n```\nI assume these are something like variations on \"irregular\". So let's go with regular or not, and then we'll have this shape variable still if we want to go more granular later.\nTaking up land contour as the next.\n```{r formatting_shape}\ntrain$regshape[train$LotShape == \"Reg\"] <- 1\ntrain$regshape[train$LotShape != \"Reg\"] <- 0\n\ntable(train$LandContour)\n```\n\nIn order to save space, I'll just go through the rest of the categoricals using the provided codebook and pick up narrating again when it's done.\n\n***\n\nCue muzak. Go look at the code tab if you want to read all of this. It's about 300 lines. Have a good time.\n\n***\n\n###Poking Around\n\n```{r formatting_contour, echo=FALSE}\ntrain$flat[train$LandContour == \"Lvl\"] <- 1\ntrain$flat[train$LandContour != \"Lvl\"] <- 0\n\ntrain$pubutil[train$Utilities == \"AllPub\"] <- 1\ntrain$pubutil[train$Utilities != \"AllPub\"] <- 0\n\ntrain$gentle_slope[train$LandSlope == \"Gtl\"] <- 1\ntrain$gentle_slope[train$LandSlope != \"Gtl\"] <- 0\n\n```\n\n```{r groupmeans_lotconfig, echo=FALSE}\n# summarize(group_by(train, LotConfig),\n#           mean(SalePrice, na.rm=T))\n\ntrain$culdesac_fr3[train$LandSlope %in% c(\"CulDSac\", \"FR3\")] <- 1\ntrain$culdesac_fr3[!train$LandSlope %in% c(\"CulDSac\", \"FR3\")] <- 0\n\n```\n\n```{r groupmeans_nbhd, echo=FALSE}\nnbhdprice <- summarize(group_by(train, Neighborhood),\n          mean(SalePrice, na.rm=T))\n\n#nbhdprice[order(nbhdprice$`mean(SalePrice, na.rm = T)`),]\n\nnbhdprice_lo <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 140000)\nnbhdprice_med <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 200000 &\n                          nbhdprice$`mean(SalePrice, na.rm = T)` >= 140000 )\nnbhdprice_hi <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` >= 200000)\n\ntrain$nbhd_price_level[train$Neighborhood %in% nbhdprice_lo$Neighborhood] <- 1\ntrain$nbhd_price_level[train$Neighborhood %in% nbhdprice_med$Neighborhood] <- 2\ntrain$nbhd_price_level[train$Neighborhood %in% nbhdprice_hi$Neighborhood] <- 3\n\n# summarize(group_by(train, Condition1),\n#           mean(SalePrice, na.rm=T))\n\ntrain$pos_features_1[train$Condition1 %in% c(\"PosA\", \"PosN\")] <- 1\ntrain$pos_features_1[!train$Condition1 %in% c(\"PosA\", \"PosN\")] <- 0\n\n# summarize(group_by(train, Condition2),\n#           mean(SalePrice, na.rm=T))\n\ntrain$pos_features_2[train$Condition1 %in% c(\"PosA\", \"PosN\")] <- 1\ntrain$pos_features_2[!train$Condition1 %in% c(\"PosA\", \"PosN\")] <- 0\n\n```\n\n```{r groupmeans_bldg, echo=FALSE}\n# summarize(group_by(train, BldgType),\n#           mean(SalePrice, na.rm=T))\n\ntrain$twnhs_end_or_1fam[train$BldgType %in% c(\"1Fam\", \"TwnhsE\")] <- 1\ntrain$twnhs_end_or_1fam[!train$BldgType %in% c(\"1Fam\", \"TwnhsE\")] <- 0\n\nhousestyle_price <- summarize(group_by(train, HouseStyle),\n          mean(SalePrice, na.rm=T))\n\nhousestyle_lo <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` < 140000)\nhousestyle_med <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` < 200000 &\n                          housestyle_price$`mean(SalePrice, na.rm = T)` >= 140000 )\nhousestyle_hi <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` >= 200000)\n\ntrain$house_style_level[train$HouseStyle %in% housestyle_lo$HouseStyle] <- 1\ntrain$house_style_level[train$HouseStyle %in% housestyle_med$HouseStyle] <- 2\ntrain$house_style_level[train$HouseStyle %in% housestyle_hi$HouseStyle] <- 3\n\n\nroofstyle_price <- summarize(group_by(train, RoofStyle),\n          mean(SalePrice, na.rm=T))\n\ntrain$roof_hip_shed[train$RoofStyle %in% c(\"Hip\", \"Shed\")] <- 1\ntrain$roof_hip_shed[!train$RoofStyle %in% c(\"Hip\", \"Shed\")] <- 0\n\nroofmatl_price <- summarize(group_by(train, RoofMatl),\n          mean(SalePrice, na.rm=T))\n\ntrain$roof_matl_hi[train$RoofMatl %in% c(\"Membran\", \"WdShake\", \"WdShngl\")] <- 1\ntrain$roof_matl_hi[!train$RoofMatl %in% c(\"Membran\", \"WdShake\", \"WdShngl\")] <- 0\n\n\nprice <- summarize(group_by(train, Exterior1st),\n          mean(SalePrice, na.rm=T))\n\nmatl_lo_1 <- filter(price, price$`mean(SalePrice, na.rm = T)` < 140000)\nmatl_med_1<- filter(price, price$`mean(SalePrice, na.rm = T)` < 200000 &\n                          price$`mean(SalePrice, na.rm = T)` >= 140000 )\nmatl_hi_1 <- filter(price, price$`mean(SalePrice, na.rm = T)` >= 200000)\n\ntrain$exterior_1[train$Exterior1st %in% matl_lo_1$Exterior1st] <- 1\ntrain$exterior_1[train$Exterior1st %in% matl_med_1$Exterior1st] <- 2\ntrain$exterior_1[train$Exterior1st %in% matl_hi_1$Exterior1st] <- 3\n\n\nprice <- summarize(group_by(train, Exterior2nd),\n          mean(SalePrice, na.rm=T))\n\nmatl_lo <- filter(price, price$`mean(SalePrice, na.rm = T)` < 140000)\nmatl_med <- filter(price, price$`mean(SalePrice, na.rm = T)` < 200000 &\n                          price$`mean(SalePrice, na.rm = T)` >= 140000 )\nmatl_hi <- filter(price, price$`mean(SalePrice, na.rm = T)` >= 200000)\n\ntrain$exterior_2[train$Exterior2nd %in% matl_lo$Exterior2nd] <- 1\ntrain$exterior_2[train$Exterior2nd %in% matl_med$Exterior2nd] <- 2\ntrain$exterior_2[train$Exterior2nd %in% matl_hi$Exterior2nd] <- 3\n\nprice <- summarize(group_by(train, MasVnrType),\n          mean(SalePrice, na.rm=T))\n\ntrain$exterior_mason_1[train$MasVnrType %in% c(\"Stone\", \"BrkFace\") | is.na(train$MasVnrType)] <- 1\ntrain$exterior_mason_1[!train$MasVnrType %in% c(\"Stone\", \"BrkFace\") & !is.na(train$MasVnrType)] <- 0\n\n\nprice <- summarize(group_by(train, ExterQual),\n          mean(SalePrice, na.rm=T))\n\ntrain$exterior_cond[train$ExterQual == \"Ex\"] <- 4\ntrain$exterior_cond[train$ExterQual == \"Gd\"] <- 3\ntrain$exterior_cond[train$ExterQual == \"TA\"] <- 2\ntrain$exterior_cond[train$ExterQual == \"Fa\"] <- 1\n\n\nprice <- summarize(group_by(train, ExterCond),\n          mean(SalePrice, na.rm=T))\n\ntrain$exterior_cond2[train$ExterCond == \"Ex\"] <- 5\ntrain$exterior_cond2[train$ExterCond == \"Gd\"] <- 4\ntrain$exterior_cond2[train$ExterCond == \"TA\"] <- 3\ntrain$exterior_cond2[train$ExterCond == \"Fa\"] <- 2\ntrain$exterior_cond2[train$ExterCond == \"Po\"] <- 1\n\n```\n\n```{r groupmeans_base, echo=FALSE}\n\nprice <- summarize(group_by(train, Foundation),\n          mean(SalePrice, na.rm=T))\n\ntrain$found_concrete[train$Foundation == \"PConc\"] <- 1\ntrain$found_concrete[train$Foundation != \"PConc\"] <- 0\n\n\nprice <- summarize(group_by(train, BsmtQual),\n          mean(SalePrice, na.rm=T))\n\ntrain$bsmt_cond1[train$BsmtQual == \"Ex\"] <- 5\ntrain$bsmt_cond1[train$BsmtQual == \"Gd\"] <- 4\ntrain$bsmt_cond1[train$BsmtQual == \"TA\"] <- 3\ntrain$bsmt_cond1[train$BsmtQual == \"Fa\"] <- 2\ntrain$bsmt_cond1[is.na(train$BsmtQual)] <- 1\n\n\nprice <- summarize(group_by(train, BsmtCond),\n          mean(SalePrice, na.rm=T))\n\ntrain$bsmt_cond2[train$BsmtCond == \"Gd\"] <- 5\ntrain$bsmt_cond2[train$BsmtCond == \"TA\"] <- 4\ntrain$bsmt_cond2[train$BsmtCond == \"Fa\"] <- 3\ntrain$bsmt_cond2[is.na(train$BsmtCond)] <- 2\ntrain$bsmt_cond2[train$BsmtCond == \"Po\"] <- 1\n\n\nprice <- summarize(group_by(train, BsmtExposure),\n          mean(SalePrice, na.rm=T))\n\ntrain$bsmt_exp[train$BsmtExposure == \"Gd\"] <- 5\ntrain$bsmt_exp[train$BsmtExposure == \"Av\"] <- 4\ntrain$bsmt_exp[train$BsmtExposure == \"Mn\"] <- 3\ntrain$bsmt_exp[train$BsmtExposure == \"No\"] <- 2\ntrain$bsmt_exp[is.na(train$BsmtExposure)] <- 1\n\n\nprice <- summarize(group_by(train, BsmtFinType1),\n          mean(SalePrice, na.rm=T))\n\ntrain$bsmt_fin1[train$BsmtFinType1 == \"GLQ\"] <- 5\ntrain$bsmt_fin1[train$BsmtFinType1 == \"Unf\"] <- 4\ntrain$bsmt_fin1[train$BsmtFinType1 == \"ALQ\"] <- 3\ntrain$bsmt_fin1[train$BsmtFinType1 %in% c(\"BLQ\", \"Rec\", \"LwQ\")] <- 2\ntrain$bsmt_fin1[is.na(train$BsmtFinType1)] <- 1\n\n\n\nprice <- summarize(group_by(train, BsmtFinType2),\n          mean(SalePrice, na.rm=T))\n\ntrain$bsmt_fin2[train$BsmtFinType2 == \"ALQ\"] <- 6\ntrain$bsmt_fin2[train$BsmtFinType2 == \"Unf\"] <- 5\ntrain$bsmt_fin2[train$BsmtFinType2 == \"GLQ\"] <- 4\ntrain$bsmt_fin2[train$BsmtFinType2 %in% c(\"Rec\", \"LwQ\")] <- 3\ntrain$bsmt_fin2[train$BsmtFinType2 == \"BLQ\"] <- 2\ntrain$bsmt_fin2[is.na(train$BsmtFinType2)] <- 1\n\n```\n\n```{r hvac, echo=FALSE}\n\nprice <- summarize(group_by(train, Heating),\n          mean(SalePrice, na.rm=T))\n\n\ntrain$gasheat[train$Heating %in% c(\"GasA\", \"GasW\")] <- 1\ntrain$gasheat[!train$Heating %in% c(\"GasA\", \"GasW\")] <- 0\n\n\nprice <- summarize(group_by(train, HeatingQC),\n          mean(SalePrice, na.rm=T))\n\ntrain$heatqual[train$HeatingQC == \"Ex\"] <- 5\ntrain$heatqual[train$HeatingQC == \"Gd\"] <- 4\ntrain$heatqual[train$HeatingQC == \"TA\"] <- 3\ntrain$heatqual[train$HeatingQC == \"Fa\"] <- 2\ntrain$heatqual[train$HeatingQC == \"Po\"] <- 1\n\n\nprice <- summarize(group_by(train, CentralAir),\n          mean(SalePrice, na.rm=T))\n\ntrain$air[train$CentralAir == \"Y\"] <- 1\ntrain$air[train$CentralAir == \"N\"] <- 0\n\n\nprice <- summarize(group_by(train, Electrical),\n          mean(SalePrice, na.rm=T))\n\ntrain$standard_electric[train$Electrical == \"SBrkr\" | is.na(train$Electrical)] <- 1\ntrain$standard_electric[!train$Electrical == \"SBrkr\" & !is.na(train$Electrical)] <- 0\n\n\nprice <- summarize(group_by(train, KitchenQual),\n          mean(SalePrice, na.rm=T))\n\ntrain$kitchen[train$KitchenQual == \"Ex\"] <- 4\ntrain$kitchen[train$KitchenQual == \"Gd\"] <- 3\ntrain$kitchen[train$KitchenQual == \"TA\"] <- 2\ntrain$kitchen[train$KitchenQual == \"Fa\"] <- 1\n\n\nprice <- summarize(group_by(train, FireplaceQu),\n          mean(SalePrice, na.rm=T))\n\ntrain$fire[train$FireplaceQu == \"Ex\"] <- 5\ntrain$fire[train$FireplaceQu == \"Gd\"] <- 4\ntrain$fire[train$FireplaceQu == \"TA\"] <- 3\ntrain$fire[train$FireplaceQu == \"Fa\"] <- 2\ntrain$fire[train$FireplaceQu == \"Po\" | is.na(train$FireplaceQu)] <- 1\n\n```\n\n```{r cars, echo=FALSE}\n\nprice <- summarize(group_by(train, GarageType),\n          mean(SalePrice, na.rm=T))\n\ntrain$gar_attach[train$GarageType %in% c(\"Attchd\", \"BuiltIn\")] <- 1\ntrain$gar_attach[!train$GarageType %in% c(\"Attchd\", \"BuiltIn\")] <- 0\n\n\nprice <- summarize(group_by(train, GarageFinish),\n          mean(SalePrice, na.rm=T))\n\ntrain$gar_finish[train$GarageFinish %in% c(\"Fin\", \"RFn\")] <- 1\ntrain$gar_finish[!train$GarageFinish %in% c(\"Fin\", \"RFn\")] <- 0\n\n\nprice <- summarize(group_by(train, GarageQual),\n          mean(SalePrice, na.rm=T))\n\ntrain$garqual[train$GarageQual == \"Ex\"] <- 5\ntrain$garqual[train$GarageQual == \"Gd\"] <- 4\ntrain$garqual[train$GarageQual == \"TA\"] <- 3\ntrain$garqual[train$GarageQual == \"Fa\"] <- 2\ntrain$garqual[train$GarageQual == \"Po\" | is.na(train$GarageQual)] <- 1\n\n\nprice <- summarize(group_by(train, GarageCond),\n          mean(SalePrice, na.rm=T))\n\ntrain$garqual2[train$GarageCond == \"Ex\"] <- 5\ntrain$garqual2[train$GarageCond == \"Gd\"] <- 4\ntrain$garqual2[train$GarageCond == \"TA\"] <- 3\ntrain$garqual2[train$GarageCond == \"Fa\"] <- 2\ntrain$garqual2[train$GarageCond == \"Po\" | is.na(train$GarageCond)] <- 1\n\n\nprice <- summarize(group_by(train, PavedDrive),\n          mean(SalePrice, na.rm=T))\n\ntrain$paved_drive[train$PavedDrive == \"Y\"] <- 1\ntrain$paved_drive[!train$PavedDrive != \"Y\"] <- 0\ntrain$paved_drive[is.na(train$paved_drive)] <- 0\n\n```\n\n```{r misc, echo=FALSE}\nprice <- summarize(group_by(train, Functional),\n          mean(SalePrice, na.rm=T))\n\ntrain$housefunction[train$Functional %in% c(\"Typ\", \"Mod\")] <- 1\ntrain$housefunction[!train$Functional %in% c(\"Typ\", \"Mod\")] <- 0\n\n\nprice <- summarize(group_by(train, PoolQC),\n          mean(SalePrice, na.rm=T))\n\ntrain$pool_good[train$PoolQC %in% c(\"Ex\")] <- 1\ntrain$pool_good[!train$PoolQC %in% c(\"Ex\")] <- 0\n\n\nprice <- summarize(group_by(train, Fence),\n          mean(SalePrice, na.rm=T))\n\ntrain$priv_fence[train$Fence %in% c(\"GdPrv\")] <- 1\ntrain$priv_fence[!train$Fence %in% c(\"GdPrv\")] <- 0\n\n\nprice <- summarize(group_by(train, MiscFeature),\n          mean(SalePrice, na.rm=T))\n#This doesn't seem worth using at the moment. May adjust later.\n\n\nprice <- summarize(group_by(train, SaleType),\n          mean(SalePrice, na.rm=T))\n\n# price[order(price$`mean(SalePrice, na.rm = T)`),]\n\ntrain$sale_cat[train$SaleType %in% c(\"New\", \"Con\")] <- 5\ntrain$sale_cat[train$SaleType %in% c(\"CWD\", \"ConLI\")] <- 4\ntrain$sale_cat[train$SaleType %in% c(\"WD\")] <- 3\ntrain$sale_cat[train$SaleType %in% c(\"COD\", \"ConLw\", \"ConLD\")] <- 2\ntrain$sale_cat[train$SaleType %in% c(\"Oth\")] <- 1\n\n\nprice <- summarize(group_by(train, SaleCondition),\n          mean(SalePrice, na.rm=T))\n\n# price[order(price$`mean(SalePrice, na.rm = T)`),]\n\ntrain$sale_cond[train$SaleCondition %in% c(\"Partial\")] <- 4\ntrain$sale_cond[train$SaleCondition %in% c(\"Normal\", \"Alloca\")] <- 3\ntrain$sale_cond[train$SaleCondition %in% c(\"Family\",\"Abnorml\")] <- 2\ntrain$sale_cond[train$SaleCondition %in% c(\"AdjLand\")] <- 1\n\n\nprice <- summarize(group_by(train, MSZoning),\n          mean(SalePrice, na.rm=T))\n\n# price[order(price$`mean(SalePrice, na.rm = T)`),]\n\ntrain$zone[train$MSZoning %in% c(\"FV\")] <- 4\ntrain$zone[train$MSZoning %in% c(\"RL\")] <- 3\ntrain$zone[train$MSZoning %in% c(\"RH\",\"RM\")] <- 2\ntrain$zone[train$MSZoning %in% c(\"C (all)\")] <- 1\n\n\nprice <- summarize(group_by(train, Alley),\n          mean(SalePrice, na.rm=T))\n\n# price[order(price$`mean(SalePrice, na.rm = T)`),]\n\ntrain$alleypave[train$Alley %in% c(\"Pave\")] <- 1\ntrain$alleypave[!train$Alley %in% c(\"Pave\")] <- 0\n\n\n```\n\nDone. Now, time to drop off the variables that have been made numeric and are no longer needed.\n```{r drop_old_vars, echo=FALSE}\n\ntrain$Street <- NULL\ntrain$LotShape <- NULL\ntrain$LandContour <- NULL\ntrain$Utilities <- NULL\ntrain$LotConfig <- NULL\ntrain$LandSlope <- NULL\ntrain$Neighborhood <- NULL\ntrain$Condition1 <- NULL\ntrain$Condition2 <- NULL\ntrain$BldgType <- NULL\ntrain$HouseStyle <- NULL\ntrain$RoofStyle <- NULL\ntrain$RoofMatl <- NULL\n\ntrain$Exterior1st <- NULL\ntrain$Exterior2nd <- NULL\ntrain$MasVnrType <- NULL\ntrain$ExterQual <- NULL\ntrain$ExterCond <- NULL\n\ntrain$Foundation <- NULL\ntrain$BsmtQual <- NULL\ntrain$BsmtCond <- NULL\ntrain$BsmtExposure <- NULL\ntrain$BsmtFinType1 <- NULL\ntrain$BsmtFinType2 <- NULL\n\ntrain$Heating <- NULL\ntrain$HeatingQC <- NULL\ntrain$CentralAir <- NULL\ntrain$Electrical <- NULL\ntrain$KitchenQual <- NULL\ntrain$FireplaceQu <- NULL\n\ntrain$GarageType <- NULL\ntrain$GarageFinish <- NULL\ntrain$GarageQual <- NULL\ntrain$GarageCond <- NULL\ntrain$PavedDrive <- NULL\n\ntrain$Functional <- NULL\ntrain$PoolQC <- NULL\ntrain$Fence <- NULL\ntrain$MiscFeature <- NULL\ntrain$SaleType <- NULL\ntrain$SaleCondition <- NULL\ntrain$MSZoning <- NULL\ntrain$Alley <- NULL\n\n```\n\n\nAnother thing I want to do is build some interactions that may be worth looking at. \nFor example, if the house has a pool, is it more important that it has a big deck, or something like that? \nI used correlation visuals like this to do it- you can choose what you'd want to put in and how many variations\nyou want to make.\n\n```{r correlations, results=\"asis\"}\nlibrary(corrplot)\n\ncorrelations <- cor(train[,c(5,6,7,8, 16:25)], use=\"everything\")\ncorrplot(correlations, method=\"circle\", type=\"lower\",  sig.level = 0.01, insig = \"blank\")\n\ncorrelations <- cor(train[,c(5,6,7,8, 26:35)], use=\"everything\")\ncorrplot(correlations, method=\"circle\", type=\"lower\",  sig.level = 0.01, insig = \"blank\")\n\ncorrelations <- cor(train[,c(5,6,7,8, 66:75)], use=\"everything\")\ncorrplot(correlations, method=\"circle\", type=\"lower\",  sig.level = 0.01, insig = \"blank\")\n```\n\n\nAnyway, the correlations that both have to do with square footage I am going to discount, because size of the total and size of a floor, for example, are obvious correlations. \n\n```{r scatters}\n\npairs(~YearBuilt+OverallQual+TotalBsmtSF+GrLivArea,data=train,\n   main=\"Simple Scatterplot Matrix\")\n```\n\nThis is fun too- I picked a few of the variables that had a lot of correlation strengths. Basements have been getting bigger over time, apparently. As have the sizes of the living areas. Good to know!\n\n\nI'm also interested in the relationship between sale price and some numeric variables, but these can be tougher to visualize.\n```{r scatter_num, message=FALSE}\nlibrary(car)\n\nscatterplot(SalePrice ~ YearBuilt, data=train,  xlab=\"Year Built\", ylab=\"Sale Price\", grid=FALSE)\nscatterplot(SalePrice ~ YrSold, data=train,  xlab=\"Year Sold\", ylab=\"Sale Price\", grid=FALSE)\nscatterplot(SalePrice ~ X1stFlrSF, data=train,  xlab=\"Square Footage Floor 1\", ylab=\"Sale Price\", grid=FALSE)\n\n```\n\nPrices are higher for new houses, that makes sense. Also, we can see that sale prices dropped when we would expect (thanks, housing crisis).\nWe also have some loopy outliers on first floor square footage- probably bad data but it's not going to have a huge influence.\n\n\n\n```{r interactions}\n#Fix some NAs\ntrain$GarageYrBlt[is.na(train$GarageYrBlt)] <- 0\ntrain$MasVnrArea[is.na(train$MasVnrArea)] <- 0\ntrain$LotFrontage[is.na(train$LotFrontage)] <- 0\n\n#Interactions based on correlation\ntrain$year_qual <- train$YearBuilt*train$OverallQual #overall condition\ntrain$year_r_qual <- train$YearRemodAdd*train$OverallQual #quality x remodel\ntrain$qual_bsmt <- train$OverallQual*train$TotalBsmtSF #quality x basement size\n\ntrain$livarea_qual <- train$OverallQual*train$GrLivArea #quality x living area\ntrain$qual_bath <- train$OverallQual*train$FullBath #quality x baths\n\ntrain$qual_ext <- train$OverallQual*train$exterior_cond #quality x exterior\n\n#names(train)\n\n```\n\n***\n\n\n###Model Prepping\n\nThen, partition! I always like to use the caret partitioning function.\n\n```{r partition}\noutcome <- train$SalePrice\n\npartition <- createDataPartition(y=outcome,\n                                 p=.5,\n                                 list=F)\ntraining <- train[partition,]\ntesting <- train[-partition,]\n```\n\n***\n\n###A Linear Model\n\nFinally, we have our data and can build some models. Since our outcome is a continuous numeric variable, we want a linear model, not a GLM. First, let's just toss it all in there. I always like to use a proper regression model as my first examination of the data, to get a feel for what's there.\n\n```{r lm}\n\nlm_model_15 <- lm(SalePrice ~ ., data=training)\nsummary(lm_model_15)\n\n```\n\nLots of stuff we can drop right off, that's good. Some multicollinearity is making the model drop a few variables, but that's ok.\n\nAlso, our R-squared is not too bad! In case you're unfamiliar, that indicates what percent of the variation in the outcome is explained using the model we designed.\n\n```{r lm2}\n\nlm_model_15 <- lm(SalePrice ~ MSSubClass+LotArea+BsmtUnfSF+\n                    X1stFlrSF+X2ndFlrSF+GarageCars+\n                    WoodDeckSF+nbhd_price_level+\n                    exterior_cond+pos_features_1+\n                    bsmt_exp+kitchen+housefunction+pool_good+sale_cond+\n                    qual_ext+qual_bsmt, data=training)\nsummary(lm_model_15)\n\n```\n\nThat's our model with the important stuff, more or less. How does the RMSE turn out? That is our outcome of interest, after all.\n\n\n```{r testing}\n\nprediction <- predict(lm_model_15, testing, type=\"response\")\nmodel_output <- cbind(testing, prediction)\n\nmodel_output$log_prediction <- log(model_output$prediction)\nmodel_output$log_SalePrice <- log(model_output$SalePrice)\n\n#Test with RMSE\n\nrmse(model_output$log_SalePrice,model_output$log_prediction)\n\n```\n\n###A Random Forest\n\nNot too bad, given that this is just an LM. Let's try training the model with an RF. Let's use all the variables and see how things look, since randomforest does its own feature selection.\n\n```{r caret1}\n\nmodel_1 <- randomForest(SalePrice ~ ., data=training)\n\n\n# Predict using the test set\nprediction <- predict(model_1, testing)\nmodel_output <- cbind(testing, prediction)\n\n\nmodel_output$log_prediction <- log(model_output$prediction)\nmodel_output$log_SalePrice <- log(model_output$SalePrice)\n\n#Test with RMSE\n\nrmse(model_output$log_SalePrice,model_output$log_prediction)\n\n\n```\n\n###An xgboost\nNice! Try it with xgboost?\n\n```{r matrices}\n\n#Assemble and format the data\n\ntraining$log_SalePrice <- log(training$SalePrice)\ntesting$log_SalePrice <- log(testing$SalePrice)\n\n#Create matrices from the data frames\ntrainData<- as.matrix(training, rownames.force=NA)\ntestData<- as.matrix(testing, rownames.force=NA)\n\n#Turn the matrices into sparse matrices\ntrain2 <- as(trainData, \"sparseMatrix\")\ntest2 <- as(testData, \"sparseMatrix\")\n\n#####\n#colnames(train2)\n#Cross Validate the model\n\nvars <- c(2:37, 39:86) #choose the columns we want to use in the prediction matrix\n\ntrainD <- xgb.DMatrix(data = train2[,vars], label = train2[,\"SalePrice\"]) #Convert to xgb.DMatrix format\n\n#Cross validate the model\ncv.sparse <- xgb.cv(data = trainD,\n                    nrounds = 600,\n                    min_child_weight = 0,\n                    max_depth = 10,\n                    eta = 0.02,\n                    subsample = .7,\n                    colsample_bytree = .7,\n                    booster = \"gbtree\",\n                    eval_metric = \"rmse\",\n                    verbose = TRUE,\n                    print_every_n = 50,\n                    nfold = 4,\n                    nthread = 2,\n                    objective=\"reg:linear\")\n\n#Train the model\n\n#Choose the parameters for the model\nparam <- list(colsample_bytree = .7,\n             subsample = .7,\n             booster = \"gbtree\",\n             max_depth = 10,\n             eta = 0.02,\n             eval_metric = \"rmse\",\n             objective=\"reg:linear\")\n\n\n#Train the model using those parameters\nbstSparse <-\n  xgb.train(params = param,\n            data = trainD,\n            nrounds = 600,\n            watchlist = list(train = trainD),\n            verbose = TRUE,\n            print_every_n = 50,\n            nthread = 2)\n```\n\n\nPredict and test the RMSE.\n```{r evaluate1}\ntestD <- xgb.DMatrix(data = test2[,vars])\n#Column names must match the inputs EXACTLY\nprediction <- predict(bstSparse, testD) #Make the prediction based on the half of the training data set aside\n\n#Put testing prediction and test dataset all together\ntest3 <- as.data.frame(as.matrix(test2))\nprediction <- as.data.frame(as.matrix(prediction))\ncolnames(prediction) <- \"prediction\"\nmodel_output <- cbind(test3, prediction)\n\nmodel_output$log_prediction <- log(model_output$prediction)\nmodel_output$log_SalePrice <- log(model_output$SalePrice)\n\n#Test with RMSE\n\nrmse(model_output$log_SalePrice,model_output$log_prediction)\n\n```\n\nNice, that's pretty good stuff. I'll take the xgboost I think, let's call that good and make up the submission. Honestly, this is where the interesting stuff basically ends, unless you want to see the retraining and submission formatting.\n\n***\n\n\n##Retrain on the full sample\n\n```{r retrain}\nrm(bstSparse)\n\n#Create matrices from the data frames\nretrainData<- as.matrix(train, rownames.force=NA)\n\n#Turn the matrices into sparse matrices\nretrain <- as(retrainData, \"sparseMatrix\")\n\nparam <- list(colsample_bytree = .7,\n             subsample = .7,\n             booster = \"gbtree\",\n             max_depth = 10,\n             eta = 0.02,\n             eval_metric = \"rmse\",\n             objective=\"reg:linear\")\n\nretrainD <- xgb.DMatrix(data = retrain[,vars], label = retrain[,\"SalePrice\"])\n\n#retrain the model using those parameters\nbstSparse <-\n xgb.train(params = param,\n           data = retrainD,\n           nrounds = 600,\n           watchlist = list(train = trainD),\n           verbose = TRUE,\n           print_every_n = 50,\n           nthread = 2)\n  \n```\n\n\n##Prepare the prediction data\n\nHere I just repeat the same work I did on the training set, check the code tab to see all the details.\n\n```{r formatting_predictiondata, echo=FALSE}\ntest$paved[test$Street == \"Pave\"] <- 1\ntest$paved[test$Street != \"Pave\"] <- 0\n\ntest$regshape[test$LotShape == \"Reg\"] <- 1\ntest$regshape[test$LotShape != \"Reg\"] <- 0\n\ntest$flat[test$LandContour == \"Lvl\"] <- 1\ntest$flat[test$LandContour != \"Lvl\"] <- 0\n\ntest$pubutil[test$Utilities == \"AllPub\"] <- 1\ntest$pubutil[test$Utilities != \"AllPub\"] <- 0\n\ntest$gentle_slope[test$LandSlope == \"Gtl\"] <- 1\ntest$gentle_slope[test$LandSlope != \"Gtl\"] <- 0\n\ntest$culdesac_fr3[test$LandSlope %in% c(\"CulDSac\", \"FR3\")] <- 1\ntest$culdesac_fr3[!test$LandSlope %in% c(\"CulDSac\", \"FR3\")] <- 0\n\ntest$nbhd_price_level[test$Neighborhood %in% nbhdprice_lo$Neighborhood] <- 1\ntest$nbhd_price_level[test$Neighborhood %in% nbhdprice_med$Neighborhood] <- 2\ntest$nbhd_price_level[test$Neighborhood %in% nbhdprice_hi$Neighborhood] <- 3\n\ntest$pos_features_1[test$Condition1 %in% c(\"PosA\", \"PosN\")] <- 1\ntest$pos_features_1[!test$Condition1 %in% c(\"PosA\", \"PosN\")] <- 0\n\ntest$pos_features_2[test$Condition1 %in% c(\"PosA\", \"PosN\")] <- 1\ntest$pos_features_2[!test$Condition1 %in% c(\"PosA\", \"PosN\")] <- 0\n\n\ntest$twnhs_end_or_1fam[test$BldgType %in% c(\"1Fam\", \"TwnhsE\")] <- 1\ntest$twnhs_end_or_1fam[!test$BldgType %in% c(\"1Fam\", \"TwnhsE\")] <- 0\n\ntest$house_style_level[test$HouseStyle %in% housestyle_lo$HouseStyle] <- 1\ntest$house_style_level[test$HouseStyle %in% housestyle_med$HouseStyle] <- 2\ntest$house_style_level[test$HouseStyle %in% housestyle_hi$HouseStyle] <- 3\n\n\ntest$roof_hip_shed[test$RoofStyle %in% c(\"Hip\", \"Shed\")] <- 1\ntest$roof_hip_shed[!test$RoofStyle %in% c(\"Hip\", \"Shed\")] <- 0\n\ntest$roof_matl_hi[test$RoofMatl %in% c(\"Membran\", \"WdShake\", \"WdShngl\")] <- 1\ntest$roof_matl_hi[!test$RoofMatl %in% c(\"Membran\", \"WdShake\", \"WdShngl\")] <- 0\n\ntest$exterior_1[test$Exterior1st %in% matl_lo_1$Exterior1st] <- 1\ntest$exterior_1[test$Exterior1st %in% matl_med_1$Exterior1st] <- 2\ntest$exterior_1[test$Exterior1st %in% matl_hi_1$Exterior1st] <- 3\n\ntest$exterior_2[test$Exterior2nd %in% matl_lo$Exterior2nd] <- 1\ntest$exterior_2[test$Exterior2nd %in% matl_med$Exterior2nd] <- 2\ntest$exterior_2[test$Exterior2nd %in% matl_hi$Exterior2nd] <- 3\n\n\ntest$exterior_mason_1[test$MasVnrType %in% c(\"Stone\", \"BrkFace\") | is.na(test$MasVnrType)] <- 1\ntest$exterior_mason_1[!test$MasVnrType %in% c(\"Stone\", \"BrkFace\") & !is.na(test$MasVnrType)] <- 0\n\ntest$exterior_cond[test$ExterQual == \"Ex\"] <- 4\ntest$exterior_cond[test$ExterQual == \"Gd\"] <- 3\ntest$exterior_cond[test$ExterQual == \"TA\"] <- 2\ntest$exterior_cond[test$ExterQual == \"Fa\"] <- 1\n\ntest$exterior_cond2[test$ExterCond == \"Ex\"] <- 5\ntest$exterior_cond2[test$ExterCond == \"Gd\"] <- 4\ntest$exterior_cond2[test$ExterCond == \"TA\"] <- 3\ntest$exterior_cond2[test$ExterCond == \"Fa\"] <- 2\ntest$exterior_cond2[test$ExterCond == \"Po\"] <- 1\n\n\ntest$found_concrete[test$Foundation == \"PConc\"] <- 1\ntest$found_concrete[test$Foundation != \"PConc\"] <- 0\n\n\ntest$bsmt_cond1[test$BsmtQual == \"Ex\"] <- 5\ntest$bsmt_cond1[test$BsmtQual == \"Gd\"] <- 4\ntest$bsmt_cond1[test$BsmtQual == \"TA\"] <- 3\ntest$bsmt_cond1[test$BsmtQual == \"Fa\"] <- 2\ntest$bsmt_cond1[is.na(test$BsmtQual)] <- 1\n\ntest$bsmt_cond2[test$BsmtCond == \"Gd\"] <- 5\ntest$bsmt_cond2[test$BsmtCond == \"TA\"] <- 4\ntest$bsmt_cond2[test$BsmtCond == \"Fa\"] <- 3\ntest$bsmt_cond2[is.na(test$BsmtCond)] <- 2\ntest$bsmt_cond2[test$BsmtCond == \"Po\"] <- 1\n\n\ntest$bsmt_exp[test$BsmtExposure == \"Gd\"] <- 5\ntest$bsmt_exp[test$BsmtExposure == \"Av\"] <- 4\ntest$bsmt_exp[test$BsmtExposure == \"Mn\"] <- 3\ntest$bsmt_exp[test$BsmtExposure == \"No\"] <- 2\ntest$bsmt_exp[is.na(test$BsmtExposure)] <- 1\n\n\ntest$bsmt_fin1[test$BsmtFinType1 == \"GLQ\"] <- 5\ntest$bsmt_fin1[test$BsmtFinType1 == \"Unf\"] <- 4\ntest$bsmt_fin1[test$BsmtFinType1 == \"ALQ\"] <- 3\ntest$bsmt_fin1[test$BsmtFinType1 %in% c(\"BLQ\", \"Rec\", \"LwQ\")] <- 2\ntest$bsmt_fin1[is.na(test$BsmtFinType1)] <- 1\n\n\ntest$bsmt_fin2[test$BsmtFinType2 == \"ALQ\"] <- 6\ntest$bsmt_fin2[test$BsmtFinType2 == \"Unf\"] <- 5\ntest$bsmt_fin2[test$BsmtFinType2 == \"GLQ\"] <- 4\ntest$bsmt_fin2[test$BsmtFinType2 %in% c(\"Rec\", \"LwQ\")] <- 3\ntest$bsmt_fin2[test$BsmtFinType2 == \"BLQ\"] <- 2\ntest$bsmt_fin2[is.na(test$BsmtFinType2)] <- 1\n\ntest$gasheat[test$Heating %in% c(\"GasA\", \"GasW\")] <- 1\ntest$gasheat[!test$Heating %in% c(\"GasA\", \"GasW\")] <- 0\n\ntest$heatqual[test$HeatingQC == \"Ex\"] <- 5\ntest$heatqual[test$HeatingQC == \"Gd\"] <- 4\ntest$heatqual[test$HeatingQC == \"TA\"] <- 3\ntest$heatqual[test$HeatingQC == \"Fa\"] <- 2\ntest$heatqual[test$HeatingQC == \"Po\"] <- 1\n\n\ntest$air[test$CentralAir == \"Y\"] <- 1\ntest$air[test$CentralAir == \"N\"] <- 0\n\ntest$standard_electric[test$Electrical == \"SBrkr\" | is.na(test$Electrical)] <- 1\ntest$standard_electric[!test$Electrical == \"SBrkr\" & !is.na(test$Electrical)] <- 0\n\n\ntest$kitchen[test$KitchenQual == \"Ex\"] <- 4\ntest$kitchen[test$KitchenQual == \"Gd\"] <- 3\ntest$kitchen[test$KitchenQual == \"TA\"] <- 2\ntest$kitchen[test$KitchenQual == \"Fa\"] <- 1\n\ntest$fire[test$FireplaceQu == \"Ex\"] <- 5\ntest$fire[test$FireplaceQu == \"Gd\"] <- 4\ntest$fire[test$FireplaceQu == \"TA\"] <- 3\ntest$fire[test$FireplaceQu == \"Fa\"] <- 2\ntest$fire[test$FireplaceQu == \"Po\" | is.na(test$FireplaceQu)] <- 1\n\n\ntest$gar_attach[test$GarageType %in% c(\"Attchd\", \"BuiltIn\")] <- 1\ntest$gar_attach[!test$GarageType %in% c(\"Attchd\", \"BuiltIn\")] <- 0\n\n\ntest$gar_finish[test$GarageFinish %in% c(\"Fin\", \"RFn\")] <- 1\ntest$gar_finish[!test$GarageFinish %in% c(\"Fin\", \"RFn\")] <- 0\n\ntest$garqual[test$GarageQual == \"Ex\"] <- 5\ntest$garqual[test$GarageQual == \"Gd\"] <- 4\ntest$garqual[test$GarageQual == \"TA\"] <- 3\ntest$garqual[test$GarageQual == \"Fa\"] <- 2\ntest$garqual[test$GarageQual == \"Po\" | is.na(test$GarageQual)] <- 1\n\n\ntest$garqual2[test$GarageCond == \"Ex\"] <- 5\ntest$garqual2[test$GarageCond == \"Gd\"] <- 4\ntest$garqual2[test$GarageCond == \"TA\"] <- 3\ntest$garqual2[test$GarageCond == \"Fa\"] <- 2\ntest$garqual2[test$GarageCond == \"Po\" | is.na(test$GarageCond)] <- 1\n\n\ntest$paved_drive[test$PavedDrive == \"Y\"] <- 1\ntest$paved_drive[!test$PavedDrive != \"Y\"] <- 0\ntest$paved_drive[is.na(test$paved_drive)] <- 0\n\ntest$housefunction[test$Functional %in% c(\"Typ\", \"Mod\")] <- 1\ntest$housefunction[!test$Functional %in% c(\"Typ\", \"Mod\")] <- 0\n\n\ntest$pool_good[test$PoolQC %in% c(\"Ex\")] <- 1\ntest$pool_good[!test$PoolQC %in% c(\"Ex\")] <- 0\n\ntest$priv_fence[test$Fence %in% c(\"GdPrv\")] <- 1\ntest$priv_fence[!test$Fence %in% c(\"GdPrv\")] <- 0\n\ntest$sale_cat[test$SaleType %in% c(\"New\", \"Con\")] <- 5\ntest$sale_cat[test$SaleType %in% c(\"CWD\", \"ConLI\")] <- 4\ntest$sale_cat[test$SaleType %in% c(\"WD\")] <- 3\ntest$sale_cat[test$SaleType %in% c(\"COD\", \"ConLw\", \"ConLD\")] <- 2\ntest$sale_cat[test$SaleType %in% c(\"Oth\")] <- 1\n\ntest$sale_cond[test$SaleCondition %in% c(\"Partial\")] <- 4\ntest$sale_cond[test$SaleCondition %in% c(\"Normal\", \"Alloca\")] <- 3\ntest$sale_cond[test$SaleCondition %in% c(\"Family\",\"Abnorml\")] <- 2\ntest$sale_cond[test$SaleCondition %in% c(\"AdjLand\")] <- 1\n\ntest$zone[test$MSZoning %in% c(\"FV\")] <- 4\ntest$zone[test$MSZoning %in% c(\"RL\")] <- 3\ntest$zone[test$MSZoning %in% c(\"RH\",\"RM\")] <- 2\ntest$zone[test$MSZoning %in% c(\"C (all)\")] <- 1\n\ntest$alleypave[test$Alley %in% c(\"Pave\")] <- 1\ntest$alleypave[!test$Alley %in% c(\"Pave\")] <- 0\n\n\n```\n\n\n```{r drop_old_vars_predictiondata, echo=FALSE}\n\ntest$Street <- NULL\ntest$LotShape <- NULL\ntest$LandContour <- NULL\ntest$Utilities <- NULL\ntest$LotConfig <- NULL\ntest$LandSlope <- NULL\ntest$Neighborhood <- NULL\ntest$Condition1 <- NULL\ntest$Condition2 <- NULL\ntest$BldgType <- NULL\ntest$HouseStyle <- NULL\ntest$RoofStyle <- NULL\ntest$RoofMatl <- NULL\n\ntest$Exterior1st <- NULL\ntest$Exterior2nd <- NULL\ntest$MasVnrType <- NULL\ntest$ExterQual <- NULL\ntest$ExterCond <- NULL\n\ntest$Foundation <- NULL\ntest$BsmtQual <- NULL\ntest$BsmtCond <- NULL\ntest$BsmtExposure <- NULL\ntest$BsmtFinType1 <- NULL\ntest$BsmtFinType2 <- NULL\n\ntest$Heating <- NULL\ntest$HeatingQC <- NULL\ntest$CentralAir <- NULL\ntest$Electrical <- NULL\ntest$KitchenQual <- NULL\ntest$FireplaceQu <- NULL\n\ntest$GarageType <- NULL\ntest$GarageFinish <- NULL\ntest$GarageQual <- NULL\ntest$GarageCond <- NULL\ntest$PavedDrive <- NULL\n\ntest$Functional <- NULL\ntest$PoolQC <- NULL\ntest$Fence <- NULL\ntest$MiscFeature <- NULL\ntest$SaleType <- NULL\ntest$SaleCondition <- NULL\ntest$MSZoning <- NULL\ntest$Alley <- NULL\n\n```\n\n\n```{r interactions_prediction, echo=FALSE}\n#Fix some NAs\n\ntest$GarageYrBlt[is.na(test$GarageYrBlt)] <- 0\ntest$MasVnrArea[is.na(test$MasVnrArea)] <- 0\ntest$LotFrontage[is.na(test$LotFrontage)] <- 0\ntest$BsmtFinSF1[is.na(test$BsmtFinSF1)] <- 0\ntest$BsmtFinSF2[is.na(test$BsmtFinSF2)] <- 0\ntest$BsmtUnfSF[is.na(test$BsmtUnfSF)] <- 0\ntest$TotalBsmtSF[is.na(test$TotalBsmtSF)] <- 0\n\ntest$BsmtFullBath[is.na(test$BsmtFullBath)] <- 0\ntest$BsmtHalfBath[is.na(test$BsmtHalfBath)] <- 0\ntest$GarageCars[is.na(test$GarageCars)] <- 0\ntest$GarageArea[is.na(test$GarageArea)] <- 0\ntest$pubutil[is.na(test$pubutil)] <- 0\n\n\n#Interactions based on correlation\ntest$year_qual <- test$YearBuilt*test$OverallQual #overall condition\ntest$year_r_qual <- test$YearRemodAdd*test$OverallQual #quality x remodel\ntest$qual_bsmt <- test$OverallQual*test$TotalBsmtSF #quality x basement size\n\ntest$livarea_qual <- test$OverallQual*test$GrLivArea #quality x living area\ntest$qual_bath <- test$OverallQual*test$FullBath #quality x baths\n\ntest$qual_ext <- test$OverallQual*test$exterior_cond #quality x exterior\n\n\n\n```\n\n\nThen, format it for xgboost, I'm just using my boilerplate code for that.\n```{r finalpredict2}\n# Get the supplied test data ready #\n\npredict <- as.data.frame(test) #Get the dataset formatted as a frame for later combining\n\n#Create matrices from the data frames\npredData<- as.matrix(predict, rownames.force=NA)\n\n#Turn the matrices into sparse matrices\npredicting <- as(predData, \"sparseMatrix\")\n\n```\n\n\nMake sure your training sample and prediction sample have the same variables. I have been including this in code lately because I was making silly mistakes on variable choice.\n\n```{r finalpredict3}\ncolnames(train[,c(2:37, 39:86)])\n\nvars <- c(\"MSSubClass\",\"LotFrontage\",\"LotArea\",\"OverallQual\",\"OverallCond\",\"YearBuilt\",\n \"YearRemodAdd\",\"MasVnrArea\",\"BsmtFinSF1\",\"BsmtFinSF2\",\"BsmtUnfSF\",\"TotalBsmtSF\"   ,   \n \"X1stFlrSF\",\"X2ndFlrSF\",\"LowQualFinSF\",\"GrLivArea\",\"BsmtFullBath\",\"BsmtHalfBath\"  ,   \n \"FullBath\",\"HalfBath\",\"BedroomAbvGr\",\"KitchenAbvGr\",\"TotRmsAbvGrd\",\"Fireplaces\"     ,  \n \"GarageYrBlt\",\"GarageCars\",\"GarageArea\",\"WoodDeckSF\",\"OpenPorchSF\",\"EnclosedPorch\"    ,\n \"X3SsnPorch\",\"ScreenPorch\",\"PoolArea\",\"MiscVal\",\"MoSold\",\"YrSold\",\n \"paved\",\"regshape\",\"flat\",\"pubutil\",\"gentle_slope\",\"culdesac_fr3\"     ,\n \"nbhd_price_level\" , \"pos_features_1\",\"pos_features_2\",\"twnhs_end_or_1fam\",\"house_style_level\", \"roof_hip_shed\"    ,\n \"roof_matl_hi\",\"exterior_1\",\"exterior_2\",\"exterior_mason_1\",\"exterior_cond\",\"exterior_cond2\"   ,\n \"found_concrete\",\"bsmt_cond1\",\"bsmt_cond2\",\"bsmt_exp\",\"bsmt_fin1\",\"bsmt_fin2\"    ,   \n \"gasheat\",\"heatqual\",\"air\",\"standard_electric\", \"kitchen\",\"fire\",\n \"gar_attach\",\"gar_finish\",\"garqual\",\"garqual2\",\"paved_drive\",\"housefunction\",\n \"pool_good\",\"priv_fence\",\"sale_cat\",\"sale_cond\",\"zone\",\"alleypave\",\n\"year_qual\",\"year_r_qual\",\"qual_bsmt\",\"livarea_qual\",\"qual_bath\", \"qual_ext\")\n\n#colnames(predicting)\ncolnames(predicting[,vars])\n```\n\nActually do the predicting.\n\n```{r finalpredict4}\n#Column names must match the inputs EXACTLY\nprediction <- predict(bstSparse, predicting[,vars])\n\nprediction <- as.data.frame(as.matrix(prediction))  #Get the dataset formatted as a frame for later combining\ncolnames(prediction) <- \"prediction\"\nmodel_output <- cbind(predict, prediction) #Combine the prediction output with the rest of the set\n\nsub2 <- data.frame(Id = model_output$Id, SalePrice = model_output$prediction)\nlength(model_output$prediction)\nwrite.csv(sub2, file = \"sub3.csv\", row.names = F)\nhead(sub2$SalePrice)\n\n```","metadata":{"collapsed":false,"_kg_hide-input":false,"jupyter":{"outputs_hidden":false},"execution":{"iopub.status.busy":"2022-01-12T07:16:22.518091Z","iopub.execute_input":"2022-01-12T07:16:22.520602Z","iopub.status.idle":"2022-01-12T07:16:22.639671Z"},"trusted":true},"execution_count":1,"outputs":[]}]}