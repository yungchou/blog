log1 <- 'blog.post/1.test.csv.prep.txt'
write(format(Sys.time(), "%a %b %d %Y %X"),log1)

test.prep.org <- read.csv('data/test.csv', stringsAsFactors=FALSE)

cat('Imported test data set: ', nrow(test.prep.org), ' obs. of '
    ,length(test.prep.org), 'variables')

#str(test.prep.org)

doit <- FALSE

if(doit){
  # Missingness
  if (!require('naniar')) install.packages('naniar'); library(naniar)
  jpeg('blog.post/1.test.csv.missingness.jpeg',width=1280,height=720)
  gg_miss_upset(test.prep.org)
  dev.off()

  # Percentage of missing values
  missing <- function(x) { round(( sum(is.na(x))/length(x) )*100, 2) }
  apply(test.prep.org,2,missing)

}

log <- function(x){
  write(x,'blog.post/yc.housing.rtm.post.txt',append=TRUE, sep=' ')}

#names(test.prep.org)
# Dropped Features after initial EDA
test.prep <- test.prep.org[-c(

  1, #Id,
  4, #LotFrontage,
  6, #Street,
  7, #Alley,
  8, #LotShape
  22, #RoofStyle,
  23, #RoofMatl,
  27, #MasVnrArea,
  30, #Foundation,
  43, #Electrical,
  48, #BsmtFullBath
  58, #FireplaceQu,
  73, #PoolQC,
  74, #Fence,
  75, #MiscFeature,
  79, #SaleType
  80  #SaleCondition

)]

#------------------
# data preparation
#------------------
# Ref: http://www-edlab.cs.umass.edu/~jianyang/data_description.pdf

#unique(test.prep$MSSubClass)
group1 <- c(
  '040','40' #	1-STORY W/FINISHED ATTIC ALL AGES
  ,'045','45' #	1-1/2 STORY - UNFINISHED ALL AGES
  ,'050','50' #	1-1/2 STORY FINISHED ALL AGES
  ,'075','75' #	2-1/2 STORY ALL AGES
  ,'090','90' #	DUPLEX - ALL STYLES AND AGES
  ,'150' #	1-1/2 STORY PUD - ALL AGES
  ,'190' #	2 FAMILY CONVERSION - ALL STYLES AND AGES
)

group2 <- c(
  '030','30' #	1-STORY 1945 & OLDER'
  ,'070','70' #	2-STORY 1945 & OLDER
)

group3 <- c(
  '020','20' #	1-STORY 1946 & NEWER ALL STYLES
  ,'060','60' #	2-STORY 1946 & NEWER
  ,'120' #	1-STORY PUD (Planned Unit Development) - 1946 & NEWER
  ,'160' #	2-STORY PUD - 1946 & NEWER
)

group4 <- c(
  '080','80' #	SPLIT OR MULTI-LEVEL
  ,'085','85' #	SPLIT FOYER
  ,'180' #	PUD - MULTILEVEL - INCL SPLIT LEV/FOYER
)

test.prep$MSSubClass[test.prep$MSSubClass%in%group2] <- 1
test.prep$MSSubClass[test.prep$MSSubClass%in%group1] <- 2
test.prep$MSSubClass[test.prep$MSSubClass%in%group3] <- 2
test.prep$MSSubClass[test.prep$MSSubClass%in%group4] <- 2
test.prep$MSSubClass[is.na(test.prep$MSSubClass)] <- 1
test.prep$MSSubClass <- as.integer(test.prep$MSSubClass)

#unique(test.prep$MSZoning)
test.prep$MSZoning[test.prep$MSZoning == "RH"] <- 2  # High - Density Residential
test.prep$MSZoning[test.prep$MSZoning == "RM"] <- 2  # Medium - Density Residential
test.prep$MSZoning[test.prep$MSZoning == "RL"] <- 2  # Low - Density Residential
test.prep$MSZoning[test.prep$MSZoning == "C (all)"] <- 2 # CM/CRC/CN/CT/CS - Commercial
test.prep$MSZoning[test.prep$MSZoning == "FV"] <- 1 # Floating Village Residential ??
test.prep$MSZoning[is.na(test.prep$MSZoning)] <- 2
test.prep$MSZoning <- as.integer(test.prep$MSZoning)

#unique(test.prep$LotArea)
test.prep$LotArea <- ifelse(is.na(test.prep$LotArea), 0, test.prep$LotArea/10000)
test.prep$LotArea <- as.numeric(test.prep$LotArea)

#unique(test.prep$LandContour)
test.prep$LandContour[test.prep$LandContour == "HLS"] <- 2 # Hillside
test.prep$LandContour[test.prep$LandContour == "Lvl"] <- 2 # Near Flat/Level
test.prep$LandContour[test.prep$LandContour == "Bnk"] <- 2 # Banked
test.prep$LandContour[test.prep$LandContour == "Low"] <- 1 # Depression
test.prep$LandContour[is.na(test.prep$LandContour)] <- 2
test.prep$LandContour <- as.integer(test.prep$LandContour)

#unique(test.prep$Utilities)
test.prep$Utilities[test.prep$Utilities == "AllPub"] <- 2
test.prep$Utilities[test.prep$Utilities == "NoSewr"] <- 1
test.prep$Utilities[test.prep$Utilities == "NoSeWa"] <- 1
test.prep$Utilities[is.na(test.prep$Utilities)] <- 2
test.prep$Utilities <- as.integer(test.prep$Utilities)

#unique(test.prep$LandSlope)
test.prep$LandSlope[test.prep$LandSlope == "Gtl"] <- 2
test.prep$LandSlope[test.prep$LandSlope == "Mod"] <- 2
test.prep$LandSlope[test.prep$LandSlope == "Sev"] <- 1
test.prep$LandSlope[is.na(test.prep$LandSlope)] <- 2
test.prep$LandSlope <- as.integer(test.prep$LandSlope)

#unique(test.prep$LotConfig)
test.prep$LotConfig[test.prep$LotConfig == "CulDSac"] <- 3
test.prep$LotConfig[test.prep$LotConfig == "Inside"] <- 2
test.prep$LotConfig[test.prep$LotConfig == "Corner"] <- 2
test.prep$LotConfig[test.prep$LotConfig == "FR2"] <- 2 # Forntage on 3 sides of property
test.prep$LotConfig[test.prep$LotConfig == "FR3"] <- 1 # Forntage on 3 sides of property
test.prep$LotConfig[is.na(test.prep$LotConfig)] <- 2
test.prep$LotConfig <- as.integer(test.prep$LotConfig)

#unique(test.prep$Neighborhood) # TO BE VALIDATED
test.prep$Neighborhood[test.prep$Neighborhood %in% c(
  "CollgCr","Veenker","Crawfor","NoRidge","Mitchel","Somerst",
  "NWAmes","OldTown","BrkSide","Sawyer","NridgHt")] <- 2
test.prep$Neighborhood[test.prep$Neighborhood %in% c(
  "NAmes","SawyerW","IDOTRR","MeadowV","Edwards","Timber","Gilbert","StoneBr",
  "ClearCr","NPkVill","Blmngtn","BrDale","SWISU","Blueste")] <- 1
test.prep$Neighborhood [is.na(test.prep$Neighborhood)] <- 1
test.prep$Neighborhood <- as.integer(test.prep$Neighborhood)

#unique(test.prep$Condition1)
test.prep$Condition1[test.prep$Condition1 %in% c("PosA","PosN")] <- 2
test.prep$Condition1[test.prep$Condition1 %in% c("Norm")] <- 1
test.prep$Condition1[test.prep$Condition1 %in% c("Feedr","Artery")] <- 1
test.prep$Condition1[test.prep$Condition1 %in% c("RRAe","RRNn","RRAn","RRNe")] <- 0
test.prep$Condition1[is.na(test.prep$Condition1)] <- 1
test.prep$Condition1 <- as.integer(test.prep$Condition1)

#unique(test.prep$Condition2)
test.prep$Condition2[test.prep$Condition2 %in% c("PosA","PosN")] <- 2
test.prep$Condition2[test.prep$Condition2 %in% c("Norm")] <- 1
test.prep$Condition2[test.prep$Condition2 %in% c("Feedr","Artery")] <- 1
test.prep$Condition2[test.prep$Condition2 %in% c("RRAe","RRNn","RRAn","RRNe")] <- 0
test.prep$Condition2[is.na(test.prep$Condition2)] <- 1
test.prep$Condition2 <- as.integer(test.prep$Condition2)

#typeof(test.prep$BldgType)
#unique(test.prep$BldgType)

test.prep$BldgType[test.prep$BldgType == "1Fam" ] <- 2
test.prep$BldgType[test.prep$BldgType == "2fmCon" ] <- 1.5
test.prep$BldgType[test.prep$BldgType == "Duplex" ] <- 1.5
test.prep$BldgType[test.prep$BldgType == "TwnhsE" ] <- 1.5
test.prep$BldgType[test.prep$BldgType == "Twnhs" ] <- 1
test.prep$BldgType[is.na(test.prep$BldgType)] <- 1
test.prep$BldgType <- as.numeric(test.prep$BldgType)

#typeof(test.prep$BldgType)
#unique(test.prep$BldgType)
#summary(test.prep$BldgType)

#unique(test.prep$HouseStyle)
test.prep$HouseStyle[test.prep$HouseStyle == "2.5Fin"] <- 2
test.prep$HouseStyle[test.prep$HouseStyle == "2.5Unf"] <- 2
test.prep$HouseStyle[test.prep$HouseStyle == "2Story"] <- 2
test.prep$HouseStyle[test.prep$HouseStyle == "1.5Fin"] <- 1.5
test.prep$HouseStyle[test.prep$HouseStyle == "1.5Unf"] <- 1.5
test.prep$HouseStyle[test.prep$HouseStyle == "1Story"] <- 1.5
test.prep$HouseStyle[test.prep$HouseStyle == "SFoyer"] <- 1
test.prep$HouseStyle[test.prep$HouseStyle == "SLvl"  ] <- 1
test.prep$HouseStyle[is.na(test.prep$HouseStyle)] <- 1
test.prep$HouseStyle<- as.numeric(test.prep$HouseStyle)

#
#unique(test.prep$OverallQual)
#summary(test.prep$OverallQual)
#boxplot(test.prep$OverallQual)
test.prep$OverallQual <- ifelse(is.na(test.prep$OverallQual),1,
                                 ifelse(test.prep$OverallQual>7,3,
                                        ifelse(test.prep$OverallQual>5,2,1)))
#unique(test.prep$OverallQual)
#summary(test.prep$OverallQual)
#boxplot(test.prep$OverallQual)

#unique(test.prep$OverallCond)
test.prep$OverallCond <- ifelse(is.na(test.prep$OverallCond),1,
                                 ifelse(test.prep$OverallCond>7,3,
                                        ifelse(test.prep$OverallCond>5,2,1)))

#unique(test.prep$YearBuilt)
test.prep$YearBuilt <- ifelse(is.na(test.prep$YearBuilt),1.90,test.prep$YearBuilt/1000)
test.prep$YearBuilt <- as.numeric(test.prep$YearBuilt)

#unique(test.prep$YearRemodAdd)
test.prep$YearRemodAdd <- ifelse(is.na(test.prep$YearRemodAdd),1.90,test.prep$YearRemodAdd/1000)
test.prep$YearRemodAdd <- as.numeric(test.prep$YearRemodAdd)

#unique(test.prep$Exterior1st)
test.prep$Exterior1st[test.prep$Exterior1st %in% c("BrkFace","BrkComm","Brk Cmn","BrkFace","Stone","Stucco")] <- 2
test.prep$Exterior1st[test.prep$Exterior1st %in% c("ImStucc","VinylSd","Plywood","AsphShn","AsbShng" )] <- 2
test.prep$Exterior1st[test.prep$Exterior1st %in% c("Wd Sdng","Wd Shng","WdShing","MetalSd","HdBoard","CemntBd","CmentBd","CBlock","Other")] <- 1
test.prep$Exterior1st[is.na(test.prep$Exterior1st)] <- 1
test.prep$Exterior1st <- as.integer(test.prep$Exterior1st)

#unique(test.prep$Exterior2nd)
test.prep$Exterior2nd[test.prep$Exterior2nd %in% c("BrkFace","BrkComm","Brk Cmn","BrkFace","Stone","Stucco")] <- 2
test.prep$Exterior2nd[test.prep$Exterior2nd %in% c("ImStucc","VinylSd","Plywood","AsphShn","AsbShng" )] <- 2
test.prep$Exterior2nd[test.prep$Exterior2nd %in% c("Wd Sdng","Wd Shng","WdShing","MetalSd","HdBoard","CemntBd","CmentBd","CBlock","Other")] <- 1
test.prep$Exterior2nd[is.na(test.prep$Exterior2nd)] <- 1
test.prep$Exterior2nd <- as.integer(test.prep$Exterior2nd)

#unique(test.prep$MasVnrType)
test.prep$MasVnrType[test.prep$MasVnrType == "BrkFace"] <- 2
test.prep$MasVnrType[test.prep$MasVnrType == "Stone" ] <- 2
test.prep$MasVnrType[test.prep$MasVnrType == "BrkCmn"] <- 2
test.prep$MasVnrType[test.prep$MasVnrType == "None" ] <- 1
test.prep$MasVnrType[is.na(test.prep$MasVnrType)] <- 1
test.prep$MasVnrType <- as.integer(test.prep$MasVnrType)

#unique(test.prep$ExterQual)
test.prep$ExterQual[test.prep$ExterQual == "Ex"] <- 2
test.prep$ExterQual[test.prep$ExterQual == "Gd"] <- 2
test.prep$ExterQual[test.prep$ExterQual == "TA"] <- 1.5
test.prep$ExterQual[test.prep$ExterQual == "Fa"] <- 1.5
test.prep$ExterQual[test.prep$ExterQual == "Po"] <- 1
test.prep$ExterQual[is.na(test.prep$ExterQual)] <- 1.5
test.prep$ExterQual <- as.numeric(test.prep$ExterQual)

#unique(test.prep$ExterCond)
test.prep$ExterCond[test.prep$ExterCond == "Ex"] <- 2
test.prep$ExterCond[test.prep$ExterCond == "Gd"] <- 2
test.prep$ExterCond[test.prep$ExterCond == "TA"] <- 1.5
test.prep$ExterCond[test.prep$ExterCond == "Fa"] <- 1.5
test.prep$ExterCond[test.prep$ExterCond == "Po"] <- 1
test.prep$ExterCond[is.na(test.prep$ExterCond)] <- 1.5
test.prep$ExterCond <- as.numeric(test.prep$ExterCond)

#unique(test.prep$BsmtQual)
test.prep$BsmtQual[test.prep$BsmtQual == "Ex"] <- 2
test.prep$BsmtQual[test.prep$BsmtQual == "Gd"] <- 2
test.prep$BsmtQual[test.prep$BsmtQual == "TA"] <- 1.5
test.prep$BsmtQual[test.prep$BsmtQual == "Fa"] <- 1.5
test.prep$BsmtQual[test.prep$BsmtQual == "Po"] <- 1
test.prep$BsmtQual[is.na(test.prep$BsmtQual)] <- 1.5
test.prep$BsmtQual <- as.numeric(test.prep$BsmtQual)

#unique(test.prep$BsmtCond)
test.prep$BsmtCond[test.prep$BsmtCond == "Ex"] <- 2
test.prep$BsmtCond[test.prep$BsmtCond == "Gd"] <- 2
test.prep$BsmtCond[test.prep$BsmtCond == "TA"] <- 1.5
test.prep$BsmtCond[test.prep$BsmtCond == "Fa"] <- 1.5
test.prep$BsmtCond[test.prep$BsmtCond == "Po"] <- 1
test.prep$BsmtCond[is.na(test.prep$BsmtCond)] <- 1
test.prep$BsmtCond <- as.numeric(test.prep$BsmtCond)

#unique(test.prep$BsmtExposure)
test.prep$BsmtExposure[test.prep$BsmtExposure == "Gd"] <- 3
test.prep$BsmtExposure[test.prep$BsmtExposure == "Av"] <- 2
test.prep$BsmtExposure[test.prep$BsmtExposure == "Mn"] <- 2
test.prep$BsmtExposure[test.prep$BsmtExposure == "No"] <- 1
test.prep$BsmtExposure[test.prep$BsmtExposure == "NA"] <- 1
test.prep$BsmtExposure[is.na(test.prep$BsmtExposure)] <- 1
test.prep$BsmtExposure <- as.integer(test.prep$BsmtExposure)

#unique(test.prep$BsmtFinType1)
test.prep$BsmtFinType1[test.prep$BsmtFinType1 == "GLQ"] <- 2
test.prep$BsmtFinType1[test.prep$BsmtFinType1 == "ALQ"] <- 2
test.prep$BsmtFinType1[test.prep$BsmtFinType1 == "BLQ"] <- 2
test.prep$BsmtFinType1[test.prep$BsmtFinType1 == "Rec"] <- 2
test.prep$BsmtFinType1[test.prep$BsmtFinType1 == "LwQ"] <- 1
test.prep$BsmtFinType1[test.prep$BsmtFinType1 == "Unf"] <- 1
test.prep$BsmtFinType1[test.prep$BsmtFinType1 == "NA"] <- 1
test.prep$BsmtFinType1[is.na(test.prep$BsmtFinType1)] <- 1
test.prep$BsmtFinType1 <- as.integer(test.prep$BsmtFinType1)

#unique(test.prep$BsmtFinSF1)
test.prep$BsmtFinSF1 <- ifelse(is.na(test.prep$BsmtFinSF1),0,test.prep$BsmtFinSF1/1000)
test.prep$BsmtFinSF1 <- as.numeric(test.prep$BsmtFinSF1)

#unique(test.prep$BsmtFinType2)
test.prep$BsmtFinType2[test.prep$BsmtFinType2 == "GLQ"] <- 2
test.prep$BsmtFinType2[test.prep$BsmtFinType2 == "ALQ"] <- 2
test.prep$BsmtFinType2[test.prep$BsmtFinType2 == "BLQ"] <- 2
test.prep$BsmtFinType2[test.prep$BsmtFinType2 == "Rec"] <- 2
test.prep$BsmtFinType2[test.prep$BsmtFinType2 == "LwQ"] <- 1
test.prep$BsmtFinType2[test.prep$BsmtFinType2 == "Unf"] <- 1
test.prep$BsmtFinType2[test.prep$BsmtFinType2 == "NA"] <- 1
test.prep$BsmtFinType2[is.na(test.prep$BsmtFinType2)] <- 1
test.prep$BsmtFinType2 <- as.integer(test.prep$BsmtFinType2)

#unique(test.prep$BsmtFinSF2)
test.prep$BsmtFinSF2 <- ifelse(is.na(test.prep$BsmtFinSF2),0,test.prep$BsmtFinSF2/1000)
test.prep$BsmtFinSF2 <- as.numeric(test.prep$BsmtFinSF2)

#unique(test.prep$BsmtUnfSF)
test.prep$BsmtUnfSF <- ifelse(is.na(test.prep$BsmtUnfSF),0,test.prep$BsmtUnfSF/1000)
test.prep$BsmtUnfSF <- as.numeric(test.prep$BsmtUnfSF)

#unique(test.prep$TotalBsmtSF)
test.prep$TotalBsmtSF <- ifelse(is.na(test.prep$TotalBsmtSF),0,test.prep$TotalBsmtSF/1000)
test.prep$TotalBsmtSF <- as.numeric(test.prep$TotalBsmtSF)

#unique(test.prep$Heating)
test.prep$Heating[test.prep$Heating %in% c("Floor","GasA","GasW")] <- 2
test.prep$Heating[test.prep$Heating %in% c("Grav","Wall","OthW")] <- 1.5
test.prep$Heating[is.na(test.prep$Heating)] <- 1
test.prep$Heating <- as.numeric(test.prep$Heating)

#unique(test.prep$HeatingQC)
test.prep$HeatingQC[test.prep$HeatingQC == "Ex"] <- 2
test.prep$HeatingQC[test.prep$HeatingQC == "Gd"] <- 2
test.prep$HeatingQC[test.prep$HeatingQC == "TA"] <- 1.5
test.prep$HeatingQC[test.prep$HeatingQC == "Fa"] <- 1.5
test.prep$HeatingQC[test.prep$HeatingQC == "Po"] <- 1
test.prep$HeatingQC[is.na(test.prep$HeatingQC)] <- 1.5
test.prep$HeatingQC <- as.numeric(test.prep$HeatingQC)

#unique(test.prep$CentralAir)
test.prep$CentralAir[test.prep$CentralAir == "Y"] <- 2
test.prep$CentralAir[test.prep$CentralAir == "N"] <- 1
test.prep$CentralAir[is.na(test.prep$CentralAir)] <- 1
test.prep$CentralAir <- as.integer(test.prep$CentralAir)

#unique(test.prep$X1stFlrSF)
test.prep$X1stFlrSF <- ifelse(is.na(test.prep$X1stFlrSF),0,test.prep$X1stFlrSF/1000)
test.prep$X1stFlrSF <- as.numeric(test.prep$X1stFlrSF)

#unique(test.prep$X2ndFlrSF)
test.prep$X2ndFlrSF <- ifelse(is.na(test.prep$X2ndFlrSF),0,test.prep$X2ndFlrSF/1000)
test.prep$X2ndFlrSF <- as.numeric(test.prep$X2ndFlrSF)

#unique(test.prep$LowQualFinSF)
test.prep$LowQualFinSF <- ifelse(is.na(test.prep$LowQualFinSF),0,test.prep$LowQualFinSF/1000)
test.prep$LowQualFinSF <- as.numeric(test.prep$LowQualFinSF)

#unique(test.prep$GrLivArea)
test.prep$GrLivArea <- ifelse(is.na(test.prep$GrLivArea),0,test.prep$GrLivArea/1000)
test.prep$GrLivArea <- as.numeric(test.prep$GrLivArea)

#unique(test.prep$KitchenQual)
test.prep$KitchenQual[test.prep$KitchenQual == "Ex"] <- 2
test.prep$KitchenQual[test.prep$KitchenQual == "Gd"] <- 2
test.prep$KitchenQual[test.prep$KitchenQual == "TA"] <- 1.5
test.prep$KitchenQual[test.prep$KitchenQual == "Fa"] <- 1.5
test.prep$KitchenQual[test.prep$KitchenQual == "Po"] <- 1
test.prep$KitchenQual[is.na(test.prep$KitchenQual)] <- 1.5
test.prep$KitchenQual <- as.numeric(test.prep$KitchenQual)

#unique(test.prep$Functional)
test.prep$Functional[test.prep$Functional %in% c("Typ","Mod")] <- 2
test.prep$Functional[test.prep$Functional %in% c("Min1","Min2")] <- 2
test.prep$Functional[test.prep$Functional %in% c("Maj1","Maj2")] <- 2
test.prep$Functional[test.prep$Functional == "Sev"] <- 1
test.prep$Functional[is.na(test.prep$Functional)] <- 1
test.prep$Functional <- as.integer(test.prep$Functional)

#unique(test.prep$GarageType)
test.prep$GarageType[test.prep$GarageType %in% c("Attchd","2Types","BuiltIn")] <- 2
test.prep$GarageType[test.prep$GarageType %in% c("Basment","Detchd")] <- 2
test.prep$GarageType[test.prep$GarageType == "CarPort"] <- 1.5
test.prep$GarageType[is.na(test.prep$GarageType)] <- 1
test.prep$GarageType <- as.numeric(test.prep$GarageType)

#unique(test.prep$GarageYrBlt)
test.prep$GarageYrBlt <- ifelse(is.na(test.prep$GarageYrBlt),1.98,test.prep$GarageYrBlt/1000)
test.prep$GarageYrBlt <- as.numeric(test.prep$GarageYrBlt)

#unique(test.prep$GarageFinish)
test.prep$GarageFinish[test.prep$GarageFinish == "Fin"] <- 2
test.prep$GarageFinish[test.prep$GarageFinish == "RFn"] <- 1.5
test.prep$GarageFinish[test.prep$GarageFinish == 'Unf'] <- 1
test.prep$GarageFinish[is.na(test.prep$GarageFinish)] <- 1
test.prep$GarageFinish <- as.numeric(test.prep$GarageFinish)

#unique(test.prep$GarageArea)
test.prep$GarageArea <- ifelse(is.na(test.prep$GarageArea),0,test.prep$GarageArea/1000)
test.prep$GarageArea <- as.numeric(test.prep$GarageArea)

#unique(test.prep$GarageQual)
test.prep$GarageQual[test.prep$GarageQual == "Ex"] <- 2
test.prep$GarageQual[test.prep$GarageQual == "Gd"] <- 2
test.prep$GarageQual[test.prep$GarageQual == 'TA'] <- 1.5
test.prep$GarageQual[test.prep$GarageQual == 'Fa'] <- 1.5
test.prep$GarageQual[test.prep$GarageQual == 'Po'] <- 1
test.prep$GarageQual[test.prep$BsmtFinType2 == "NA"] <- 1.5
test.prep$GarageQual[is.na(test.prep$GarageQual)] <- 1
test.prep$GarageQual <- as.numeric(test.prep$GarageQual)

#unique(test.prep$GarageCond)
test.prep$GarageCond[test.prep$GarageCond == "Ex"] <- 2
test.prep$GarageCond[test.prep$GarageCond == "Gd"] <- 2
test.prep$GarageCond[test.prep$GarageCond == 'TA'] <- 1.5
test.prep$GarageCond[test.prep$GarageCond == 'Fa'] <- 1.5
test.prep$GarageCond[test.prep$GarageCond == 'Po'] <- 1
test.prep$GarageCond[test.prep$GarageCond == "NA"] <- 1.5
test.prep$GarageCond[is.na(test.prep$GarageCond)] <- 1
test.prep$GarageCond <- as.numeric(test.prep$GarageCond)

#unique(test.prep$PavedDrive)
test.prep$PavedDrive[test.prep$PavedDrive == "Y"] <- 2
test.prep$PavedDrive[test.prep$PavedDrive == "P"] <- 1.5
test.prep$PavedDrive[test.prep$PavedDrive == "N"] <- 1
test.prep$PavedDrive[is.na(test.prep$PavedDrive)] <- 1.5
test.prep$PavedDrive <- as.numeric(test.prep$PavedDrive)

#unique(test.prep$WoodDeckSF)
test.prep$WoodDeckSF <- ifelse(is.na(test.prep$WoodDeckSF),0,test.prep$WoodDeckSF/1000)
test.prep$WoodDeckSF <- as.numeric(test.prep$WoodDeckSF)

#unique(test.prep$OpenPorchSF)
test.prep$OpenPorchSF <- ifelse(is.na(test.prep$OpenPorchSF),0,test.prep$OpenPorchSF/1000)
test.prep$OpenPorchSF <- as.numeric(test.prep$OpenPorchSF)

#unique(test.prep$X3SsnPorch)
test.prep$X3SsnPorch <- ifelse(is.na(test.prep$X3SsnPorch),0,test.prep$X3SsnPorch/1000)
test.prep$X3SsnPorch <- as.numeric(test.prep$X3SsnPorch)

#unique(test.prep$ScreenPorch)
test.prep$ScreenPorch <- ifelse(is.na(test.prep$ScreenPorch),0,test.prep$ScreenPorch/1000)
test.prep$X3SsnPorch <- as.numeric(test.prep$X3SsnPorch)

#unique(test.prep$PoolArea)
test.prep$PoolArea <- ifelse(is.na(test.prep$PoolArea),0,test.prep$PoolArea/1000)
test.prep$PoolArea <- as.numeric(test.prep$PoolArea)

#unique(test.prep$MiscVal)
test.prep$MiscVal <- ifelse(is.na(test.prep$MiscVal),0,test.prep$MiscVal/1000)
test.prep$MiscVal <- as.numeric(test.prep$MiscVal)

if(doit){

  par(mfrow=c(1,2),mar=c(3,5,2,1)+0.1,mgp=c(4,1,0))
  plot(test.prep$MoSold,test.prep$SalePrice,
       main='MoSold Before Conversion',las=1,ylab='SalePrice')

  typeof(test.prep$MoSold)
  unique(test.prep$MoSold)
  summary(test.prep$MoSold)
}

test.prep$MoSold[test.prep$MoSold %in% c(1,2,3)] <- 1
test.prep$MoSold[test.prep$MoSold %in% c(4,5,6)] <- 1.5
test.prep$MoSold[test.prep$MoSold %in% c(7,8,9)] <- 2
test.prep$MoSold[test.prep$MoSold %in% c(10,11,12)] <- 1.5
test.prep$MoSold[is.na(test.prep$MoSold)] <- 1
test.prep$MoSold <- as.numeric(test.prep$MoSold)

if (doit){
  typeof(test.prep$MoSold)
  unique(test.prep$MoSold)
  summary(test.prep$MoSold)

  plot(test.prep$MoSold,test.prep$SalePrice,
       main='MoSold After Conversion',las=1,ylab='SalePrice')
  par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))
}

#unique(test.prep$YrSold)
test.prep$YrSold <- ifelse(is.na(test.prep$YrSold),2.0,test.prep$YrSold/1000)
test.prep$YrSold <- as.numeric(test.prep$YrSold)

#sum(is.na(test.prep))
#test.prep <- na.omit(test.prep)
