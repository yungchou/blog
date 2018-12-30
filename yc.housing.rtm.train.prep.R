log1 <- 'blog.post/1.train.csv.prep.txt'
write(format(Sys.time(), "%a %b %d %Y %X"),log1)

train.prep.org <- read.csv('data/train.csv', stringsAsFactors=FALSE)

cat('Imported train data set: ', nrow(train.prep.org), ' obs. of '
    ,length(train.prep.org), 'variables')

#names(train.prep.org)

doit <- FALSE

if(doit){
# Missingness
if (!require('naniar')) install.packages('naniar'); library(naniar)
jpeg('blog.post/1.train.csv.missingness.jpeg',width=1280,height=720)
gg_miss_upset(train.prep.org)
dev.off()

# Percentage of missing values
missing <- function(x) { round(( sum(is.na(x))/length(x) )*100, 2) }
apply(train.prep.org,2,missing)

}

log <- function(x){
  write(x,'blog.post/yc.housing.rtm.post.txt',append=TRUE, sep=' ')}

#names(train.prep.org)
# Dropped Features after initial EDA
train.prep <- train.prep.org[-c(

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

#unique(train.prep$MSSubClass)
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

train.prep$MSSubClass[train.prep$MSSubClass%in%group2] <- 1
train.prep$MSSubClass[train.prep$MSSubClass%in%group1] <- 2
train.prep$MSSubClass[train.prep$MSSubClass%in%group3] <- 2
train.prep$MSSubClass[train.prep$MSSubClass%in%group4] <- 2
train.prep$MSSubClass[is.na(train.prep$MSSubClass)] <- 1
train.prep$MSSubClass <- as.integer(train.prep$MSSubClass)

#unique(train.prep$MSZoning)
train.prep$MSZoning[train.prep$MSZoning == "RH"] <- 2  # High - Density Residential
train.prep$MSZoning[train.prep$MSZoning == "RM"] <- 2  # Medium - Density Residential
train.prep$MSZoning[train.prep$MSZoning == "RL"] <- 2  # Low - Density Residential
train.prep$MSZoning[train.prep$MSZoning == "C (all)"] <- 2 # CM/CRC/CN/CT/CS - Commercial
train.prep$MSZoning[train.prep$MSZoning == "FV"] <- 1 # Floating Village Residential ??
train.prep$MSZoning[is.na(train.prep$MSZoning)] <- 2
train.prep$MSZoning <- as.integer(train.prep$MSZoning)

#unique(train.prep$LotArea)
train.prep$LotArea <- ifelse(is.na(train.prep$LotArea), 0, train.prep$LotArea/10000)
train.prep$LotArea <- as.numeric(train.prep$LotArea)

#unique(train.prep$LandContour)
train.prep$LandContour[train.prep$LandContour == "HLS"] <- 2 # Hillside
train.prep$LandContour[train.prep$LandContour == "Lvl"] <- 2 # Near Flat/Level
train.prep$LandContour[train.prep$LandContour == "Bnk"] <- 2 # Banked
train.prep$LandContour[train.prep$LandContour == "Low"] <- 1 # Depression
train.prep$LandContour[is.na(train.prep$LandContour)] <- 2
train.prep$LandContour <- as.integer(train.prep$LandContour)

#unique(train.prep$Utilities)
train.prep$Utilities[train.prep$Utilities == "AllPub"] <- 2
train.prep$Utilities[train.prep$Utilities == "NoSewr"] <- 1
train.prep$Utilities[train.prep$Utilities == "NoSeWa"] <- 1
train.prep$Utilities[is.na(train.prep$Utilities)] <- 2
train.prep$Utilities <- as.integer(train.prep$Utilities)

#unique(train.prep$LandSlope)
train.prep$LandSlope[train.prep$LandSlope == "Gtl"] <- 2
train.prep$LandSlope[train.prep$LandSlope == "Mod"] <- 2
train.prep$LandSlope[train.prep$LandSlope == "Sev"] <- 1
train.prep$LandSlope[is.na(train.prep$LandSlope)] <- 2
train.prep$LandSlope <- as.integer(train.prep$LandSlope)

#unique(train.prep$LotConfig)
train.prep$LotConfig[train.prep$LotConfig == "CulDSac"] <- 3
train.prep$LotConfig[train.prep$LotConfig == "Inside"] <- 2
train.prep$LotConfig[train.prep$LotConfig == "Corner"] <- 2
train.prep$LotConfig[train.prep$LotConfig == "FR2"] <- 2 # Forntage on 3 sides of property
train.prep$LotConfig[train.prep$LotConfig == "FR3"] <- 1 # Forntage on 3 sides of property
train.prep$LotConfig[is.na(train.prep$LotConfig)] <- 2
train.prep$LotConfig <- as.integer(train.prep$LotConfig)

#unique(train.prep$Neighborhood) # TO BE VALIDATED
train.prep$Neighborhood[train.prep$Neighborhood %in% c(
  "CollgCr","Veenker","Crawfor","NoRidge","Mitchel","Somerst",
  "NWAmes","OldTown","BrkSide","Sawyer","NridgHt")] <- 2
train.prep$Neighborhood[train.prep$Neighborhood %in% c(
  "NAmes","SawyerW","IDOTRR","MeadowV","Edwards","Timber","Gilbert","StoneBr",
  "ClearCr","NPkVill","Blmngtn","BrDale","SWISU","Blueste")] <- 1
train.prep$Neighborhood [is.na(train.prep$Neighborhood)] <- 1
train.prep$Neighborhood <- as.integer(train.prep$Neighborhood)

#unique(train.prep$Condition1)
train.prep$Condition1[train.prep$Condition1 %in% c("PosA","PosN")] <- 2
train.prep$Condition1[train.prep$Condition1 %in% c("Norm")] <- 1
train.prep$Condition1[train.prep$Condition1 %in% c("Feedr","Artery")] <- 1
train.prep$Condition1[train.prep$Condition1 %in% c("RRAe","RRNn","RRAn","RRNe")] <- 0
train.prep$Condition1[is.na(train.prep$Condition1)] <- 1
train.prep$Condition1 <- as.integer(train.prep$Condition1)

#unique(train.prep$Condition2)
train.prep$Condition2[train.prep$Condition2 %in% c("PosA","PosN")] <- 2
train.prep$Condition2[train.prep$Condition2 %in% c("Norm")] <- 1
train.prep$Condition2[train.prep$Condition2 %in% c("Feedr","Artery")] <- 1
train.prep$Condition2[train.prep$Condition2 %in% c("RRAe","RRNn","RRAn","RRNe")] <- 0
train.prep$Condition2[is.na(train.prep$Condition2)] <- 1
train.prep$Condition2 <- as.integer(train.prep$Condition2)

#typeof(train.prep$BldgType)
#unique(train.prep$BldgType)

train.prep$BldgType[train.prep$BldgType == "1Fam" ] <- 2
train.prep$BldgType[train.prep$BldgType == "2fmCon" ] <- 1.5
train.prep$BldgType[train.prep$BldgType == "Duplex" ] <- 1.5
train.prep$BldgType[train.prep$BldgType == "TwnhsE" ] <- 1.5
train.prep$BldgType[train.prep$BldgType == "Twnhs" ] <- 1
train.prep$BldgType[is.na(train.prep$BldgType)] <- 1
train.prep$BldgType <- as.numeric(train.prep$BldgType)

#typeof(train.prep$BldgType)
#unique(train.prep$BldgType)
#summary(train.prep$BldgType)

#unique(train.prep$HouseStyle)
train.prep$HouseStyle[train.prep$HouseStyle == "2.5Fin"] <- 2
train.prep$HouseStyle[train.prep$HouseStyle == "2.5Unf"] <- 2
train.prep$HouseStyle[train.prep$HouseStyle == "2Story"] <- 2
train.prep$HouseStyle[train.prep$HouseStyle == "1.5Fin"] <- 1.5
train.prep$HouseStyle[train.prep$HouseStyle == "1.5Unf"] <- 1.5
train.prep$HouseStyle[train.prep$HouseStyle == "1Story"] <- 1.5
train.prep$HouseStyle[train.prep$HouseStyle == "SFoyer"] <- 1
train.prep$HouseStyle[train.prep$HouseStyle == "SLvl"  ] <- 1
train.prep$HouseStyle[is.na(train.prep$HouseStyle)] <- 1
train.prep$HouseStyle<- as.numeric(train.prep$HouseStyle)

#
#unique(train.prep$OverallQual)
#summary(train.prep$OverallQual)
#boxplot(train.prep$OverallQual)
train.prep$OverallQual <- ifelse(is.na(train.prep$OverallQual),1,
                                 ifelse(train.prep$OverallQual>7,3,
                                        ifelse(train.prep$OverallQual>5,2,1)))
#unique(train.prep$OverallQual)
#summary(train.prep$OverallQual)
#boxplot(train.prep$OverallQual)

#unique(train.prep$OverallCond)
train.prep$OverallCond <- ifelse(is.na(train.prep$OverallCond),1,
                                 ifelse(train.prep$OverallCond>7,3,
                                        ifelse(train.prep$OverallCond>5,2,1)))

#unique(train.prep$YearBuilt)
train.prep$YearBuilt <- ifelse(is.na(train.prep$YearBuilt),1.90,train.prep$YearBuilt/1000)
train.prep$YearBuilt <- as.numeric(train.prep$YearBuilt)

#unique(train.prep$YearRemodAdd)
train.prep$YearRemodAdd <- ifelse(is.na(train.prep$YearRemodAdd),1.90,train.prep$YearRemodAdd/1000)
train.prep$YearRemodAdd <- as.numeric(train.prep$YearRemodAdd)

#unique(train.prep$Exterior1st)
train.prep$Exterior1st[train.prep$Exterior1st %in% c("BrkFace","BrkComm","Brk Cmn","BrkFace","Stone","Stucco")] <- 2
train.prep$Exterior1st[train.prep$Exterior1st %in% c("ImStucc","VinylSd","Plywood","AsphShn","AsbShng" )] <- 2
train.prep$Exterior1st[train.prep$Exterior1st %in% c("Wd Sdng","Wd Shng","WdShing","MetalSd","HdBoard","CemntBd","CmentBd","CBlock","Other")] <- 1
train.prep$Exterior1st[is.na(train.prep$Exterior1st)] <- 1
train.prep$Exterior1st <- as.integer(train.prep$Exterior1st)

#unique(train.prep$Exterior2nd)
train.prep$Exterior2nd[train.prep$Exterior2nd %in% c("BrkFace","BrkComm","Brk Cmn","BrkFace","Stone","Stucco")] <- 2
train.prep$Exterior2nd[train.prep$Exterior2nd %in% c("ImStucc","VinylSd","Plywood","AsphShn","AsbShng" )] <- 2
train.prep$Exterior2nd[train.prep$Exterior2nd %in% c("Wd Sdng","Wd Shng","WdShing","MetalSd","HdBoard","CemntBd","CmentBd","CBlock","Other")] <- 1
train.prep$Exterior2nd[is.na(train.prep$Exterior2nd)] <- 1
train.prep$Exterior2nd <- as.integer(train.prep$Exterior2nd)

#unique(train.prep$MasVnrType)
train.prep$MasVnrType[train.prep$MasVnrType == "BrkFace"] <- 2
train.prep$MasVnrType[train.prep$MasVnrType == "Stone" ] <- 2
train.prep$MasVnrType[train.prep$MasVnrType == "BrkCmn"] <- 2
train.prep$MasVnrType[train.prep$MasVnrType == "None" ] <- 1
train.prep$MasVnrType[is.na(train.prep$MasVnrType)] <- 1
train.prep$MasVnrType <- as.integer(train.prep$MasVnrType)

#unique(train.prep$ExterQual)
train.prep$ExterQual[train.prep$ExterQual == "Ex"] <- 2
train.prep$ExterQual[train.prep$ExterQual == "Gd"] <- 2
train.prep$ExterQual[train.prep$ExterQual == "TA"] <- 1.5
train.prep$ExterQual[train.prep$ExterQual == "Fa"] <- 1.5
train.prep$ExterQual[train.prep$ExterQual == "Po"] <- 1
train.prep$ExterQual[is.na(train.prep$ExterQual)] <- 1.5
train.prep$ExterQual <- as.numeric(train.prep$ExterQual)

#unique(train.prep$ExterCond)
train.prep$ExterCond[train.prep$ExterCond == "Ex"] <- 2
train.prep$ExterCond[train.prep$ExterCond == "Gd"] <- 2
train.prep$ExterCond[train.prep$ExterCond == "TA"] <- 1.5
train.prep$ExterCond[train.prep$ExterCond == "Fa"] <- 1.5
train.prep$ExterCond[train.prep$ExterCond == "Po"] <- 1
train.prep$ExterCond[is.na(train.prep$ExterCond)] <- 1.5
train.prep$ExterCond <- as.numeric(train.prep$ExterCond)

#unique(train.prep$BsmtQual)
train.prep$BsmtQual[train.prep$BsmtQual == "Ex"] <- 2
train.prep$BsmtQual[train.prep$BsmtQual == "Gd"] <- 2
train.prep$BsmtQual[train.prep$BsmtQual == "TA"] <- 1.5
train.prep$BsmtQual[train.prep$BsmtQual == "Fa"] <- 1.5
train.prep$BsmtQual[train.prep$BsmtQual == "Po"] <- 1
train.prep$BsmtQual[is.na(train.prep$BsmtQual)] <- 1.5
train.prep$BsmtQual <- as.numeric(train.prep$BsmtQual)

#unique(train.prep$BsmtCond)
train.prep$BsmtCond[train.prep$BsmtCond == "Ex"] <- 2
train.prep$BsmtCond[train.prep$BsmtCond == "Gd"] <- 2
train.prep$BsmtCond[train.prep$BsmtCond == "TA"] <- 1.5
train.prep$BsmtCond[train.prep$BsmtCond == "Fa"] <- 1.5
train.prep$BsmtCond[train.prep$BsmtCond == "Po"] <- 1
train.prep$BsmtCond[is.na(train.prep$BsmtCond)] <- 1
train.prep$BsmtCond <- as.numeric(train.prep$BsmtCond)

#unique(train.prep$BsmtExposure)
train.prep$BsmtExposure[train.prep$BsmtExposure == "Gd"] <- 3
train.prep$BsmtExposure[train.prep$BsmtExposure == "Av"] <- 2
train.prep$BsmtExposure[train.prep$BsmtExposure == "Mn"] <- 2
train.prep$BsmtExposure[train.prep$BsmtExposure == "No"] <- 1
train.prep$BsmtExposure[train.prep$BsmtExposure == "NA"] <- 1
train.prep$BsmtExposure[is.na(train.prep$BsmtExposure)] <- 1
train.prep$BsmtExposure <- as.integer(train.prep$BsmtExposure)

#unique(train.prep$BsmtFinType1)
train.prep$BsmtFinType1[train.prep$BsmtFinType1 == "GLQ"] <- 2
train.prep$BsmtFinType1[train.prep$BsmtFinType1 == "ALQ"] <- 2
train.prep$BsmtFinType1[train.prep$BsmtFinType1 == "BLQ"] <- 2
train.prep$BsmtFinType1[train.prep$BsmtFinType1 == "Rec"] <- 2
train.prep$BsmtFinType1[train.prep$BsmtFinType1 == "LwQ"] <- 1
train.prep$BsmtFinType1[train.prep$BsmtFinType1 == "Unf"] <- 1
train.prep$BsmtFinType1[train.prep$BsmtFinType1 == "NA"] <- 1
train.prep$BsmtFinType1[is.na(train.prep$BsmtFinType1)] <- 1
train.prep$BsmtFinType1 <- as.integer(train.prep$BsmtFinType1)

#unique(train.prep$BsmtFinSF1)
train.prep$BsmtFinSF1 <- ifelse(is.na(train.prep$BsmtFinSF1),0,train.prep$BsmtFinSF1/1000)
train.prep$BsmtFinSF1 <- as.numeric(train.prep$BsmtFinSF1)

#unique(train.prep$BsmtFinType2)
train.prep$BsmtFinType2[train.prep$BsmtFinType2 == "GLQ"] <- 2
train.prep$BsmtFinType2[train.prep$BsmtFinType2 == "ALQ"] <- 2
train.prep$BsmtFinType2[train.prep$BsmtFinType2 == "BLQ"] <- 2
train.prep$BsmtFinType2[train.prep$BsmtFinType2 == "Rec"] <- 2
train.prep$BsmtFinType2[train.prep$BsmtFinType2 == "LwQ"] <- 1
train.prep$BsmtFinType2[train.prep$BsmtFinType2 == "Unf"] <- 1
train.prep$BsmtFinType2[train.prep$BsmtFinType2 == "NA"] <- 1
train.prep$BsmtFinType2[is.na(train.prep$BsmtFinType2)] <- 1
train.prep$BsmtFinType2 <- as.integer(train.prep$BsmtFinType2)

#unique(train.prep$BsmtFinSF2)
train.prep$BsmtFinSF2 <- ifelse(is.na(train.prep$BsmtFinSF2),0,train.prep$BsmtFinSF2/1000)
train.prep$BsmtFinSF2 <- as.numeric(train.prep$BsmtFinSF2)

#unique(train.prep$BsmtUnfSF)
train.prep$BsmtUnfSF <- ifelse(is.na(train.prep$BsmtUnfSF),0,train.prep$BsmtUnfSF/1000)
train.prep$BsmtUnfSF <- as.numeric(train.prep$BsmtUnfSF)

#unique(train.prep$TotalBsmtSF)
train.prep$TotalBsmtSF <- ifelse(is.na(train.prep$TotalBsmtSF),0,train.prep$TotalBsmtSF/1000)
train.prep$TotalBsmtSF <- as.numeric(train.prep$TotalBsmtSF)

#unique(train.prep$Heating)
train.prep$Heating[train.prep$Heating %in% c("Floor","GasA","GasW")] <- 2
train.prep$Heating[train.prep$Heating %in% c("Grav","Wall","OthW")] <- 1.5
train.prep$Heating[is.na(train.prep$Heating)] <- 1
train.prep$Heating <- as.numeric(train.prep$Heating)

#unique(train.prep$HeatingQC)
train.prep$HeatingQC[train.prep$HeatingQC == "Ex"] <- 2
train.prep$HeatingQC[train.prep$HeatingQC == "Gd"] <- 2
train.prep$HeatingQC[train.prep$HeatingQC == "TA"] <- 1.5
train.prep$HeatingQC[train.prep$HeatingQC == "Fa"] <- 1.5
train.prep$HeatingQC[train.prep$HeatingQC == "Po"] <- 1
train.prep$HeatingQC[is.na(train.prep$HeatingQC)] <- 1.5
train.prep$HeatingQC <- as.numeric(train.prep$HeatingQC)

#unique(train.prep$CentralAir)
train.prep$CentralAir[train.prep$CentralAir == "Y"] <- 2
train.prep$CentralAir[train.prep$CentralAir == "N"] <- 1
train.prep$CentralAir[is.na(train.prep$CentralAir)] <- 1
train.prep$CentralAir <- as.integer(train.prep$CentralAir)

#unique(train.prep$X1stFlrSF)
train.prep$X1stFlrSF <- ifelse(is.na(train.prep$X1stFlrSF),0,train.prep$X1stFlrSF/1000)
train.prep$X1stFlrSF <- as.numeric(train.prep$X1stFlrSF)

#unique(train.prep$X2ndFlrSF)
train.prep$X2ndFlrSF <- ifelse(is.na(train.prep$X2ndFlrSF),0,train.prep$X2ndFlrSF/1000)
train.prep$X2ndFlrSF <- as.numeric(train.prep$X2ndFlrSF)

#unique(train.prep$LowQualFinSF)
train.prep$LowQualFinSF <- ifelse(is.na(train.prep$LowQualFinSF),0,train.prep$LowQualFinSF/1000)
train.prep$LowQualFinSF <- as.numeric(train.prep$LowQualFinSF)

#unique(train.prep$GrLivArea)
train.prep$GrLivArea <- ifelse(is.na(train.prep$GrLivArea),0,train.prep$GrLivArea/1000)
train.prep$GrLivArea <- as.numeric(train.prep$GrLivArea)

#unique(train.prep$KitchenQual)
train.prep$KitchenQual[train.prep$KitchenQual == "Ex"] <- 2
train.prep$KitchenQual[train.prep$KitchenQual == "Gd"] <- 2
train.prep$KitchenQual[train.prep$KitchenQual == "TA"] <- 1.5
train.prep$KitchenQual[train.prep$KitchenQual == "Fa"] <- 1.5
train.prep$KitchenQual[train.prep$KitchenQual == "Po"] <- 1
train.prep$KitchenQual[is.na(train.prep$KitchenQual)] <- 1.5
train.prep$KitchenQual <- as.numeric(train.prep$KitchenQual)

#unique(train.prep$Functional)
train.prep$Functional[train.prep$Functional %in% c("Typ","Mod")] <- 2
train.prep$Functional[train.prep$Functional %in% c("Min1","Min2")] <- 2
train.prep$Functional[train.prep$Functional %in% c("Maj1","Maj2")] <- 2
train.prep$Functional[train.prep$Functional == "Sev"] <- 1
train.prep$Functional[is.na(train.prep$Functional)] <- 1
train.prep$Functional <- as.integer(train.prep$Functional)

#unique(train.prep$GarageType)
train.prep$GarageType[train.prep$GarageType %in% c("Attchd","2Types","BuiltIn")] <- 2
train.prep$GarageType[train.prep$GarageType %in% c("Basment","Detchd")] <- 2
train.prep$GarageType[train.prep$GarageType == "CarPort"] <- 1.5
train.prep$GarageType[is.na(train.prep$GarageType)] <- 1
train.prep$GarageType <- as.numeric(train.prep$GarageType)

#unique(train.prep$GarageYrBlt)
train.prep$GarageYrBlt <- ifelse(is.na(train.prep$GarageYrBlt),1.98,train.prep$GarageYrBlt/1000)
train.prep$GarageYrBlt <- as.numeric(train.prep$GarageYrBlt)

#unique(train.prep$GarageFinish)
train.prep$GarageFinish[train.prep$GarageFinish == "Fin"] <- 2
train.prep$GarageFinish[train.prep$GarageFinish == "RFn"] <- 1.5
train.prep$GarageFinish[train.prep$GarageFinish == 'Unf'] <- 1
train.prep$GarageFinish[is.na(train.prep$GarageFinish)] <- 1
train.prep$GarageFinish <- as.numeric(train.prep$GarageFinish)

#unique(train.prep$GarageArea)
train.prep$GarageArea <- ifelse(is.na(train.prep$GarageArea),0,train.prep$GarageArea/1000)
train.prep$GarageArea <- as.numeric(train.prep$GarageArea)

#unique(train.prep$GarageQual)
train.prep$GarageQual[train.prep$GarageQual == "Ex"] <- 2
train.prep$GarageQual[train.prep$GarageQual == "Gd"] <- 2
train.prep$GarageQual[train.prep$GarageQual == 'TA'] <- 1.5
train.prep$GarageQual[train.prep$GarageQual == 'Fa'] <- 1.5
train.prep$GarageQual[train.prep$GarageQual == 'Po'] <- 1
train.prep$GarageQual[train.prep$BsmtFinType2 == "NA"] <- 1.5
train.prep$GarageQual[is.na(train.prep$GarageQual)] <- 1
train.prep$GarageQual <- as.numeric(train.prep$GarageQual)

#unique(train.prep$GarageCond)
train.prep$GarageCond[train.prep$GarageCond == "Ex"] <- 2
train.prep$GarageCond[train.prep$GarageCond == "Gd"] <- 2
train.prep$GarageCond[train.prep$GarageCond == 'TA'] <- 1.5
train.prep$GarageCond[train.prep$GarageCond == 'Fa'] <- 1.5
train.prep$GarageCond[train.prep$GarageCond == 'Po'] <- 1
train.prep$GarageCond[train.prep$GarageCond == "NA"] <- 1.5
train.prep$GarageCond[is.na(train.prep$GarageCond)] <- 1
train.prep$GarageCond <- as.numeric(train.prep$GarageCond)

#unique(train.prep$PavedDrive)
train.prep$PavedDrive[train.prep$PavedDrive == "Y"] <- 2
train.prep$PavedDrive[train.prep$PavedDrive == "P"] <- 1.5
train.prep$PavedDrive[train.prep$PavedDrive == "N"] <- 1
train.prep$PavedDrive[is.na(train.prep$PavedDrive)] <- 1.5
train.prep$PavedDrive <- as.numeric(train.prep$PavedDrive)

#unique(train.prep$WoodDeckSF)
train.prep$WoodDeckSF <- ifelse(is.na(train.prep$WoodDeckSF),0,train.prep$WoodDeckSF/1000)
train.prep$WoodDeckSF <- as.numeric(train.prep$WoodDeckSF)

#unique(train.prep$OpenPorchSF)
train.prep$OpenPorchSF <- ifelse(is.na(train.prep$OpenPorchSF),0,train.prep$OpenPorchSF/1000)
train.prep$OpenPorchSF <- as.numeric(train.prep$OpenPorchSF)

#unique(train.prep$X3SsnPorch)
train.prep$X3SsnPorch <- ifelse(is.na(train.prep$X3SsnPorch),0,train.prep$X3SsnPorch/1000)
train.prep$X3SsnPorch <- as.numeric(train.prep$X3SsnPorch)

#unique(train.prep$ScreenPorch)
train.prep$ScreenPorch <- ifelse(is.na(train.prep$ScreenPorch),0,train.prep$ScreenPorch/1000)
train.prep$X3SsnPorch <- as.numeric(train.prep$X3SsnPorch)

#unique(train.prep$PoolArea)
train.prep$PoolArea <- ifelse(is.na(train.prep$PoolArea),0,train.prep$PoolArea/1000)
train.prep$PoolArea <- as.numeric(train.prep$PoolArea)

#unique(train.prep$MiscVal)
train.prep$MiscVal <- ifelse(is.na(train.prep$MiscVal),0,train.prep$MiscVal/1000)
train.prep$MiscVal <- as.numeric(train.prep$MiscVal)

if(doit){
par(mfrow=c(1,2),mar=c(3,5,2,1)+0.1,mgp=c(4,1,0)
);plot(train.prep$MoSold,train.prep$SalePrice,
     main='MoSold Before Conversion',las=1,ylab='SalePrice')

typeof(train.prep$MoSold)
unique(train.prep$MoSold)
summary(train.prep$MoSold)
}

train.prep$MoSold[train.prep$MoSold %in% c(1,2,3)] <- 1
train.prep$MoSold[train.prep$MoSold %in% c(4,5,6)] <- 1.5
train.prep$MoSold[train.prep$MoSold %in% c(7,8,9)] <- 2
train.prep$MoSold[train.prep$MoSold %in% c(10,11,12)] <- 1.5
train.prep$MoSold[is.na(train.prep$MoSold)] <- 1
train.prep$MoSold <- as.numeric(train.prep$MoSold)

if (doit){
typeof(train.prep$MoSold)
unique(train.prep$MoSold)
summary(train.prep$MoSold)

plot(train.prep$MoSold,train.prep$SalePrice,
     main='MoSold After Conversion',las=1,ylab='SalePrice'
);par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))
}

#unique(train.prep$YrSold)
train.prep$YrSold <- ifelse(is.na(train.prep$YrSold),2.0,train.prep$YrSold/1000)
train.prep$YrSold <- as.numeric(train.prep$YrSold)

#sum(is.na(train.prep))
#train.prep <- na.omit(train.prep)
