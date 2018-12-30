train.org <- read.csv('data/train.csv', stringsAsFactors=FALSE)
test.org  <- read.csv('data/test.csv' , stringsAsFactors=FALSE)

if (nrow(test.org)>nrow(train.org)){
  cat('test.csv (', nrow(test.org)
      ,') has more observations than that of train.csv(',nrow(train.org))
  return
}

# Making sure train data has the same number of rows with that of test data
rows <- nrow(test.org)
train.org <- train.org[1:rows,]

SalePrice <- seq(1:rows)
test.org <-  cbind(test.org,SalePrice )

names(test.org)

all.prep <- rbind(train.org,test.org)

#names(all.prep)

# Missingness
#library(naniar)
#gg_miss_upset(all.prep)

# Percentage of missing values
#missing <- function(x) { round(( sum(is.na(x))/length(x) )*100, 2) }
#apply(all.prep,2,missing)

#names(all.prep)
# Dropped Features after initial EDA
all.prep <- all.prep[-c(

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

unique(all.prep$MSSubClass)
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

all.prep$MSSubClass[all.prep$MSSubClass%in%group2] <- 1
all.prep$MSSubClass[all.prep$MSSubClass%in%group1] <- 2
all.prep$MSSubClass[all.prep$MSSubClass%in%group3] <- 2
all.prep$MSSubClass[all.prep$MSSubClass%in%group4] <- 2
all.prep$MSSubClass[is.na(all.prep$MSSubClass)] <- 1
all.prep$MSSubClass <- as.integer(all.prep$MSSubClass)

#unique(all.prep$MSZoning)
all.prep$MSZoning[all.prep$MSZoning == "RH"] <- 1  # High - Density Residential
all.prep$MSZoning[all.prep$MSZoning == "RM"] <- 1  # Medium - Density Residential
all.prep$MSZoning[all.prep$MSZoning == "RL"] <- 1  # Low - Density Residential
all.prep$MSZoning[all.prep$MSZoning == "C (all)"] <- 1 # CM/CRC/CN/CT/CS - Commercial
all.prep$MSZoning[all.prep$MSZoning == "FV"] <- 0 # Floating Village Residential ??
all.prep$MSZoning[is.na(all.prep$MSZoning)] <- 1
all.prep$MSZoning <- as.integer(all.prep$MSZoning)

#unique(all.prep$LandContour)
all.prep$LandContour[all.prep$LandContour == "HLS"] <- 1 # Hillside
all.prep$LandContour[all.prep$LandContour == "Lvl"] <- 1 # Near Flat/Level
all.prep$LandContour[all.prep$LandContour == "Bnk"] <- 1 # Banked
all.prep$LandContour[all.prep$LandContour == "Low"] <- 0 # Depression
all.prep$LandContour[is.na(all.prep$LandContour)] <- 1
all.prep$LandContour <- as.integer(all.prep$LandContour)

#unique(all.prep$Utilities)
all.prep$Utilities[all.prep$Utilities == "AllPub"] <- 1
all.prep$Utilities[all.prep$Utilities == "NoSewr"] <- 0
all.prep$Utilities[all.prep$Utilities == "NoSeWa"] <- 0
all.prep$Utilities[is.na(all.prep$Utilities)] <- 1
all.prep$Utilities <- as.integer(all.prep$Utilities)


#unique(all.prep$LandSlope)
all.prep$LandSlope[all.prep$LandSlope == "Gtl"] <- 1
all.prep$LandSlope[all.prep$LandSlope == "Mod"] <- 1
all.prep$LandSlope[all.prep$LandSlope == "Sev"] <- 0
all.prep$LandSlope[is.na(all.prep$LandSlope)] <- 1
all.prep$LandSlope <- as.integer(all.prep$LandSlope)

#unique(all.prep$LotConfig)
all.prep$LotConfig[all.prep$LotConfig == "CulDSac"] <- 2
all.prep$LotConfig[all.prep$LotConfig == "Inside"] <- 1
all.prep$LotConfig[all.prep$LotConfig == "Corner"] <- 1
all.prep$LotConfig[all.prep$LotConfig == "FR2"] <- 1 # Forntage on 3 sides of property
all.prep$LotConfig[all.prep$LotConfig == "FR3"] <- 0 # Forntage on 3 sides of property
all.prep$LotConfig[is.na(all.prep$LotConfig)] <- 1
all.prep$LotConfig <- as.integer(all.prep$LotConfig)

#unique(all.prep$Neighborhood) # TO BE VALIDATED
all.prep$Neighborhood[all.prep$Neighborhood %in% c(
  "CollgCr","Veenker","Crawfor","NoRidge","Mitchel","Somerst",
  "NWAmes","OldTown","BrkSide","Sawyer","NridgHt",
  "NAmes","SawyerW","IDOTRR","MeadowV","Edwards","Timber","Gilbert","StoneBr",
  "ClearCr","NPkVill","Blmngtn","BrDale","SWISU","Blueste")] <- 2
all.prep$Neighborhood [is.na(all.prep$Neighborhood)] <- 1
all.prep$Neighborhood <- as.integer(all.prep$Neighborhood)

#unique(all.prep$Condition1)
all.prep$Condition1[all.prep$Condition1 %in% c("PosA","PosN")] <- 2
all.prep$Condition1[all.prep$Condition1 %in% c("Norm")] <- 1
all.prep$Condition1[all.prep$Condition1 %in% c("Feedr","Artery")] <- 1
all.prep$Condition1[all.prep$Condition1 %in% c("RRAe","RRNn","RRAn","RRNe")] <- 0
all.prep$Condition1[is.na(all.prep$Condition1)] <- 1
all.prep$Condition1 <- as.integer(all.prep$Condition1)

#unique(all.prep$Condition2)
all.prep$Condition2[all.prep$Condition2 %in% c("PosA","PosN")] <- 2
all.prep$Condition2[all.prep$Condition2 %in% c("Norm")] <- 1
all.prep$Condition2[all.prep$Condition2 %in% c("Feedr","Artery")] <- 1
all.prep$Condition2[all.prep$Condition2 %in% c("RRAe","RRNn","RRAn","RRNe")] <- 0
all.prep$Condition2[is.na(all.prep$Condition2)] <- 1
all.prep$Condition2 <- as.integer(all.prep$Condition2)

#unique(all.prep$BldgType)
all.prep$BldgType[all.prep$BldgType == "1Fam" ] <- 3
all.prep$BldgType[all.prep$BldgType == "2fmCon" ] <- 2
all.prep$BldgType[all.prep$BldgType == "Duplex" ] <- 2
all.prep$BldgType[all.prep$BldgType == "TwnhsE" ] <- 2
all.prep$BldgType[all.prep$BldgType == "Twnhs" ] <- 1
all.prep$BldgType[is.na(all.prep$BldgType)] <- 1
all.prep$BldgType <- as.integer(all.prep$BldgType)

#unique(all.prep$HouseStyle)
all.prep$HouseStyle[all.prep$HouseStyle == "2.5Fin"] <- 2
all.prep$HouseStyle[all.prep$HouseStyle == "2.5Unf"] <- 2
all.prep$HouseStyle[all.prep$HouseStyle == "2Story"] <- 2
all.prep$HouseStyle[all.prep$HouseStyle == "1.5Fin"] <- 1
all.prep$HouseStyle[all.prep$HouseStyle == "1.5Unf"] <- 1
all.prep$HouseStyle[all.prep$HouseStyle == "1Story"] <- 1
all.prep$HouseStyle[all.prep$HouseStyle == "SFoyer"] <- 1
all.prep$HouseStyle[all.prep$HouseStyle == "SLvl"  ] <- 1
all.prep$HouseStyle[is.na(all.prep$HouseStyle)] <- 1
all.prep$HouseStyle<- as.integer(all.prep$HouseStyle)

#unique(all.prep$Exterior1st)
all.prep$Exterior1st[all.prep$Exterior1st %in% c("BrkFace","BrkComm","Brk Cmn","BrkFace","Stone","Stucco")] <- 2
all.prep$Exterior1st[all.prep$Exterior1st %in% c("ImStucc","VinylSd","Plywood","AsphShn","AsbShng" )] <- 2
all.prep$Exterior1st[all.prep$Exterior1st %in% c("Wd Sdng","Wd Shng","WdShing","MetalSd","HdBoard","CemntBd","CmentBd","CBlock","Other")] <- 1
all.prep$Exterior1st[is.na(all.prep$Exterior1st)] <- 1
all.prep$Exterior1st <- as.integer(all.prep$Exterior1st)

#unique(all.prep$Exterior2nd)
all.prep$Exterior2nd[all.prep$Exterior2nd %in% c("BrkFace","BrkComm","Brk Cmn","BrkFace","Stone","Stucco")] <- 2
all.prep$Exterior2nd[all.prep$Exterior2nd %in% c("ImStucc","VinylSd","Plywood","AsphShn","AsbShng" )] <- 2
all.prep$Exterior2nd[all.prep$Exterior2nd %in% c("Wd Sdng","Wd Shng","WdShing","MetalSd","HdBoard","CemntBd","CmentBd","CBlock","Other")] <- 1
all.prep$Exterior2nd[is.na(all.prep$Exterior2nd)] <- 1
all.prep$Exterior2nd <- as.integer(all.prep$Exterior2nd)

#unique(all.prep$MasVnrType)
all.prep$MasVnrType[all.prep$MasVnrType == "BrkFace"] <- 2
all.prep$MasVnrType[all.prep$MasVnrType == "Stone" ] <- 2
all.prep$MasVnrType[all.prep$MasVnrType == "BrkCmn"] <- 2
all.prep$MasVnrType[all.prep$MasVnrType == "None" ] <- 1
all.prep$MasVnrType[is.na(all.prep$MasVnrType)] <- 1
all.prep$MasVnrType <- as.integer(all.prep$MasVnrType)

#unique(all.prep$ExterQual)
all.prep$ExterQual[all.prep$ExterQual == "Ex"] <- 3
all.prep$ExterQual[all.prep$ExterQual == "Gd"] <- 2
all.prep$ExterQual[all.prep$ExterQual == "TA"] <- 2
all.prep$ExterQual[all.prep$ExterQual == "Fa"] <- 2
all.prep$ExterQual[all.prep$ExterQual == "Po"] <- 1
all.prep$ExterQual[is.na(all.prep$ExterQual)] <- 2
all.prep$ExterQual <- as.integer(all.prep$ExterQual)

#unique(all.prep$ExterCond)
all.prep$ExterCond[all.prep$ExterCond == "Ex"] <- 3
all.prep$ExterCond[all.prep$ExterCond == "Gd"] <- 2
all.prep$ExterCond[all.prep$ExterCond == "TA"] <- 2
all.prep$ExterCond[all.prep$ExterCond == "Fa"] <- 2
all.prep$ExterCond[all.prep$ExterCond == "Po"] <- 1
all.prep$ExterCond[is.na(all.prep$ExterCond)] <- 2
all.prep$ExterCond <- as.integer(all.prep$ExterCond)

#unique(all.prep$BsmtQual)
all.prep$BsmtQual[all.prep$BsmtQual == "Ex"] <- 2
all.prep$BsmtQual[all.prep$BsmtQual == "Gd"] <- 2
all.prep$BsmtQual[all.prep$BsmtQual == "TA"] <- 1
all.prep$BsmtQual[all.prep$BsmtQual == "Fa"] <- 1
all.prep$BsmtQual[all.prep$BsmtQual == "Po"] <- 1
all.prep$BsmtQual[is.na(all.prep$BsmtQual)] <- 1
all.prep$BsmtQual <- as.integer(all.prep$BsmtQual)

#unique(all.prep$BsmtCond)
all.prep$BsmtCond[all.prep$BsmtCond == "Ex"] <- 2
all.prep$BsmtCond[all.prep$BsmtCond == "Gd"] <- 2
all.prep$BsmtCond[all.prep$BsmtCond == "TA"] <- 1
all.prep$BsmtCond[all.prep$BsmtCond == "Fa"] <- 1
all.prep$BsmtCond[all.prep$BsmtCond == "Po"] <- 1
all.prep$BsmtCond[is.na(all.prep$BsmtCond)] <- 1
all.prep$BsmtCond <- as.integer(all.prep$BsmtCond)

#unique(all.prep$BsmtExposure)
all.prep$BsmtExposure[all.prep$BsmtExposure == "Gd"] <- 3
all.prep$BsmtExposure[all.prep$BsmtExposure == "Av"] <- 2
all.prep$BsmtExposure[all.prep$BsmtExposure == "Mn"] <- 2
all.prep$BsmtExposure[all.prep$BsmtExposure == "No"] <- 1
all.prep$BsmtExposure[all.prep$BsmtExposure == "NA"] <- 1
all.prep$BsmtExposure[is.na(all.prep$BsmtExposure)] <- 1
all.prep$BsmtExposure <- as.integer(all.prep$BsmtExposure)

#unique(all.prep$BsmtFinType1)
all.prep$BsmtFinType1[all.prep$BsmtFinType1 == "GLQ"] <- 2
all.prep$BsmtFinType1[all.prep$BsmtFinType1 == "ALQ"] <- 2
all.prep$BsmtFinType1[all.prep$BsmtFinType1 == "BLQ"] <- 2
all.prep$BsmtFinType1[all.prep$BsmtFinType1 == "Rec"] <- 2
all.prep$BsmtFinType1[all.prep$BsmtFinType1 == "LwQ"] <- 1
all.prep$BsmtFinType1[all.prep$BsmtFinType1 == "Unf"] <- 1
all.prep$BsmtFinType1[all.prep$BsmtFinType1 == "NA"] <- 1
all.prep$BsmtFinType1[is.na(all.prep$BsmtFinType1)] <- 1
all.prep$BsmtFinType1 <- as.integer(all.prep$BsmtFinType1)

#unique(all.prep$BsmtFinType2)
all.prep$BsmtFinType2[all.prep$BsmtFinType2 == "GLQ"] <- 2
all.prep$BsmtFinType2[all.prep$BsmtFinType2 == "ALQ"] <- 2
all.prep$BsmtFinType2[all.prep$BsmtFinType2 == "BLQ"] <- 2
all.prep$BsmtFinType2[all.prep$BsmtFinType2 == "Rec"] <- 2
all.prep$BsmtFinType2[all.prep$BsmtFinType2 == "LwQ"] <- 1
all.prep$BsmtFinType2[all.prep$BsmtFinType2 == "Unf"] <- 1
all.prep$BsmtFinType2[all.prep$BsmtFinType2 == "NA"] <- 1
all.prep$BsmtFinType2[is.na(all.prep$BsmtFinType2)] <- 1
all.prep$BsmtFinType2 <- as.integer(all.prep$BsmtFinType2)

#unique(all.prep$Heating)
all.prep$Heating[all.prep$Heating %in% c("Floor","GasA","GasW")] <- 2
all.prep$Heating[all.prep$Heating %in% c("Grav","Wall","OthW")] <- 1
all.prep$Heating[is.na(all.prep$Heating)] <- 1
all.prep$Heating <- as.integer(all.prep$Heating)

#unique(all.prep$HeatingQC)
all.prep$HeatingQC[all.prep$HeatingQC == "Ex"] <- 2
all.prep$HeatingQC[all.prep$HeatingQC == "Gd"] <- 2
all.prep$HeatingQC[all.prep$HeatingQC == "TA"] <- 2
all.prep$HeatingQC[all.prep$HeatingQC == "Fa"] <- 2
all.prep$HeatingQC[all.prep$HeatingQC == "Po"] <- 1
all.prep$HeatingQC[is.na(all.prep$HeatingQC)] <- 1
all.prep$HeatingQC <- as.integer(all.prep$HeatingQC)

#unique(all.prep$CentralAir)
all.prep$CentralAir[all.prep$CentralAir == "Y"] <- 2
all.prep$CentralAir[all.prep$CentralAir == "N"] <- 1
all.prep$CentralAir[is.na(all.prep$CentralAir)] <- 1
all.prep$CentralAir <- as.integer(all.prep$CentralAir)

#unique(all.prep$KitchenQual)
all.prep$KitchenQual[all.prep$KitchenQual == "Ex"] <- 2
all.prep$KitchenQual[all.prep$KitchenQual == "Gd"] <- 2
all.prep$KitchenQual[all.prep$KitchenQual == "TA"] <- 2
all.prep$KitchenQual[all.prep$KitchenQual == "Fa"] <- 1
all.prep$KitchenQual[all.prep$KitchenQual == "Po"] <- 1
all.prep$KitchenQual[is.na(all.prep$KitchenQual)] <- 1
all.prep$KitchenQual <- as.integer(all.prep$KitchenQual)

#unique(all.prep$Functional)
all.prep$Functional[all.prep$Functional %in% c("Typ","Mod")] <- 2
all.prep$Functional[all.prep$Functional %in% c("Min1","Min2")] <- 2
all.prep$Functional[all.prep$Functional %in% c("Maj1","Maj2")] <- 2
all.prep$Functional[all.prep$Functional == "Sev"] <- 1
all.prep$Functional[is.na(all.prep$Functional)] <- 1
all.prep$Functional <- as.integer(all.prep$Functional)

#unique(all.prep$GarageType)
all.prep$GarageType[all.prep$GarageType %in% c("Attchd","2Types","BuiltIn")] <- 2
all.prep$GarageType[all.prep$GarageType %in% c("Basment","Detchd")] <- 2
all.prep$GarageType[all.prep$GarageType == "CarPort"] <- 2
all.prep$GarageType[is.na(all.prep$GarageType)] <- 1
all.prep$GarageType <- as.integer(all.prep$GarageType)

#unique(all.prep$GarageFinish)
all.prep$GarageFinish[all.prep$GarageFinish == "Fin"] <- 2
all.prep$GarageFinish[all.prep$GarageFinish == "RFn"] <- 2
all.prep$GarageFinish[all.prep$GarageFinish == 'Unf'] <- 1
all.prep$GarageFinish[is.na(all.prep$GarageFinish)] <- 1
all.prep$GarageFinish <- as.integer(all.prep$GarageFinish)

#unique(all.prep$GarageQual)
all.prep$GarageQual[all.prep$GarageQual == "Ex"] <- 2
all.prep$GarageQual[all.prep$GarageQual == "Gd"] <- 2
all.prep$GarageQual[all.prep$GarageQual == 'TA'] <- 2
all.prep$GarageQual[all.prep$GarageQual == 'Fa'] <- 2
all.prep$GarageQual[all.prep$GarageQual == 'Po'] <- 1
all.prep$GarageQual[all.prep$BsmtFinType2 == "NA"] <- 1
all.prep$GarageQual[is.na(all.prep$GarageQual)] <- 1
all.prep$GarageQual <- as.integer(all.prep$GarageQual)

#unique(all.prep$GarageCond)
all.prep$GarageCond[all.prep$GarageCond == "Ex"] <- 2
all.prep$GarageCond[all.prep$GarageCond == "Gd"] <- 2
all.prep$GarageCond[all.prep$GarageCond == 'TA'] <- 2
all.prep$GarageCond[all.prep$GarageCond == 'Fa'] <- 2
all.prep$GarageCond[all.prep$GarageCond == 'Po'] <- 1
all.prep$GarageCond[all.prep$GarageCond == "NA"] <- 1
all.prep$GarageCond[is.na(all.prep$GarageCond)] <- 1
all.prep$GarageCond <- as.integer(all.prep$GarageCond)

#unique(all.prep$PavedDrive)
all.prep$PavedDrive[all.prep$PavedDrive == "Y"] <- 2
all.prep$PavedDrive[all.prep$PavedDrive == "P"] <- 1
all.prep$PavedDrive[all.prep$PavedDrive == "N"] <- 1
all.prep$PavedDrive[is.na(all.prep$PavedDrive)] <- 1
all.prep$PavedDrive <- as.integer(all.prep$PavedDrive)

#unique(all.prep$MoSold)
all.prep$MoSold[all.prep$MoSold %in% c(10,11,12)] <- 1  # seasonality
all.prep$MoSold[all.prep$MoSold %in% c(7,8,9)] <- 2
all.prep$MoSold[all.prep$MoSold %in% c(4,5,6)] <- 2
all.prep$MoSold[all.prep$MoSold %in% c(1,2,3)] <- 1
all.prep$MoSold[is.na(all.prep$MoSold)] <- 1
all.prep$MoSold <- as.integer(all.prep$MoSold)

#sum(is.na(all.prep))
#all.prep <- na.omit(all.prep)
train.org.prep <- all.prep[1:rows,]
test.org.prep  <- all.prep[(rows+1):nrow(all.prep),-64]

write.csv(train.org.prep,'data/train.org.prep.csv')
write.csv(test.org.prep ,'data/test.org.prep.csv' )
