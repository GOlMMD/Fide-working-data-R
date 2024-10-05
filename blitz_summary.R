# Install and load required libraries
install.packages("extrafont")
install.packages("ggtext")
library(extrafont)
library(ggtext)

# Font import and initialization

font_import(pattern = "YourPath/B Nazanin.ttf")  # Replace YourPath with the actual path
loadfonts()

# Code to set font in ggplot
ggplot() +  
  # Your other plot layers...
  
  labs(
    title = "میانگین رتبه بلیتز در طول زمان",
    title = element_textbox(width = 0.5, halign = 0.5, family = "B Nazanin")
  ) +
  # Other settings...
filepath4 <- 'A:\\payan_name\\fide_data\\2016\\csv16\\checkup\\merged_data\\clean\\covid\\'
setwd(filepath4)
UBCNFinalB <- readRDS("UBCNFinalB.rds")
UBCNFinalB$Year_Month <- as.Date(paste(UBCNFinalB$Year, UBCNFinalB$Month, "01", sep = "-"))
length(unique(UBCNFinalB$ID.Number))
#169893  of 664051 obs
mean( UBCNFinalB$Blitz_Rating)
#1706.67
sd( UBCNFinalB$Blitz_Rating)
#361.44
min( UBCNFinalB$Blitz_Rating)
#1000
max( UBCNFinalB$Blitz_Rating)
#2986
#########Male sumary after omit bio gender
MUBCNFinalB <- subset(UBCNFinalB, Sex == 0)
#582878 obs
min( MUBCNFinalB$Blitz_Rating)
#1000
max( MUBCNFinalB$Blitz_Rating)
#2986
mean( MUBCNFinalB$Blitz_Rating)
#1731.49
sd( MUBCNFinalB$Blitz_Rating)
#359.27
length(unique( MUBCNFinalB$ID.Number))
#151062
#########Female sumary after omit bio gender
FUBCNFinalB <- subset(UBCNFinalB, Sex == 1)
#81173 obs
min( FUBCNFinalB$Blitz_Rating)
#1000
max( FUBCNFinalB$Blitz_Rating)
#2676
mean( FUBCNFinalB$Blitz_Rating)
#1528.47
sd( FUBCNFinalB$Blitz_Rating)
#325.05
length(unique( FUBCNFinalB$ID.Number))
#18831
################For AGe of them #################333
dNFinal <- readRDS("dNFinal.rds")
#792799 obs
UBdNFinal <- dNFinal[(dNFinal$ID.Number %in% UBCNFinalB$ID.Number),]
#421117 
length(unique( UBdNFinal$ID.Number))
#169893  
UBdNFinal <-  UBdNFinal[!duplicated( UBdNFinal$ID.Number), ]
#169893
#filepath2 <-  paste0(filepath4, "BdNFinal.rds")
#saveRDS(BdNFinal, file = filepath2)
min(UBdNFinal$Min_Age)
#4
max(UBdNFinal$Min_Age)
#122
mean(UBdNFinal$Min_Age)
#28.55
sd(UBdNFinal$Min_Age)
#18.36
MUBdNFinal<- subset(UBdNFinal, Sex == 0)
min(MUBdNFinal$Min_Age)
#4
max(MUBdNFinal$Min_Age)
#122
mean(MUBdNFinal$Min_Age)
#29.87
sd(MUBdNFinal$Min_Age)
#18.56
#######Female age summary
FUBdNFinal<- subset(UBdNFinal, Sex == 1)
min(FUBdNFinal$Min_Age)
#5
max(FUBdNFinal$Min_Age)
#96
mean(FUBdNFinal$Min_Age)
#17.94
sd(FUBdNFinal$Min_Age)
#12.35
####
##############BEFORE & AFTER covid Blitz Rating#############

PUBCNFinalB <- subset(UBCNFinalB, covid_dummy == 0)
#430982 obs
mean(PUBCNFinalB$Blitz_Rating)
#1744.92
sd(PUBCNFinalB$Blitz_Rating)
#360.31 
# 
MPUBCNFinalB <- subset(PUBCNFinalB, Sex == 0)
#378614 obs 
mean(MPUBCNFinalB$Blitz_Rating)
#1770.19
sd(MPUBCNFinalB$Blitz_Rating)
#357.23
FPUBCNFinalB <- subset(PUBCNFinalB, Sex == 1)
#52368 obs
mean(FPUBCNFinalB$Blitz_Rating)
#1562.26
sd(FPUBCNFinalB$Blitz_Rating)
#328.35
#
#######AFTER COVID PANDAMic#############
AUBCNFinalB <- subset(UBCNFinalB, covid_dummy == 1)
#233069 obs
mean(AUBCNFinalB$Blitz_Rating)
#1635.94
#
sd(AUBCNFinalB$Blitz_Rating)
#352.76
#
MAUBCNFinalB <- subset(AUBCNFinalB, Sex == 0)
#204264 obs
mean(MAUBCNFinalB$Blitz_Rating)
#1659.76
#
sd(MAUBCNFinalB$Blitz_Rating)
#351.96 
#
FAUBCNFinalB <- subset(AUBCNFinalB, Sex == 1)
#28805 obs
mean(FAUBCNFinalB$Blitz_Rating)
#1467.05
#
sd(FAUBCNFinalB$Blitz_Rating)
#309.66
#
#####################ُBlitz summary and pooled ols AFTER WE OMIT month later than 202305 (BY May)####################
names(UBCNFinalB) <- c("ID.Number"   ,    "Month_Blitz" , "Blitz_Rating" , "Blitz_Gms"  ,   "Blitz_K"   ,   "Sex"     ,      "Name"      ,     
                        "B.day"       ,    "Fed"      ,       "Flag"         ,     "Year"      ,      "Month"    ,       "covid_dummy"   ,
                        "Age_EachYear"  ,  "time" )
names()
library(plm)
model_olsBC <- plm(formula = Blitz_Rating ~  covid_dummy * Sex + covid_dummy  + Sex+Year + Month + Age_EachYear + 1 , 
                   data = UBCNFinalB, 
                   index = c("ID.Number", "time"), # c(group index, time index)
                   model = "pooling")  
summary(model_olsBC)
model_olsYBC <- plm(formula = Blitz_Rating ~  covid_dummy * Sex + covid_dummy  + Sex+Year + Month + Age_EachYear + 1 , 
                   data = UBCNFinalB, 
                   index = c("ID.Number", "Year_Month"), # c(group index, time index)
                   model = "pooling")  
summary(model_olsYBC)
#########################t.test for blitz
t.test(PUBCNFinalB$Blitz_Rating,AUBCNFinalB$Blitz_Rating)
t.test(MPUBCNFinalB$Blitz_Rating,MAUBCNFinalB$Blitz_Rating)
t.test(FPUBCNFinalB$Blitz_Rating,FAUBCNFinalB$Blitz_Rating)
MAUBCNFinalB<- NULL
FAUBCNFinalB <- NULL
model_olsBC<- NULL
model_olsYBC<- NULL
AUBCNFinalB <- NULL
BDlitsBlack  <- NULL
BLSS1503<- NULL
BLSR1503<- NULL
MPUBCNFinalB<- NULL
MUBdNFinal<- NULL
FUBdNFinal<- NULL
FPUBCNFinalB<- NULL
