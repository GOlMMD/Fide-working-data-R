file_path8 <- 'A:\\payan_name\\fide_data\\2016\\csv16\\checkup\\merged_data\\clean\\covid\\'
setwd(file_path8)
getwd()
BCNFinalB <- readRDS("BCNFinalB.rds")
UBCNFinalB <- readRDS("UBCNFinalB.rds")
BCNFinalR<-readRDS("BCNFinalR.rds")
BCNFinalS<-readRDS("BCNFinalS.rds")
BCNFinal<- readRDS("BCNFinal.rds")
PackageNames <- c("tidyverse", "stargazer", "magrittr", "haven", "plm")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
} 
model_olsR <- plm(formula = Rapid_Rating ~ Year + Month + Fed + Age_EachYear + covid_dummy + covid_dummy * Sex + Sex + 1 , 
                  data = NFinalR, 
                  index = c("ID.Number", "Month_Rapid"), # c(group index, time index)
                  model = "pooling")  
summary(model_olsR)
model_olsRC <- plm(formula = Rapid_Rating ~  covid_dummy * Sex + covid_dummy  + Sex+Year + Month + Age_EachYear + 1 , 
                   data = BCNFinalR, 
                   index = c("ID.Number", "time"), # c(group index, time index)
                   model = "pooling")  
summary(model_olsRC)
# Define the cluster variable
cluster_var <- c("covid_dummy", "Sex", "Year", "Month", "Age_EachYear")

# Compute clustered standard errors for specific variables
coeftest(model_olsRC, vcov. = function(x) vcovHC(x, cluster = ~ covid_dummy + Sex + Year 
                                                 + Month + Age_EachYear, data = BCNFinalR))
model_olsRC <- plm(formula = Rapid_Rating ~ covid_dummy * Sex + covid_dummy + Sex + Year + Month + Age_EachYear + 1,
                   data = BCNfinalR,
                   index = c("ID.Number", "time"),
                   model = "pooling")

# List of variables for which to calculate clustered standard errors
vars_to_cluster <- c("covid_dummy", "Sex", "covid_dummy:Sex", "Year", "Month", "Age_EachYear")

# Compute clustered standard errors for each variable separately
clustered_se <- lapply(vars_to_cluster, function(var) {
  coeftest(model_olsRC, vcov. = function(x) vcovHC(x, cluster = "group"))
})

# Print or access the clustered standard errors for each variable
names(clustered_se) <- vars_to_cluster
clustered_se
model_olsS <- plm(formula = Standard_Rating ~ covid_dummyS * Sex + covid_dummyS  + Sex+ Year + Month  + Age_EachYear  + 1 , 
                  data = NFinalS, 
                  index = c("ID.Number", "time"), # c(group index, time index)
                  model = "pooling")  
summary(model_olsS)
BCNFinalS<- readRDS("BCNFinalS.rds")
length(unique(BCNFinalS$ID.Number))
names(BCNFinalS) <- c("ID.Number"   ,    "Month_Standard" , "Standard_Rating" , "Standard_Gms"  ,   "Standard_K"   ,   "Sex"     ,      "Name"      ,     
                      "B.day"       ,    "Fed"      ,       "Flag"      ,      "Gamecat"    ,     "Year"      ,      "Month"    ,       "covid_dummy"   ,
                      "Age_EachYear"  ,  "time" )
model_olsSC <- plm(formula = Standard_Rating ~  covid_dummy * Sex + covid_dummy  + Sex+Year + Month + Age_EachYear + 1+Fed , 
                   data = BCNFinalS, 
                   index = c("ID.Number", "time"), # c(group index, time index)
                   model = "pooling")  
summary(model_olsSC)

model_olsFCG <- plm(formula = Rating ~  Gamecat+ covid_dummy * Sex + covid_dummy  + Sex+Year + Month + Age_EachYear + 1 , 
                    data = BCNFinal, 
                    index = c("ID.Number", "time"), 
                    model = "pooling")  # c(group index, time index)
summary(model_olsFCG)
# Check for duplicate combinations of (id-time)
max(table(index(BCNFinal), useNA = "ifany"))
model_olsF <- plm(formula = Rating ~ covid_dummy * Sex + covid_dummy  + Sex+ Year + Month  + Age_EachYear + Gamecat + 1 + Fed , 
                  data = NFinal, 
                  index = c("ID.Number", "time"), # c(group index, time index)
                  model = "pooling")  
summary(model_olsF)