#when you want to use the initial data.
filepath2<-'A:\\payan_name\\fide_data\\2016\\csv16\\checkup'
setwd(filepath2)
getwd()
B1502.rds <- readRDS("B1502.rds")
R1502.rds <- readRDS("R1502.rds")
S1502.rds <- readRDS("S1502.rds")
unique(B1502.rds$Flag)
names(B1503)
#New work
B1503_demog<-B1503 %>% 
  select("ID.Number" , "Sex" , "Name" , "B.day" , "Fed" , "Flag" ) %>% 
  setnames(., c("ID.Number", "Sex", "Name", "B.day", "Fed", "Flag"), c("ID.Number", "SexB1503", "NameB1503", "B.dayB1503", "FedB1503", "FlagB1503"))
#ID_na <- sum(is.na(B1503_demog$ID.NumberB1503))

B2003_demog<-B2003 %>% 
  select("ID.Number" , "Sex" , "Name" , "B.day" , "Fed" , "Flag" ) %>% 
  setnames(., c("ID.Number", "Sex", "Name", "B.day", "Fed", "Flag"), c("ID.Number", "SexB2003", "NameB2003", "B.dayB2003", "FedB2003", "FlagB2003"))
#ID_na <- sum(is.na(B1503_demog$ID.NumberB1503))

#Demog Rbind
B1502_demog <- B1502.rds %>% 
  select("ID.Number" , "Sex" , "Name" , "B.day" , "Fed" , "Flag")#
R1502_demog <- R1502.rds %>% 
  select("ID.Number" , "Sex" , "Name" , "B.day" , "Fed" , "Flag")#
S1502_demog <- S1502.rds %>% 
  select("ID.Number" , "Sex" , "Name" , "B.day" , "Fed" , "Flag")#
# Combine the data frames and remove duplicates
combined_demog <- rbind(S1502_demog, R1502_demog, B1502_demog)
# combined_demog contain last obs
# Remove duplicate rows based on ID.Number
combined_demog <- combined_demog[!duplicated(combined_demog$ID.Number), ]
combined_demog <- left_join(combined_demog,DNAFinal, by = "ID.Number")

# Print the result
print(combined_demog)
saveRDS(combined_demog, file="combined_demog.rds")
combined_demog <- readRDS("combined_demog.rds")
combined_data <- readRDS("combined_data.rds")
# now it turns to produce the rest of our table
names( B1502_wide)
B1502_wide<-NULL
B1502.rds  
excol <- c("Name","Fed","Sex","Tit", "WTit","OTit", "B.day","Flag","FOA" )
B1502_wide<- B1502.rds[,! names(B1502.rds) %in% excol]
R1502_wide<- R1502.rds[,! names(B1502.rds) %in% excol]
S1502_wide<- S1502.rds[,! names(S1502.rds) %in% excol]
names(B1502_wide) <-c(  "ID.Number" , "B_X15.Feb"  , "B_GmsX15.Feb" , "B_KX15.Feb" , "B_X15.Mar" , "B_GmsX15.Mar" , "B_KX15.Mar" , "B_X15.Apr" , "B_GmsX15.Apr"   
                        , "B_KX15.Apr" , "B_X15.May" , "B_GmsX15.May" , "B_KX15.May" , "B_X15.Jun" , "B_GmsX15.Jun" , "B_KX15.Jun" , "B_X15.Jul" , "B_GmsX15.Jul"   
                        , "B_KX15.Jul" , "B_X15.Aug" , "B_GmsX15.Aug" , "B_KX15.Aug" , "B_X15.Sep" , "B_GmsX15.Sep" , "B_KX15.Sep" , "B_X15.Oct" , "B_GmsX15.Oct"   
                        , "B_KX15.Oct" , "B_X15.Nov" , "B_GmsX15.Nov" , "B_KX15.Nov" , "B_X15.Dec" , "B_GmsX15.Dec" , "B_KX15.Dec" , "B_X16.Jan" , "B_GmsX16.Jan"   
                        , "B_KX16.Jan" , "B_X16.Feb" , "B_GmsX16.Feb" , "B_KX16.Feb" , "B_X16.Mar" , "B_GmsX16.Mar" , "B_KX16.Mar" , "B_X16.Apr" , "B_GmsX16.Apr"   
                        , "B_KX16.Apr" , "B_X16.May" , "B_GmsX16.May" , "B_KX16.May" , "B_X16.Jun" , "B_GmsX16.Jun" , "B_KX16.Jun" , "B_X16.Jul" , "B_GmsX16.Jul"   
                        , "B_KX16.Jul" , "B_X16.Aug" , "B_GmsX16.Aug" , "B_KX16.Aug" , "B_X16.Sep" , "B_GmsX16.Sep" , "B_KX16.Sep" , "B_X16.Oct" , "B_GmsX16.Oct"   
                        , "B_KX16.Oct" , "B_X16.Nov" , "B_GmsX16.Nov" , "B_KX16.Nov" , "B_X16.Dec" , "B_GmsX16.Dec" , "B_KX16.Dec" , "B_X17.Jan" , "B_GmsX17.Jan"   
                        , "B_KX17.Jan" , "B_X17.Feb" , "B_GmsX17.Feb" , "B_KX17.Feb" , "B_X17.Mar" , "B_GmsX17.Mar" , "B_KX17.Mar" , "B_X17.Apr" , "B_GmsX17.Apr"   
                        , "B_KX17.Apr" , "B_X17.May" , "B_GmsX17.May" , "B_KX17.May" , "B_X17.Jun" , "B_GmsX17.Jun" , "B_KX17.Jun" , "B_X17.Jul" , "B_GmsX17.Jul"   
                        , "B_KX17.Jul" , "B_X17.Aug" , "B_GmsX17.Aug" , "B_KX17.Aug" , "B_X17.Sep" , "B_GmsX17.Sep" , "B_KX17.Sep" , "B_X17.Oct" , "B_GmsX17.Oct"   
                        , "B_KX17.Oct" , "B_X17.Nov" , "B_GmsX17.Nov" , "B_KX17.Nov" , "B_X17.Dec" , "B_GmsX17.Dec" , "B_KX17.Dec" , "B_X18.Jan" , "B_GmsX18.Jan"   
                        , "B_KX18.Jan" , "B_X18.Feb" , "B_GmsX18.Feb" , "B_KX18.Feb" , "B_X18.Mar" , "B_GmsX18.Mar" , "B_KX18.Mar" , "B_X18.Apr" , "B_GmsX18.Apr"   
                        , "B_KX18.Apr" , "B_X18.May" , "B_GmsX18.May" , "B_KX18.May" , "B_X18.Jun" , "B_GmsX18.Jun" , "B_KX18.Jun" , "B_X18.Jul" , "B_GmsX18.Jul"   
                        , "B_KX18.Jul" , "B_X18.Aug" , "B_GmsX18.Aug" , "B_KX18.Aug" , "B_X18.Sep" , "B_GmsX18.Sep" , "B_KX18.Sep" , "B_X18.Oct" , "B_GmsX18.Oct"   
                        , "B_KX18.Oct" , "B_X18.Nov" , "B_GmsX18.Nov" , "B_KX18.Nov" , "B_X18.Dec" , "B_GmsX18.Dec" , "B_KX18.Dec" , "B_X19.Jan" , "B_GmsX19.Jan"   
                        , "B_KX19.Jan" , "B_X19.Feb" , "B_GmsX19.Feb" , "B_KX19.Feb" , "B_X19.Mar" , "B_GmsX19.Mar" , "B_KX19.Mar" , "B_X19.Apr" , "B_GmsX19.Apr"   
                        , "B_KX19.Apr" , "B_X19.May" , "B_GmsX19.May" , "B_KX19.May" , "B_X19.Jun" , "B_GmsX19.Jun" , "B_KX19.Jun" , "B_X19.Jul" , "B_GmsX19.Jul"   
                        , "B_KX19.Jul" , "B_X19.Aug" , "B_GmsX19.Aug" , "B_KX19.Aug" , "B_X19.Sep" , "B_GmsX19.Sep" , "B_KX19.Sep" , "B_X19.Oct" , "B_GmsX19.Oct"   
                        , "B_KX19.Oct" , "B_X19.Nov" , "B_GmsX19.Nov" , "B_KX19.Nov" , "B_X19.Dec" , "B_GmsX19.Dec" , "B_KX19.Dec" , "B_X20.Jan" , "B_GmsX20.Jan"   
                        , "B_KX20.Jan" , "B_X20.Feb" , "B_GmsX20.Feb" , "B_KX20.Feb" , "B_X20.Mar" , "B_GmsX20.Mar" , "B_KX20.Mar" , "B_X20.Apr" , "B_GmsX20.Apr"   
                        , "B_KX20.Apr" , "B_X20.May" , "B_GmsX20.May" , "B_KX20.May" , "B_X20.Jun" , "B_GmsX20.Jun" , "B_KX20.Jun" , "B_X20.Jul" , "B_GmsX20.Jul"   
                        , "B_KX20.Jul" , "B_X20.Aug" , "B_GmsX20.Aug" , "B_KX20.Aug" , "B_X20.Sep" , "B_GmsX20.Sep" , "B_KX20.Sep" , "B_X20.Oct" , "B_GmsX20.Oct"   
                        , "B_KX20.Oct" , "B_X20.Nov" , "B_GmsX20.Nov" , "B_KX20.Nov" , "B_X20.Dec" , "B_GmsX20.Dec" , "B_KX20.Dec" , "B_X21.Jan" , "B_GmsX21.Jan"   
                        , "B_KX21.Jan" , "B_X21.Feb" , "B_GmsX21.Feb" , "B_KX21.Feb" , "B_X21.Mar" , "B_GmsX21.Mar" , "B_KX21.Mar" , "B_X21.Apr" , "B_GmsX21.Apr"   
                        , "B_KX21.Apr" , "B_X21.May" , "B_GmsX21.May" , "B_KX21.May" , "B_X21.Jun" , "B_GmsX21.Jun" , "B_KX21.Jun" , "B_X21.Jul" , "B_GmsX21.Jul"   
                        , "B_KX21.Jul" , "B_X21.Aug" , "B_GmsX21.Aug" , "B_KX21.Aug" , "B_X21.Sep" , "B_GmsX21.Sep" , "B_KX21.Sep" , "B_X21.Oct" , "B_GmsX21.Oct"   
                        , "B_KX21.Oct" , "B_X21.Nov" , "B_GmsX21.Nov" , "B_KX21.Nov" , "B_X21.Dec" , "B_GmsX21.Dec" , "B_KX21.Dec" , "B_X22.Jan" , "B_GmsX22.Jan"   
                        , "B_KX22.Jan" , "B_X22.Feb" , "B_GmsX22.Feb" , "B_KX22.Feb" , "B_X22.Mar" , "B_GmsX22.Mar" , "B_KX22.Mar" , "B_X22.Apr" , "B_GmsX22.Apr"   
                        , "B_KX22.Apr" , "B_X22.May" , "B_GmsX22.May" , "B_KX22.May" , "B_X22.Jun" , "B_GmsX22.Jun" , "B_KX22.Jun" , "B_X22.Jul" , "B_GmsX22.Jul"   
                        , "B_KX22.Jul" , "B_X22.Aug" , "B_GmsX22.Aug" , "B_KX22.Aug" , "B_X22.Sep" , "B_GmsX22.Sep" , "B_KX22.Sep" , "B_X22.Oct" , "B_GmsX22.Oct"   
                        , "B_KX22.Oct" , "B_X22.Nov" , "B_GmsX22.Nov" , "B_KX22.Nov" , "B_X22.Dec" , "B_GmsX22.Dec" , "B_KX22.Dec" , "B_X23.Jan" , "B_GmsX23.Jan"   
                        , "B_KX23.Jan" , "B_X23.Feb" , "B_GmsX23.Feb" , "B_KX23.Feb" , "B_X23.Mar" , "B_GmsX23.Mar" , "B_KX23.Mar" , "B_X23.Apr" , "B_GmsX23.Apr"   
                        , "B_KX23.Apr" , "B_X23.May" , "B_GmsX23.May" , "B_KX23.May" , "B_X23.Jun" , "B_GmsX23.Jun" , "B_KX23.Jun" , "B_X23.Jul" , "B_GmsX23.Jul"  
                        , "B_KX23.Jul" )  

#start fide number 2 end 305 by 3

B1502_wide$B_X23.Mar <- as.integer(gsub("[^0-9]+", "", B1502_wide$B_X23.Mar))
column_classes <- sapply(B1502_wide, class)
character_columns <- names(column_classes[column_classes == "character"])
B1502_wide$B_X23.Mar
Fid_seq <-seq(2,307,by=3)
Gam <- seq(3,307,by=3) 
K <- seq(4,307,by=3)
library(dplyr)
library(data.table)
library(tidyverse)
B1502_wide_clean <- names(B1502_wide) %>%
  B1502_wide_clean <- B1502_wide[, c("ID.Number", names(B1502_wide)[Fid_seq])]  %>% 
  pivot_longer( cols = -ID.Number,
                names_to = "Month_Blitz",
                values_to = "Blitz_Rating"
  )

library(dplyr)

Fid_seq <- seq(2, 307, by = 3)
B1502_wide_clean <- B1502_wide %>%
  select(ID.Number, all_of(Fid_seq)) %>% 
  pivot_longer( cols = -ID.Number,
                names_to = "Month_Blitz",
                values_to = "Blitz_Rating"
  )
NB1502_wide_clean<-na.omit(B1502_wide_clean) 
Gam <- seq(3,307,by=3) 
B1502_wide_clean_Gms <- B1502_wide %>%
  select(ID.Number, all_of(Gam)) %>% 
  pivot_longer( cols = -ID.Number,
                names_to = "Month_BG",
                values_to = "Blitz_Gms"
  )
NB1502_wide_clean_Gms<-na.omit(B1502_wide_clean_Gms) 
K <- seq(4,307,by=3)
B1502_wide_clean_K <- B1502_wide %>%
  select(ID.Number, all_of(K)) %>% 
  pivot_longer( cols = -ID.Number,
                names_to = "Month_BK",
                values_to = "Blitz_K"
  )
str(NB1502_wide_clean)
NB1502_wide_clean_K<-na.omit(B1502_wide_clean_K) 
B1502_long<- merge(B1502_wide_clean, B1502_wide_clean_Gms , by = "ID.Number", all = TRUE)
B1502_final <- merge(B1502_long,B1502_wide_clean_K , by = "ID.Number", all = TRUE)
B1502_long <- merge(NB1502_wide_clean, NB1502_wide_clean_Gms, all = TRUE)
#unfortunately, I couldn't do that as the error occurred : Negative series

# Select the ID.Number column from one of the data frames (assuming they are the same)
result <- B1502_wide_clean["ID.Number"]

# Merge the other columns into the result data frame
result<- cbind(result, B1502_wide_clean[, -1], B1502_wide_clean_Gms[, -1], B1502_wide_clean_K[, -1])

# Rename the columns if needed
colnames(result) <- c("ID.Number","Month_Blitz", "Blitz_Rating", "Month_BG", "Blitz_Gms",  "Month_BK", "Blitz_K")
saveRDS(result, file="result.rds")
#NBresult <- na.omit(result$Blitz_Rating)
NBresult <- subset(result,!is.na(Blitz_Rating))
saveRDS(NBresult, file="NBresult.rds")
unique(NBresult$Blitz_K)
# contains Na
unique(NBresult$Blitz_Gms)
#without NA
# Now, 'result' contains the merged data with the desired order
names(R1502_wide)<-c(  "ID.Number","R_X15.Feb" , "R_GmsX15.Feb", "R_KX15.Feb", "R_X15.Mar", "R_GmsX15.Mar", "R_KX15.Mar", "R_X15.Apr", "R_GmsX15.Apr",
                       "R_KX15.Apr", "R_X15.May", "R_GmsX15.May", "R_KX15.May", "R_X15.Jun", "R_GmsX15.Jun", "R_KX15.Jun", "R_X15.Jul",
                       "R_GmsX15.Jul", "R_KX15.Jul", "R_X15.Aug", "R_GmsX15.Aug", "R_KX15.Aug", "R_X15.Sep", "R_GmsX15.Sep", "R_KX15.Sep",
                       "R_X15.Oct", "R_GmsX15.Oct", "R_KX15.Oct", "R_X15.Nov", "R_GmsX15.Nov", "R_KX15.Nov", "R_X15.Dec", "R_GmsX15.Dec",
                       "R_KX15.Dec", "R_X16.Jan", "R_GmsX16.Jan", "R_KX16.Jan", "R_X16.Feb", "R_GmsX16.Feb", "R_KX16.Feb", "R_X16.Mar",
                       "R_GmsX16.Mar", "R_KX16.Mar", "R_X16.Apr", "R_GmsX16.Apr", "R_KX16.Apr", "R_X16.May", "R_GmsX16.May", "R_KX16.May",
                       "R_X16.Jun", "R_GmsX16.Jun", "R_KX16.Jun", "R_X16.Jul", "R_GmsX16.Jul", "R_KX16.Jul", "R_X16.Aug", "R_GmsX16.Aug",
                       "R_KX16.Aug", "R_X16.Sep", "R_GmsX16.Sep", "R_KX16.Sep", "R_X16.Oct", "R_GmsX16.Oct", "R_KX16.Oct", "R_X16.Nov",
                       "R_GmsX16.Nov", "R_KX16.Nov", "R_X16.Dec", "R_GmsX16.Dec", "R_KX16.Dec", "R_X17.Jan", "R_GmsX17.Jan", "R_KX17.Jan",
                       "R_X17.Feb", "R_GmsX17.Feb", "R_KX17.Feb", "R_X17.Mar", "R_GmsX17.Mar", "R_KX17.Mar", "R_X17.Apr", "R_GmsX17.Apr",
                       "R_KX17.Apr", "R_X17.May", "R_GmsX17.May", "R_KX17.May", "R_X17.Jun", "R_GmsX17.Jun", "R_KX17.Jun", "R_X17.Jul",
                       "R_GmsX17.Jul", "R_KX17.Jul", "R_X17.Aug", "R_GmsX17.Aug", "R_KX17.Aug", "R_X17.Sep", "R_GmsX17.Sep", "R_KX17.Sep",
                       "R_X17.Oct", "R_GmsX17.Oct", "R_KX17.Oct", "R_X17.Nov", "R_GmsX17.Nov", "R_KX17.Nov", "R_X17.Dec", "R_GmsX17.Dec",
                       "R_KX17.Dec", "R_X18.Jan", "R_GmsX18.Jan", "R_KX18.Jan", "R_X18.Feb", "R_GmsX18.Feb", "R_KX18.Feb", "R_X18.Mar",
                       "R_GmsX18.Mar", "R_KX18.Mar", "R_X18.Apr", "R_GmsX18.Apr", "R_KX18.Apr", "R_X18.May", "R_GmsX18.May", "R_KX18.May",
                       "R_X18.Jun", "R_GmsX18.Jun", "R_KX18.Jun", "R_X18.Jul", "R_GmsX18.Jul", "R_KX18.Jul", "R_X18.Aug", "R_GmsX18.Aug",
                       "R_KX18.Aug", "R_X18.Sep", "R_GmsX18.Sep", "R_KX18.Sep", "R_X18.Oct", "R_GmsX18.Oct", "R_KX18.Oct", "R_X18.Nov",
                       "R_GmsX18.Nov", "R_KX18.Nov", "R_X18.Dec", "R_GmsX18.Dec", "R_KX18.Dec", "R_X19.Jan", "R_GmsX19.Jan", "R_KX19.Jan",
                       "R_X19.Feb", "R_GmsX19.Feb", "R_KX19.Feb", "R_X19.Mar", "R_GmsX19.Mar", "R_KX19.Mar", "R_X19.Apr", "R_GmsX19.Apr",
                       "R_KX19.Apr", "R_X19.May", "R_GmsX19.May", "R_KX19.May", "R_X19.Jun", "R_GmsX19.Jun", "R_KX19.Jun", "R_X19.Jul",
                       "R_GmsX19.Jul", "R_KX19.Jul", "R_X19.Aug", "R_GmsX19.Aug", "R_KX19.Aug", "R_X19.Sep", "R_GmsX19.Sep", "R_KX19.Sep",
                       "R_X19.Oct", "R_GmsX19.Oct", "R_KX19.Oct", "R_X19.Nov", "R_GmsX19.Nov", "R_KX19.Nov", "R_X19.Dec", "R_GmsX19.Dec",
                       "R_KX19.Dec", "R_X20.Jan", "R_GmsX20.Jan", "R_KX20.Jan", "R_X20.Feb", "R_GmsX20.Feb", "R_KX20.Feb", "R_X20.Mar",
                       "R_GmsX20.Mar", "R_KX20.Mar", "R_X20.Apr", "R_GmsX20.Apr", "R_KX20.Apr", "R_X20.May", "R_GmsX20.May", "R_KX20.May",
                       "R_X20.Jun", "R_GmsX20.Jun", "R_KX20.Jun", "R_X20.Jul", "R_GmsX20.Jul", "R_KX20.Jul", "R_X20.Aug", "R_GmsX20.Aug",
                       "R_KX20.Aug", "R_X20.Sep", "R_GmsX20.Sep", "R_KX20.Sep", "R_X20.Oct", "R_GmsX20.Oct", "R_KX20.Oct", "R_X20.Nov",
                       "R_GmsX20.Nov", "R_KX20.Nov", "R_X20.Dec", "R_GmsX20.Dec", "R_KX20.Dec", "R_X21.Jan", "R_GmsX21.Jan", "R_KX21.Jan",
                       "R_X21.Feb", "R_GmsX21.Feb", "R_KX21.Feb", "R_X21.Mar", "R_GmsX21.Mar", "R_KX21.Mar", "R_X21.Apr", "R_GmsX21.Apr",
                       "R_KX21.Apr", "R_X21.May", "R_GmsX21.May", "R_KX21.May", "R_X21.Jun", "R_GmsX21.Jun", "R_KX21.Jun", "R_X21.Jul",
                       "R_GmsX21.Jul", "R_KX21.Jul", "R_X21.Aug", "R_GmsX21.Aug", "R_KX21.Aug", "R_X21.Sep", "R_GmsX21.Sep" , "R_KX21.Sep",
                       "R_X21.Oct", "R_GmsX21.Oct", "R_KX21.Oct", "R_X21.Nov", "R_GmsX21.Nov", "R_KX21.Nov", "R_X21.Dec"    , "R_GmsX21.Dec",
                       "R_KX21.Dec", "R_X22.Jan", "R_GmsX22.Jan", "R_KX22.Jan", "R_X22.Feb", "R_GmsX22.Feb", "R_KX22.Feb"   , "R_X22.Mar",
                       "R_GmsX22.Mar", "R_KX22.Mar", "R_X22.Apr", "R_GmsX22.Apr", "R_KX22.Apr", "R_X22.May", "R_GmsX22.May" , "R_KX22.May",
                       "R_X22.Jun", "R_GmsX22.Jun", "R_KX22.Jun", "R_X22.Jul", "R_GmsX22.Jul", "R_KX22.Jul", "R_X22.Aug"    , "R_GmsX22.Aug",
                       "R_KX22.Aug", "R_X22.Sep", "R_GmsX22.Sep", "R_KX22.Sep", "R_X22.Oct", "R_GmsX22.Oct", "R_KX22.Oct"   , "R_X22.Nov",
                       "R_GmsX22.Nov", "R_KX22.Nov", "R_X22.Dec", "R_GmsX22.Dec", "R_KX22.Dec", "R_X23.Jan", "R_GmsX23.Jan" , "R_KX23.Jan",
                       "R_X23.Feb", "R_GmsX23.Feb", "R_KX23.Feb", "R_X23.Mar", "R_GmsX23.Mar", "R_KX23.Mar", "R_X23.Apr"    , "R_GmsX23.Apr",
                       "R_KX23.Apr", "R_X23.May", "R_GmsX23.May", "R_KX23.May", "R_X23.Jun", "R_GmsX23.Jun", "R_KX23.Jun"   , "R_X23.Jul", 
                       "R_GmsX23.Jul", "R_KX23.Jul")
#lets turn to rapid
R1502_wide$R_X23.Mar <- as.integer(gsub("[^0-9]+", "", R1502_wide$R_X23.Mar))
column_classes <- sapply(R1502_wide, class)
character_columns <- names(column_classes[column_classes == "character"])
library(dplyr)
Fid_seq <-seq(2,307,by=3)
R1502_wide_clean <- R1502_wide %>%
  select(ID.Number, all_of(Fid_seq )) %>% 
  pivot_longer( cols = -ID.Number,
                names_to = "Month_Rapid",
                values_to = "Rapid_Rating"
  )

NR1502_wide_clean<-na.omit(R1502_wide_clean) 
Gam <- seq(3,307,by=3) 
R1502_wide_clean_Gms <- R1502_wide %>%
  select(ID.Number, all_of(Gam)) %>% 
  pivot_longer( cols = -ID.Number,
                names_to = "Month_RG",
                values_to = "Rapid_Gms"
  )
NR1502_wide_clean_Gms<-na.omit(R1502_wide_clean_Gms) 
K <- seq(4,307,by=3)
R1502_wide_clean_K <- R1502_wide %>%
  select(ID.Number, all_of(K)) %>% 
  pivot_longer( cols = -ID.Number,
                names_to = "Month_RK",
                values_to = "Rapid_K"
  )
resultR <- R1502_wide_clean["ID.Number"]

# Merge the other columns into the result data frame
resultR<- cbind(resultR, R1502_wide_clean[, -1], R1502_wide_clean_Gms[, -1], R1502_wide_clean_K[, -1])

# Rename the columns if needed
colnames(resultR) <- c("ID.Number","Month_Rapid", "Rapid_Rating", "Month_RG", "Rapid_Gms",  "Month_RK", "Rapid_K")
saveRDS(resultR, file="resultR.rds")
NRresult <- subset(resultR,!is.na(Rapid_Rating))
saveRDS(NRresult, file="NRresult.rds")


unique(NRresult$Rapid_K)
# contains Na
unique(NRresult$Rapid_Gms)
##################
##################
# we can start standard
names( S1502_wide)<-       c( "ID.Number", "S_X15.Feb"  , "S_GmsX15.Feb", "S_KX15.Feb", "S_X15.Mar", "S_GmsX15.Mar", "S_KX15.Mar", "S_X15.Apr", "S_GmsX15.Apr",
                              "S_KX15.Apr" , "S_X15.May", "S_GmsX15.May" , "S_KX15.May", "S_X15.Jun", "S_GmsX15.Jun", "S_KX15.Jun", "S_X15.Jul",
                              "S_GmsX15.Jul", "S_KX15.Jul", "S_X15.Aug"  , "S_GmsX15.Aug", "S_KX15.Aug", "S_X15.Sep", "S_GmsX15.Sep", "S_KX15.Sep",
                              "S_X15.Oct", "S_GmsX15.Oct", "S_KX15.Oct"  , "S_X15.Nov", "S_GmsX15.Nov", "S_KX15.Nov", "S_X15.Dec", "S_GmsX15.Dec",
                              "S_KX15.Dec", "S_X16.Jan", "S_GmsX16.Jan"  , "S_KX16.Jan", "S_X16.Feb", "S_GmsX16.Feb", "S_KX16.Feb", "S_X16.Mar",
                              "S_GmsX16.Mar", "S_KX16.Mar", "S_X16.Apr"  , "S_GmsX16.Apr", "S_KX16.Apr", "S_X16.May", "S_GmsX16.May", "S_KX16.May",
                              "S_X16.Jun", "S_GmsX16.Jun", "S_KX16.Jun"  , "S_X16.Jul", "S_GmsX16.Jul", "S_KX16.Jul", "S_X16.Aug", "S_GmsX16.Aug",
                              "S_KX16.Aug", "S_X16.Sep", "S_GmsX16.Sep"  , "S_KX16.Sep", "S_X16.Oct", "S_GmsX16.Oct", "S_KX16.Oct", "S_X16.Nov",
                              "S_GmsX16.Nov", "S_KX16.Nov", "S_X16.Dec"  , "S_GmsX16.Dec", "S_KX16.Dec", "S_X17.Jan", "S_GmsX17.Jan", "S_KX17.Jan",
                              "S_X17.Feb", "S_GmsX17.Feb", "S_KX17.Feb"  , "S_X17.Mar", "S_GmsX17.Mar", "S_KX17.Mar", "S_X17.Apr", "S_GmsX17.Apr",
                              "S_KX17.Apr", "S_X17.May", "S_GmsX17.May"  , "S_KX17.May", "S_X17.Jun", "S_GmsX17.Jun", "S_KX17.Jun", "S_X17.Jul",
                              "S_GmsX17.Jul", "S_KX17.Jul", "S_X17.Aug"  , "S_GmsX17.Aug", "S_KX17.Aug", "S_X17.Sep", "S_GmsX17.Sep", "S_KX17.Sep",
                              "S_X17.Oct", "S_GmsX17.Oct", "S_KX17.Oct"  , "S_X17.Nov", "S_GmsX17.Nov", "S_KX17.Nov", "S_X17.Dec", "S_GmsX17.Dec",
                              "S_KX17.Dec", "S_X18.Jan", "S_GmsX18.Jan"  , "S_KX18.Jan", "S_X18.Feb", "S_GmsX18.Feb", "S_KX18.Feb", "S_X18.Mar",
                              "S_GmsX18.Mar", "S_KX18.Mar", "S_X18.Apr", "S_GmsX18.Apr", "S_KX18.Apr", "S_X18.May", "S_GmsX18.May", "S_KX18.May",
                              "S_X18.Jun", "S_GmsX18.Jun", "S_KX18.Jun", "S_X18.Jul", "S_GmsX18.Jul", "S_KX18.Jul", "S_X18.Aug", "S_GmsX18.Aug",
                              "S_KX18.Aug", "S_X18.Sep", "S_GmsX18.Sep", "S_KX18.Sep", "S_X18.Oct", "S_GmsX18.Oct", "S_KX18.Oct", "S_X18.Nov",
                              "S_GmsX18.Nov", "S_KX18.Nov", "S_X18.Dec", "S_GmsX18.Dec", "S_KX18.Dec", "S_X19.Jan", "S_GmsX19.Jan", "S_KX19.Jan",
                              "S_X19.Feb", "S_GmsX19.Feb", "S_KX19.Feb", "S_X19.Mar", "S_GmsX19.Mar", "S_KX19.Mar", "S_X19.Apr", "S_GmsX19.Apr",
                              "S_KX19.Apr", "S_X19.May", "S_GmsX19.May", "S_KX19.May", "S_X19.Jun", "S_GmsX19.Jun", "S_KX19.Jun", "S_X19.Jul",
                              "S_GmsX19.Jul", "S_KX19.Jul", "S_X19.Aug", "S_GmsX19.Aug", "S_KX19.Aug", "S_X19.Sep", "S_GmsX19.Sep", "S_KX19.Sep",
                              "S_X19.Oct", "S_GmsX19.Oct", "S_KX19.Oct", "S_X19.Nov", "S_GmsX19.Nov", "S_KX19.Nov", "S_X19.Dec", "S_GmsX19.Dec",
                              "S_KX19.Dec", "S_X20.Jan", "S_GmsX20.Jan", "S_KX20.Jan", "S_X20.Feb", "S_GmsX20.Feb", "S_KX20.Feb", "S_X20.Mar",
                              "S_GmsX20.Mar", "S_KX20.Mar", "S_X20.Apr", "S_GmsX20.Apr", "S_KX20.Apr", "S_X20.May", "S_GmsX20.May", "S_KX20.May",
                              "S_X20.Jun", "S_GmsX20.Jun", "S_KX20.Jun", "S_X20.Jul", "S_GmsX20.Jul", "S_KX20.Jul", "S_X20.Aug", "S_GmsX20.Aug",
                              "S_KX20.Aug", "S_X20.Sep", "S_GmsX20.Sep", "S_KX20.Sep", "S_X20.Oct", "S_GmsX20.Oct", "S_KX20.Oct", "S_X20.Nov",
                              "S_GmsX20.Nov", "S_KX20.Nov", "S_X20.Dec", "S_GmsX20.Dec", "S_KX20.Dec", "S_X21.Jan", "S_GmsX21.Jan", "S_KX21.Jan",
                              "S_X21.Feb", "S_GmsX21.Feb", "S_KX21.Feb", "S_X21.Mar", "S_GmsX21.Mar", "S_KX21.Mar", "S_X21.Apr", "S_GmsX21.Apr",
                              "S_KX21.Apr", "S_X21.May", "S_GmsX21.May", "S_KX21.May", "S_X21.Jun", "S_GmsX21.Jun", "S_KX21.Jun", "S_X21.Jul",
                              "S_GmsX21.Jul", "S_KX21.Jul", "S_X21.Aug", "S_GmsX21.Aug", "S_KX21.Aug", "S_X21.Sep", "S_GmsX21.Sep", "S_KX21.Sep",
                              "S_X21.Oct", "S_GmsX21.Oct", "S_KX21.Oct", "S_X21.Nov", "S_GmsX21.Nov", "S_KX21.Nov", "S_X21.Dec", "S_GmsX21.Dec",
                              "S_KX21.Dec", "S_X22.Jan", "S_GmsX22.Jan", "S_KX22.Jan", "S_X22.Feb", "S_GmsX22.Feb", "S_KX22.Feb", "S_X22.Mar",
                              "S_GmsX22.Mar", "S_KX22.Mar", "S_X22.Apr", "S_GmsX22.Apr", "S_KX22.Apr", "S_X22.May", "S_GmsX22.May", "S_KX22.May",
                              "S_X22.Jun", "S_GmsX22.Jun", "S_KX22.Jun", "S_X22.Jul", "S_GmsX22.Jul", "S_KX22.Jul", "S_X22.Aug", "S_GmsX22.Aug",
                              "S_KX22.Aug", "S_X22.Sep", "S_GmsX22.Sep", "S_KX22.Sep", "S_X22.Oct", "S_GmsX22.Oct", "S_KX22.Oct", "S_X22.Nov",
                              "S_GmsX22.Nov", "S_KX22.Nov", "S_X22.Dec", "S_GmsX22.Dec", "S_KX22.Dec", "S_X23.Jan", "S_GmsX23.Jan", "S_KX23.Jan",
                              "S_X23.Feb" , "S_GmsX23.Feb", "S_KX23.Feb", "S_X23.Mar", "S_GmsX23.Mar", "S_KX23.Mar", "S_X23.Apr", "S_GmsX23.Apr",
                              "S_KX23.Apr", "S_X23.May", "S_GmsX23.May", "S_KX23.May", "S_X23.Jun", "S_GmsX23.Jun", "S_KX23.Jun",   "S_X23.Jul",
                              "S_GmsX23.Jul",   "S_KX23.Jul" )

S1502_wide$S_X23.Feb  <- as.integer(gsub("[^0-9]+", "", S1502_wide$S_X23.Feb ))
#S1502_wide$S_X19.Apr  <- as.integer(gsub("[^0-9]+", "", S1502_wide$S_X23.Feb ))
# Assuming your data frame is named 'your_data'
selected_columns <- c("S_X19.Mar", "S_X19.Apr", "S_X19.May", "S_X19.Jun", "S_X19.Jul",
                      "S_X19.Aug", "S_X19.Sep", "S_X19.Oct", "S_X19.Nov", "S_X19.Dec",
                      "S_X20.Jan", "S_X20.Feb", "S_X20.Dec", "S_X21.Jan", "S_X21.Feb",
                      "S_X21.Mar", "S_X23.Feb" )
# Select and print the specified columns
view(S1502_wide[selected_columns])
result <- any(sapply(S1502_wide[selected_columns], function(col) any(!is.na(col) & !grepl("^\\d+$", col))))

if (result) {
  cat("There are non-numeric values (excluding NA) in the specified columns.\n")
} else {
  cat("All values in the specified columns are either NA or numeric.\n")
}
#There are non-numeric values (excluding NA) in the specified columns.

# Loop through the columns and clean them
for (col_name in selected_columns ) {
  S1502_wide[[col_name]] <- as.integer(gsub("[^0-9]+", "", S1502_wide[[col_name]]))
}

# Now, all the selected columns should contain only numeric values.
column_classes2 <- sapply(S1502_wide[selected_columns], class)
#all of them are intiger
column_classes <- sapply(S1502_wide, class)
character_columns <- names(column_classes[column_classes == "character"])
#controversy
# Check if there are non-numeric characters in each column
result <- any(sapply(S1502_wide[selected_columns], function(col) any(!is.na(col) & !grepl("^\\d+$", col))))

# Print the result
print(result)
#the result is false so well done :))
Fid_seq <- seq(2,307,by=3) 
S1502_wide_clean <- S1502_wide %>%
  select(ID.Number, all_of(Fid_seq ))  %>% 
  pivot_longer( cols = -ID.Number,
                names_to = "Month_Standard",
                values_to = "Standard_Rating"
  )

NS1502_wide_clean<-na.omit(S1502_wide_clean) 
Gam <- seq(3,307,by=3) 
S1502_wide_clean_Gms <- S1502_wide %>%
  select(ID.Number, all_of(Gam)) %>% 
  pivot_longer( cols = -ID.Number,
                names_to = "Month_SG",
                values_to = "Standard_Gms"
  )
NS1502_wide_clean_Gms<-na.omit(S1502_wide_clean_Gms) 
K <- seq(4,307,by=3)
S1502_wide_clean_K <- S1502_wide %>%
  select(ID.Number, all_of(K)) %>% 
  pivot_longer( cols = -ID.Number,
                names_to = "Month_SK",
                values_to = "Standard_K"
  )


resultS <- S1502_wide_clean["ID.Number"]

# Merge the other columns into the result data frame
resultS<- cbind(resultS, S1502_wide_clean[, -1], S1502_wide_clean_Gms[, -1], S1502_wide_clean_K[, -1])

# Rename the columns if needed
colnames(resultS) <- c("ID.Number","Month_Standard", "Standard_Rating", "Month_SG", "Standard_Gms",  "Month_SK", "Standard_K")
saveRDS(resultS, file="resultS.rds")
NSresult <- subset(resultS,!is.na(Standard_Rating))
saveRDS(NSresult, file="NSresult.rds")
unique(NSresult$Standard_K)
# contains Na
unique(NSresult$Standard_Gms)
# whithout NA values
max(NSresult$Standard_Gms)
#max 168
#for the next time(1) we should run read these file NSresult,NRresult,NBresult and combined_demog
NRresult <-readRDS("NRresult.rds")
NBresult <-readRDS("NBresult.rds")
NSresult <-readRDS("NSresult.rds")
combined_demog <- readRDS("combined_demog.rds")
#532197
Ncombined_demog <-na.omit(combined_demog )
#less than 2000 than 1% 
combined_data <- readRDS("combined_data.rds")
# Assuming your data frames are named df1 (for the time-varying data) and df2 (for the static data)

# Load the dplyr library if it's not already loaded
library(dplyr)
library(stargazer)
install.packages("stargazer")
PackageNames <- c("tidyverse", "stargazer", "magrittr", "haven")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}


# Summarize house prices near and far from incinerator, and before and after
dNFinal <- readRDS("dNFinal.rds")
names(NFinal)
dNFinalR <- NFinalR %>% 
  select("ID.Number" ,"Sex" , "B.day", "Flag", "Fed","Name"  )
dNFinalR <-  dNFinalR[!duplicated( dNFinalR$ID.Number), ]

dNFinalB <- NFinalB %>% 
  select("ID.Number" ,"Sex" , "B.day", "Flag", "Fed", "Name" )
dNFinalB <-  dNFinalB[!duplicated( dNFinalB$ID.Number), ]
#905812 -> 178887
dNFinalS <- NFinalS %>% 
  select("ID.Number" ,"Sex" , "B.day", "Flag", "Fed","Name"  )
dNFinalS <-  dNFinalS[!duplicated( dNFinalS$ID.Number), ]
#3650138 -> 352379
dNFinal <- rbind(dNFinalS,dNFinalR,dNFinalB)
dNFinal <- left_join(dNFinal,DNAFinal, by = "ID.Number")
file_path15 <- paste0(directory_path, "dNFinal.rds")
saveRDS(dNFinal, file = file_path15)
DNFinalR <- NFinalR %>% 
  select("ID.Number" ,"Sex" , "Flag" )
DNFinalR <-  DNFinalR[!duplicated( DNFinalR$ID.Number), ]
DNFinalR <- left_join(DNFinalR,min_age_each_year, by = "ID.Number")
names(DNFinalR) <- c("ID.Number" , "Sex" , "Flag", "Min_Age")
DNFinal<- rbind(DNFinalS,DNFinalR,DNFinalB)
#792799
DNFinal <- DNFinal[!duplicated(DNFinal$ID.Number), ]
#464204
DNAFinal <- DNFinal %>% 
  select("ID.Number" , "Min_Age")
names(DNFinal)
#Summary and aggregate also pooled ols

DNFinalR %>% str
DNFinalR %>% stargazer(type = "text")
result <- aggregate(Min_Age ~ Sex, data = DNFinalR, FUN = function(x) c(Mean = mean(x), Variance = var(x), Count = length(x))) 
resultNFR <- aggregate(Rapid_Rating ~ Sex + covid_dummy, data = NFinalR, FUN = function(x) c(Mean = mean(x), Variance = var(x), Count = length(x))) 
print(resultNFR)
print(result)
# Pooled OLS estimator
#NFinalR
model_olsR <- plm(formula = Rapid_Rating ~ Year + Month + Fed + Age_EachYear + covid_dummy + covid_dummy * Sex + Sex + 1 , 
                  data = NFinalR, 
                  index = c("ID.Number", "Month_Rapid"), # c(group index, time index)
                  model = "pooling")  
summary(model_olsR)
group_by(Sex) %>%
  summarize_at(.vars = vars(Min_Age), 
               .funs = list(mean = mean, obs = length))
################Rapid covid 202305##############
############## we should omit months after 202305.###############
file_path5 <- 'A:\\payan_name\\fide_data\\2016\\csv16\\checkup\\merged_data\\clean\\'
setwd(file_path5)
getwd()
filepath4 <- 'A:\\payan_name\\fide_data\\2016\\csv16\\checkup\\merged_data\\clean\\covid\\'
setwd(filepath4)
filepath17 <-  paste0(filepath4, "CNFinalR.rds")
saveRDS(CNFinalR, file = filepath17)
filepath18 <-  paste0(filepath4, "BCNFinalR.rds")
saveRDS(BCNFinalR, file = filepath18)
BCNFinalR <- readRDS("BCNFinalR.rds")
UBCNFinalB <- readRDS("UBCNFinalB.rds")

NFinalR <- readRDS("NFinalR.rds")
BLSR1503 <- readRDS("BLSR1503.rds")
#1245659 obs
CNFinalR <- NFinalR %>% 
  filter(time < 202305)
#1170115 obs
#########we should omit bio country and Gender
BCNFinalR <- CNFinalR[!(CNFinalR$ID.Number %in% BLSR1503$ID.Number),]
#1162258 obs
length(unique(BCNFinalR$ID.Number))
#247289
mean( BCNFinalR$Rapid_Rating)
#1605.841
sd( BCNFinalR$Rapid_Rating)
#355.1499
min( BCNFinalR$Rapid_Rating)
#1000
max( BCNFinalR$Rapid_Rating)
#2919
#########Male sumary after omit bio gender
MBCNFinalR <- subset(BCNFinalR, Sex == 0)
min( MBCNFinalR$Rapid_Rating)
#1000
max( MBCNFinalR$Rapid_Rating)
#2919
mean( MBCNFinalR$Rapid_Rating)
#1627.569
sd( MBCNFinalR$Rapid_Rating)
#354.8381
length(unique( MBCNFinalR$ID.Number))
#221132
#########Female sumary after omit bio gender
FBCNFinalR <- subset(BCNFinalR, Sex == 1)
min( FBCNFinalR$Rapid_Rating)
#1000
max( FBCNFinalR$Rapid_Rating)
#2645
mean( FBCNFinalR$Rapid_Rating)
#1443.768
sd( FBCNFinalR$Rapid_Rating)
#313.04
length(unique( FBCNFinalR$ID.Number))
#26157
################For AGe of them #################333
dNFinal <- readRDS("dNFinal.rds")
RdNFinal <- dNFinal[(dNFinal$ID.Number %in% BCNFinalR$ID.Number),]
# 540608
length(unique( RdNFinal$ID.Number))
# 247289 unique Id number 
RdNFinal <-  RdNFinal[!duplicated( RdNFinal$ID.Number), ]
#247289
#filepath2 <-  paste0(filepath4, "BdNFinal.rds")
#saveRDS(BdNFinal, file = filepath2)
min(RdNFinal$Min_Age)
#3
max(RdNFinal$Min_Age)
#101
mean(RdNFinal$Min_Age)
#29.31727
sd(RdNFinal$Min_Age)
#19.02334
MRdNFinal<- subset(RdNFinal, Sex == 0)
min(MRdNFinal$Min_Age)
#3
max(MRdNFinal$Min_Age)
#101
mean(MRdNFinal$Min_Age)
#30.61301
sd(MRdNFinal$Min_Age)
#19.18871
#######Female age summary
FRdNFinal<- subset(RdNFinal, Sex == 1)
min(FRdNFinal$Min_Age)
#3
max(FRdNFinal$Min_Age)
#96
mean(FRdNFinal$Min_Age)
#18.36304
sd(FRdNFinal$Min_Age)
#13.20159
####



#######################that is the original kernel density population
library(ggplot2)
install.packages("kdensity")
library("kdensity")

# Organize density values and labels into a dataframe
density_values <- list(
  "Before Covid Overall" = density(PBCNFinalR$Rapid_Rating),
  "Before Covid Male" = density(MPBCNFinalR$Rapid_Rating),
  "Before Covid Female" = density(FPBCNFinalR$Rapid_Rating),
  "After Covid Overall" = density(ABCNFinalR$Rapid_Rating),
  "After Covid Male" = density(MABCNFinalR$Rapid_Rating),
  "After Covid Female" = density(FABCNFinalR$Rapid_Rating)
)

density_df <- do.call(rbind, lapply(names(density_values), function(name) {
  data.frame(
    x = density_values[[name]]$x,
    y = density_values[[name]]$y,
    Category = name
  )
}))

# Plot using ggplot
ggplot(density_df, aes(x = x, y = y, color = Category)) +
  geom_line() +
  labs(
    title = "Kernel Density Plot for Rapid Ratings",
    x = "Rapid Rating",
    y = "Density"
  ) +
  theme_minimal()
#######################ggplot rapid for thesis####
##################Rapid###########
BCNFinalR$Year_Month <- as.Date(paste(BCNFinalR$Year, BCNFinalR$Month, "01", sep = "-"))
#Male
Average_Rapid_Male <- MBCNFinalR %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Male = mean(Rapid_Rating))
#Female
Average_Rapid_Female <- FBCNFinalR %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Female = mean(Rapid_Rating))
#ggplot(Average_Rapid, aes(x = Year_Month, y = mean_rating)) +
ggplot()+  
  geom_line(data = Average_Rapid_Male, aes(x = Year_Month, y = mean_rating_Male, color = "مردها")) +
  geom_point(data = Average_Rapid_Male, aes(x = Year_Month, y = mean_rating_Male), color = "blue")  +
  geom_line(data = Average_Rapid_Female, aes(x =Year_Month, y = mean_rating_Female, color = "زن ها")) +
  geom_point(data = Average_Rapid_Female, aes(x = Year_Month, y = mean_rating_Female), color = "red") +
  geom_line() +
  scale_color_manual(values = c("مردها" = "blue", "زن ها" = "red")) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  labs(title = "متوسط سالانه درجه بین المللی زنان و مردان شطرنج باز در بازی سرعتی", x = "سال", y = "میانگین درجه بین المللی" , color="جنسیت") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month")

ggplot()+  
  geom_line(data = Average_Rapid_Male, aes(x = Year_Month, y = mean_rating_Male, color = "Male")) +
  geom_point(data = Average_Rapid_Male, aes(x = Year_Month, y = mean_rating_Male), color = "blue")  +
  geom_line(data = Average_Rapid_Female, aes(x =Year_Month, y = mean_rating_Female, color = "Female")) +
  geom_point(data = Average_Rapid_Female, aes(x = Year_Month, y = mean_rating_Female), color = "red") +
  geom_line() +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  labs(title = "Mean Rapid Rating Over Time", x = "Time", y = "Mean Rapid Rating" , color="Gender") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month")

######################Number of participates########


library(dplyr)
library(ggplot2)

library(ggplot2)

# Assuming the dataset is named 'BCNFinalR'
# Convert 'Sex' to a factor for better plotting aesthetics
BCNFinalR$Sex <- factor(BCNFinalR$Sex, levels = c(0, 1), labels = c("Male", "Female"))

# Aggregate unique participants based on year and sex
unique_participants <- BCNFinalR %>%
  group_by(Year, Sex) %>%
  summarise(UniqueParticipants = n_distinct(ID.Number)) %>%
  ungroup()

# Plotting using ggplot
# Plotting using ggplot
# Ensure 'Year' factor levels are in the correct order
unique_participants$Year <- factor(unique_participants$Year, levels = unique(as.character(unique_participants$Year)))

# Plotting using ggplot
ggplot(unique_participants, aes(x = Year, y = UniqueParticipants, color = Sex, group = Sex)) +
  geom_line() +
  labs(
    title = "Number of Unique Participants for Rapid games",
    x = "Year",
    y = "Number of Unique Participants"
  ) +
  theme_minimal()
####################################################
##############BEFORE & AFTER covid Rapid Rating#############

######################3
PBCNFinalR <- subset(BCNFinalR, covid_dummy == 0)
#734256 obs
mean(PBCNFinalR$Rapid_Rating)
#1647.073
sd(PBCNFinalR$Rapid_Rating)
#356.3235
MPBCNFinalR <- subset(PBCNFinalR, Sex == 0)
#647344
mean(MPBCNFinalR$Rapid_Rating)
#1670.192
sd(MPBCNFinalR$Rapid_Rating)
#354.7008
FPBCNFinalR <- subset(PBCNFinalR, Sex == 1)
#86912
mean(FPBCNFinalR$Rapid_Rating)
#1474.875
sd(FPBCNFinalR$Rapid_Rating)
#319.2612
#######AFTER COVID PANDAMic#############
ABCNFinalR <- subset(BCNFinalR, covid_dummy == 1)
#429978 obs   -> 428002 obs
coeftest(PBCNFinalR, ABCNFinalR,cluster=Rapid_Rating)
mean(ABCNFinalR$Rapid_Rating)
#1538.174 -> 1535.107
sd(ABCNFinalR$Rapid_Rating)
#344.6183 -> 341.7298
MABCNFinalR <- subset(ABCNFinalR, Sex == 0)
#377521 obs
names(PBCNFinalR)


mean(MABCNFinalR$Rapid_Rating)
#1557.371 -> 1554.481
sd(MABCNFinalR$Rapid_Rating)
#345.7827 -> 342.9583
FABCNFinalR <- subset(ABCNFinalR, Sex == 1)
#50481 obs
mean(FABCNFinalR$Rapid_Rating)
#1395.18   -> 1390.212
sd(FABCNFinalR$Rapid_Rating)
#299.304 -> 294.4301
ttest_generalR<- t.test(PBCNFinalR$Rapid_Rating,ABCNFinalR$Rapid_Rating)
ttest_maleR <- t.test(MABCNFinalR$Rapid_Rating,MPBCNFinalR$Rapid_Rating)
ttest_femaleR<- t.test(FPBCNFinalR$Rapid_Rating,FABCNFinalR$Rapid_Rating)
#####################Rapid summary and pooled ols AFTER WE OMIT month later than 202305 (BY May)####################
model_olsRC <- NULL
library(plm)
library(sandwich)
library(lmtest)
install.packages("lmtest")
install.packages("zoo")
install.packages("sandwich")
library(lmtest)
library(zoo)
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
####
##############
library(plm)
library(lmtest)

# Model specification
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
##############

# Merge the data frames using a left_join
FinalB <- left_join(NBresult,combined_demog, by = "ID.Number")
# Define the full path to the directory where you want to save the file
directory_path <- "A:\\payan_name\\fide_data\\2016\\csv16\\checkup\\merged_data\\clean\\"
setwd(directory_path)

getwd()
FinalB <- readRDS("FinalB.rds")
unique(FinalB$Month_Blitz)
FinalB$Gamecat="Blitz"
combined_demog$Bio <- ifelse(
  B1503$ID.Number == combined_demog$ID.Number,
  ifelse(B1503$Sex == combined_demog$Sex, "No", "Yes"),
  NA
)
PackageNames <- c("tidyverse", "stargazer", "magrittr", "haven", "plm")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
} 

unique(combined_demog$Bio)
covid_names <- c(
  "B_X20.Mar", "B_X20.Apr", "B_X20.May", "B_X20.Jun", "B_X20.Jul",
  "B_X20.Aug", "B_X20.Sep", "B_X20.Oct", "B_X20.Nov", "B_X20.Dec",
  "B_X21.Jan", "B_X21.Feb", "B_X21.Mar", "B_X21.Apr", "B_X21.May",
  "B_X21.Jun", "B_X21.Jul", "B_X21.Aug", "B_X21.Sep", "B_X21.Oct",
  "B_X21.Nov", "B_X21.Dec", "B_X22.Jan", "B_X22.Feb", "B_X22.Mar",
  "B_X22.Apr", "B_X22.May", "B_X22.Jun", "B_X22.Jul", "B_X22.Aug",
  "B_X22.Sep", "B_X22.Oct", "B_X22.Nov", "B_X22.Dec", "B_X23.Jan",
  "B_X23.Feb", "B_X23.Mar", "B_X23.Apr", "B_X23.May", "B_X23.Jun", "B_X23.Jul")
covid_namesR <- c(
  "R_X20.Mar", "R_X20.Apr", "R_X20.May", "R_X20.Jun", "R_X20.Jul",
  "R_X20.Aug", "R_X20.Sep", "R_X20.Oct", "R_X20.Nov", "R_X20.Dec",
  "R_X21.Jan", "R_X21.Feb", "R_X21.Mar", "R_X21.Apr", "R_X21.May",
  "R_X21.Jun", "R_X21.Jul", "R_X21.Aug", "R_X21.Sep", "R_X21.Oct",
  "R_X21.Nov", "R_X21.Dec", "R_X22.Jan", "R_X22.Feb", "R_X22.Mar",
  "R_X22.Apr", "R_X22.May", "R_X22.Jun", "R_X22.Jul", "R_X22.Aug",
  "R_X22.Sep", "R_X22.Oct", "R_X22.Nov", "R_X22.Dec", "R_X23.Jan",
  "R_X23.Feb", "R_X23.Mar", "R_X23.Apr", "R_X23.May", "R_X23.Jun", "R_X23.Jul"
)
covid_namesS <- c(
  "S_X20.Mar", "S_X20.Apr", "S_X20.May", "S_X20.Jun", "S_X20.Jul",
  "S_X20.Aug", "S_X20.Sep", "S_X20.Oct", "S_X20.Nov", "S_X20.Dec",
  "S_X21.Jan", "S_X21.Feb", "S_X21.Mar", "S_X21.Apr", "S_X21.May",
  "S_X21.Jun", "S_X21.Jul", "S_X21.Aug", "S_X21.Sep", "S_X21.Oct",
  "S_X21.Nov", "S_X21.Dec", "S_X22.Jan", "S_X22.Feb", "S_X22.Mar",
  "S_X22.Apr", "S_X22.May", "S_X22.Jun", "S_X22.Jul", "S_X22.Aug",
  "S_X22.Sep", "S_X22.Oct", "S_X22.Nov", "S_X22.Dec", "S_X23.Jan",
  "S_X23.Feb", "S_X23.Mar", "S_X23.Apr", "S_X23.May", "S_X23.Jun", "S_X23.Jul"
)

# Create the covid_dummy column
FinalR$Month_Rapid
FinalB$covid_dummy <- ifelse(FinalB$Month_Blitz %in% covid_names, 1, 0)
NFinalR$covid_dummy <- ifelse(NFinalR$Month_Rapid %in% covid_namesR, 1, 0)

FinalR <- readRDS("FinalR.rds")
FinalR$covid_dummyR <- ifelse(FinalR$Month_Rapid %in% covid_namesR, 1, 0)
FinalR$Year <- substr(FinalR$Month_Rapid, 4, 5)

# Convert the year to a 4-digit format (e.g., change "15" to "2015")
FinalR$Year <- paste("20", FinalR$Year, sep = "")
# Create a function to convert month names to numerical values
# Create a function to convert month names to numerical values

month_name_to_number <- function(month_name) {
  # Define a mapping of month names to numerical values
  month_mapping <- c("Jan" = "01", "Feb" = "02", "Mar" = "03", "Apr" = "04", "May" = "05", "Jun" = "06", "Jul" = "07", "Aug" = "08", "Sep" = "09", "Oct" = "10", "Nov" = "11", "Dec" = "12")
  
  # Use the mapping to convert month names
  return(month_mapping[month_name])
}
#####################Standard summary and pooled ols ####################
FinalS <- readRDS("FinalS.rds")
NFinalS <- readRDS("NFinalS.rds")

FinalS$Year <- substr(FinalS$Month_Standard, 4, 5)
NFinalS$time <- paste0(NFinalS$Year , NFinalS$Month)
class(NFinalS$time)
NFinalS$time <- as.numeric(NFinalS$time)
# Convert the year to a 4-digit format (e.g., change "15" to "2015")
FinalS$Year <- paste("20", FinalS$Year, sep = "")
FinalS$Month <- substr(FinalS$Month_Standard, 7, 9)
FinalS$Month <- month_name_to_number(FinalS$Month)
#32779254 obs
NFinalS <-na.omit(FinalS)
#32596743 obs
names(NFinalS)
NFinalS <- NFinalS[NFinalS$Standard_Gms!=0,]
#3677890 rows
NFinalS <- NFinalS[NFinalS$B.day != 0, ]
#3650138 rows
NFinalS$Year<-as.numeric(NFinalS$Year)

NFinalS$Age_EachYear <- NFinalS$Year - NFinalS$B.day
head(NFinalS)
class(NFinalS$Year)
class(NFinalS$B.day)
min_age_each_yearS <- aggregate(Age_EachYear ~ ID.Number, data = NFinalS, FUN = min)
names(min_age_each_yearS) <- c("ID.Number","Min_Age")
# I want to rewrite the cells of Sex 
NFinalS$Sex <- ifelse(NFinalS$Sex == "M", 0, 1)
NFinalS$Fed <- as.factor(NFinalS$Fed)
NFinalS$Year <- as.factor(NFinalS$Year)
NFinalS$Month <- as.factor(NFinalS$Month)
NFinalS$Standard_Rating <- as.numeric(NFinalS$Standard_Rating)
NFinalS$Age_EachYear <- as.numeric(NFinalS$Age_EachYear)
min_age_each_yearS$Min_Age <- as.numeric(min_age_each_yearS$Min_Age)

class(NFinalS$Standard_Rating)
class(NFinalS$Sex)
class(NFinalS$Month)
class(NFinalS$Year)
class(NFinalS$Age_EachYear)
names(NFinlaS)
class(min_age_each_yearS$Min_Age)
names(NFinalS)
NFinalS <- NFinalS %>% 
  select("ID.Number" ,"Month_Standard"  , "Standard_Rating" , "Standard_Gms", "Standard_K", "Sex","Name", "B.day" , "Fed" , "Flag" ,"Gamecat"  ,    "Year"  , "Month", "covid_dummyS" ,"Age_EachYear" )

######NFinalSI <- NFinalS[NFinalS$Flag = i || wi && NFinalS$Year = 23 , ]

# Summarize house prices near and far from incinerator, and before and after
DNFinalS <- NFinalS %>% 
  select("ID.Number" ,"Sex" , "Flag" )
DNFinalS <-  DNFinalS[!duplicated( DNFinalS$ID.Number), ]
#352379 obs
DNFinalS <- left_join(DNFinalS,min_age_each_yearS, by = "ID.Number")
NFinalS <- left_join(DNFinalS,min_age_each_yearS, by = "ID.Number")
names(DNFinalS) <- c("ID.Number" , "Sex" , "Flag", "Min_Age")
#Summary and aggregate also pooled ols
DNFinalS %>% str
DNFinalS %>% stargazer(type = "text")
resultS <- aggregate(Min_Age ~ Sex, data = DNFinalS, FUN = function(x) c(Mean = mean(x), Variance = var(x), Count = length(x))) 
resultNFS<- aggregate(Standard_Rating ~ Sex + covid_dummyS, data = NFinalS, FUN = function(x) c(Mean = mean(x), Variance = var(x), Count = length(x))) 
print(resultNFS)
print(resultS)
# Pooled OLS estimator
#NFinalR
names(NFinalS)
model_olsS <- plm(formula = Standard_Rating ~ covid_dummyS * Sex + covid_dummyS  + Sex+ Year + Month  + Age_EachYear  + 1 , 
                  data = NFinalS, 
                  index = c("ID.Number", "time"), # c(group index, time index)
                  model = "pooling")  
summary(model_olsS)
model_olsS <- NULL
group_by(Sex) %>%
  summarize_at(.vars = vars(Min_Age), 
               .funs = list(mean = mean, obs = length))
file_path11 <- paste0(directory_path, "min_age_each_yearS.rds")
file_path12 <- paste0(directory_path, "NFinalS.rds")
file_path13 <- paste0(directory_path, "DNFinalS.rds")

saveRDS(min_age_each_yearS, file = file_path11)
saveRDS(NFinalS, file = file_path12)
saveRDS(DNFinalS, file = file_path13)
###################################################
#####################################################################NEW STANDARD AFTER Covide omitted ############
################STANDARD covid 202305##############
############## we should omit months after 202305.###############
file_path5 <- 'A:\\payan_name\\fide_data\\2016\\csv16\\checkup\\merged_data\\clean\\'
setwd(file_path5)
getwd()
filepath5 <-  paste0(filepath4, "CNFinalS.rds")
saveRDS(CNFinalS, file = filepath5)
filepath6 <-  paste0(filepath4, "BCNFinal.rds")
saveRDS(BCNFinal, file = filepath6)
filepath7 <-  paste0(filepath4, "NFinal.rds")
saveRDS(NFinal, file = filepath7)
filepath8 <-  paste0(filepath4, "SdNFinal.rds")
saveRDS(SdNFinal, file = filepath8)
NFinalS <- readRDS("NFinalS.rds")
BLSS1503 <- readRDS("BLSS1503.rds")
BCNFinalS <- readRDS("BCNFinalS.rds")
CNFinalS <- readRDS("CNFinalS.rds")
BLSS1503 <- readRDS("BLSS1503.rds")
#3650135 obs
CNFinalS <- NFinalS %>% 
  filter(time < 202305)
#3518408 obs
#########we should omit bio country and Gender
BCNFinalS <- CNFinalS[!(CNFinalS$ID.Number %in% BLSS1503$ID.Number),]
#3492416 obs
length(unique(BCNFinalS$ID.Number))
#341470
mean( BCNFinalS$Standard_Rating)
#1727.337
sd( BCNFinalS$Standard_Rating)
#354.1527
min( BCNFinalS$Standard_Rating)
#1001
max( BCNFinalS$Standard_Rating)
#2882
#########Male sumary after omit bio gender
MBCNFinalS <- subset(BCNFinalS, Sex == 0)
min( MBCNFinalS$Standard_Rating)
#1001
max( MBCNFinalS$Standard_Rating)
#2882
mean( MBCNFinalS$Standard_Rating)
#1748.955
sd( MBCNFinalS$Standard_Rating)
#348.2201
length(unique( MBCNFinalS$ID.Number))
#305503
#########Female sumary after omit bio gender
FBCNFinalS <- subset(BCNFinalS, Sex == 1)
min( FBCNFinalS$Standard_Rating)
#1001
max( FBCNFinalS$Standard_Rating)
#2686
mean( FBCNFinalS$Standard_Rating)
#1535.108
sd( FBCNFinalS$Standard_Rating)
#348.3823
length(unique( FBCNFinalS$ID.Number))
#35967
################For AGe of them #################333
dNFinal <- readRDS("dNFinal.rds")
SdNFinal <- dNFinal[(dNFinal$ID.Number %in% BCNFinalS$ID.Number),]
# 641444
length(unique( SdNFinal$ID.Number))
# 341470 unique Id number 
SdNFinal <-  SdNFinal[!duplicated( SdNFinal$ID.Number), ]
#341470
#filepath2 <-  paste0(filepath4, "BdNFinal.rds")
#saveRDS(BdNFinal, file = filepath2)
min(SdNFinal$Min_Age)
#4
max(SdNFinal$Min_Age)
#118
mean(SdNFinal$Min_Age)
#31.23434
sd(SdNFinal$Min_Age)
#19.51305
MSdNFinal<- subset(SdNFinal, Sex == 0)
min(MSdNFinal$Min_Age)
#4
max(MSdNFinal$Min_Age)
#118
mean(MSdNFinal$Min_Age)
#32.66655
sd(MSdNFinal$Min_Age)
#19.63381
#######Female age summary
FSdNFinal<- subset(SdNFinal, Sex == 1)
min(FSdNFinal$Min_Age)
#5
max(FSdNFinal$Min_Age)
#97
mean(FSdNFinal$Min_Age)
#19.06917
sd(FSdNFinal$Min_Age)
#13.23616
####
##############BEFORE & AFTER covid Standard Rating#############

PBCNFinalS <- subset(BCNFinalS, covid_dummyS == 0)
#2472696 obs CNFinal
#2457180
mean(PBCNFinalS$Standard_Rating)
#1744.809
#1742.098
sd(PBCNFinalS$Standard_Rating)
# 354.3338
# 352.83
MPBCNFinalS <- subset(PBCNFinalS, Sex == 0)
mean(MPBCNFinalS$Standard_Rating)
#1766.67
#1764.098
sd(MPBCNFinalS$Standard_Rating)
#348.0108
#346.528
FPBCNFinalS <- subset(PBCNFinalS, Sex == 1)
mean(FPBCNFinalS$Standard_Rating)
#1555.599
#1548.54
sd(FPBCNFinalS$Standard_Rating)
#352.2136
#348.632
#######AFTER COVID PANDAMic#############اول بلک لیست حذف نشده بعدی شده
ABCNFinalS <- subset(BCNFinalS, covid_dummyS == 1)
#1042112 obs
#1035236 obs
mean(ABCNFinalS$Standard_Rating)
#1695.439
#1692.302
sd(ABCNFinalS$Standard_Rating)
#356.6759
#354.8226
MABCNFinalS <- subset(ABCNFinalS, Sex == 0)
mean(MABCNFinalS$Standard_Rating)
#1715.966
#1713.142
sd(MABCNFinalS$Standard_Rating)
# 351.3915
#349.5895
FABCNFinalS <- subset(ABCNFinalS, Sex == 1)
#102264  obs
mean(FABCNFinalS$Standard_Rating)
#1510.476
#1502.168
sd(FABCNFinalS$Standard_Rating)
#350.6103
#345.5678
# Standard Ratings kernel density
standard_density_values <- list(
  "Before Covid Overall" = density(PBCNFinalS$Standard_Rating),
  "Before Covid Male" = density(MPBCNFinalS$Standard_Rating),
  "Before Covid Female" = density(FPBCNFinalS$Standard_Rating),
  "After Covid Overall" = density(ABCNFinalS$Standard_Rating),
  "After Covid Male" = density(MABCNFinalS$Standard_Rating),
  "After Covid Female" = density(FABCNFinalS$Standard_Rating)
)

standard_density_df <- do.call(rbind, lapply(names(standard_density_values), function(name) {
  data.frame(
    x = standard_density_values[[name]]$x,
    y = standard_density_values[[name]]$y,
    Category = name
  )
}))

ggplot(standard_density_df, aes(x = x, y = y, color = Category)) +
  geom_line() +
  labs(
    title = "Kernel Density Plot for Standard Ratings",
    x = "Standard Rating",
    y = "Density"
  ) +
  theme_minimal()
###################3standard for thesis ggplot
BCNFinalS$Year_Month <- as.Date(paste(BCNFinalS$Year, BCNFinalS$Month, "01", sep = "-"))
#Male
Average_Standard_Male <- MBCNFinalS %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Male = mean(Standard_Rating))
#Female
Average_Standard_Female <- FBCNFinalS %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Female = mean(Standard_Rating))
#ggplot(Average_Standard, aes(x = Year_Month, y = mean_rating)) +
ggplot()+  
  geom_line(data = Average_Standard_Male, aes(x = Year_Month, y = mean_rating_Male, color = "مردها")) +
  geom_point(data = Average_Standard_Male, aes(x = Year_Month, y = mean_rating_Male), color = "blue")  +
  geom_line(data = Average_Standard_Female, aes(x =Year_Month, y = mean_rating_Female, color = "زن ها")) +
  geom_point(data = Average_Standard_Female, aes(x = Year_Month, y = mean_rating_Female), color = "red") +
  geom_line() +
  scale_color_manual(values = c("مردها" = "blue", "زن ها" = "red")) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  labs( x = "سال", y = "میانگین درجه بین المللی" , color="جنسیت") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month")

ggplot()+  
  geom_line(data = Average_Standard_Male, aes(x = Year_Month, y = mean_rating_Male, color = "Male")) +
  geom_point(data = Average_Standard_Male, aes(x = Year_Month, y = mean_rating_Male), color = "blue")  +
  geom_line(data = Average_Standard_Female, aes(x =Year_Month, y = mean_rating_Female, color = "Female")) +
  geom_point(data = Average_Standard_Female, aes(x = Year_Month, y = mean_rating_Female), color = "red") +
  geom_line() +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  labs(title = "Mean Standard Rating Over Time", x = "Time", y = "Mean Standard Rating" , color="Gender") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month")

####Number of participant
BCNFinalS$Sex <- factor(BCNFinalS$Sex, levels = c(0, 1), labels = c("Male", "Female"))

# Aggregate unique participants based on year and sex
unique_participantsS <- BCNFinalS %>%
  group_by(Year, Sex) %>%
  summarise(UniqueParticipantsS = n_distinct(ID.Number)) %>%
  ungroup()

# Plotting using ggplot
# Plotting using ggplot
# Ensure 'Year' factor levels are in the correct order
unique_participantsS$Year <- factor(unique_participantsS$Year, levels = unique(as.character(unique_participantsS$Year)))

# Plotting using ggplot
ggplot(unique_participantsS, aes(x = Year, y = UniqueParticipantsS, color = Sex, group = Sex)) +
  geom_line() +
  labs(
    title = "Number of Unique Participants for Standard games",
    x = "Year",
    y = "Number of Unique Participants"
  ) +
  theme_minimal()
unique(BCNFinalS$Sex)
t.test(PBCNFinalS$Standard_Rating,ABCNFinalS$Standard_Rating)
t.test(MPBCNFinalS$Standard_Rating,MABCNFinalS$Standard_Rating)
t.test(FPBCNFinalS$Standard_Rating,FABCNFinalS$Standard_Rating)
#####################ُStandard summary and pooled ols AFTER WE OMIT month later than 202305 (BY May)####################
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
BCNFinalS <- NULL
########################################################
file_path8 <- 'A:\\payan_name\\fide_data\\2016\\csv16\\checkup\\merged_data\\clean\\covid\\'
setwd(file_path8)
getwd()
filepath10 <-  paste0(file_path8, "CNFinalS.rds")
saveRDS(CNFinalS, file = filepath10)
filepath11 <-  paste0(file_path8, "BCNFinalS.rds")

saveRDS(BCNFinalS, file = filepath11)
SdNFinal
filepath12 <-  paste0(file_path8, " SdNFinal.rds")
saveRDS( SdNFinal, file = filepath12)
#####################################################################NEW STANDARD AFTER Covide omitted ############
###################################################
#####################################################################NEW ALL summary after omittion############
###################################################
BDlitsBlack <- readRDS("BDlitsBlack.rds")
BLSS1503 <- readRDS("BLSS1503.rds")
BLSR1503 <- readRDS("BLSR1503.rds")
BCNFinal <- readRDS("BCNFinal.rds")
# BCNFinal 5509592 obs
BCNFinal <- unique(BCNFinal)
#5312987 final data set obs 
CNFinal <- NFinal %>% 
  filter(time < 202305)
#before 5801609 After 5555196
BCNFinal <- CNFinal[!(CNFinal$ID.Number %in% BLSS1503$ID.Number),]
#5511368 obs 
BCNFinal <- BCNFinal[!(BCNFinal$ID.Number %in% BLSR1503$ID.Number),]
#5510081 obs
BCNFinal <- BCNFinal[!(BCNFinal$ID.Number %in% BDlitsBlack$ID.Number),]
#5509592 obs
length(unique(BCNFinal$ID.Number))
#445941
unique(BCNFinal$time)
names(BCNFinal)
mean( BCNFinal$Rating)
#1698.008
sd( BCNFinal$Rating)
#358.6857
min( BCNFinal$Rating)
#1000
max( BCNFinal$Rating)
#2986
#########Male sumary after omit bio gender
MBCNFinal <- subset(BCNFinal, Sex == 0)
min( MBCNFinal$Rating)
#
max( MBCNFinal$Rating)
#
mean( MBCNFinal$Rating)
#1725.545
# 1720.401
sd( MBCNFinal$Rating)
#348.2327
# 354.4295
length(unique( MBCNFinal$ID.Number))
#401788
#########Female sumary after omit bio gender
FBCNFinal <- subset(BCNFinal, Sex == 1)
min( FBCNFinal$Rating)
#
max( FBCNFinal$Rating)
#2686
mean( FBCNFinal$Rating)
#1511.583
sd( FBCNFinal$Rating)
# 339.1095
length(unique( FBCNFinal$ID.Number))
#44153
################For AGe of them #################333
dNFinal <- readRDS("dNFinal.rds")
SdNFinal <- dNFinal[(dNFinal$ID.Number %in% BCNFinal$ID.Number),]
# 792799
length(unique( SdNFinal$ID.Number))
#445941  
SdNFinal <-  SdNFinal[!duplicated( SdNFinal$ID.Number), ]
#445941
#filepath2 <-  paste0(filepath4, "BdNFinal.rds")
#saveRDS(BdNFinal, file = filepath2)
min(SdNFinal$Min_Age)
#3
max(SdNFinal$Min_Age)
#122
mean(SdNFinal$Min_Age)
# 31.13803
sd(SdNFinal$Min_Age)
#19.36411
MSdNFinal<- subset(SdNFinal, Sex == 0)
#401788 obs
min(MSdNFinal$Min_Age)
#3
max(MSdNFinal$Min_Age)
#122
mean(MSdNFinal$Min_Age)
#32.44584
sd(MSdNFinal$Min_Age)
#19.47269
#######Female age summary
FSdNFinal<- subset(SdNFinal, Sex == 1)
#44153 obs
min(FSdNFinal$Min_Age)
#3
max(FSdNFinal$Min_Age)
#97
mean(FSdNFinal$Min_Age)
# 19.23708
sd(FSdNFinal$Min_Age)
#13.3941
####
##############BEFORE & AFTER covid Standard Rating#############

PBCNFinal <- subset(BCNFinal, covid_dummy == 0)
#3815268 -> 3618663 obs unique
mean(PBCNFinal$Rating)
#1729.559 3
#1723/033
sd(PBCNFinal$Rating)
# 348.3424
# 356.51
MPBCNFinal <- subset(PBCNFinal, Sex == 0)
mean(MPBCNFinal$Rating)
#1751.805
#1745.848
sd(MPBCNFinal$Rating)
#
#351.4889
FPBCNFinal <- subset(PBCNFinal, Sex == 1)
mean(FPBCNFinal$Rating)
#1533.485
sd(FPBCNFinal$Rating)
#341.1482
#
#######AFTER COVID PANDAMic#############اول بلک لیست حذف نشده بعدی شده
ABCNFinal <- subset(BCNFinal, covid_dummy == 1)
#1694324
mean(ABCNFinal$Rating)
# 1644.56
#
sd(ABCNFinal$Rating)
#357.4696
#
MABCNFinal <- subset(ABCNFinal, Sex == 0)
mean(MABCNFinal$Rating)
#1666.089
#
sd(MABCNFinal$Rating)
# 354.5687
#
FABCNFinal <- subset(ABCNFinal, Sex == 1)
#180976
mean(FABCNFinal$Rating)
# 1464.534
#
sd(FABCNFinal$Rating)
#329.807
t.test(PBCNFinal$Rating,ABCNFinal$Rating)
t.test(MPBCNFinal$Rating,MABCNFinal$Rating)
t.test(FPBCNFinal$Rating,FABCNFinal$Rating)

######################################################################NEW ALL summary after omittion ############
###################################################
names(BCNFinal) <- c("ID.Number"   ,    "Month_Year" , "Rating" , "Gms"  ,   "K"   ,   "Sex"     ,      "Name"      ,     
                     "B.day"       ,    "Fed"      ,       "Flag"      ,      "Gamecat"    ,     "Year"      ,      "Month"    ,       "covid_dummy"   ,
                     "Age_EachYear"  ,  "time" )
class(BCNFinal$Gamecat)
BCNFinal$Gamecat <- as.factor(BCNFinal$Gamecat)
BCNFinal$Gamecat <- relevel(BCNFinal$Gamecat,ref = "Standard")
class(BCNFinal$Month_Year)
class(BCNFinal$Month)
class(BCNFinal$Year)
class(BCNFinal$covid_dummy)
class(BCNFinal$Sex)
class(BCNFinal$Age_EachYear)

model_olsFCG <- plm(formula = Rating ~  Gamecat+ covid_dummy * Sex + covid_dummy  + Sex+Year + Month + Age_EachYear + 1 , 
                    data = BCNFinal, 
                    index = c("ID.Number", "time"), 
                    model = "pooling")  # c(group index, time index)
summary(model_olsFCG)
# Check for duplicate combinations of (id-time)
max(table(index(BCNFinal), useNA = "ifany"))
###################################################
###################################################

#####################All pooled Ols################
names(NFinalS) <-c("ID.Number"      , "Month_Year" , "Rating" ,"Gms"   , "K"    ,  "Sex"        ,     "Name"           
                   , "B.day"       ,    "Fed"         ,    "Flag"           , "Gamecat"    ,     "Year"      ,      "Month"     ,      "covid_dummy" , "Age_EachYear","time" )
names(NFinalR) <- c("ID.Number"      , "Month_Year" , "Rating" ,"Gms"   , "K"    ,  "Sex"        ,     "Name"           
                    , "B.day"       ,    "Fed"         ,    "Flag"           , "Gamecat"    ,     "Year"      ,      "Month"     ,   "covid_dummy", "Age_EachYear","time"   )

NFinalR <- NFinalR[, c("ID.Number"   , "Month_Rapid" ,  "Rapid_Rating", "Rapid_Gms" ,   "Rapid_K"   ,   "Sex"     ,     "Name"    ,     "B.day"   ,     "Fed"         
                       , "Flag"    ,     "Gamecat" ,     "Year"     ,    "Month"   ,     "covid_dummy" , "Age_EachYear" , "time")]      

names(NFinalB) <- c( "ID.Number" ,   "Month_Year" , "Rating", "Gms"  ,   "K"    ,  "Sex"    ,      "Name"     ,    "B.day"   ,     "Fed"         
                     , "Flag"     ,    "Gamecat"   ,   "Year"       ,  "Month"     ,   "covid_dummy" , "Age_EachYear","time" )
class(NFinal$Gamecat)
class(NFinal$Sex)
class(NFinal$Fed)
class(NFinal$Year)
class(NFinal$Month)
class(NFinal$Rating)
class(NFinal$covid_dummy)
class(NFinal$Month_Year)
NFinal$Gamecat <- as.factor(NFinal$Gamecat)
library(dplyr)  # Load the dplyr package

# Replace "your_data" with the name of your data frame
selected_data <- NFinal %>%
  select(ID.Number, Month_Year, Rating, Sex, Fed, Gamecat, Year, Month, covid_dummy, Age_EachYear)

#  NFinal$Month_Year <- as.numeric(NFinal$Month_Year) %>% 
#Warning message:
# NAs introduced by coercion
FinalB <- NULL
NFinalS <- readRDS("NFinalS.rds")
NFinalR <- readRDS("NFinalR.rds")
NFinalB <- readRDS("NFinalB.rds")
#905812
unique(NFinalB$time)
max(NFinalB$time)
class(NFinalB$time)
names(CNFinalB)
BCNFinalB <- CNFinalB[!(CNFinalB$ID.Number %in% BDlitsBlack$ID.Number),]
#860656 obs of 866633
NFinal <- readRDS("NFinal.rds")
DNFinalS <- readRDS("DNFinalS.rds")
DNFinalR <- readRDS("DNFinalR.rds")
DNFinalB <- readRDS("DNFinalB.rds")

R2307_demog <- readRDS("R2307_demog.rds")

NFinalR$time <- paste0(NFinalR$Year,NFinalR$Month)
NFinalS$time <- paste0(NFinalS$Year,NFinalS$Month)
NFinalS$time <- as.numeric(NFinalS$time)
NFinalR$time <- as.numeric(NFinalR$time)
NFinal<-rbind(NFinalB,NFinalR,NFinalS)
file_path14 <- paste0(directory_path, "NFinal.rds")
saveRDS(NFinal, file = file_path14)
NFinalB <-NULL
NFinalR <-NULL
NFinalS <-NULL
model_olsS<-NULL
model_olsB <-NULL
model_olsF <-NULL

model_olsF <- plm(formula = Rating ~ covid_dummy * Sex + covid_dummy  + Sex+ Year + Month  + Age_EachYear + Gamecat + 1 + Fed , 
                  data = NFinal, 
                  index = c("ID.Number", "time"), # c(group index, time index)
                  model = "pooling")  
summary(model_olsF)
############## we should omit months after 202305.###############
filepath4 <- 'A:\\payan_name\\fide_data\\2016\\csv16\\checkup\\merged_data\\clean\\covid\\'
setwd(filepath4)
getwd()
filepath <-  paste0(filepath4, "CNFinalB.rds")
saveRDS(CNFinalB, file = filepath)
filepath16 <-  paste0(filepath4, "BCNFinalB.rds")
saveRDS(BCNFinalB, file = filepath16)
CNFinalB <- readRDS("CNFinalB.rds")
BCNFinalB <- readRDS("BCNFinalB.rds")

BDlitsBlack <- readRDS("BDlitsBlack.rds")
BCNFinalB <- CNFinalB[!(CNFinalB$ID.Number %in% BDlitsBlack$ID.Number),]
unique(CNFinalB$time)
max(CNFinalB$time)
CNFinalB <- NFinalB %>% 
  filter(time < 202305)
CNFinalB %>% 
  group_by(Sex) %>%
  summarize_at(.vars = vars(Blitz_Rating), 
               .funs = list(mean = mean, obs = length))
length(unique(CNFinalB$ID.Number))
###170403 unique ID-Number 
#########we should omit bio country and Gender

length(unique(BCNFinalB$ID.Number))
mean( BCNFinalB$Blitz_Rating)
#1739.337
sd( BCNFinalB$Blitz_Rating)
#323.1672
#########Male sumary after omit bio gender
MBCNFinalB <- subset(BCNFinalB, Sex == 0)
min( MBCNFinalB$Blitz_Rating)
#1000
max( MBCNFinalB$Blitz_Rating)
#2986
mean( MBCNFinalB$Blitz_Rating)
#1761.296
sd( MBCNFinalB$Blitz_Rating)
#314.9567
length(unique( MBCNFinalB$ID.Number))
#151062
#########Male sumary after omit bio gender
FBCNFinalB <- subset(BCNFinalB, Sex == 1)
min( FBCNFinalB$Blitz_Rating)
#1000
max( FBCNFinalB$Blitz_Rating)
#2676
mean( FBCNFinalB$Blitz_Rating)
#1528.474
sd( FBCNFinalB$Blitz_Rating)
#325.0492
length(unique( FBCNFinalB$ID.Number))
#18831
##############Age without exclude Bio sex o country##################only for blitz################
WNFinal <- dNFinal[(dNFinal$ID.Number %in% CNFinalB$ID.Number),]
#422592
WNFinal <-  WNFinal[!duplicated( WNFinal$ID.Number), ]
#170403
min(WNFinal$Min_Age)
#4
max(WNFinal$Min_Age)
#122
mean(WNFinal$Min_Age)
#28.5463
sd(WNFinal$Min_Age)
#18.36075
MWNFinal<- subset(WNFinal, Sex == 0)
min(MWNFinal$Min_Age)
#4
max(MWNFinal$Min_Age)
#122
mean(MWNFinal$Min_Age)
#29.8705
sd(MWNFinal$Min_Age)
#18.55656
#######Female age summary without omit
FWNFinal<- subset(WNFinal, Sex == 1)
min(FWNFinal$Min_Age)
#5
max(FWNFinal$Min_Age)
#96
mean(FWNFinal$Min_Age)
#17.96301
sd(FWNFinal$Min_Age)
#12.34346
##############BEFORE & AFTER covid Blits Rating#############
CCNFinalB <- CNFinalB %>% 
  filter(time < 202305)
names(CNFinalB)
PCNFinalB <- subset(CNFinalB, covid_dummy == 0)
#632026 obs
mean(PCNFinalB$Blitz_Rating)
#1780.596
sd(PCNFinalB$Blitz_Rating)
#304.7129
MPCNFinalB <- subset(PCNFinalB, Sex == 0)
mean(MPCNFinalB$Blitz_Rating)
#1800.004
sd(MPCNFinalB$Blitz_Rating)
#294.491
FPCNFinalB <- subset(PCNFinalB, Sex == 1)
mean(FPCNFinalB$Blitz_Rating)
#1800.004
sd(FPCNFinalB$Blitz_Rating)
#333.0013
#######AFTER COVID PANDAMic#############
ACNFinalB <- subset(CNFinalB, covid_dummy == 1)
#234607 obs
mean(ACNFinalB$Blitz_Rating)
#1639.741
sd(ACNFinalB$Blitz_Rating)
#355.4582
MACNFinalB <- subset(ACNFinalB, Sex == 0)
mean(MACNFinalB$Blitz_Rating)
#1663.345
sd(MACNFinalB$Blitz_Rating)
#354.6049
FACNFinalB <- subset(ACNFinalB, Sex == 1)
mean(FACNFinalB$Blitz_Rating)
#1473.185
sd(FACNFinalB$Blitz_Rating)
#314.5746

######################################################
############################AFter omit Black list
##############BEFORE & AFTER covid Blits Rating#############
PBCNFinalB <- subset(BCNFinalB, covid_dummy == 0)
#627587 obs
mean(PBCNFinalB$Blitz_Rating)
#1777.735
sd(PBCNFinalB$Blitz_Rating)
# 302.5978
MPBCNFinalB <- subset(PBCNFinalB, Sex == 0)
#575219
mean(MPBCNFinalB$Blitz_Rating)
#1797.352
sd(MPBCNFinalB$Blitz_Rating)
#292.36
FPBCNFinalB <- subset(PBCNFinalB, Sex == 1)
#52368
mean(FPBCNFinalB$Blitz_Rating)
#1562.261
sd(FPBCNFinalB$Blitz_Rating)
#328.3538
#######AFTER COVID PANDAMic#############
ABCNFinalB <- subset(BCNFinalB, covid_dummy == 1)
#233069 obs
mean(ABCNFinalB$Blitz_Rating)
#1635.943
sd(ABCNFinalB$Blitz_Rating)
#352.7559
MABCNFinalB <- subset(ABCNFinalB, Sex == 0)
#204264
mean(MABCNFinalB$Blitz_Rating)
#1659.76
sd(MABCNFinalB$Blitz_Rating)
#351.9566
FABCNFinalB <- subset(ABCNFinalB, Sex == 1)
#28805
mean(FABCNFinalB$Blitz_Rating)
#1467.049
sd(FABCNFinalB$Blitz_Rating)
#309.6554
# Blitz Ratings
blitz_density_values <- list(
  "Before Covid Overall" = density(PBCNFinalB$Blitz_Rating),
  "Before Covid Male" = density(MPBCNFinalB$Blitz_Rating),
  "Before Covid Female" = density(FPBCNFinalB$Blitz_Rating),
  "After Covid Overall" = density(ABCNFinalB$Blitz_Rating),
  "After Covid Male" = density(MABCNFinalB$Blitz_Rating),
  "After Covid Female" = density(FABCNFinalB$Blitz_Rating)
)

blitz_density_df <- do.call(rbind, lapply(names(blitz_density_values), function(name) {
  data.frame(
    x = blitz_density_values[[name]]$x,
    y = blitz_density_values[[name]]$y,
    Category = name
  )
}))

ggplot(blitz_density_df, aes(x = x, y = y, color = Category)) +
  geom_line() +
  labs(
    title = "Kernel Density Plot for Blitz Ratings",
    x = "Blitz Rating",
    y = "Density"
  ) +
  theme_minimal()

######################Blitz Unique for table
UBCNFinalB$Year_Month <- as.Date(paste(UBCNFinalB$Year, UBCNFinalB$Month, "01", sep = "-"))
length(unique(UBCNFinalB$ID.Number))
#
mean( UBCNFinalB$Blitz_Rating)
#
sd( UBCNFinalB$Blitz_Rating)
#
min( UBCNFinalB$Blitz_Rating)
#
max( UBCNFinalB$Blitz_Rating)
#
#########Male sumary after omit bio gender
MUBCNFinalB <- subset(UBCNFinalB, Sex == 0)
min( MUBCNFinalB$Blitz_Rating)
#
max( MUBCNFinalB$Blitz_Rating)
#
mean( MUBCNFinalB$Blitz_Rating)
#
sd( MUBCNFinalB$Blitz_Rating)
#
length(unique( MUBCNFinalB$ID.Number))
#
#########Female sumary after omit bio gender
FUBCNFinalB <- subset(UBCNFinalB, Sex == 1)
min( FUBCNFinalB$Blitz_Rating)
#
max( FUBCNFinalB$Blitz_Rating)
#
mean( FUBCNFinalB$Blitz_Rating)
#
sd( FUBCNFinalB$Blitz_Rating)
#
length(unique( FUBCNFinalB$ID.Number))
#
################For AGe of them #################333
dNFinal <- readRDS("dNFinal.rds")
UBdNFinal <- dNFinal[(dNFinal$ID.Number %in% UBCNFinalB$ID.Number),]
# 
length(unique( UBdNFinal$ID.Number))
#  
UBdNFinal <-  UBdNFinal[!duplicated( UBdNFinal$ID.Number), ]
#
#filepath2 <-  paste0(filepath4, "BdNFinal.rds")
#saveRDS(BdNFinal, file = filepath2)
min(UBdNFinal$Min_Age)
#
max(UBdNFinal$Min_Age)
#
mean(UBdNFinal$Min_Age)
#
sd(UBdNFinal$Min_Age)
#
MUBdNFinal<- subset(UBdNFinal, Sex == 0)
min(MUBdNFinal$Min_Age)
#
max(MUBdNFinal$Min_Age)
#
mean(MUBdNFinal$Min_Age)
#
sd(MUBdNFinal$Min_Age)
#
#######Female age summary
FUBdNFinal<- subset(UBdNFinal, Sex == 1)
min(FUBdNFinal$Min_Age)
#
max(FUBdNFinal$Min_Age)
#
mean(FUBdNFinal$Min_Age)
#
sd(FUBdNFinal$Min_Age)
#
####
##############BEFORE & AFTER covid Blitz Rating#############

PUBCNFinalB <- subset(UBCNFinalB, covid_dummyS == 0)
#
mean(PUBCNFinalB$Blitz_Rating)
#
sd(PUBCNFinalB$Blitz_Rating)
# 
# 
MPUBCNFinalB <- subset(PUBCNFinalB, Sex == 0)
mean(MPUBCNFinalB$Blitz_Rating)
#
sd(MPUBCNFinalB$Blitz_Rating)
#
FPUBCNFinalB <- subset(PUBCNFinalB, Sex == 1)
mean(FPUBCNFinalB$Blitz_Rating)
#
sd(FPUBCNFinalB$Blitz_Rating)
#
#
#######AFTER COVID PANDAMic#############
AUBCNFinalB <- subset(UBCNFinalB, covid_dummyS == 1)
#
mean(AUBCNFinalB$Blitz_Rating)
#
#
sd(AUBCNFinalB$Blitz_Rating)
#
#
MAUBCNFinalB <- subset(AUBCNFinalB, Sex == 0)
mean(MAUBCNFinalB$Blitz_Rating)
#
#
sd(MAUBCNFinalB$Blitz_Rating)
# 
#
FAUBCNFinalB <- subset(AUBCNFinalB, Sex == 1)
#
mean(FAUBCNFinalB$Blitz_Rating)
#
#
sd(FAUBCNFinalB$Blitz_Rating)
#
#
#####################ُBlitz summary and pooled ols AFTER WE OMIT month later than 202305 (BY May)####################
names(UBCNFinalB) <- c("ID.Number"   ,    "Month_Blitz" , "Blitz_Rating" , "Blitz_Gms"  ,   "Blitz_K"   ,   "Sex"     ,      "Name"      ,     
                       "B.day"       ,    "Fed"      ,       "Flag"         ,     "Year"      ,      "Month"    ,       "covid_dummy"   ,
                       "Age_EachYear"  ,  "time" )
model_olsBC <- plm(formula = Blitz_Rating ~  covid_dummy * Sex + covid_dummy  + Sex+Year + Month + Age_EachYear + 1 , 
                   data = UBCNFinalB, 
                   index = c("ID.Number", "time"), # c(group index, time index)
                   model = "pooling")  
summary(model_olsBC)


##########################Blitz Unique for table
############### ggplot matn asli blitz
UBCNFinalB$Year_Month <- as.Date(paste(UBCNFinalB$Year, UBCNFinalB$Month, "01", sep = "-"))
#Male
Average_blitz_Male <- MUBCNFinalB %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Male = mean(Blitz_Rating))
#Female
Average_blitz_Female <- FUBCNFinalB %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Female = mean(Blitz_Rating))
#ggplot(Average_blitz, aes(x = Year_Month, y = mean_rating)) +
ggplot()+  
  geom_line(data = Average_blitz_Male, aes(x = Year_Month, y = mean_rating_Male, color = "مردها")) +
  geom_point(data = Average_blitz_Male, aes(x = Year_Month, y = mean_rating_Male), color = "blue")  +
  geom_line(data = Average_blitz_Female, aes(x =Year_Month, y = mean_rating_Female, color = "زن ها")) +
  geom_point(data = Average_blitz_Female, aes(x = Year_Month, y = mean_rating_Female), color = "red") +
  geom_line() +
  scale_color_manual(values = c("مردها" = "blue", "زن ها" = "red")) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  labs(title = "متوسط سالانه درجه بین المللی زنان و مردان شطرنج باز در بازی برق آسا", x = "سال", y = "میانگین درجه بین المللی" , color="جنسیت") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month")

ggplot()+  
  geom_line(data = Average_blitz_Male, aes(x = Year_Month, y = mean_rating_Male, color = "Male")) +
  geom_point(data = Average_blitz_Male, aes(x = Year_Month, y = mean_rating_Male), color = "blue")  +
  geom_line(data = Average_blitz_Female, aes(x =Year_Month, y = mean_rating_Female, color = "Female")) +
  geom_point(data = Average_blitz_Female, aes(x = Year_Month, y = mean_rating_Female), color = "red") +
  geom_line() +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  labs(title = "Mean Blitz Rating Over Time", x = "Time", y = "Mean Blitz Rating" , color="Gender") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month")
################
UBCNFinalB<-readRDS("UBCNFinalB.rds")
AUBCNFinalB<- subset(UBCNFinalB,covid_dummy == 1 )
min(AUBCNFinalB$time1)
head(AUBCNFinalB)
names(UBCNFinalB)
UBCNFinalB$time1 <- paste0(UBCNFinalB$Year,"-", UBCNFinalB$Month)
class(UBCNFinalB$time1)
library(tidyverse)
library(dplyr)
UBCNFinalB %>% 
  ggplot(time,mean(Blitz_Rating),colour = Sex)+
  geom_point(size=5, alpha= 0.3)+
  geom_line(size=1)+
  +
  theme_minimal()+
  labs(title = "متوسط سالانه درجه بین المللی برای مردان وزنان ازسال 2015 تا 2023")
##########333333
library(dplyr)
library(ggplot2)

# Convert 'time1' column to Date format
UBCNFinalB <- UBCNFinalB %>%
  mutate(time1 = as.Date(as.character(time1))) 
UBCNFinalB$time1
# Plotting
UBCNFinalB %>%
  ggplot(aes(x = time1, y = Blitz_Rating, color = factor(Sex))) +
  geom_point(size = 5, alpha = 0.3) +
  geom_line(size = 1) +
  geom_vline(data = subset(UBCNFinalB, time1 == "2020-03"), aes(xintercept = as.numeric(time1)),
             linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Average Annual International Rating for Men and Women from 2015 to 2023")
##############
#Number of participant
BCNFinalB$Sex <- factor(BCNFinalB$Sex, levels = c(0, 1), labels = c("Male", "Female"))

# Aggregate unique participants based on year and sex
unique_participantsB <- BCNFinalB %>%
  group_by(Year, Sex) %>%
  summarise(UniqueParticipantsB = n_distinct(ID.Number)) %>%
  ungroup()

# Plotting using ggplot
# Plotting using ggplot
# Ensure 'Year' factor levels are in the correct order
unique_participantsB$Year <- factor(unique_participantsB$Year, levels = unique(as.character(unique_participantsB$Year)))

# Plotting using ggplot
ggplot(unique_participantsB, aes(x = Year, y = UniqueParticipantsB, color = Sex, group = Sex)) +
  geom_line() +
  labs(
    title = "Number of Unique Participants for Blitz games",
    x = "Year",
    y = "Number of Unique Participants"
  ) +
  theme_minimal()

max(BCNFinalB$time)
t.test(PBCNFinalB$Blitz_Rating,ABCNFinalB$Blitz_Rating)
t.test(MPBCNFinalB$Blitz_Rating,MABCNFinalB$Blitz_Rating)
t.test(FPBCNFinalB$Blitz_Rating,FABCNFinalB$Blitz_Rating)
############################AFter omit Black list
######################################################
############Male summary length min , max ############
MCNFinalB <- subset(CNFinalB, Sex == 0)
min(MCNFinalB$Blitz_Rating)
#1000
max(MCNFinalB$Blitz_Rating)
#2986
mean(MCNFinalB$Blitz_Rating)
#1764.193
sd(MCNFinalB$Blitz_Rating)
#317.114
length(unique(MCNFinalB$ID.Number))
################For AGe of them #################333
dNFinal <- readRDS("dNFinal.rds")
BdNFinal <- dNFinal[(dNFinal$ID.Number %in% BCNFinalB$ID.Number),]
#421117 
length(unique( BdNFinal$ID.Number))
#169893 unique Id number 
BdNFinal <-  BdNFinal[!duplicated( BdNFinal$ID.Number), ]
filepath2 <-  paste0(filepath4, "BdNFinal.rds")
saveRDS(BdNFinal, file = filepath2)
min(BdNFinal$Min_Age)
#4
max(BdNFinal$Min_Age)
#122
mean(BdNFinal$Min_Age)
#28.5499
sd(BdNFinal$Min_Age)
#18.36075
MBdNFinal<- subset(BdNFinal, Sex == 0)
min(MBdNFinal$Min_Age)
#4
max(MBdNFinal$Min_Age)
#122
mean(MBdNFinal$Min_Age)
#29.87209
sd(MBdNFinal$Min_Age)
#18.55656
#######Female age summary
FBdNFinal<- subset(BdNFinal, Sex == 1)
min(FBdNFinal$Min_Age)
#5
max(FBdNFinal$Min_Age)
#96
mean(FBdNFinal$Min_Age)
#17.94334
sd(FBdNFinal$Min_Age)
#12.35368
############Male summary length min , max############
MCNFinalB <- subset(CNFinalB, Sex == 0)
min(MCNFinalB$Blitz_Rating)
#1000
max(MCNFinalB$Blitz_Rating)
#2986
mean(MCNFinalB$Blitz_Rating)
#1764.193
sd(MCNFinalB$Blitz_Rating)
#317.114
length(unique(MCNFinalB$ID.Number))
#151453 lenght of unique ID_number 
#866633 CNFinalB obs 
unique(CNFinalB$time)
min(CNFinalB$Blitz_Rating)
max(CNFinalB$Blitz_Rating)
mean(CNFinalB$Blitz_Rating)
sd(CNFinalB$Blitz_Rating)
var(CNFinalB$Blitz_Rating)
class(CNFinalB$Sex)
names(CNFinalB)
###########Female summary length min , max############
FCNFinalB <- subset(CNFinalB, Sex == 1)
#82477 obs
min(FCNFinalB$Blitz_Rating)
#1000
max(FCNFinalB$Blitz_Rating)
#2676
mean(FCNFinalB$Blitz_Rating)
#1535.886
sd(FCNFinalB$Blitz_Rating)
#329.8805
length(unique(FCNFinalB$ID.Number))
#18950 unique ID_Number 
##MCNFinalB <- CNFinalB[CNFinalB$Sex == "0"]
##MCNFinalB <- select(CNFinalB$Sex == "0")
filter(CNFinalB$Sex == 0)
#?filter
#####################Blitz summary and pooled ols AFTER WE OMIT month later than 202305 (BY May)####################
NFinalR<- NULL
names(BCNFinalB)
model_olsBC <- plm(formula = Blitz_Rating ~  covid_dummy * Sex + covid_dummy  + Sex+Year + Month + Age_EachYear + 1 +Fed, 
                   data = BCNFinalB, 
                   index = c("ID.Number", "time"), # c(group index, time index)
                   model = "pooling")  
summary(model_olsBC)
#####################Blitz summary and pooled ols ####################

FinalB$Year <- substr(FinalB$Month_Blitz, 4, 5)
NFinalB <- readRDS("NFinalB.rds")

NFinalB$time <- paste0(NFinalB$Year, NFinalB$Month)

class(NFinalB$time) 
NFinalB$time <- as.numeric(NFinalB$time)
# Convert the year to a 4-digit format (e.g., change "15" to "2015")
FinalB$Year <- paste("20", FinalB$Year, sep = "")
NFinalB$Year <- paste("20", NFinalB$Year, sep = "")
FinalB$Month <- substr(FinalB$Month_Blitz, 7, 9)
FinalB$Month <- month_name_to_number(FinalB$Month)
#17609007 obs
NFinalB <-na.omit(FinalB)
#17527357 obs
names(NFinalB)
NFinalB <- NFinalB[NFinalB$Blitz_Gms!=0,]
#914529 rows
NFinalB <- NFinalB[NFinalB$B.day != 0, ]
#905812 rows
NFinalB$Year<-as.integer(NFinalB$Year)
NFinalB$Age_EachYear <- NFinalB$Year - NFinalB$B.day
head(NFinalB)
class(NFinalB$Year)
class(NFinalB$B.day)
min_age_each_yearB <- aggregate(Age_EachYear ~ ID.Number, data = NFinalB, FUN = min)
names(min_age_each_yearB) <- c("ID.Number","Min_Age")
# I want to rewrite the cells of Sex 
NFinalB$Sex <- ifelse(NFinalB$Sex == "M", 0, 1)
NFinalB$Fed <- as.factor(NFinalB$Fed)
NFinalB$Year <- as.factor(NFinalB$Year)
NFinalB$Month <- as.factor(NFinalB$Month)
NFinalB$Blitz_Rating <- as.numeric(NFinalB$Blitz_Rating)
NFinalB$Age_EachYear <- as.numeric(NFinalB$Age_EachYear)
min_age_each_yearB$Min_Age <- as.numeric(min_age_each_yearB$Min_Age)
class(NFinalB$Age_EachYear)
class(NFinalB$Blitz_Rating)
class(NFinalB$Sex)
class(NFinalB$Month)
class(NFinalB$Year)
class(min_age_each_yearB$Min_Age)
names(NFinalB)
NFinalB <- NFinalB %>% 
  select("ID.Number" ,"Month_Blitz"  , "Blitz_Rating" , "Blitz_Gms", "Blitz_K", "Sex","Name", "B.day" , "Fed" , "Flag" ,"Gamecat"  ,    "Year"  , "Month", "covid_dummy" , "Age_EachYear", "time")  
#Rapid 15574918
######NFinalBI <- NFinalB[NFinalB$Flag = i || wi && NFinalB$Year = 23 , ]

# Summarize house prices near and far from incinerator, and before and after
DNFinalB <- NFinalB %>% 
  select("ID.Number" ,"Sex" , "Flag" )
DNFinalB <-  DNFinalB[!duplicated( DNFinalB$ID.Number), ]
DNFinalB <- left_join(DNFinalB,min_age_each_yearB, by = "ID.Number")
names(DNFinalB) <- c("ID.Number" , "Sex" , "Flag", "Min_Age")
#Summary and aggregate also pooled ols
DNFinalB %>% str
DNFinalB %>% stargazer(type = "text")
n_row <- sum(DNFinalB$Sex == 0)
#159054
names(NFinalB)
resultB <- aggregate(Min_Age ~ Sex, data = DNFinalB, FUN = function(x) c(Mean = mean(x), Variance = var(x), Count = length(x))) 
resultNFB <- aggregate(Blitz_Rating ~ Sex + covid_dummy, data = NFinalB, FUN = function(x) c(Mean = mean(x), Variance = var(x), Count = length(x))) 
head(min_age_each_yearB)
print(resultNFB)
print(resultB)
# Pooled OLS estimator
#NFinalR
install.packages("plm")
NFinalB <- NULL
model_olsB<- NULL

# Load the plm package
library(plm)
model_olsB <- plm(formula = Blitz_Rating ~  covid_dummy * Sex + covid_dummy  + Sex+Year + Month + Age_EachYear + 1 , 
                  data = NFinalB, 
                  index = c("ID.Number", "time"), # c(group index, time index)
                  model = "pooling")  
summary(model_olsB)
options(max.print = 5000)  

group_by(Sex) %>%
  summarize_at(.vars = vars(Min_Age), 
               .funs = list(mean = mean, obs = length))
file_path8 <- paste0(directory_path, "min_age_each_yearB.rds")
file_path9 <- paste0(directory_path, "NFinalB.rds")
file_path10 <- paste0(directory_path, "DNFinalB.rds")

saveRDS(min_age_each_yearB, file = file_path8)
saveRDS(NFinalB, file = file_path9)
saveRDS(DNFinalB, file = file_path10)

############################# Rapid####################
readRDS()
FinalR$Month <- month_name_to_number(FinalR$Month)
#Rapid 15696701
#Blitz 17609007
NFinalR <-na.omit(FinalR)
names(NFinalR)
NFinalR <- NFinalR[NFinalR$Rapid_Gms != 0, ]
#1259371
NFinalR <- NFinalR[NFinalR$B.day != 0, ]
#1245659
NFinalR$Year<-as.integer(NFinalR$Year)
NFinalR$Age_EachYear <- NFinalR$Year - NFinalR$B.day
head(NFinalR)
class(NFinalR$Year)
class(NFinalR$B.day)
min_age_each_year <- aggregate(Age_EachYear ~ ID.Number, data = NFinalR, FUN = min)
names(min_age_each_year) <- c("ID.Number","Min_Age")
# I want to rewrite the cells of Sex 
NFinalR$Sex <- ifelse(NFinalR$Sex == "M", 0, 1)
NFinalR$Fed <- as.factor(NFinalR$Fed)
NFinalR$Year <- as.factor(NFinalR$Year)
NFinalR$Month <- as.factor(NFinalR$Month)
NFinalR$Rapid_Rating <- as.numeric(NFinalR$Rapid_Rating)
NFinalR$Age_EachYear <- as.numeric(NFinalR$Age_EachYear)

class(NFinalR$Rapid_Rating)
class(NFinalR$Sex)
class(NFinalR$Month)
class(NFinalR$Year)
class(min_age_each_year$Age_EachYear)
names(NFinalR)
NFinalRI <- NFinalR[NFinalR$Flag == i, ]
NFinalR <- NFinalR %>% 
  select("ID.Number" ,"Month_Rapid"  , "Rapid_Rating" , "Rapid_Gms", "Rapid_K", "Sex","Name", "B.day" , "Fed" , "Flag" ,"Gamecat"  ,    "Year"  ,       "Month"  )
#Rapid 15574918

# Apply the function to your data frame
FinalR$Month <- substr(FinalR$Month_Rapid, 7, 9)
FinalR$Month <- month_name_to_number(FinalR$Month)

FinalR$Gamecat="Rapid"
FinalS <- readRDS("FinalS.rds")
FinalS$covid_dummyS <- ifelse(FinalS$Month_Standard %in% covid_namesS, 1, 0)
na.omit(R1502_demog)
#279117
FinalS$Gamecat="Standard"
print(FinalR)
print(FinalB)
print(FinalS)
#Final<-rbind(FinalB,FinalR,FinalS)
Final <- left_join(FinalS, FinalB, by = "ID.Number")
#Error: cannot allocate vector of size 5.6 Gb
Final <- left_join(FinalB, FinalR, by = "ID.Number")
merged_dataB1502_demog <- merge(merged_dataB2003, B1502_demog, by = "ID.Number", all = TRUE)

# Specify the full path including the file name and extension
file_path <- paste0(directory_path, "FinalB.rds")
file_path2 <- paste0(directory_path, "FinalR.rds")
file_path3 <- paste0(directory_path, "FinalS.rds")
file_path4 <- paste0(directory_path, "NFinalR.rds")
# file_path5 <- paste0(directory_path, "FinalS.rds")
file_path6 <- paste0(directory_path, "min_age_each_year.rds")
file_path7 <- paste0(directory_path, "DNFinalR.rds")
saveRDS(DNFinalR, file = file_path7)
saveRDS(NFinalR, file = file_path4)
NFinalR <- readRDS("NFinalR.rds")
min_age_each_year <- readRDS("min_age_each_year.rds")
min_age_each_yearB <- readRDS("min_age_each_yearB.rds")

saveRDS(min_age_each_year, file = file_path6)
# rewite for the master
directory_path <- 'A:\\payan_name\\fide_data\\2016\\csv16\\checkup\\merged_data\\clean'
setwd(directory_path)

getwd()

file_pathc <- paste0(directory_pathc, "min_age_each_year.csv")
save(min_age_each_year, file = file_pathc)
output_dir <- 'A:\\payan_name\\fide_data\\2016\\csv16\\checkup\\merged_data'

output_file <- file.path(output_dir, paste0("min_age_each_year",".csv"))
output_file1 <- file.path(output_dir, paste0("NFinal",".csv"))
output_file2 <- file.path(output_dir, paste0("dNFinal",".csv"))

# Save the merged data for the current game type
write.csv(min_age_each_year, output_file)
write.csv(NFinal, output_file1)
write.csv(dNFinal, output_file2)

write.csv(min_age_each_year, output_file)
write.csv(min_age_each_year, output_file)

# Enclose the path in quotes
write.csv(min_age_each_year, "D:\\Desktop\\reportr")
write.csv(NFinal, "A:\\payan_name\\fide_data\\2016\\csv16\\checkup\\merged_data\\clean")

# Save the R object to the specified file path
saveRDS(FinalB, file = file_path)
#saveRDS(FinalB, file="FinalB.rds")
FinalR <- left_join(NRresult,combined_demog, by = "ID.Number")
saveRDS(FinalR, file = file_path2)

FinalS <- left_join(NSresult,combined_demog, by = "ID.Number")
saveRDS(FinalS, file = file_path3)

B2307_demog <- B2307 %>% 
  select("ID.Number" , "Sex" , "Name" , "B.day" , "Fed" , "Flag")#
saveRDS( B2307_demog, file_path6)
FinalB2307_demog <- left_join(NBresult,B2307_demog, by = "ID.Number")
# Check if "ColumnA" and "ColumnB" have NA values
has_na_columnA <- any(is.na(FinalB2307_demog$Name))
has_na_columnB <- any(is.na(FinalB2307_demog$Sex))
has_na_columnC <- any(is.na(FinalB2307_demog$B.day))
has_na_columnD <- any(is.na(FinalB2307_demog$Fed))
# Print the results
if (has_na_columnA) {
  cat("ColumnA has NA values.\n")
} else {
  cat("ColumnA does not have NA values.\n")
}

if (has_na_columnB) {
  cat("ColumnB has NA values.\n")
} else {
  cat("ColumnB does not have NA values.\n")
}
if (has_na_columnC) {
  cat("ColumnC has NA values.\n")
} else {
  cat("ColumnC does not have NA values.\n")
}

if (has_na_columnD) {
  cat("ColumnD has NA values.\n")
} else {
  cat("ColumnD does not have NA values.\n")
}
# All columns have NA values.
R2307_demog <- R2307 %>% 
  select("ID.Number" , "Sex" , "Name" , "B.day" , "Fed" , "Flag")#
file_path4 <- paste0(directory_path, "R2307_demog.rds")
file_path5 <- paste0(directory_path, "S2307_demog.rds")
file_path6 <- paste0(directory_path, "B2307_demog.rds")
saveRDS( R2307_demog,file_path4)
FinalR2307_demog <- left_join(NBresult,R2307_demog, by = "ID.Number")
S2307_demog <- S2307 %>% 
  select("ID.Number" , "Sex" , "Name" , "B.day" , "Fed" , "Flag")#
saveRDS( S2307_demog,file_path5)
FinalS2307_demog <- left_join(NBresult,S2307_demog, by = "ID.Number")
# Replace missing values with "NA"
result[is.na(result)] <- "NA"
head(FinalR)
# Print the result
print(result)

# Subsetting columns
B1502_wide_clean <- B1502_wide_clean[, c("ID.Number", "Month_Blitz", "Blitz_Rating")]
B1502_wide_clean_Gms <- B1502_wide_clean_Gms[, c("ID.Number", "Blitz_Gms")]

# Then perform the join
B1502_merged <- B1502_wide_clean %>%
  left_join(B1502_wide_clean_Gms, by = c("ID.Number"))



#  select("ID.Number",all_of(Fid_seq))
###################BEFORE
B1502_gender<-B1502 %>% 
  select("ID.Number","Sex")
B2307_gender<-B2307 %>% 
  select("sex")
B2307 <- merge(B2307, B1502_gender, by = "ID.Number", all = TRUE)
B2307$Bio <- ifelse(B2307$ID.Number %in% B1502_gender$ID.Number, ifelse(B2307$Sex.x == B1502_gender$Sex, "No", "Yes"), NA)
unique(B2307$Bio)
B2307$Bio <-  ifelse(B2307$Sex.x == B2307$Sex.y, "No", "Yes")
B2307$taghir<-B2307$Bio=="Yes" 
B2307$taghir <- B2307$Bio == "Yes"
count_yes <- sum(B2307$taghir)

B2307$taghir <- B2307$Bio == "Yes"
count_yes <- sum(B2307$taghir)

if (is.na(count_yes)) {
  count_yes <- 0
}

print(count_yes)
#asir shodima the number is 0 However we have Yes in unique
bio_gen
merged_dataB2003 <- merge(B1503_demog,B2003_demog, by = "ID.Number", all = TRUE)
merged_dataB1502_demog <- merge(merged_dataB2003, B1502_demog, by = "ID.Number", all = TRUE)
#select("ID.Number" , "Sex" , "Name" , "B.day" , "Fed" , "Flag" , "Tit","WTit","OTit","FOA")  

# FOA_name<- unique(B1502_demog$FOA)
# ""    NA    "AGM" "AIM" "AFM" "ACM"

dataaa<-NULL
dataaa <- merge(B1502.rds,R1502.rds,all=T, by="ID.Number" )
dataaa <- merge(dataaa,S1502.rds,all=T, by="ID.Number" )

#filter if sex or fed changed
B1502 <- data_list[["B1502.csv"]]
names(B1502)
excol2 <- c(  "Tit"  , "WTit"  ,  "OTit"   ,   "X15.Feb" , "Gms"  ,"K" , "Flag"  )
B1502 <- B1502[,! names(B1502) %in% excol2]
names(B1502)
names(B1502) <- c("ID.Number", "Name1502", "Fed1502"  ,  "Sex1502"  ,  "B.day1502"    )

bio <- merge(dataaa,B1502,all=T, by="ID.Number" )
#604122 obseof  950 variables
bio_diffilter<- bio[bio$Sex.x == bio$Sex1502  & bio$Fed1502==bio$Fed  & bio$Name1502==bio$Name.y & bio$B.day==bio$B.day1502, ]
saveRDS(bio_diffilter, file="bio_diffilter.rds")
#601790 obs of 950 var
bio_diffilter2 <-bio[bio$Sex== bio$Sex1502, ]
saveRDS(bio_diffilter2, file="bio_diffilter2.rds") 
bio_diffilter2<- readRDS("bio_diffilter2.rds")
#604078 obs of 950
bio2na<-na.omit(bio_diffilter2)

fbio2na <- bio2na [!((bio2na$X23.Jul < 1400) | 
                       (bio2na$X23.Jul.x < 1400) | 
                       (bio2na$X23.Jul.y < 1400)), ]
saveRDS(fbio2na, file="fbio2na.rds") 
fbio2na<- readRDS("fbio2na.rds")
#19926
#counting
# Check for duplicate rows
duplicate_rows <- fbio2na[duplicated(fbio2na) | duplicated(fbio2na, fromLast = TRUE), ]
# the result is 0 so we don't have any duplication all the rows of my data frame are unique
names(fbio2na)
print(B1603)
column_counts <- colSums(!is.na(fbio2na))
count_non_missing <- sum(!is.na(fbio2na$ID.Number))
# Display the number of non-missing values for each column
print(count_non_missing)
duplicate_names <- names(fbio2na)[duplicated(names(fbio2na))]

names(fbio2na)<-c(  "ID.Number" , "B_X15.Feb" , "B_GmsX15.Feb" , "B_KX15.Feb" , "B_X15.Mar" , "B_GmsX15.Mar" , "B_KX15.Mar" , "B_X15.Apr" , "B_GmsX15.Apr"   
                    ,  "B_KX15.Apr" , "B_X15.May" , "B_GmsX15.May" , "B_KX15.May" , "B_X15.Jun" , "B_GmsX15.Jun" , "B_KX15.Jun" , "B_X15.Jul" , "B_GmsX15.Jul"   
                    , "B_KX15.Jul" , "B_X15.Aug" , "B_GmsX15.Aug" , "B_KX15.Aug" , "B_X15.Sep" , "B_GmsX15.Sep" , "B_KX15.Sep" , "B_X15.Oct" , "B_GmsX15.Oct"   
                    , "B_KX15.Oct" , "B_X15.Nov" , "B_GmsX15.Nov" , "B_KX15.Nov" , "B_X15.Dec" , "B_GmsX15.Dec" , "B_KX15.Dec" , "B_X16.Jan" , "B_GmsX16.Jan"   
                    , "B_KX16.Jan" , "B_X16.Feb" , "B_GmsX16.Feb" , "B_KX16.Feb" , "B_X16.Mar" , "B_GmsX16.Mar" , "B_KX16.Mar" , "B_X16.Apr" , "B_GmsX16.Apr"   
                    , "B_KX16.Apr" , "B_X16.May" , "B_GmsX16.May" , "B_KX16.May" , "B_X16.Jun" , "B_GmsX16.Jun" , "B_KX16.Jun" , "B_X16.Jul" , "B_GmsX16.Jul"   
                    , "B_KX16.Jul" , "B_X16.Aug" , "B_GmsX16.Aug" , "B_KX16.Aug" , "B_X16.Sep" , "B_GmsX16.Sep" , "B_KX16.Sep" , "B_X16.Oct" , "B_GmsX16.Oct"   
                    ,  "B_KX16.Oct" , "B_X16.Nov" , "B_GmsX16.Nov" , "B_KX16.Nov" , "B_X16.Dec" , "B_GmsX16.Dec" , "B_KX16.Dec" , "B_X17.Jan" , "B_GmsX17.Jan"   
                    ,  "B_KX17.Jan" ,  "B_X17.Feb" , "B_GmsX17.Feb" , "B_KX17.Feb" , "B_X17.Mar" , "B_GmsX17.Mar" , "B_KX17.Mar" , "B_X17.Apr" , "B_GmsX17.Apr"   
                    ,  "B_KX17.Apr" , "B_X17.May" , "B_GmsX17.May" , "B_KX17.May" , "B_X17.Jun" , "B_GmsX17.Jun" , "B_KX17.Jun" , "B_X17.Jul" , "B_GmsX17.Jul"   
                    ,       "B_KX17.Jul" , "B_X17.Aug" , "B_GmsX17.Aug" , "B_KX17.Aug" , "B_X17.Sep" , "B_GmsX17.Sep" , "B_KX17.Sep" , "B_X17.Oct" , "B_GmsX17.Oct"   
                    , "B_KX17.Oct" , "B_X17.Nov" , "B_GmsX17.Nov" , "B_KX17.Nov" , "B_X17.Dec" , "B_GmsX17.Dec" , "B_KX17.Dec" , "B_X18.Jan" , "B_GmsX18.Jan"   
                    , "B_KX18.Jan" , "B_X18.Feb" , "B_GmsX18.Feb" , "B_KX18.Feb" , "B_X18.Mar" , "B_GmsX18.Mar" , "B_KX18.Mar" , "B_X18.Apr" , "B_GmsX18.Apr"   
                    , "B_KX18.Apr" , "B_X18.May" , "B_GmsX18.May" , "B_KX18.May" , "B_X18.Jun" , "B_GmsX18.Jun" , "B_KX18.Jun" , "B_X18.Jul" , "B_GmsX18.Jul"   
                    , "B_KX18.Jul" , "B_X18.Aug" , "B_GmsX18.Aug" , "B_KX18.Aug" , "B_X18.Sep" , "B_GmsX18.Sep" , "B_KX18.Sep" , "B_X18.Oct" , "B_GmsX18.Oct"   
                    , "B_KX18.Oct" , "B_X18.Nov" , "B_GmsX18.Nov" , "B_KX18.Nov" , "B_X18.Dec" , "B_GmsX18.Dec" , "B_KX18.Dec" , "B_X19.Jan" , "B_GmsX19.Jan"   
                    , "B_KX19.Jan" , "B_X19.Feb" , "B_GmsX19.Feb" , "B_KX19.Feb" , "B_X19.Mar" , "B_GmsX19.Mar" , "B_KX19.Mar" , "B_X19.Apr" , "B_GmsX19.Apr"   
                    , "B_KX19.Apr" , "B_X19.May" , "B_GmsX19.May" , "B_KX19.May" , "B_X19.Jun" , "B_GmsX19.Jun" , "B_KX19.Jun" , "B_X19.Jul" , "B_GmsX19.Jul"   
                    , "B_KX19.Jul" , "B_X19.Aug" , "B_GmsX19.Aug" , "B_KX19.Aug" , "B_X19.Sep" , "B_GmsX19.Sep" , "B_KX19.Sep" , "B_X19.Oct" , "B_GmsX19.Oct"   
                    , "B_KX19.Oct" , "B_X19.Nov" , "B_GmsX19.Nov" , "B_KX19.Nov" , "B_X19.Dec" , "B_GmsX19.Dec" , "B_KX19.Dec" , "B_X20.Jan" , "B_GmsX20.Jan"   
                    , "B_KX20.Jan" , "B_X20.Feb" , "B_GmsX20.Feb" , "B_KX20.Feb" , "B_X20.Mar" , "B_GmsX20.Mar" , "B_KX20.Mar" , "B_X20.Apr" , "B_GmsX20.Apr"   
                    , "B_KX20.Apr" , "B_X20.May" , "B_GmsX20.May" , "B_KX20.May" , "B_X20.Jun" , "B_GmsX20.Jun" , "B_KX20.Jun" , "B_X20.Jul" , "B_GmsX20.Jul"   
                    , "B_KX20.Jul" , "B_X20.Aug" , "B_GmsX20.Aug" , "B_KX20.Aug" , "B_X20.Sep" , "B_GmsX20.Sep" , "B_KX20.Sep" , "B_X20.Oct" , "B_GmsX20.Oct"   
                    , "B_KX20.Oct" , "B_X20.Nov" , "B_GmsX20.Nov" , "B_KX20.Nov" , "B_X20.Dec" , "B_GmsX20.Dec" , "B_KX20.Dec" , "B_X21.Jan" , "B_GmsX21.Jan"   
                    , "B_KX21.Jan" , "B_X21.Feb" , "B_GmsX21.Feb" , "B_KX21.Feb" , "B_X21.Mar" , "B_GmsX21.Mar" , "B_KX21.Mar" , "B_X21.Apr" , "B_GmsX21.Apr"   
                    , "B_KX21.Apr" , "B_X21.May" , "B_GmsX21.May" , "B_KX21.May" , "B_X21.Jun" , "B_GmsX21.Jun" , "B_KX21.Jun" , "B_X21.Jul" , "B_GmsX21.Jul"   
                    , "B_KX21.Jul" , "B_X21.Aug" , "B_GmsX21.Aug" , "B_KX21.Aug" , "B_X21.Sep" , "B_GmsX21.Sep" , "B_KX21.Sep" , "B_X21.Oct" , "B_GmsX21.Oct"   
                    , "B_KX21.Oct" , "B_X21.Nov" , "B_GmsX21.Nov" , "B_KX21.Nov" , "B_X21.Dec" , "B_GmsX21.Dec" , "B_KX21.Dec" , "B_X22.Jan" , "B_GmsX22.Jan"   
                    , "B_KX22.Jan" , "B_X22.Feb" , "B_GmsX22.Feb" , "B_KX22.Feb" , "B_X22.Mar" , "B_GmsX22.Mar" , "B_KX22.Mar" , "B_X22.Apr" , "B_GmsX22.Apr"   
                    , "B_KX22.Apr" , "B_X22.May" , "B_GmsX22.May" , "B_KX22.May" , "B_X22.Jun" , "B_GmsX22.Jun" , "B_KX22.Jun" , "B_X22.Jul" , "B_GmsX22.Jul"   
                    , "B_KX22.Jul" , "B_X22.Aug" , "B_GmsX22.Aug" , "B_KX22.Aug" , "B_X22.Sep" , "B_GmsX22.Sep" , "B_KX22.Sep" , "B_X22.Oct" , "B_GmsX22.Oct"   
                    , "B_KX22.Oct" , "B_X22.Nov" , "B_GmsX22.Nov" , "B_KX22.Nov" , "B_X22.Dec" , "B_GmsX22.Dec" , "B_KX22.Dec" , "B_X23.Jan" , "B_GmsX23.Jan"   
                    , "B_KX23.Jan" , "B_X23.Feb" , "B_GmsX23.Feb" , "B_KX23.Feb" , "B_X23.Mar" , "B_GmsX23.Mar" , "B_KX23.Mar" , "B_X23.Apr" , "B_GmsX23.Apr"   
                    , "B_KX23.Apr" , "B_X23.May" , "B_GmsX23.May" , "B_KX23.May"  ,    "B_X23.Jun" , "B_GmsX23.Jun" , "B_KX23.Jun" , "Name.x" ,"Fed.x"     
                    , "Sex.x"   ,   "Tit.x"    ,  "WTit.x"  ,   "OTit.x"    , "FOA.x"  ,    "B_X23.Jul" , "B_GmsX23.Jul"   ,   "B_KX23.Jul"    ,    "B.day.x"   
                    ,  "Flag.x" ,"R_X15.Feb", "R_GmsX15.Feb", "R_KX15.Feb", "R_X15.Mar", "R_GmsX15.Mar", "R_KX15.Mar", "R_X15.Apr", "R_GmsX15.Apr",
                    "R_KX15.Apr", "R_X15.May", "R_GmsX15.May", "R_KX15.May", "R_X15.Jun", "R_GmsX15.Jun", "R_KX15.Jun", "R_X15.Jul",
                    "R_GmsX15.Jul", "R_KX15.Jul", "R_X15.Aug", "R_GmsX15.Aug", "R_KX15.Aug", "R_X15.Sep", "R_GmsX15.Sep", "R_KX15.Sep",
                    "R_X15.Oct", "R_GmsX15.Oct", "R_KX15.Oct", "R_X15.Nov", "R_GmsX15.Nov", "R_KX15.Nov", "R_X15.Dec", "R_GmsX15.Dec",
                    "R_KX15.Dec", "R_X16.Jan", "R_GmsX16.Jan", "R_KX16.Jan", "R_X16.Feb", "R_GmsX16.Feb", "R_KX16.Feb", "R_X16.Mar",
                    "R_GmsX16.Mar", "R_KX16.Mar", "R_X16.Apr", "R_GmsX16.Apr", "R_KX16.Apr", "R_X16.May", "R_GmsX16.May", "R_KX16.May",
                    "R_X16.Jun", "R_GmsX16.Jun", "R_KX16.Jun", "R_X16.Jul", "R_GmsX16.Jul", "R_KX16.Jul", "R_X16.Aug", "R_GmsX16.Aug",
                    "R_KX16.Aug", "R_X16.Sep", "R_GmsX16.Sep", "R_KX16.Sep", "R_X16.Oct", "R_GmsX16.Oct", "R_KX16.Oct", "R_X16.Nov",
                    "R_GmsX16.Nov", "R_KX16.Nov", "R_X16.Dec", "R_GmsX16.Dec", "R_KX16.Dec", "R_X17.Jan", "R_GmsX17.Jan", "R_KX17.Jan",
                    "R_X17.Feb", "R_GmsX17.Feb", "R_KX17.Feb", "R_X17.Mar", "R_GmsX17.Mar", "R_KX17.Mar", "R_X17.Apr", "R_GmsX17.Apr",
                    "R_KX17.Apr", "R_X17.May", "R_GmsX17.May", "R_KX17.May", "R_X17.Jun", "R_GmsX17.Jun", "R_KX17.Jun", "R_X17.Jul",
                    "R_GmsX17.Jul", "R_KX17.Jul", "R_X17.Aug", "R_GmsX17.Aug", "R_KX17.Aug", "R_X17.Sep", "R_GmsX17.Sep", "R_KX17.Sep",
                    "R_X17.Oct", "R_GmsX17.Oct", "R_KX17.Oct", "R_X17.Nov", "R_GmsX17.Nov", "R_KX17.Nov", "R_X17.Dec", "R_GmsX17.Dec",
                    "R_KX17.Dec", "R_X18.Jan", "R_GmsX18.Jan", "R_KX18.Jan", "R_X18.Feb", "R_GmsX18.Feb", "R_KX18.Feb", "R_X18.Mar",
                    "R_GmsX18.Mar", "R_KX18.Mar", "R_X18.Apr", "R_GmsX18.Apr", "R_KX18.Apr", "R_X18.May", "R_GmsX18.May", "R_KX18.May",
                    "R_X18.Jun", "R_GmsX18.Jun", "R_KX18.Jun", "R_X18.Jul", "R_GmsX18.Jul", "R_KX18.Jul", "R_X18.Aug", "R_GmsX18.Aug",
                    "R_KX18.Aug", "R_X18.Sep", "R_GmsX18.Sep", "R_KX18.Sep", "R_X18.Oct", "R_GmsX18.Oct", "R_KX18.Oct", "R_X18.Nov",
                    "R_GmsX18.Nov", "R_KX18.Nov", "R_X18.Dec", "R_GmsX18.Dec", "R_KX18.Dec", "R_X19.Jan", "R_GmsX19.Jan", "R_KX19.Jan",
                    "R_X19.Feb", "R_GmsX19.Feb", "R_KX19.Feb", "R_X19.Mar", "R_GmsX19.Mar", "R_KX19.Mar", "R_X19.Apr", "R_GmsX19.Apr",
                    "R_KX19.Apr", "R_X19.May", "R_GmsX19.May", "R_KX19.May", "R_X19.Jun", "R_GmsX19.Jun", "R_KX19.Jun", "R_X19.Jul",
                    "R_GmsX19.Jul", "R_KX19.Jul", "R_X19.Aug", "R_GmsX19.Aug", "R_KX19.Aug", "R_X19.Sep", "R_GmsX19.Sep", "R_KX19.Sep",
                    "R_X19.Oct", "R_GmsX19.Oct", "R_KX19.Oct", "R_X19.Nov", "R_GmsX19.Nov", "R_KX19.Nov", "R_X19.Dec", "R_GmsX19.Dec",
                    "R_KX19.Dec", "R_X20.Jan", "R_GmsX20.Jan", "R_KX20.Jan", "R_X20.Feb", "R_GmsX20.Feb", "R_KX20.Feb", "R_X20.Mar",
                    "R_GmsX20.Mar", "R_KX20.Mar", "R_X20.Apr", "R_GmsX20.Apr", "R_KX20.Apr", "R_X20.May", "R_GmsX20.May", "R_KX20.May",
                    "R_X20.Jun", "R_GmsX20.Jun", "R_KX20.Jun", "R_X20.Jul", "R_GmsX20.Jul", "R_KX20.Jul", "R_X20.Aug", "R_GmsX20.Aug",
                    "R_KX20.Aug", "R_X20.Sep", "R_GmsX20.Sep", "R_KX20.Sep", "R_X20.Oct", "R_GmsX20.Oct", "R_KX20.Oct", "R_X20.Nov",
                    "R_GmsX20.Nov", "R_KX20.Nov", "R_X20.Dec", "R_GmsX20.Dec", "R_KX20.Dec", "R_X21.Jan", "R_GmsX21.Jan", "R_KX21.Jan",
                    "R_X21.Feb", "R_GmsX21.Feb", "R_KX21.Feb", "R_X21.Mar", "R_GmsX21.Mar", "R_KX21.Mar", "R_X21.Apr", "R_GmsX21.Apr",
                    "R_KX21.Apr", "R_X21.May", "R_GmsX21.May", "R_KX21.May", "R_X21.Jun", "R_GmsX21.Jun", "R_KX21.Jun", "R_X21.Jul",
                    "R_GmsX21.Jul", "R_KX21.Jul", "R_X21.Aug", "R_GmsX21.Aug", "R_KX21.Aug", "R_X21.Sep", "R_GmsX21.Sep", "R_KX21.Sep",
                    "R_X21.Oct", "R_GmsX21.Oct", "R_KX21.Oct", "R_X21.Nov", "R_GmsX21.Nov", "R_KX21.Nov", "R_X21.Dec", "R_GmsX21.Dec",
                    "R_KX21.Dec", "R_X22.Jan", "R_GmsX22.Jan", "R_KX22.Jan", "R_X22.Feb", "R_GmsX22.Feb", "R_KX22.Feb", "R_X22.Mar",
                    "R_GmsX22.Mar", "R_KX22.Mar", "R_X22.Apr", "R_GmsX22.Apr", "R_KX22.Apr", "R_X22.May", "R_GmsX22.May", "R_KX22.May",
                    "R_X22.Jun", "R_GmsX22.Jun", "R_KX22.Jun", "R_X22.Jul", "R_GmsX22.Jul", "R_KX22.Jul", "R_X22.Aug", "R_GmsX22.Aug",
                    "R_KX22.Aug", "R_X22.Sep", "R_GmsX22.Sep", "R_KX22.Sep", "R_X22.Oct", "R_GmsX22.Oct", "R_KX22.Oct", "R_X22.Nov",
                    "R_GmsX22.Nov", "R_KX22.Nov", "R_X22.Dec", "R_GmsX22.Dec", "R_KX22.Dec", "R_X23.Jan", "R_GmsX23.Jan", "R_KX23.Jan",
                    "R_X23.Feb", "R_GmsX23.Feb", "R_KX23.Feb", "R_X23.Mar", "R_GmsX23.Mar", "R_KX23.Mar", "R_X23.Apr", "R_GmsX23.Apr",
                    "R_KX23.Apr", "R_X23.May", "R_GmsX23.May", "R_KX23.May", "R_X23.Jun", "R_GmsX23.Jun", "R_KX23.Jun",   "Name.y" , "Fed.y"     
                    ,  "Sex.y" , "Tit.y" , "WTit.y" , "OTit.y" , "FOA.y" , "R_X23.Jul", "R_GmsX23.Jul", "R_KX23.Jul" , "B.day.y"   
                    , "Flag.y", "S_X15.Feb", "S_GmsX15.Feb", "S_KX15.Feb", "S_X15.Mar", "S_GmsX15.Mar", "S_KX15.Mar", "S_X15.Apr", "S_GmsX15.Apr",
                    "S_KX15.Apr", "S_X15.May", "S_GmsX15.May", "S_KX15.May", "S_X15.Jun", "S_GmsX15.Jun", "S_KX15.Jun", "S_X15.Jul",
                    "S_GmsX15.Jul", "S_KX15.Jul", "S_X15.Aug", "S_GmsX15.Aug", "S_KX15.Aug", "S_X15.Sep", "S_GmsX15.Sep", "S_KX15.Sep",
                    "S_X15.Oct", "S_GmsX15.Oct", "S_KX15.Oct", "S_X15.Nov", "S_GmsX15.Nov", "S_KX15.Nov", "S_X15.Dec", "S_GmsX15.Dec",
                    "S_KX15.Dec", "S_X16.Jan", "S_GmsX16.Jan", "S_KX16.Jan", "S_X16.Feb", "S_GmsX16.Feb", "S_KX16.Feb", "S_X16.Mar",
                    "S_GmsX16.Mar", "S_KX16.Mar", "S_X16.Apr", "S_GmsX16.Apr", "S_KX16.Apr", "S_X16.May", "S_GmsX16.May", "S_KX16.May",
                    "S_X16.Jun", "S_GmsX16.Jun", "S_KX16.Jun", "S_X16.Jul", "S_GmsX16.Jul", "S_KX16.Jul", "S_X16.Aug", "S_GmsX16.Aug",
                    "S_KX16.Aug", "S_X16.Sep", "S_GmsX16.Sep", "S_KX16.Sep", "S_X16.Oct", "S_GmsX16.Oct", "S_KX16.Oct", "S_X16.Nov",
                    "S_GmsX16.Nov", "S_KX16.Nov", "S_X16.Dec", "S_GmsX16.Dec", "S_KX16.Dec", "S_X17.Jan", "S_GmsX17.Jan", "S_KX17.Jan",
                    "S_X17.Feb", "S_GmsX17.Feb", "S_KX17.Feb", "S_X17.Mar", "S_GmsX17.Mar", "S_KX17.Mar", "S_X17.Apr", "S_GmsX17.Apr",
                    "S_KX17.Apr", "S_X17.May", "S_GmsX17.May", "S_KX17.May", "S_X17.Jun", "S_GmsX17.Jun", "S_KX17.Jun", "S_X17.Jul",
                    "S_GmsX17.Jul", "S_KX17.Jul", "S_X17.Aug", "S_GmsX17.Aug", "S_KX17.Aug", "S_X17.Sep", "S_GmsX17.Sep", "S_KX17.Sep",
                    "S_X17.Oct", "S_GmsX17.Oct", "S_KX17.Oct", "S_X17.Nov", "S_GmsX17.Nov", "S_KX17.Nov", "S_X17.Dec", "S_GmsX17.Dec",
                    "S_KX17.Dec", "S_X18.Jan", "S_GmsX18.Jan", "S_KX18.Jan", "S_X18.Feb", "S_GmsX18.Feb", "S_KX18.Feb", "S_X18.Mar",
                    "S_GmsX18.Mar", "S_KX18.Mar", "S_X18.Apr", "S_GmsX18.Apr", "S_KX18.Apr", "S_X18.May", "S_GmsX18.May", "S_KX18.May",
                    "S_X18.Jun", "S_GmsX18.Jun", "S_KX18.Jun", "S_X18.Jul", "S_GmsX18.Jul", "S_KX18.Jul", "S_X18.Aug", "S_GmsX18.Aug",
                    "S_KX18.Aug", "S_X18.Sep", "S_GmsX18.Sep", "S_KX18.Sep", "S_X18.Oct", "S_GmsX18.Oct", "S_KX18.Oct", "S_X18.Nov",
                    "S_GmsX18.Nov", "S_KX18.Nov", "S_X18.Dec", "S_GmsX18.Dec", "S_KX18.Dec", "S_X19.Jan", "S_GmsX19.Jan", "S_KX19.Jan",
                    "S_X19.Feb", "S_GmsX19.Feb", "S_KX19.Feb", "S_X19.Mar", "S_GmsX19.Mar", "S_KX19.Mar", "S_X19.Apr", "S_GmsX19.Apr",
                    "S_KX19.Apr", "S_X19.May", "S_GmsX19.May", "S_KX19.May", "S_X19.Jun", "S_GmsX19.Jun", "S_KX19.Jun", "S_X19.Jul",
                    "S_GmsX19.Jul", "S_KX19.Jul", "S_X19.Aug", "S_GmsX19.Aug", "S_KX19.Aug", "S_X19.Sep", "S_GmsX19.Sep", "S_KX19.Sep",
                    "S_X19.Oct", "S_GmsX19.Oct", "S_KX19.Oct", "S_X19.Nov", "S_GmsX19.Nov", "S_KX19.Nov", "S_X19.Dec", "S_GmsX19.Dec",
                    "S_KX19.Dec", "S_X20.Jan", "S_GmsX20.Jan", "S_KX20.Jan", "S_X20.Feb", "S_GmsX20.Feb", "S_KX20.Feb", "S_X20.Mar",
                    "S_GmsX20.Mar", "S_KX20.Mar", "S_X20.Apr", "S_GmsX20.Apr", "S_KX20.Apr", "S_X20.May", "S_GmsX20.May", "S_KX20.May",
                    "S_X20.Jun", "S_GmsX20.Jun", "S_KX20.Jun", "S_X20.Jul", "S_GmsX20.Jul", "S_KX20.Jul", "S_X20.Aug", "S_GmsX20.Aug",
                    "S_KX20.Aug", "S_X20.Sep", "S_GmsX20.Sep", "S_KX20.Sep", "S_X20.Oct", "S_GmsX20.Oct", "S_KX20.Oct", "S_X20.Nov",
                    "S_GmsX20.Nov", "S_KX20.Nov", "S_X20.Dec", "S_GmsX20.Dec", "S_KX20.Dec", "S_X21.Jan", "S_GmsX21.Jan", "S_KX21.Jan",
                    "S_X21.Feb", "S_GmsX21.Feb", "S_KX21.Feb", "S_X21.Mar", "S_GmsX21.Mar", "S_KX21.Mar", "S_X21.Apr", "S_GmsX21.Apr",
                    "S_KX21.Apr", "S_X21.May", "S_GmsX21.May", "S_KX21.May", "S_X21.Jun", "S_GmsX21.Jun", "S_KX21.Jun", "S_X21.Jul",
                    "S_GmsX21.Jul", "S_KX21.Jul", "S_X21.Aug", "S_GmsX21.Aug", "S_KX21.Aug", "S_X21.Sep", "S_GmsX21.Sep", "S_KX21.Sep",
                    "S_X21.Oct", "S_GmsX21.Oct", "S_KX21.Oct", "S_X21.Nov", "S_GmsX21.Nov", "S_KX21.Nov", "S_X21.Dec", "S_GmsX21.Dec",
                    "S_KX21.Dec", "S_X22.Jan", "S_GmsX22.Jan", "S_KX22.Jan", "S_X22.Feb", "S_GmsX22.Feb", "S_KX22.Feb", "S_X22.Mar",
                    "S_GmsX22.Mar", "S_KX22.Mar", "S_X22.Apr", "S_GmsX22.Apr", "S_KX22.Apr", "S_X22.May", "S_GmsX22.May", "S_KX22.May",
                    "S_X22.Jun", "S_GmsX22.Jun", "S_KX22.Jun", "S_X22.Jul", "S_GmsX22.Jul", "S_KX22.Jul", "S_X22.Aug", "S_GmsX22.Aug",
                    "S_KX22.Aug", "S_X22.Sep", "S_GmsX22.Sep", "S_KX22.Sep", "S_X22.Oct", "S_GmsX22.Oct", "S_KX22.Oct", "S_X22.Nov",
                    "S_GmsX22.Nov", "S_KX22.Nov", "S_X22.Dec", "S_GmsX22.Dec", "S_KX22.Dec", "S_X23.Jan", "S_GmsX23.Jan", "S_KX23.Jan",
                    "S_X23.Feb", "S_GmsX23.Feb", "S_KX23.Feb", "S_X23.Mar", "S_GmsX23.Mar", "S_KX23.Mar", "S_X23.Apr", "S_GmsX23.Apr",
                    "S_KX23.Apr", "S_X23.May", "S_GmsX23.May", "S_KX23.May", "S_X23.Jun", "S_GmsX23.Jun", "S_KX23.Jun", "Name" , "Fed",       
                    "Sex" , "Tit" , "WTit" , "OTit" , "FOA" , "S_X23.Jul","S_GmsX23.Jul", "S_KX23.Jul","B.day" , "Flag" , "Name1502"  , "Fed1502" , "Sex1502" , "B.day1502" 
) 


names(without.na)
unique(without.na)
names(without.na)
# It contains 200 numbers which are mostly country's name
varage  <- unique(dataaa$B.day)
class(varage)
varage2<- na.omit(varage)
min(varage2)
# Calculate the minimum value while ignoring NAs
# Remove NA values and zeros from the "B.day" column
filtered_data <- dataaa[!is.na(dataaa$B.day) & dataaa$B.day != 0, ]

filtered_data <- dataaa[dataaa$Sex == dataaa$Sex.y & dataaa$Sex== dataaa$Sex.x, ]
# I dont know why the answer is 0???????????????
# Find the minimum value in the filtered data
min_value <- min(filtered_data$B.day)
max_value <- max(filtered_data$B.day)
# Print the minimum value
print(min_value)
print(max_value)
# answer is 1900 which means the oldest one is 33
#max 2017 is 6 years old
max_raw <-filtered_data[filtered_data$B.day==max_value]
max_raw <-dataaa[dataaa$B.day.y==max_value,]
max_raw$Sex
# If that is inapropriate to get ride of all missing value

# Find the minimum "B.day" value
min_value <- min(dataaa$B.day, na.rm = TRUE)

# Filter the data to find rows with the minimum "B.day" value
min_rows <- dataaa[dataaa$B.day == min_value, ]

# If there are multiple rows with the minimum value, min_rows will contain all of them

# Extract additional information from the min_rows
min_birthdays <- min_rows$Birthday
min_ratings <- min_rows$S_X23.Jan
# totally NA
# Print the additional information
cat("Minimum Ratings: ", paste(min_ratings, collapse = ", "), "\n")

without.na<-na.omit(dataaa)
#21351 out of 604122
filtered_NA <- without.na [!((without.na$X23.Jul < 1400) | 
                               (without.na$X23.Jul.x < 1400) | 
                               (without.na$X23.Jul.y < 1400)), ]
#19946

saveRDS(filtered_NA, file="filtered_NA.rds")
#filtered_data2 <- filtered_data2[(!is.na(filtered_data2$B.day.x) & filtered_data2$B.day.x != "" & filtered_data2$B.day.x != 0) |
#                                  (!is.na(filtered_data2$B.day.y) & filtered_data2$B.day.y != "" & filtered_data2$B.day.y != 0) |
#                                 (!is.na(filtered_data2$B.day )   & filtered_data2$B.day != ""   & filtered_data2$B.day!= 0), ]

#save as a Rds file 
saveRDS(dataaa, file="dataaa.rds")
#when you want to work again
dataaa <- readRDS("dataaa.rds")

#output storage in another path 
output_file <- file.path(output_dir, paste0("dataaa",".csv"))
# Save the merged data for the current game type
write.csv(dataaa, output_file)
#filter
view(dataaa)
View(B1601)
#B1601$Date <- "201602"
dataaa

filtered_data1 <- dataaa %>%
  filter(!(is.na(X23.Jul.x) | X23.Jul.x == "" | X23.Jul.x == 0) | 
           !(is.na(X23.Jul.y) | X23.Jul.y == "" | X23.Jul.y == 0) | 
           !(is.na(X23.Jul) | X23.Jul == "" | X23.Jul == 0))
dup_columns <- names(dataaa)[duplicated(names(dataaa))]
print(dup_columns)

# Find duplicate columns by name
duplicates <- which(duplicated(names(dataaa)) | duplicated(names(dataaa), fromLast = TRUE))

# Print the names of the duplicate columns
duplicate_names <- names(dataaa)[duplicates]
print(duplicate_names)
#unfotunately we have multiple duplicate file

# Check if your specified columns are among the duplicates
specified_cols <- c("X23.Jul.x", "X23.Jul.y", "X23.Jul")
specified_cols %in% duplicate_names
# hupfully their not dupplicated in your file:)
#there was not in duplicate data
filtered_data1 <- dataaa[!is.na(dataaa$X23.Jul.x) | dataaa$X23.Jul.x != "" | dataaa$X23.Jul.x != 0 |
                           !is.na(dataaa$X23.Jul.y) | dataaa$X23.Jul.y != "" | dataaa$X23.Jul.y != 0 |
                           !is.na(dataaa$X23.Jul) | dataaa$X23.Jul != "" | dataaa$X23.Jul != 0, ]
#save as a Rds file 
saveRDS(filtered_data1 , file="filtered_data1.rds")
#when you want to work again
filtered_data1  <- readRDS("filtered_data1.rds")
#خودش
filtered_data2 <- dataaa[(!is.na(dataaa$X23.Jul.x) & dataaa$X23.Jul.x != "" & dataaa$X23.Jul.x != 0) &
                           (!is.na(dataaa$X23.Jul.y) & dataaa$X23.Jul.y != "" & dataaa$X23.Jul.y != 0) &
                           (!is.na(dataaa$X23.Jul)   & dataaa$X23.Jul != ""   & dataaa$X23.Jul != 0), ]
filtered_data2 <- filtered_data2 [!((filtered_data2$X23.Jul < 1400) | 
                                      (filtered_data2$X23.Jul.x < 1400) | 
                                      (filtered_data2$X23.Jul.y < 1400)), ]
filtered_data2 <- filtered_data2[(!is.na(filtered_data2$B.day.x) & filtered_data2$B.day.x != "" & filtered_data2$B.day.x != 0) |
                                   (!is.na(filtered_data2$B.day.y) & filtered_data2$B.day.y != "" & filtered_data2$B.day.y != 0) |
                                   (!is.na(filtered_data2$B.day )   & filtered_data2$B.day != ""   & filtered_data2$B.day!= 0), ]
filtered_data2 <- filtered_data2[(!is.na(filtered_data2$Sex) & filtered_data2$Sex %in% c("M", "F")) |
                                   (!is.na(filtered_data2$Sex.x) & filtered_data2$Sex.x %in% c("M", "F")) |
                                   (!is.na(filtered_data2$Sex.y) & filtered_data2$Sex.y %in% c("M", "F")),]
filtered_data2 <- filtered_data2[(!is.na(dataaa$B_X22.Mar) & dataaa$B_X22.Mar!= "" & dataaa$B_X22.Mar != 0) &
                                   (!is.na(dataaa$S_X22.Mar) & dataaa$S_X22.Mar != "" & dataaa$S_X22.Mar!= 0) &
                                   (!is.na(dataaa$R_X22.Mar)   & dataaa$R_X22.Mar != ""   & dataaa$R_X22.Mar != 0), ]
filtered_data2 <- filtered_data2[(!is.na(dataaa$X23.Jul.x) & dataaa$X23.Jul.x != "" & dataaa$X23.Jul.x != 0) &
                                   (!is.na(dataaa$X23.Jul.y) & dataaa$X23.Jul.y != "" & dataaa$X23.Jul.y != 0) &
                                   (!is.na(dataaa$X23.Jul)   & dataaa$X23.Jul != ""   & dataaa$X23.Jul != 0), ]
filtered_data2 <- filtered_data2 [!((filtered_data2$X23.Jul < 1400) | 
                                      (filtered_data2$X23.Jul.x < 1400) | 
                                      (filtered_data2$X23.Jul.y < 1400)), ]
filtered_data2 <- filtered_data2[(!is.na(filtered_data2$B.day.x) & filtered_data2$B.day.x != "" & filtered_data2$B.day.x != 0) |
                                   (!is.na(filtered_data2$B.day.y) & filtered_data2$B.day.y != "" & filtered_data2$B.day.y != 0) |
                                   (!is.na(filtered_data2$B.day )   & filtered_data2$B.day != ""   & filtered_data2$B.day!= 0), ]
filtered_data2 <- filtered_data2[(!is.na(filtered_data2$Sex) & filtered_data2$Sex %in% c("M", "F")) |
                                   (!is.na(filtered_data2$Sex.x) & filtered_data2$Sex.x %in% c("M", "F")) |
                                   (!is.na(filtered_data2$Sex.y) & filtered_data2$Sex.y %in% c("M", "F")),]
unique(filtered_data2$B.day)
unique(filtered_data2$Sex)
unique(filtered_data2$Fed)
table(filtered_data2$Sex, useNA = "always")
table(filtered_data2$B.day, useNA = "always")
table(filtered_data2$Fed, useNA = "always")
# only work on filter_data2 for the next time
saveRDS(filtered_data2 , file="filtered_data2.rds")
#when you want to work again
filtered_data2 <- readRDS("filtered_data2.rds")

filtered_data3 <- dataaa[( !is.na(dataaa$X23.Jul.x) | dataaa$X23.Jul.x != "" | dataaa$X23.Jul.x != 0 ) &
                           ( !is.na(dataaa$X23.Jul.y) | dataaa$X23.Jul.y != "" | dataaa$X23.Jul.y != 0 ) &
                           ( !is.na(dataaa$X23.Jul)   | dataaa$X23.Jul != ""   | dataaa$X23.Jul != 0 ), ]

saveRDS(filtered_data3 , file="filtered_data3.rds")
#when you want to work again
filtered_data3 <- readRDS("filtered_data3.rds")

names(B1601)
names(B1604)
names(B1603)

Bunique<-unique(B1502.rds$ID.Number)
NBunique<-length(Bunique)
Bnum<-any(is.na(B1502.rds$ID.Number))
#193430 out of 258965
Runique<-unique(R1502.rds$ID.Number)
NRunique<-length(Runique)
Rnum<-any(is.na(R1502.rds$ID.Number))
#279117 out of 279117
Sunique<-unique(S1502.rds$ID.Number)
NSunique<-length(Sunique)
Snum<-any(is.na(S1502.rds$ID.Number))
summary(dataaa)
#429034 out of 429034
#data unique
rm(list = ls(all = TRUE))
ls()

library(dplyr)

# Set your working directory to where the CSV files are located
filepath <- 'A:\\payan_name\\fide_data\\2016\\csv16\\checkup'
setwd(filepath)

# Define the years and months
years <- c("15", "16", "17", "18","19", "20", "21", "22","23")  # Last two digits of the year
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# Initialize the merged data frame
merged_data <- NULL

# Loop through years
for (year in years) {
  # Loop through months
  for (month in months) {
    if(year=="15" && month=="01")
    {
      next
    }
    if(year=="23" && month=="08")
    {   break
    }
    # Construct the file name
    file_prefix <- "B"  # Blitz game type
    csv_file <- paste0(file_prefix, year, month, ".csv")
    
    # Read CSV file
    data <- read.csv(csv_file, header = TRUE, sep = ",")
    
    # Remove excluded columns
    excol <- c("Name", "Fed", "Sex", "Tit", "WTit", "OTit", "B.day", "Flag", "FOA")
    data <- data[, !names(data) %in% excol]
    
    # Rename columns
    col_names <- c("ID.Number", paste0("B_X20", year, ".", month), "Gms", "K")
    names(data) <- col_names
    
    # Merge data
    if (!is.null(merged_data)) {
      merged_data <- merge(merged_data, data, all = TRUE, by = "ID.Number")
    } else {
      merged_data <- data
    }
  }
}
View(merged_data)

library(data.table)
dt <- as.data.table(merged_data)

name(B1701)

#vers3
rm(list = ls(all = TRUE))
ls()

library(dplyr)

# Set your working directory to where the CSV files are located
filepath <- 'A:\\payan_name\\fide_data\\2016\\csv16\\checkup'
setwd(filepath)

# Define the years and months
years <- c("15", "16", "17", "18","19", "20", "21", "22","23")  # Last two digits of the year
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# Initialize the merged data frame
# Define the directory where you want to save the CSV files
output_dir <- 'A:\\payan_name\\fide_data\\2016\\csv16\\checkup\\merged_data'
#A:\payan_name\fide_data\2016\csv16\checkup\merged_data
# Loop through game types
for (game_type in c("B", "R", "S")) {
  # Initialize the merged data frame
  merged_data2 <- NULL
  
  # Loop through years
  for (year in years) {
    
    # Loop through months
    for (month in months) {
      # Skip January 2015
      if (year == "15" && month == "01") {
        next
      }
      
      # Stop at July 2023
      if (year == "23" && month == "07") {
        break
      }
      
      # Construct the file name
      csv_file <- paste0(game_type, year, month, ".csv")
      
      # Read CSV file
      data <- read.csv(csv_file, header = TRUE, sep = ",")
      
      # Remove excluded columns
      excol <- c("Name", "Fed", "Sex", "Tit", "WTit", "OTit", "B.day", "Flag",  "FOA")
      data <- data[, !names(data) %in% excol]
      
      # Rename columns
      col_names <- c("ID.Number", paste0(game_type, "_X20", year, ".", month), "Gms", "K")
      names(data) <- col_names
      
      # Merge data
      if (!is.null(merged_data2)) {
        merged_data2 <- merge(merged_data2, data, all = TRUE, by = "ID.Number")
      } else {
        merged_data2 <- data
      }
    }
  }
  saveRDS(merged_data2, file="merged_data2.rds")
  #when you want to work again
  merged_data2.rds <- readRDS("merged_data2.rds")
  # Save the merged data to a CSV file
  output_file <- file.path(output_dir, paste0("merged_data2", game_type, ".csv"))
  write.csv(merged_data2, output_file, row.names = FALSE)
}


# View the merged data frame
View(merged_data2)

























































































# Initialize an empty data frame to store the merged data
merged_data <- data.frame()
csv_files <- sort(csv_files)  # Sort the list

increment_folder <- function(folder_name) {
  prefix <- substr(folder_name, 1, 1)
  numeric_part <- as.integer(substr(folder_name, 2))
  incremented_numeric_part <- numeric_part + 1
  updated_folder_name <- paste0(prefix, formatC(incremented_numeric_part, width = 4, flag = "0"))
  return(updated_folder_name)
}
for (csv_file in csv_files) {
  csv_file <- paste(folder, ".csv", sep = "")
  data <- read.csv(csv_file, stringsAsFactors = FALSE)
  
  merged_data <- bind_rows(merged_data, data)
  
  folder <- increment_folder(folder)
}


for (csv_file in csv_files) {
  data <- read.csv(csv_file, stringsAsFactors = FALSE)
  
  # Add the data to the merged_data data frame
  merged_data <- bind_rows(merged_data, data)
}


merged_data <- data.frame()

# Loop through CSV files, read data, and merge
for (csv_file in csv_files) {
  data <- read.csv(csv_file, stringsAsFactors = FALSE)
  
  # Add the data to the merged_data data frame
  merged_data <- bind_rows(merged_data, data)
}

# Print the first few rows of the merged data
head(merged_data)
# Loop through each CSV file, read it, and merge with the existing data
for (file in csv_files) {
  file_path <- file
  data <- read.csv(file_path, stringsAsFactors = FALSE)  # Adjust options as needed
  merged_data <- bind_rows(merged_data, data)
}

# Print the first few rows of the merged data
head(merged_data)






########################################################################
table(combined_data$Flag)


#################we only want to check our flag to find out the logic behind##################
names(B1503)
FB1503 <- B1503%>% 
  select("ID.Number", "Flag")
FB1504 <- B1504%>% 
  select("ID.Number", "Flag")
FB1505 <- B1505%>% 
  select("ID.Number", "Flag")
FB1506 <- B1506%>% 
  select("ID.Number", "Flag")
FB1507 <- B1507%>% 
  select("ID.Number", "Flag")
FB1508 <- B1508%>% 
  select("ID.Number", "Flag")
FB1509 <- B1509%>% 
  select("ID.Number", "Flag")
FB1510 <- B1510%>% 
  select("ID.Number", "Flag")
FB1511 <- B1511%>% 
  select("ID.Number", "Flag")
FB1512 <- B1512%>% 
  select("ID.Number", "Flag")


FB1601 <- B1601%>% 
  select("ID.Number", "Flag")
FB1602 <- B1602%>% 
  select("ID.Number", "Flag")
FB1603 <- B1603%>% 
  select("ID.Number", "Flag")
FB1604 <- B1604%>% 
  select("ID.Number", "Flag")
FB1605 <- B1605%>% 
  select("ID.Number", "Flag")
FB1606 <- B1606%>% 
  select("ID.Number", "Flag")
FB1607 <- B1607%>% 
  select("ID.Number", "Flag")
FB1608 <- B1608%>% 
  select("ID.Number", "Flag")
FB1609 <- B1609%>% 
  select("ID.Number", "Flag")
FB1610 <- B1610%>% 
  select("ID.Number", "Flag")
FB1611 <- B1611%>% 
  select("ID.Number", "Flag")
FB1612 <- B1612%>% 
  select("ID.Number", "Flag")

FB1701 <- B1701%>% 
  select("ID.Number", "Flag")
FB1702 <- B1702%>% 
  select("ID.Number", "Flag")
FB1703 <- B1703%>% 
  select("ID.Number", "Flag")
FB1704 <- B1704%>% 
  select("ID.Number", "Flag")
FB1705 <- B1705%>% 
  select("ID.Number", "Flag")
FB1706 <- B1706%>% 
  select("ID.Number", "Flag")
FB1707 <- B1707%>% 
  select("ID.Number", "Flag")
FB1708 <- B1708%>% 
  select("ID.Number", "Flag")
FB1709 <- B1709%>% 
  select("ID.Number", "Flag")
FB1710 <- B1710%>% 
  select("ID.Number", "Flag")
FB1711 <- B1711%>% 
  select("ID.Number", "Flag")
FB1712 <- B1712%>% 
  select("ID.Number", "Flag")
#####
FB1503 <- left_join(FB1503,FB1504, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1505, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1506, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1507, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1508, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1509, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1510, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1511, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1512, by = "ID.Number")


FB1503 <- left_join(FB1503,FB1601, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1602, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1603, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1604, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1605, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1606, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1607, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1608, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1609, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1610, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1611, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1612, by = "ID.Number")


FB1503 <- left_join(FB1503,FB1701, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1702, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1703, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1704, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1705, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1706, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1707, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1708, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1709, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1710, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1711, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1712, by = "ID.Number")
names(FB1503) <- c( "ID.Number" ,"Flag.1503","Flag.1504","Flag.1505","Flag.1506","Flag.1507","Flag.1508","Flag.1509","Flag.1510","Flag.1511","Flag.1512"
                    ,"Flag.1601","Flag.1602","Flag.1603","Flag.1604","Flag.1605","Flag.1606","Flag.1607","Flag.1608","Flag.1609","Flag.1610","Flag.1611","Flag.1612"
                    ,"Flag.1701","Flag.1702" ,"Flag.1703","Flag.1704","Flag.1705","Flag.1706","Flag.1707","Flag.1708","Flag.1709","Flag.1710","Flag.1711","Flag.1712")

######
FB1704 <- B1704%>% 
  select("ID.Number", "Flag")
FB1805 <- B1805%>% 
  select("ID.Number", "Flag")
FB1905 <- B1905%>% 
  select("ID.Number", "Flag")
FB2005 <- B2005%>% 
  select("ID.Number", "Flag")
FB2105 <- B2105%>% 
  select("ID.Number", "Flag")
FB2205 <- B2205%>% 
  select("ID.Number", "Flag")
FB2305 <- B2305%>% 
  select("ID.Number", "Flag")
FB2306 <- B2306%>% 
  select("ID.Number", "Flag")
FB2307 <- B2307%>% 
  select("ID.Number", "Flag")

FB1503 <- left_join(FB1503,FB1604, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1704, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1805, by = "ID.Number")
FB1503 <- left_join(FB1503,FB1905, by = "ID.Number")
FB1503 <- left_join(FB1503,FB2005, by = "ID.Number")
FB1503 <- left_join(FB1503,FB2105, by = "ID.Number")
FB1503 <- left_join(FB1503,FB2205, by = "ID.Number")
FB1503 <- left_join(FB1503,FB2305, by = "ID.Number")
FB1503 <- left_join(FB1503,FB2306, by = "ID.Number")
FB1503 <- left_join(FB1503,FB2307, by = "ID.Number")
#########################################

# Create a list of data frames (B1503, B1603, etc.)
df_list <- lapply(seq(1503, 2307, by = 100), function(i) get(paste0("B", i)))

# Initialize variables to track changes
changes <- list()

# Iterate over pairs of data frames
for (i in 1:(length(df_list) - 1)) {
  current_df <- df_list[[i]]
  next_df <- df_list[[i + 1]]
  
  # Check if the "ID.Number" column exists in both data frames
  if ("ID.Number" %in% colnames(current_df) && "ID.Number" %in% colnames(next_df)) {
    # Remove rows with missing or NA values in "ID.Number" column
    current_df <- current_df[!is.na(current_df$ID.Number), ]
    next_df <- next_df[!is.na(next_df$ID.Number), ]
    
    # Compare the "Sex" column between the current and next data frames
    sex_changed <- any(current_df$Sex != next_df$Sex)
  } else {
    # If the "ID.Number" column is missing in either data frame, consider it a change in "Sex"
    sex_changed <- TRUE
  }
  
  # Store the result in the changes list
  changes[[i]] <- sex_changed
}

# Create a data frame to store the results
result_df <- data.frame(StartID = seq(1503, 2303, by = 100), SexChanged = changes)

# Print the result
print(result_df)
################################Sex####################
SB1503 <- B1503%>% 
  select("ID.Number", "Sex","Fed")
SB1603 <- B1603%>% 
  select("ID.Number", "Sex","Fed")
SB1703 <- B1703%>% 
  select("ID.Number", "Sex","Fed")
SB1803 <- B1803%>% 
  select("ID.Number", "Sex","Fed")
SB1903 <- B1903%>% 
  select("ID.Number", "Sex","Fed")
SB2003 <- B2003%>% 
  select("ID.Number", "Sex","Fed")
SB2103 <- B2103%>% 
  select("ID.Number", "Sex","Fed")
SB2203 <- B2203%>% 
  select("ID.Number", "Sex","Fed")
SB2303 <- B2303%>% 
  select("ID.Number", "Sex","Fed")
#SB2307 <- B2307%>% 
#select("ID.Number", "Sex")
names(SB1603) <- c("ID.Number", "Sex6")
SSB1503 <- left_join(SB1503,SB1603, by = "ID.Number")

SB1503 <- left_join(SB1503,SB1603, by = "ID.Number")
SB1503 <- left_join(SB1503,SB1703, by = "ID.Number")
SB1503 <- left_join(SB1503,SB1803, by = "ID.Number")
SB1503 <- left_join(SB1503,SB1903, by = "ID.Number")
SB1503 <- left_join(SB1503,SB2003, by = "ID.Number")
SB1503 <- left_join(SB1503,SB2103, by = "ID.Number")
SB1503 <- left_join(SB1503,SB2203, by = "ID.Number")
SB1503 <- left_join(SB1503,SB2303, by = "ID.Number")
head(SB1503)
B1503 <- NULL
B1603 <- NULL
B1703 <- NULL
B1803 <- NULL
B1903 <- NULL
B2003 <- NULL
B2103 <- NULL
B2203 <- NULL
B2303 <- NULL
B2307 <- NULL

SB1503 <- NULL
SB1603 <- NULL
SB1703 <- NULL
SB1803 <- NULL
SB1903 <- NULL
SB2003 <- NULL
SB2103 <- NULL
SB2203 <- NULL
SB2303 <- NULL

R2307_demog <- NULL
combined_data <- NULL
combined_demog<- NULL
DNFinalB<- NULL
DNFinalR <- NULL
DNFinalS <- NULL
DNFinal <- NULL
DNFial <- NULL
DNAFinal <- NULL
dNFinalB<- NULL
dNFinalR <- NULL
dNFinalS <- NULL
#######3Rapid##########
SR1503 <- R1503%>% 
  select("ID.Number", "Sex","Fed")
SR2303 <- R2303%>% 
  select("ID.Number", "Sex","Fed")
SR1503 <- left_join(SR1503,SR2303, by = "ID.Number")
SR1503$Bio <-  ifelse(SR1503$Sex.x == SR1503$Sex.y, "No", "Yes")
unique(SB1503$Bio )

SR1503$Bcountry <-  ifelse(SR1503$Fed.x == SR1503$Fed.y, "No", "Yes") 
SR1503 <- na.omit(SR1503)
BCSR1503 <- SR1503 [SR1503$Bcountry == "Yes",]
#674 obs
BSESR1503 <- SR1503 [SR1503$Bio == "Yes",]
#72 obs
BLSR1503 <- SR1503 [!((SR1503$Bcountry == "No") & 
                        (SR1503$Bio == "No")),]
#744 obs and 2 of them change both sex and country
filepath3 <- paste0(filepath4, "BLSR1503.rds")
saveRDS(BLSR1503, file = filepath3)
names(SR1503)
R1503 <- NULL
R2303 <- NULL
############Standard############
SS1503 <- S1503%>% 
  select("ID.Number", "Sex","Fed")
SS2303 <- S2303%>% 
  select("ID.Number", "Sex","Fed")
#411323
SS1503 <- left_join(SS1503,SS2303, by = "ID.Number")
SS1503$Bio <-  ifelse(SS1503$Sex.x == SS1503$Sex.y, "No", "Yes")

SS1503$Bcountry <-  ifelse(SS1503$Fed.x == SS1503$Fed.y, "No", "Yes") 
#202398
SS1503 <- na.omit(SS1503)
#200072
BCSS1503 <- SS1503 [SS1503$Bcountry == "Yes",]
#1293 obs
BSESS1503 <- SS1503 [SS1503$Bio == "Yes",]
#270 obs
BLSS1503 <- SS1503 [!((SS1503$Bcountry == "No") & 
                        (SS1503$Bio == "No")),]
#1559 obs and 4 of them change both sex and country
Both <- SS1503 [!((SS1503$Bcountry == "No") | 
                    (SS1503$Bio == "No")),]
filepath4 <- paste0(filepath4, "BLSS1503.rds")
saveRDS(BLSS1503, file = filepath4)
################################
SB1503 <- na.omit(SB1503)
SB1503$Sex_change <- "No"
# Iterate over rows in the data frame
for (i in 2:nrow(SB1503)) {
  if (SB1503$Sex.x.x.x.x[i] != SB1503$Sex.x.x.x.x[i - 1]) {
    SB1503$Sex_change[i] <- "Yes"
    
  }
}
names(SB1503)
# Print the updated data frame
print(SB1503)
if (SB1503$Sex.x[row_number] != SB1503$Sex.y.y.y.y.y[row_number]) {
  cat("Column1 and Column2 are not equal in row", row_number, "\n")
} else {
  cat("Column1 and Column2 are equal in row", row_number, "\n")
}
names(SB1503)
SB1503$Bio <-  ifelse(SB1503$Sex.x == SB1503$Sex, "No", "Yes")
unique(SB1503$Bio )

SB1503$Bcountry <-  ifelse(SB1503$Fed.x == SB1503$Fed, "No", "Yes") 
FSB1503<- SB1503[SB1503$Bio == "Yes", ]

#586 players change their country 
#52 players change their sex
# 2 person change their sex and country
CSB1503<- SB1503[SB1503$Bcountry == "Yes", ]

FFSB1503 <- na.omit(FSB1503)
CCSB1503 <- na.omit(CSB1503)
BLCCSB1503 <- CCSB1503%>% 
  select("ID.Number","Sex","Fed","Bio","Bcountry")
BLFFSB1503 <- FFSB1503%>% 
  select("ID.Number","Sex","Fed","Bio")
BlitsBlack <- rbind(BLFFSB1503,BLCCSB1503)
length(unique(BlitsBlack$ID.Number))
#638 unique 2 people change their country and sex so 636 Id-number should be block
BDlitsBlack <-  BlitsBlack[!duplicated( BlitsBlack$ID.Number), ]
filepath1 <- paste0(filepath4, "BDlitsBlack.rds")
saveRDS(BDlitsBlack, file = filepath1)
directory_path
litsBlack<- readRDS("BDlitsBlack.rds")
FBDlitsBlack<- BDlitsBlack[BDlitsBlack$Sex == "F",]
### 161 observation Femail change    
# Print the filtered data frame

print(filtered_df)
# Check for the existence of the 'Bio' column in SB1503
if ("Bio" %in% colnames(SB1503)) {
  # Create FSB1503 by subsetting based on 'Bio' column
  FSB1503 <- SB1503[SB1503$Bio == "Yes", ]
} else {
  cat("The 'Bio' column does not exist in SB1503.\n")
}
PBCNFinalS <- NULL
PBCNFinal <- NULL
MPBCNFinal <- NULL
FPBCNFinal <- NULL
ABCNFinal <- NULL
MABCNFinal <- NULL
FABCNFinal <- NULL
FBCNFinal <- NULL
MBCNFinal <- NULL
FSdNFinal <- NULL
MSdNFinal <- NULL
CNFinalS <- NULL
model_olsSC <- NULL
model_olsFC <- NULL
model_olsFCG <- NULL

ttest_generalR <- NULL
H <- NULL
HM <- NULL

PBCNFinalB <- NULL
MPBCNFinalB <- NULL
FPBCNFinalB <- NULL
ABCNFinalB <- NULL
MABCNFinalB <- NULL
FABCNFinalB <- NULL
FBCNFinalB <- NULL
MBCNFinalB <- NULL
FSdNFinalB <- NULL
MSdNFinal <- NULL
CNFinalB <- NULL
BDlitsBlack <- NULL
BCNFinalB <- NULL

PBCNFinalR <- NULL
MPBCNFinalR <- NULL
FPBCNFinalR <- NULL
ABCNFinalR <- NULL
MABCNFinalR <- NULL
FABCNFinalR <- NULL
FBCNFinalR <- NULL
MBCNFinalR <- NULL
FSdNFinalR <- NULL
MSdNFinalR <- NULL
CNFinalR <- NULL
BLSR1503 <- NULL
NFinalR <- NULL
model_olsBC <- NULL
getwd()

density_df <- NULL
densities <- NULL
blitz_density_df <- NULL
blitz_density_values <- NULL
density_values <- NULL
density_valuesM <- NULL
unique_participantsS <- NULL
unique_participantM <- NULL
unique_participantsB <- NULL
normalized_data <- NULL
participants_count<-NULL
standard_density_df<- NULL
standard_density_values <- NULL

library(dplyr)
library(ggplot2)
UBCNFinalB$time1 <- paste0(UBCNFinalB$Year, "-",UBCNFinalB$Month)

# Assuming 'time' is in a Date format
UBCNFinalB$time1 <- as.(UBCNFinalB$time1)
UBCNFinalB$time1 <- as.Date(UBCNFinalB$time1)
# Extracting only one observation per year
subset_data <- UBCNFinalB %>%
  group_by(Year) %>%
  slice(1)  # Selects the first observation for each Year

# Plotting
ggplot(subset_data, aes(x = time, y = Blitz_Rating)) +
  geom_point(size = 3) +  # Points for each observation
  geom_line(aes(group = 1), stat = "summary", fun = "mean", color = "blue") +  # Line for mean
  theme_minimal() +
  labs(
    title = "Mean Blitz Rating per Year",
    x = "Time (Month and Year)",
    y = "Mean Blitz Rating"
  )
# Convert 'time1' to yearmon format
UBCNFinalB <-readRDS("UBCNFinalB.rds")
library(zoo)
UBCNFinalB$time1 <- paste0(UBCNFinalB$Year,"-" ,UBCNFinalB$Month)
UBCNFinalB$time1 <- as.yearmon(UBCNFinalB$time1)
MUBCNFinalB
# Plotting
library(ggplot2)
library(tidyverse)
names(UBCNFinalB)
class(UBCNFinalB$time1)
# Group by time1 to get the mean Blitz_Rating for each year and month
MUBCNFinalB <- subset(UBCNFinalB, Sex == 0)
FUBCNFinalB <- subset(UBCNFinalB, Sex == 1)
#Male
Average_blitz_Male <- MUBCNFinalB %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Male = mean(Blitz_Rating))
#Female
Average_blitz_Female <- FUBCNFinalB %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Female = mean(Blitz_Rating))

average_blitz <- UBCNFinalB %>%
  group_by(time1) %>%
  summarise(mean_rating = mean(Blitz_Rating))
#########
Average_blitz <- UBCNFinalB %>%
  group_by(Year_Month) %>%
  summarise(mean_rating = mean(Blitz_Rating))
ggplot(Average_blitz, aes(x = Year_Month, y = mean_rating)) +
  geom_vline(data=subset(UBCNFinalB,Year_Month == "2020-03-01"))+
  geom_point()+
  geom_line() +
  labs(title = "Mean Blitz Rating Over Time") +
  xlab("Time") +
  ylab("Mean Blitz Rating")
##########3
# Create a plot
ggplot(average_blitz, aes(x = time1, y = mean_rating)) +
  geom_line() +
  labs(title = "Mean Blitz Rating Over Time") +
  xlab("Time") +
  ylab("Mean Blitz Rating")
ggplot(average_blitz_Male, aes(x = time1, y = mean_rating_Male)) +
  geom_line() +
  labs(title = "Mean Male Blitz Rating Over Time") +
  xlab("Time") +
  ylab("Mean Male Blitz Rating")
ggplot(average_blitz_Female, aes(x = time1, y = mean_rating_Female)) +
  geom_line() +
  labs(title = "Mean Female Blitz Rating Over Time") +
  xlab("Time") +
  ylab("Mean Female Blitz Rating")
#########
AUBCNFinalB <- subset(UBCNFinalB, covid_dummy == 1)
min(AUBCNFinalB$time)
unique(FUBCNFinalB$time1)
# Combine Male and Female plots into one
ggplot() +
  geom_line(data = average_blitz_Male, aes(x = time1, y = mean_rating_Male, color = "Male")) +
  geom_line(data = average_blitz_Female, aes(x = time1, y = mean_rating_Female, color = "Female")) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype = "dashed", color = "green")+
  labs(title = "Mean Blitz Rating Over Time by Gender",
       x = "Time",
       y = "Mean Blitz Rating",
       color = "Gender") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()
class(time)
unique(UBCNFinalB$time)
class(UBCNFinalB$Year)
class(UBCNFinalB$Month)

#########

# Combine Year and Month columns into a single column in YYYY-MM format
UBCNFinalB$Year_Month <- as.Date(paste(UBCNFinalB$Year, UBCNFinalB$Month, "01", sep = "-"))
ggplot() +
  geom_line(data = average_blitz_Male, aes(x = time1, y = mean_rating_Male, color = "Male")) +
  geom_line(data = average_blitz_Female, aes(x = time1, y = mean_rating_Female, color = "Female")) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype = "dashed", color = "green")+
  labs(title = "Mean Blitz Rating Over Time by Gender",
       x = "Time",
       y = "Mean Blitz Rating",
       color = "Gender") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()
# Convert to Year-Month format (YYYY-MM) and store as Date
your_data$Year_Month <- as.Date(your_data$Year_Month)
UBCNFinalB$time2 <- as.yearmon(paste(UBCNFinalB$Year, UBCNFinalB$Month, sep = "-"))

# View the updated data
print(your_data)

# Assuming your data is in a format like "2020-03" representing Year-Month

# Convert Year_Month to a date format for better handling
Average_blitz$Year_Month <- as.Date(paste0(Average_blitz$Year_Month, "-01"))

# Plot with the Year_Month as x-axis and a vertical line at "2020-03-01"
ggplot(Average_blitz, aes(x = Year_Month, y = mean_rating)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  labs(title = "Mean Blitz Rating Over Time", x = "Time", y = "Mean Blitz Rating")
#Male
Average_blitz_Male <- MUBCNFinalB %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Male = mean(Blitz_Rating))
#Female
Average_blitz_Female <- FUBCNFinalB %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Female = mean(Blitz_Rating))
#ggplot(Average_blitz, aes(x = Year_Month, y = mean_rating)) +
ggplot()+  
  geom_line(data = Average_blitz_Male, aes(x = Year_Month, y = mean_rating_Male, color = "مردها")) +
  geom_point(data = Average_blitz_Male, aes(x = Year_Month, y = mean_rating_Male), color = "blue")  +
  geom_line(data = Average_blitz_Female, aes(x =Year_Month, y = mean_rating_Female, color = "زن ها")) +
  geom_point(data = Average_blitz_Female, aes(x = Year_Month, y = mean_rating_Female), color = "red") +
  geom_line() +
  scale_color_manual(values = c("مردها" = "blue", "زن ها" = "red")) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  labs(title = "متوسط سالانه درجه بین المللی زنان و مردان شطرنج باز در بازی برق آسا", x = "سال", y = "میانگین درجه بین المللی" , color="جنسیت") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month")

ggplot()+  
  geom_line(data = Average_blitz_Male, aes(x = Year_Month, y = mean_rating_Male, color = "Male")) +
  geom_point(data = Average_blitz_Male, aes(x = Year_Month, y = mean_rating_Male), color = "blue")  +
  geom_line(data = Average_blitz_Female, aes(x =Year_Month, y = mean_rating_Female, color = "Female")) +
  geom_point(data = Average_blitz_Female, aes(x = Year_Month, y = mean_rating_Female), color = "red") +
  geom_line() +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  labs(title = "Mean Blitz Rating Over Time", x = "Time", y = "Mean Blitz Rating" , color="Gender") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month")
