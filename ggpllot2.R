filepath4 <- 'A:\\payan_name\\fide_data\\2016\\csv16\\checkup\\merged_data\\clean\\covid\\'
setwd(filepath4)
getwd()
UBCNFinalB <- readRDS("UBCNFinalB.rds")
BCNFinalB <- NULL
UBCNFinalB$Year_Month <- as.Date(paste(UBCNFinalB$Year, UBCNFinalB$Month, "01", sep = "-"))
MUBCNFinalB <- subset(UBCNFinalB, Sex == 0)
FUBCNFinalB <- subset(UBCNFinalB, Sex == 1)
#Male
library(extrafont)
font_import(pattern = "B Nazanin")
loadfonts()
library(ggplot2)
library(tidyverse)
library(dplyr)
Average_blitz_Male <- MUBCNFinalB %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Male = mean(Blitz_Rating))
#Female
Average_blitz_Female <- FUBCNFinalB %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Female = mean(Blitz_Rating))
#ggplot(Average_blitz, aes(x = Year_Month, y = mean_rating)) +
ggplot()+  
  geom_line(data = Average_blitz_Male, aes(x = Year_Month, y = mean_rating_Male, color = "مرد")) +
  geom_point(data = Average_blitz_Male, fill="blue" ,aes(x = Year_Month, y = mean_rating_Male, color = "blue"),color = "blue")  +
  geom_line(data = Average_blitz_Female,aes(x =Year_Month, y = mean_rating_Female, color = "زن")) +
  geom_point(data = Average_blitz_Female, fill="red", aes(x = Year_Month, y = mean_rating_Female, color = "red"),color = "red") +
  geom_line() +
  #labs( x = "سال", y = "میانگین درجه بین المللی" , color="") +
  labs(x = "\nسال-ماه\n", y = "\nمیانگین درجه بین المللی \n", color = "")+
  scale_color_manual(values = c("مرد" = "blue", "زن" = "red")) +

  geom_vline(xintercept = as.Date("2020-03-01")) +
  #labs(title = bquote(paste(" متوسط سالانه درجه بین المللی فرم برق آسا برای زنان و مردان شطرنج باز از فوریه 2015 تا آوریل 2023 ",bold("برق آسا"))),x = "سال", y = "میانگین درجه بین المللی" , color=" ") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month")+
  theme(text = element_text(family = "B Nazanin"),  axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),   
        axis.title = element_text(size = 18),  
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 12))

ggsave("3برق آسا.png",
       width = 40,
       height = 20,
       units = "cm",
       dpi=900)
##############################فونت IRLoutous برای مقاله بی ناموس 
# بارگذاری و تنظیمات کتابخانه‌ها و داده‌ها
library(extrafont)
font_import(pattern = "IRLotus")
loadfonts(device = "win") # مخصوص سیستم عامل ویندوز
library(ggplot2)
library(tidyverse)
library(dplyr)

# محاسبه میانگین رتبه‌ها
Average_blitz_Male <- MUBCNFinalB %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Male = mean(Blitz_Rating))

Average_blitz_Female <- FUBCNFinalB %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Female = mean(Blitz_Rating))

# رسم نمودار با فونت IRLotus
ggplot() +  
  geom_line(data = Average_blitz_Male, aes(x = Year_Month, y = mean_rating_Male, color = "مرد")) +
  geom_point(data = Average_blitz_Male, fill="blue" ,aes(x = Year_Month, y = mean_rating_Male, color = "blue"), color = "blue") +
  geom_line(data = Average_blitz_Female, aes(x = Year_Month, y = mean_rating_Female, color = "زن")) +
  geom_point(data = Average_blitz_Female, fill="red", aes(x = Year_Month, y = mean_rating_Female, color = "red"), color = "red") +
  labs(x = "سال", y = "\nمیانگین درجه بین المللی\n", color = "") +
  scale_color_manual(values = c("مرد" = "blue", "زن" = "red")) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month") +
  
  # تنظیمات کامل فونت IRLotus برای تمامی متن‌ها و اعداد
  theme(
    text = element_text(family = "IRLotus", size = 10),  # تنظیم فونت کلی
    axis.text.x = element_text(family = "IRLotus", size = 10),  # اعداد محور افقی
    axis.text.y = element_text(family = "IRLotus", size = 10),  # اعداد محور عمودی
    axis.title = element_text(family = "IRLotus", size = 12),  # عنوان محورها
    legend.text = element_text(family = "IRLotus", size = 10),  # نوشته‌های لجند
    plot.title = element_text(family = "IRLotus", size = 14, hjust = 0.5)  # عنوان کلی نمودار
  )

# ذخیره نمودار با فونت IRLotus
ggsave("Lبرق آسا.png",
       width = 40,
       height = 20,
       units = "cm",
       dpi=900)

##############################
##################Rapid###########
BCNFinalR<-readRDS("BCNFinalR.rds")
names(BCNFinalR)
BCNFinalR$Year_Month <- as.Date(paste(BCNFinalR$Year, BCNFinalR$Month, "01", sep = "-"))
#Male
MBCNFinalR <- subset(BCNFinalR, Sex == 0)
FBCNFinalR<- subset(BCNFinalR, Sex == 1)
Average_Rapid_Male <- MBCNFinalR %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Male = mean(Rapid_Rating))
#Female
Average_Rapid_Female <- FBCNFinalR %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Female = mean(Rapid_Rating))
#ggplot(Average_Rapid, aes(x = Year_Month, y = mean_rating)) +
ggplot()+  
  geom_line(data = Average_Rapid_Male, aes(x = Year_Month, y = mean_rating_Male, color = "مرد")) +
  geom_point(data = Average_Rapid_Male, aes(x = Year_Month, y = mean_rating_Male), color = "blue")  +
  geom_line(data = Average_Rapid_Female, aes(x =Year_Month, y = mean_rating_Female, color = "زن")) +
  geom_point(data = Average_Rapid_Female, aes(x = Year_Month, y = mean_rating_Female), color = "red") +
  geom_line() +
  scale_color_manual(values = c("مرد" = "blue", "زن" = "red")) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  labs( x = "سال", y = "میانگین درجه بین المللی" , color=" ") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month")+
  theme(text = element_text(family = "B Nazanin"),  axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),   
        axis.title = element_text(size = 18),  
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 12))
warning()
ggsave("6سرعتی .png",
       width = 40,
       height = 20,
       units = "cm",
       dpi=900)
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
##################Standard###########
BCNFinalS<-readRDS("BCNFinalS.rds")
BCNFinalS$Year_Month <- as.Date(paste(BCNFinalS$Year, BCNFinalS$Month, "01", sep = "-"))
MBCNFinalS<- NULL
FBCNFinalS<- NULL
MBCNFinalS <- subset(BCNFinalS, Sex == 0)
FBCNFinalS<- subset(BCNFinalS, Sex == 1)
#Male
names(BCNFinalS)
names(MBCNFinalS)
Average_Standard_Male <- MBCNFinalS %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Male = mean(Standard_Rating))
#Female
Average_Standard_Female <- FBCNFinalS %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Female = mean(Standard_Rating))
#ggplot(Average_Standard, aes(x = Year_Month, y = mean_rating)) +
ggplot()+  
  geom_line(data = Average_Standard_Male, aes(x = Year_Month, y = mean_rating_Male, color = "مرد")) +
  geom_point(data = Average_Standard_Male, aes(x = Year_Month, y = mean_rating_Male), color = "blue")  +
  geom_line(data = Average_Standard_Female, aes(x =Year_Month, y = mean_rating_Female, color = "زن")) +
  geom_point(data = Average_Standard_Female, aes(x = Year_Month, y = mean_rating_Female), color = "red") +
  geom_line() +
  scale_color_manual(values = c("مرد" = "blue", "زن" = "red")) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  labs( x = "سال", y = "میانگین درجه بین المللی" , color=" ") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month")+
  theme(text = element_text(family = "B Nazanin"),  axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),   
        axis.title = element_text(size = 18),  
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 12))
ggsave("استاندارد3.png",
       width = 40,
       height = 20,
       units = "cm",
       dpi=900)
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
###############################ALL 
BCNFinal<- readRDS("BCNFinal.rds")
BCNFinal$Year_Month <- as.Date(paste(BCNFinal$Year, BCNFinal$Month, "01", sep = "-"))
MBCNFinal <- subset(BCNFinal, Sex == 0)
FBCNFinal <- subset(BCNFinal, Sex == 1)
Average_Male <- MBCNFinal %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Male = mean( Rating))
#Female
names(BCNFinal)
Average_Female <- FBCNFinal %>%
  group_by(Year_Month) %>%
  summarise(mean_rating_Female = mean( Rating))
#ggplot(Average_ , aes(x = Year_Month, y = mean_rating)) +
ggplot()+  
  geom_line(data = Average_Male, aes(x = Year_Month, y = mean_rating_Male, color = "مرد")) +
  geom_point(data = Average_Male, aes(x = Year_Month, y = mean_rating_Male), color = "blue")  +
  geom_line(data = Average_Female, aes(x =Year_Month, y = mean_rating_Female, color = "زن")) +
  geom_point(data = Average_Female, aes(x = Year_Month, y = mean_rating_Female), color = "red") +
  geom_line() +
  scale_color_manual(values = c("مرد" = "blue", "زن" = "red")) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  labs( x = "سال", y = "میانگین درجه بین المللی" , color=" ") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month")+
  theme(text = element_text(family = "B Nazanin"),  axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),   
        axis.title = element_text(size = 18),  
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 12))
ggsave("کلی 3.png",
       width = 40,
       height = 20,
       units = "cm",
       dpi=900)
ggplot()+  
  geom_line(data = Average_Male, aes(x = Year_Month, y = mean_rating_Male, color = "Male")) +
  geom_point(data = Average_Male, aes(x = Year_Month, y = mean_rating_Male), color = "blue")  +
  geom_line(data = Average_Female, aes(x =Year_Month, y = mean_rating_Female, color = "Female")) +
  geom_point(data = Average_Female, aes(x = Year_Month, y = mean_rating_Female), color = "red") +
  geom_line() +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  labs(title = "Mean   Rating Over Time", x = "Time", y = "Mean   Rating" , color="Gender") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month")
#5312987 obs All
#3492416 standard
#1162258 rapid
#664051 blitz