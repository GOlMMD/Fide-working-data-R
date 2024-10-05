library(ggplot2)
library(extrafont)
font_import(pattern = "B Nazanin")
loadfonts()
library(ggplot2)
library(tidyverse)
library(dplyr)
################################ Rapid####################
filepath4 <- 'A:\\payan_name\\fide_data\\2016\\csv16\\checkup\\merged_data\\clean\\covid\\'
setwd(filepath4)
getwd()
BCNFinalR <- readRDS("BCNFinalR.rds")
#MBCNFinal <- subset(BCNFinal, Sex == 0)
#FBCNFinal <- subset(BCNFinal, Sex == 1)
names(BCNFinalR)
PBCNFinalR <- subset(BCNFinalR,covid_dummy ==0)
MPBCNFinalR <- subset(PBCNFinalR, Sex == 0)
FPBCNFinalR <- subset(PBCNFinalR, Sex == 1)
ABCNFinalR <- subset(BCNFinalR,covid_dummy ==1)
MABCNFinalR <- subset(ABCNFinalR, Sex == 0)
FABCNFinalR <- subset(ABCNFinalR, Sex == 1)
# Organize density values and labels into a dataframe
#MPBCNFinalR$Rapid_Rating
#class(MPBCNFinalR$Rapid_Rating)
density_values <- list(
  "Before Covid Overall" = density(PBCNFinalR$Rapid_Rating),
  "Before Covid Male" = density(MPBCNFinalR$Rapid_Rating),
  "Before Covid Female" = density(FPBCNFinalR$Rapid_Rating),
  "After Covid Overall" = density(ABCNFinalR$Rapid_Rating),
  "After Covid Male" = density(MABCNFinalR$Rapid_Rating),
     "After Covid Female" = density(FABCNFinalR$Rapid_Rating)
   )
density_values<- NULL
density_values <- list(
  "قبل کویید-19 کلی" = density(PBCNFinalR$Rapid_Rating),
  "قبل کویید-19 مردان" = density(MPBCNFinalR$Rapid_Rating),
  "قبل کویید-19 زنان" = density(FPBCNFinalR$Rapid_Rating),
  "بعد کویید-19 کلی" = density(ABCNFinalR$Rapid_Rating),
  "بعد کویید-19 مردان" = density(MABCNFinalR$Rapid_Rating),
  "بعد کویید-19 زنان" = density(FABCNFinalR$Rapid_Rating)
)

density_values <- list(
  "قبل کرونا کلی" = density(PBCNFinalR$Rapid_Rating),
  "قبل کرونا مردان" = density(MPBCNFinalR$Rapid_Rating),
  "قبل کرونا زنان" = density(FPBCNFinalR$Rapid_Rating),
  "بعد کرونا کلی" = density(ABCNFinalR$Rapid_Rating),
  "بعد کرونا مردان" = density(MABCNFinalR$Rapid_Rating),
  "بعد کرونا زنان" = density(FABCNFinalR$Rapid_Rating)
)
density_values <- list(
  "قبل کرونا کلی" = density(PUBCNFinalR$Rapid_Rating),
  "قبل کرونا مردان" = density(MPUBCNFinalB$Rapid_Rating),
  "قبل کرونا زنان" = density(FPUBCNFinalB$Rapid_Rating),
  "بعد کرونا کلی" = density(AUBCNFinalB$Rapid_Rating),
  "بعد کرونا مردان" = density(MAUBCNFinalB$Rapid_Rating),
  "بعد کرونا زنان" = density(FAUBCNFinalB$Rapid_Rating)
)
new_names <- c(
  "Before Covid Overall" = "قبل کرونا کلی",
  "Before Covid Male" = "قبل کرونا مردان",
  "Before Covid Female" = "قبل کرونا زنان",
  "After Covid Overall" = "بعد کرونا کلی",
  "After Covid Male" = "بعد کرونا مردان",
  "After Covid Female" ="بعد کرونا زنان"
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
       title = "", 
       x = expression("درجه بین\u200cالمللی"), 
       y = "چگالی", 
       color = ""
     ) +
    # scale_color_manual(values = new_names) +
     theme(text = element_text(family = "B Nazanin"),  axis.text.x = element_text(size = 14), 
           axis.text.y = element_text(size = 14),   
           axis.title = element_text(size = 18),  
           axis.text = element_text(size = 18),
           legend.text = element_text(size = 12))
     ggsave("کرنل چگالی سرعتی2.png",
          width = 40,
          height = 20,
          units = "cm",
          dpi=900)
     ###########################
     density_df<- NULL
     ggsave("چگالی سرعتی.png", ggplot(density_df, aes(x = x, y = y, color = Category)) +
              geom_line() +
              labs(
                title = "", 
                # x =  expression("درجه بین\u200cالمللی فرم برق\u200cآسا"), 
                x = expression("درجه بین\u200cالمللی"), 
                y = "\nچگالی\n", 
                color = ""
              ) +
              # scale_color_manual(values = new_names) +
              theme(text = element_text(family = "B Nazanin"),  axis.text.x = element_text(size = 18), 
                    axis.text.y = element_text(size = 18),   
                    axis.title = element_text(size = 18),  
                    axis.text = element_text(size = 18),
                    legend.text = element_text(size = 14),
                    legend.position = c(.95, .95),
                    legend.justification = c("right", "top"),
                    legend.box.just = "right",
                    legend.margin = margin(6, 6, 6, 6))+ 
              scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
            ,
            width = 40,
            height = 20,
            units = "cm",
            dpi=900)
     
     
     ###########################
   ################################ Blitz####################
   library(ggplot2)
     UBCNFinalB <- readRDS("UBCNFinalB.rds")
     PUBCNFinalB <- subset(UBCNFinalB,covid_dummy ==0)
     MPUBCNFinalB <- subset(PUBCNFinalB, Sex == 0)
     FPUBCNFinalB <- subset(PUBCNFinalB, Sex == 1)
     AUBCNFinalB <- subset(UBCNFinalB,covid_dummy ==1)
     MAUBCNFinalB <- subset(AUBCNFinalB, Sex == 0)
     FAUBCNFinalB <- subset(AUBCNFinalB, Sex == 1)
   # Organize density values and labels into a dataframe
   density_values <- list(
     "Before Covid Overall" = density(PBCNFinalB$Blitz_Rating),
     "Before Covid Male" = density(MPBCNFinalB$Blitz_Rating),
     "Before Covid Female" = density(FPBCNFinalB$Blitz_Rating),
     "After Covid Overall" = density(ABCNFinalB$Blitz_Rating),
     "After Covid Male" = density(MABCNFinalB$Blitz_Rating),
     "After Covid Female" = density(FABCNFinalB$Blitz_Rating)
   )
   density_values <- list(
     "قبل کرونا کلی" = density(PUBCNFinalB$Blitz_Rating),
     "قبل کرونا مردان" = density(MPUBCNFinalB$Blitz_Rating),
     "قبل کرونا زنان" = density(FPUBCNFinalB$Blitz_Rating),
     "بعد کرونا کلی" = density(AUBCNFinalB$Blitz_Rating),
     "بعد کرونا مردان" = density(MAUBCNFinalB$Blitz_Rating),
     "بعد کرونا زنان" = density(FAUBCNFinalB$Blitz_Rating)
   )
   
   density_df <- do.call(rbind, lapply(names(density_values), function(name) {
     data.frame(
       x = density_values[[name]]$x,
       y = density_values[[name]]$y,
       Category = name
     )
   }))
   
   # Plot using ggplot
   ggsave("چگالی برق.png", ggplot(density_df, aes(x = x, y = y, color = Category)) +
     geom_line() +
     labs(
       title = "", 
      # x =  expression("درجه بین\u200cالمللی فرم برق\u200cآسا"), 
      x = expression("درجه بین\u200cالمللی"), 
      y = "\nچگالی\n", 
       color = ""
     ) +
     # scale_color_manual(values = new_names) +
     theme(text = element_text(family = "B Nazanin"),  axis.text.x = element_text(size = 18), 
           axis.text.y = element_text(size = 18),   
           axis.title = element_text(size = 18),  
           axis.text = element_text(size = 18),
           legend.text = element_text(size = 14),
           legend.position = c(.95, .95),
           legend.justification = c("right", "top"),
           legend.box.just = "right",
           legend.margin = margin(6, 6, 6, 6))
,
          width = 40,
          height = 20,
          units = "cm",
          dpi=900)
   ################################ Blitz####################
   ################################Standard####################
   library(ggplot2)
   
   # Organize density values and labels into a dataframe
   density_values <- list(
     "Before Covid Overall" = density(PBCNFinalS$Standard_Rating),
     "Before Covid Male" = density(MPBCNFinalS$Standard_Rating),
     "Before Covid Female" = density(FPBCNFinalS$Standard_Rating),
     "After Covid Overall" = density(ABCNFinalS$Standard_Rating),
     "After Covid Male" = density(MABCNFinalS$Standard_Rating),
     "After Covid Female" = density(FABCNFinalS$Standard_Rating)
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
       title = "Kernel Density Plot for Standard Ratings",
       x = "Standard Rating",
       y = "Density"
     ) +
     theme_minimal()
   ################################Standard####################
   
   BCNFinalS <- readRDS("BCNFinalS.rds")
   
   
   library(ggplot2)
   
   # Standard Ratings
   rapid_density_values <- list(
     "Before Covid Overall" = density(PBCNFinalS$Rapid_Rating),
     "Before Covid Male" = density(MPBCNFinalS$Rapid_Rating),
     "Before Covid Female" = density(FPBCNFinalS$Rapid_Rating),
     "After Covid Overall" = density(ABCNFinalS$Rapid_Rating),
     "After Covid Male" = density(MABCNFinalS$Rapid_Rating),
     "After Covid Female" = density(FABCNFinalS$Rapid_Rating)
   )
   
   rapid_density_df <- do.call(rbind, lapply(names(rapid_density_values), function(name) {
     data.frame(
       x = rapid_density_values[[name]]$x,
       y = rapid_density_values[[name]]$y,
       Category = name
     )
   }))
   
   ggplot(rapid_density_df, aes(x = x, y = y, color = Category)) +
     geom_line() +
     labs(
       title = "Kernel Density Plot for Rapid Ratings",
       x = "Rapid Rating",
       y = "Density"
     ) +
     theme_minimal()
   
   # Blitz Ratings
   blitz_density_values <- list(
     "Before Covid Overall" = density(PUBCNFinalB$Blitz_Rating),
     "Before Covid Male" = density(MPUBCNFinalB$Blitz_Rating),
     "Before Covid Female" = density(FPUBCNFinalB$Blitz_Rating),
     "After Covid Overall" = density(AUBCNFinalB$Blitz_Rating),
     "After Covid Male" = density(MAUBCNFinalB$Blitz_Rating),
     "After Covid Female" = density(FAUBCNFinalB$Blitz_Rating)
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
   BCNFinalS <- readRDS("BCNFinalS.rds")
   names(BCNFinalS)
   PBCNFinalS <- subset(BCNFinalS,covid_dummyS ==0)
   MPBCNFinalS <- subset(PBCNFinalS, Sex == 0)
   FPBCNFinalS <- subset(PBCNFinalS, Sex == 1)
   ABCNFinalS <- subset(BCNFinalS,covid_dummyS ==1)
   MABCNFinalS <- subset(ABCNFinalS, Sex == 0)
   FABCNFinalS <- subset(ABCNFinalS, Sex == 1)
   # Standard Ratings
   standard_density_values <- list(
     "Before Covid Overall" = density(PBCNFinalS$Standard_Rating),
     "Before Covid Male" = density(MPBCNFinalS$Standard_Rating),
     "Before Covid Female" = density(FPBCNFinalS$Standard_Rating),
     "After Covid Overall" = density(ABCNFinalS$Standard_Rating),
     "After Covid Male" = density(MABCNFinalS$Standard_Rating),
     "After Covid Female" = density(FABCNFinalS$Standard_Rating)
   )
   
   standard_density_values <- list(
     "قبل کرونا کلی" = density(PBCNFinalS$Standard_Rating),
     "قبل کرونا مردان" = density(MPBCNFinalS$Standard_Rating),
     "قبل کرونا زنان" = density(FPBCNFinalS$Standard_Rating),
     "بعد کرونا کلی" = density(ABCNFinalS$Standard_Rating),
     "بعد کرونا مردان" = density(MABCNFinalS$Standard_Rating),
     "بعد کرونا زنان" = density(FABCNFinalS$Standard_Rating)
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
       title = "", 
       x = expression("درجه بین\u200cالمللی فرم استاندارد"), 
       y = "چگالی", 
       color = ""
     ) +
     # scale_color_manual(values = new_names) +
     theme(text = element_text(family = "B Nazanin"),  axis.text.x = element_text(size = 14), 
           axis.text.y = element_text(size = 14),   
           axis.title = element_text(size = 18),  
           axis.text = element_text(size = 18),
           legend.text = element_text(size = 12))+ 
     scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
   ggsave("چگالی استاندارد.png",
          width = 40,
          height = 20,
          units = "cm",
          dpi=900)
   ############################################احتمالا نهایی
   # Plot using ggplot
   ggsave("چگالی استاندارد.png", ggplot(standard_density_df, aes(x = x, y = y, color = Category)) +
            geom_line() +
            labs(
              title = "", 
              # x =  expression("درجه بین\u200cالمللی فرم برق\u200cآسا"), 
              x = expression("درجه بین\u200cالمللی"), 
              y = "\nچگالی\n", 
              color = ""
            ) +
            # scale_color_manual(values = new_names) +
            theme(text = element_text(family = "B Nazanin"),  axis.text.x = element_text(size = 18), 
                  axis.text.y = element_text(size = 18),   
                  axis.title = element_text(size = 18),  
                  axis.text = element_text(size = 18),
                  legend.text = element_text(size = 14),
                  legend.position = c(.95, .95),
                  legend.justification = c("right", "top"),
                  legend.box.just = "right",
                  legend.margin = margin(6, 6, 6, 6))+ 
            scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
          ,
          width = 40,
          height = 20,
          units = "cm",
          dpi=900)
   ###########################################
   ###########################AAAAAAAAAAAALLLLLLLLLLLLLLLLL
   BCNFinal <-  readRDS("BCNFinal.rds")
   names(BCNFinal)
   PBCNFinal <- subset(BCNFinal,covid_dummy ==0)
   MPBCNFinal <- subset(PBCNFinal, Sex == 0)
   FPBCNFinal <- subset(PBCNFinal, Sex == 1)
   ABCNFinal <- subset(BCNFinal,covid_dummy ==1)
   MABCNFinal <- subset(ABCNFinal, Sex == 0)
   FABCNFinal <- subset(ABCNFinal, Sex == 1)
   ALL_density_values <- list(
     "قبل کرونا کلی" = density(PBCNFinal$Rating),
     "قبل کرونا مردان" = density(MPBCNFinal$Rating),
     "قبل کرونا زنان" = density(FPBCNFinal$Rating),
     "بعد کرونا کلی" = density(ABCNFinal$Rating),
     "بعد کرونا مردان" = density(MABCNFinal$Rating),
     "بعد کرونا زنان" = density(FABCNFinal$Rating)
   )
   ALL_density_df <- do.call(rbind, lapply(names(ALL_density_values), function(name) {
     data.frame(
       x = ALL_density_values[[name]]$x,
       y = ALL_density_values[[name]]$y,
       Category = name
     )
   }))
   
   ggplot(ALL_density_df, aes(x = x, y = y, color = Category)) +
     geom_line() +
     labs(
       title = "", 
       x = expression("درجه بین\u200cالمللی کل فرم\u200cها"), 
       y = "چگالی", 
       color = ""
     ) +
     # scale_color_manual(values = new_names) +
     theme(text = element_text(family = "B Nazanin"),  axis.text.x = element_text(size = 14), 
           axis.text.y = element_text(size = 14),   
           axis.title = element_text(size = 18),  
           axis.text = element_text(size = 18),
           legend.text = element_text(size = 12))
   ggsave("چگالی کل.png",
          width = 40,
          height = 20,
          units = "cm",
          dpi=900)
   ####################گزارش پایان نامه یحتمل
   ggsave("تجمیع.png", ggplot(ALL_density_df, aes(x = x, y = y, color = Category)) +
            geom_line() +
            labs(
              title = "", 
              # x =  expression("درجه بین\u200cالمللی فرم برق\u200cآسا"), 
              x = expression("درجه بین\u200cالمللی"), 
              y = "\nچگالی\n", 
              color = ""
            ) +
            # scale_color_manual(values = new_names) +
            theme(text = element_text(family = "B Nazanin"),  axis.text.x = element_text(size = 18), 
                  axis.text.y = element_text(size = 18),   
                  axis.title = element_text(size = 18),  
                  axis.text = element_text(size = 18),
                  legend.text = element_text(size = 14),
                  legend.position = c(.95, .95),
                  legend.justification = c("right", "top"),
                  legend.box.just = "right",
                  legend.margin = margin(6, 6, 6, 6))+ 
            scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
          ,
          width = 40,
          height = 20,
          units = "cm",
          dpi=900)
   