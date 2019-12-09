library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(pROC)
library(randomForest)
library(nnet)


prob <- c(1:101)
for(i in 1:101){
  prob[i] <- 0.01*(i-1)
}


u_curve <- function(x,rho){
  if (rho > 0) {
    y <- -exp(-x/rho)
  }
  else {
    y <- exp(-x/rho)
  }
  return(y)
}

CE <- function(u,rho){
  x <- -log(abs(u))*rho
  return(x)
}

Bottle  <- function(Thin,Sales,Reputation_lose,rho){
  y <- u_curve(Thin*Sales*1.075+Reputation_lose,rho)
  return(y)
}

Bulk <- function(Thin,Sales,rho){
  y <- u_curve(1.075*Thin*Sales/2,rho)
  return(y)
}

BotrytisF <- function(Botrytis,Sales,Reputation_gain,rho){
  y <- u_curve(Botrytis*Sales*0.7+Reputation_gain,rho)
  return(y)
}
Not_Botrytis <- function(Thin,Sales,Reputation_lose,rho){
  y <- max(Bottle(Thin,Sales,Reputation_lose,rho),Bulk(Thin,Sales,rho))
  return(y)
}
Sugar_25 <- function(Price_25,Sales,rho){
  y <- u_curve(Price_25*Sales,rho)
  return(y)
}

Sugar_20 <- function(Price_20,Sales,rho){
  y <- u_curve(Price_20*Sales,rho)
  return(y)
}

Sugar_19 <- function(Price_19,Sales,rho){
  y <- u_curve(Price_19*Sales,rho)
  return(y)
}
Warm_Rain <- function(Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,rho){
  y <- p_Botrytis*BotrytisF(Botrytis,Sales,Reputation_gain,rho)+(1-p_Botrytis)*Not_Botrytis(Thin,Sales,Reputation_lose,rho)
  return(y)
}
Not_Rain <- function(Price_25,Price_20,Price_19,Sales,p_25,p_20,p_19,rho){
  y <- p_25*Sugar_25(Price_25,Sales,rho)+p_20*Sugar_20(Price_20,Sales,rho)+p_19*Sugar_19(Price_19,Sales,rho)
  return(y)
}

Harvest_Immediately <- function(Ordinary,Sales,rho){
  y <- u_curve(Ordinary*Sales,rho)
  return(y)
}

Not_Harvest <- function(Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho){
  y <- p_warm_rain*Warm_Rain(Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,rho) + (1-p_warm_rain)*Not_Rain(Price_25,Price_20,Price_19,Sales,p_25,p_20,p_19,rho)
  return(y)
}  

Bottle_Decision <- function(Thin,Sales,Reputation_lose,rho){
  y <- ifelse(Bottle(Thin,Sales,Reputation_lose,rho)>Bulk(Thin,Sales,rho),"Bottle","Bulk")
  return(y)
}

Harvest_u_value <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho){
  y <- max(Harvest_Immediately(Ordinary,Sales,rho),Not_Harvest(Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho))
  return(y)
}

Harvest_Decision <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho){
  u1 <- Harvest_Immediately(Ordinary,Sales,rho)
  u2 <- Not_Harvest(Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho)
  y <- ifelse(u1>u2,"Harvest Immediately","Not Harvest")
  return(list(y, CE(u1, rho), CE(u2, rho)))
}

Harvest_Decision_Only <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho){
  u1 <- Harvest_Immediately(Ordinary,Sales,rho)
  u2 <- Not_Harvest(Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho)
  y <- ifelse(u1>u2,"Harvest Immediately","Not Harvest")
  return(y)
}

Harvest_Spores_Decision <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho){
  u1 <- Harvest_Immediately(Ordinary,Sales,rho)
  u2 <- Not_Harvest(Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,1,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho)
  ce1 <- CE(u1, rho)-10000
  ce2 <- CE(u2, rho)-10000
  y <- ifelse(ce1>ce2,"Harvest Immediately","Not Harvest")
  return(list(y, ce1, ce2))
}


P_Detector_Warm_Rain <- function(p_warm_rain,TP,TN){
  y <- p_warm_rain*TP+(1-p_warm_rain)*(1-TN)
  return(y)
}

p_warm_rain_given_warm <- function(p_warm_rain,TP,TN){
  y <- p_warm_rain*TP/P_Detector_Warm_Rain(p_warm_rain,TP,TN)
  return(y)
}

p_warm_rain_given_not_warm <- function(p_warm_rain,TP,TN){
  y <- p_warm_rain*(1-TP)/(1-P_Detector_Warm_Rain(p_warm_rain,TP,TN))
  return(y)
}

Harvest_u_value_Detector_Warm_Rain <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TN,rho){
  p_warm_used <- p_warm_rain_given_warm(p_warm_rain,TP,TN)
  y <- Harvest_u_value(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_used,rho)
  return(y)
}


Harvest_Detector_Warm_Rain_Decision <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TN,rho){
  p_warm_used <- p_warm_rain_given_warm(p_warm_rain,TP,TN)
  y <- Harvest_Decision_Only(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_used,rho)
  u1 <- Harvest_Immediately(Ordinary,Sales,rho)
  u2 <- Not_Harvest(Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_used,rho)
  ce1 <- CE(u1, rho)-1000
  ce2 <- CE(u2, rho)-1000
  return(list(y, ce1, ce2))

}

Harvest_u_value_Detector_Not_Rain <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TN,rho){
  p_warm_used <-  p_warm_rain_given_not_warm(p_warm_rain,TP,TN)
  y <- Harvest_u_value(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_used,rho)
  return (y)
}

Harvest_Detector_Not_Rain_Decision <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TN,rho){
  p_warm_used <- p_warm_rain_given_not_warm(p_warm_rain,TP,TN)
  y <- Harvest_Decision_Only(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_used,rho)
  u1 <- Harvest_Immediately(Ordinary,Sales,rho)
  u2 <- Not_Harvest(Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_used,rho)
  ce1 <- CE(u1, rho)-1000
  ce2 <- CE(u2, rho)-1000
  
  return(list(y, ce1, ce2))
}

Value_of_Detector_Data_Only <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TN,rho){
  p_Detector <- P_Detector_Warm_Rain(p_warm_rain,TP,TN)
  u_Detector <- p_Detector*Harvest_u_value_Detector_Warm_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TN,rho) + (1-p_Detector)*Harvest_u_value_Detector_Not_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TN,rho)
  CE_Detector <- CE(u_Detector,rho)
  u_No_Detector <- Harvest_u_value(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho)
  CE_Not_Detector <- CE(u_No_Detector,rho)
  y <- CE_Detector - CE_Not_Detector
  return(y)
}

Value_of_Spore_Spore_Only <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho){
  u_buy_spore <- Harvest_u_value(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,1,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho)
  u_no_spore <- Harvest_u_value(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho)
  CE_buy_spore <- CE(u_buy_spore,rho)
  CE_no_spore <- CE(u_no_spore,rho)
  y <- CE_buy_spore - CE_no_spore
  return(y)
}

Spore_Decision_Spore_Only <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho,Spore_Cost){
  y <- ifelse(Spore_Cost < Value_of_Spore_Spore_Only(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho),"Buy spores","Don't buy spores")
  return(y)
}

Value_of_Spore_Data_First_Detector_Warm <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho){
  u_buy_spore <- Harvest_u_value_Detector_Warm_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,1,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho)
  u_no_spore <- Harvest_u_value_Detector_Warm_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho)
  CE_buy_spore <- CE(u_buy_spore,rho)
  CE_no_spore <- CE(u_no_spore,rho)
  y <- CE_buy_spore - CE_no_spore
  return(y)
}

Spore_Decision_Data_First_Detector_Warm <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho,Spore_Cost){
  y <- ifelse(Spore_Cost < Value_of_Spore_Data_First_Detector_Warm(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho),"Buy spores","Don't buy spores")
  return(y)
}

Value_of_Spore_Data_First_Detector_Not_Rain <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho){
  u_buy_spore <- Harvest_u_value_Detector_Not_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,1,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho)
  u_no_spore <- Harvest_u_value_Detector_Not_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho)
  CE_buy_spore <- CE(u_buy_spore,rho)
  CE_no_spore <- CE(u_no_spore,rho)
  y <- CE_buy_spore - CE_no_spore
  return(y)
}

Spore_Decision_Data_First_Detector_Not_Rain <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho,Spore_Cost){
  y <- ifelse(Spore_Cost < Value_of_Spore_Data_First_Detector_Not_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho),"Buy spores","Don't buy spores")
  return(y)
}

Value_of_Detector_Data_First <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho){
  u_buy_spore_Warm <- Harvest_u_value_Detector_Warm_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,1,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho)
  u_no_spore_Warm <- Harvest_u_value_Detector_Warm_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho)
  u_buy_spore_Not <- Harvest_u_value_Detector_Not_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,1,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho)
  u_no_spore_Not <- Harvest_u_value_Detector_Not_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho)
  u_Warm <- max(u_buy_spore_Warm,u_no_spore_Warm)
  u_Not_Rain <- max(u_buy_spore_Not,u_no_spore_Not)
  u <- P_Detector_Warm_Rain(p_warm_rain,TP,TF)*u_Warm + (1-P_Detector_Warm_Rain(p_warm_rain,TP,TF))*u_Not_Rain
  CE_Data_First <- CE(u,rho)
  y <- CE_Data_First - CE(Harvest_u_value(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho),rho) - Value_of_Spore_Spore_Only(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho)
  return(y)
}

Data_Only_Decision <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TN,rho,data_cost){
  P <- P_Detector_Warm_Rain(p_warm_rain,TP,TN)
  u1 <- Harvest_u_value_Detector_Warm_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TN,rho)
  u2 <- Harvest_u_value_Detector_Not_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TN,rho)
  u3 <- P*u1 + (1-P)*u2
  u4 <- Harvest_u_value(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho)
  ce_buy_data <- CE(u3,rho)-data_cost
  ce_not_buy_data <- CE(u4,rho)
  dec <- ifelse(ce_buy_data > ce_not_buy_data, "Buy data","Don't buy data")
  y <- dec
  return(y)
}

Data_Only_CE <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TN,rho,data_cost){
  P <- P_Detector_Warm_Rain(p_warm_rain,TP,TN)
  u1 <- Harvest_u_value_Detector_Warm_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TN,rho)
  u2 <- Harvest_u_value_Detector_Not_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TN,rho)
  u3 <- P*u1 + (1-P)*u2
  u4 <- Harvest_u_value(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho)
  ce_buy_data <- CE(u3,rho)-data_cost
  ce_not_buy_data <- CE(u4,rho)
  y <- max(ce_buy_data,ce_not_buy_data)
  return(y)
}

Data_First_Decision <- function(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho,data_cost,spore_cost){
  u1 <- Harvest_u_value_Detector_Warm_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,1,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho)
  u1 <- u_curve(CE(u1,rho) - spore_cost,rho)
  u2 <- Harvest_u_value_Detector_Warm_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho)
  u_warm <- max(u1,u2)
  u3 <- Harvest_u_value_Detector_Not_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,1,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho)
  u3 <- u_curve(CE(u3,rho) - spore_cost,rho)
  u4 <- Harvest_u_value_Detector_Not_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,TP,TF,rho)
  u_not_rain <- max(u3,u4)
  P <- P_Detector_Warm_Rain(p_warm_rain,TP,TF)
  u_detector <- P*u_warm + (1-P)*u_not_rain
  ce_buy_data <- CE(u_detector,rho) - data_cost
  u5 <- Harvest_u_value(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,1,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho)
  u6 <- Harvest_u_value(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,p_warm_rain,rho)
  u5 <- u_curve(CE(u5,rho) - spore_cost,rho)
  u_no_detector <- max(u5,u6)
  ce_not_buy_data <- CE(u_no_detector,rho)
  y <- ifelse(ce_not_buy_data > ce_buy_data,"Don't buy data","Buy data")
  return(y)
}


#Sensitivity Analysis on Probability of Warm Rain
sensitivity1 <- function(prob, rho_SA, p_warm_rain) {
  uval_Harvest <- c(1:101)
  ce_Harvest <- c(1:101)
  for(i in 1:101){
    uval_Harvest[i] <- Harvest_Immediately(Ordinary = 28.5,Sales = 12000,rho_SA)
    ce_Harvest[i] <- CE(uval_Harvest[i],rho_SA)
  }
  uval_Not_Harvest <- c(1:101)
  ce_Not_Harvest <- c(1:101)
  for(i in 1:101){
    uval_Not_Harvest[i] <- Not_Harvest(Thin = 20,Sales = 12000,Reputation_lose = -250000,Botrytis = 80,Reputation_gain = 150000,p_Botrytis = 0.4,Price_25 = 35,Price_20 = 30,Price_19 = 25,p_25 = 0.4,p_20 = 0.4,p_19 = 0.2,p_warm_rain = prob[i],rho = rho_SA)
    ce_Not_Harvest[i] <- CE(uval_Not_Harvest[i],rho_SA)
  }
  
  sensitivity_prob <- data.frame(prob,ce_Harvest,ce_Not_Harvest)  
  return(sensitivity_prob)
}

#Sensitivity on Probability of Botrytis
sensitivity2 <- function(prob, rho_SA, p_warm_rain){
  uval_Harvest_Botrytis <- c(1:101)
  ce_Harvest_Botrytis <- c(1:101)
  for(i in 1:101){
    uval_Harvest_Botrytis[i] <- Harvest_Immediately(Ordinary = 28.5,Sales = 12000,rho_SA)
    ce_Harvest_Botrytis[i] <- CE(uval_Harvest_Botrytis[i],rho_SA)
  }
  uval_Not_Harvest_Botrytis <- c(1:101)
  ce_Not_Harvest_Botrytis <- c(1:101)
  for(i in 1:101){
    uval_Not_Harvest_Botrytis[i] <- Not_Harvest(Thin = 20,Sales = 12000,Reputation_lose = -250000,Botrytis = 80,Reputation_gain = 150000,p_Botrytis = prob[i],Price_25 = 35,Price_20 = 30,Price_19 = 25,p_25 = 0.4,p_20 = 0.4,p_19 = 0.2,p_warm_rain,rho_SA)
    ce_Not_Harvest_Botrytis[i] <- CE(uval_Not_Harvest_Botrytis[i],rho_SA)
  }
  sensitivity_prob_Botrytis <- data.frame(prob,ce_Harvest_Botrytis,ce_Not_Harvest_Botrytis)
  return(sensitivity_prob_Botrytis)
}

#Sensitivity Analysis on Sugar
sensitivity3 <- function(prob, rho_SA, p_warm_rain) {
  uval_Harvest_Sugar <- c(1:51)
  ce_Harvest_Sugar <- c(1:51)
  for(i in 1:51){
    uval_Harvest_Sugar[i] <- Harvest_Immediately(Ordinary = 28.5,Sales = 12000,rho_SA)
    ce_Harvest_Sugar[i] <- CE(uval_Harvest_Sugar[i],rho_SA)
  }
  
  uval_Not_Harvest_Sugar <- c(1:51)
  ce_Not_Harvest_Sugar <- c(1:51)
  for(i in 1:51){
    uval_Not_Harvest_Sugar[i] <- Not_Harvest(Thin = 20,Sales = 12000,Reputation_lose = -250000,Botrytis = 80,Reputation_gain = 150000,p_Botrytis = 0.4,Price_25 = 35,Price_20 = 30,Price_19 = 25,p_25 = prob[i],p_20 = prob[i],p_19 = 1 - 2 * prob[i],p_warm_rain,rho_SA)
    ce_Not_Harvest_Sugar[i] <- CE(uval_Not_Harvest_Sugar[i],rho_SA)
  }
  sensitivity_prob_Sugar <- data.frame(prob[1:51],ce_Harvest_Sugar,ce_Not_Harvest_Sugar)
  return(sensitivity_prob_Sugar)
}


#Sensitivity Analysis on rho
sensitivity4 <- function(prob, p_warm_rain) {
  rho_chang <- c(1:2000)
  for(i in 1:2000){
    ifelse(i<=1000,rho_chang[i] <- i*1000-1001000,rho_chang[i] <- i*1000-1000000)
  }
  uval_Harvest_rho <- c(1:2000)
  ce_Harvest_rho <- c(1:2000)
  for(i in 1:2000){
    uval_Harvest_rho[i] <- Harvest_Immediately(Ordinary = 28.5,Sales = 12000,rho = rho_chang[i])
    ce_Harvest_rho[i] <- CE(uval_Harvest_rho[i],rho_chang[i])
  }
  uval_Not_Harvest_rho <- c(1:2000)
  ce_Not_Harvest_rho <- c(1:2000)
  for(i in 1:2000){
    uval_Not_Harvest_rho[i] <-  Not_Harvest(Thin = 20,Sales = 12000,Reputation_lose = -250000,Botrytis = 80,Reputation_gain = 150000,p_Botrytis = 0.4,Price_25 = 35,Price_20 = 30,Price_19 = 25,p_25 = 0.4,p_20 = 0.4,p_19 = 0.2,p_warm_rain,rho = rho_chang[i])
    ce_Not_Harvest_rho[i] <- CE(uval_Not_Harvest_rho[i],rho_chang[i])
  }
  
  sensitivity_rho <- data.frame(rho_chang,ce_Harvest_rho,ce_Not_Harvest_rho)
  sensitivity_rho_negative <- filter(sensitivity_rho, rho_chang <0)
  sensitivity_rho_positive <- filter(sensitivity_rho, rho_chang >0)
  return(list(a = sensitivity_rho, b = sensitivity_rho_negative, c = sensitivity_rho_positive, d = rho_chang, e = ce_Harvest_rho, f = ce_Not_Harvest_rho))
}


Calculate_TP <- function(TPs, model) {
    return(TPs[model])
}

Calculate_TN <- function(TNs, model) {
    return(TNs[model])
}


dataProcess <- function(file) {
  weather <- read.csv(file$datapath)
  weather$PRCP <- ifelse(is.na(weather$PRCP),mean(weather$PRCP,na.rm = TRUE),weather$PRCP)
  weather$RAIN <- ifelse((weather$PRCP>0)&(is.na(weather$RAIN)),TRUE,weather$RAIN)
  weather$WEEK <- week(weather$DATE)
  weather$YEAR <- year(weather$DATE)
  weather$RAIN <- ifelse(weather$PRCP>0.1,TRUE,FALSE)
  weather$TEMPRAIN <- ifelse(weather$RAIN == TRUE, weather$TMAX,0)
  weather$PRCPRAIN <- ifelse(weather$RAIN == TRUE, weather$PRCP,0)
  weather_weekly <- weather %>%
    group_by(YEAR,WEEK) %>%
    summarise(PRCP = sum(PRCP),
              TMAX = max(TMAX),
              TMIN = min(TMIN),
              RAIN = sum(RAIN),
              TMAX_AVG = mean(TMAX),
              TMIN_AVG = mean(TMIN),
              TEMPRAIN = max(TEMPRAIN),
              PRCPRAIN = sum(PRCPRAIN))
  weather_weekly$WARMRAIN <- ifelse((weather_weekly$TEMPRAIN<80)&(weather_weekly$RAIN >= 3),TRUE,FALSE)
  DATA_weather <- data.frame("WARMRAIN" = NA,
                             "YEAR" = NA,
                             "WEEK" = NA,
                             "PRCP1" = NA,
                             "TMAX1" = NA,
                             "TMIN1" = NA,
                             "RAIN1" = NA,
                             "TEMPRAIN1" = NA,
                             "PRCPRAIN1" = NA,
                             "PRCP2" = NA,
                             "TMAX2" = NA,
                             "TMIN2" = NA,
                             "RAIN2" = NA,
                             "TEMPRAIN2" = NA,
                             "PRCPRAIN2" = NA)
  for(i in 1:(nrow(weather_weekly)-2)){
    DATA_weather[i,1] = weather_weekly[i+2,11]
    DATA_weather[i,2] = weather_weekly[i,1]
    DATA_weather[i,3] = weather_weekly[i,2]
    DATA_weather[i,4] = weather_weekly[i,3]
    DATA_weather[i,5] = weather_weekly[i,4]
    DATA_weather[i,6] = weather_weekly[i,5]
    DATA_weather[i,7] = weather_weekly[i,6]
    DATA_weather[i,8] = weather_weekly[i,9]
    DATA_weather[i,9] = weather_weekly[i,10]
    DATA_weather[i,10] = weather_weekly[i+1,3]
    DATA_weather[i,11] = weather_weekly[i+1,4]
    DATA_weather[i,12] = weather_weekly[i+1,5]
    DATA_weather[i,13] = weather_weekly[i+1,6]
    DATA_weather[i,14] = weather_weekly[i+1,9]
    DATA_weather[i,15] = weather_weekly[i+1,10]
  }
  
  DATA_weather$WARMRAIN = factor(DATA_weather$WARMRAIN, levels = c(FALSE,TRUE))
  return(DATA_weather)
}

predictData <- function(file, model) {
  test = dataProcess(file)
  if (model == "logreg") {
    model = readRDS("models/logreg.RDS")
    p1 = predict(model, test,type="response")
    predict_result = ifelse(p1 > 0.28,"Warm rain","Not warm rain")
  }
  else if (model == "rf") {
    model = readRDS("models/rf.RDS")
    p1 = predict(model,test, type = "class")
    result = summary(p1)
    result = as.character(result)
    if(result[1] == "1")
      predict_result = "Not warm rain"
    else
      predict_result = "Warm rain"
  }
  else {
    model = readRDS("models/neural.RDS")
    p1 = predict(model, test,type=("class"))
    predict_result = ifelse(p1,"Warm rain","Not warm rain")
    
  }
  return(predict_result)    

}


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Decision Analysis"),
  fluidRow(
    column(6,
           wellPanel(
             selectInput("dataav", "Data availability:",
                         c(" " = "empty",
                           "Data available" = "adata",
                           "Data not available" = "nodata")),           
           ),
    ),
    column(6,
           wellPanel(
             selectInput("sporesav", "Spores availability:",
                         c(" " = "empty",
                           "Spores available" = "aspores",
                           "Spores not available" = "nospores")),          
           ),
    ),
  ),
    conditionalPanel(
      condition = "input.dataav == 'adata'  && input.sporesav != 'empty'",
      # select input
      wellPanel(
        selectInput("model", "Model:",
                    c("Logistic Regression" = "logreg",
                      "Random Forest" = "rf",
                      "Neural Networks" = "neural")),
        fileInput("file", "Upload the input data file (Please find the input_data.csv file in submitted files. Feel free to change the data). If there is no file uploaded, the decisions will be based on training data.",
          accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv")),
        conditionalPanel(
          condition = "input.dataav == 'adata'  && input.sporesav != 'empty'",
          wellPanel(
            textOutput("prediction"),
          )
        ),         
    
        )
      
      
    ),
    conditionalPanel(
      condition = "input.dataav == 'adata' && input.sporesav == 'nospores' ",
      fluidRow(
        column(12,
               wellPanel(
                 htmlOutput("data.decision")
               )
        ),
      ),   
      fluidRow(
        column(12,
               wellPanel(
                 htmlOutput("data.value")
               )
        ),
      ),  
    ),
  conditionalPanel(
    condition = "input.dataav == 'adata' && input.sporesav == 'aspores' ",
    fluidRow(
      column(12,
             wellPanel(
               htmlOutput("data.decision1")
             )
      ),
    ),  
  ),
  conditionalPanel(
    condition = "input.sporesav != 'empty' && input.dataav != 'empty'",
    conditionalPanel(
      condition = "input.data != 'empty' && input.spores != 'empty'",
      fluidRow(
        column(12,
               wellPanel(
                 htmlOutput("decision")
               )
        ),
      ),
      fluidRow(
        column(12,
               wellPanel(
                 htmlOutput("ce")
               )
        ),
      ),    
    ),
    
    conditionalPanel(
      condition = "input.sporesav == 'aspores' && input.dataav == 'nodata'",
      fluidRow(
        column(12,
               wellPanel(
                 htmlOutput("spores.decision")
               )
        ),
      ), 
      fluidRow(
        column(12,
               wellPanel(
                 htmlOutput("spores.value")
               )
        ),
      ),  
    ),
    
    conditionalPanel(
      condition = "input.dataav == 'adata' && input.sporesav == 'aspores' ",  
      fluidRow(
        column(12,
               wellPanel(
                 htmlOutput("spores.decision1")
               )
        ),
      ),  
    ),
    
    
    fluidRow(
      column(12,
             wellPanel(
               htmlOutput("parameters")
             )
      ),
    ),
  ),
  fluidRow(
    column(6,
           wellPanel(
             sliderInput(inputId = "pwarm",
                         label = "Probability of warm rain:",
                         min = 0,
                         max = 1,
                         value = 0.66),          
           ),
    ),
    column(6,
           wellPanel(
             sliderInput(inputId = "risk",
                         label = "Risk tolerance:",
                         min = 0,
                         max = 100000,
                         value = 72000),          
           ),
    ),
  ),   
  
  titlePanel("Sensitivity Analysis"),
  sidebarLayout(
    
    sidebarPanel(
      selectInput("param", "Parameter:",
                  c("Probability of Warm Rain" = "pwarm",
                    "Probability of Botrytis" = "pbot",
                    "Probability of Sugar Level" = "sugar",
                    "Risk Tolerance" = "rho")),
      textOutput("senstext")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1")
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot

  sales = "12,000"
  ordinary = "$28.5"
  thin = "$20"
  price25 = "$35"
  price20 = "$30"
  price19 = "$25"
  boprice = "$80"
  repgain = "$150,000"
  reploss = "-$250,000"
  pbot = "$10,000"
  p25 = "0.4"
  p20 = "0.4"
  p19 = "0.2"

  Sales = 12000
  Ordinary = 28.5
  Thin = 20
  Price_25 = 35
  Price_20 = 30
  Price_19 = 25
  Botrytis = 80
  Reputation_gain = 150000
  Reputation_lose = -250000
  p_Botrytis = 0.4
  p_25 = 0.4
  p_20 = 0.4
  p_19 = 0.2
  TPs <- c(0.722, 0.407, 0.416)
  TNs <- c(0.637, 0.816, 0.821)
  names(TPs) <- c("logreg", "rf", "neural")
  names(TNs) <- c("logreg", "rf", "neural")
  
  TP <- reactive({Calculate_TP(TPs, input$model)})
  TN <- reactive({Calculate_TN(TNs, input$model)})
  dec1 <- reactive({Harvest_Decision(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19, as.numeric(input$pwarm),as.numeric(input$risk))})
  dec2 <- reactive({Harvest_Spores_Decision(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,Price_25,Price_20,Price_19,p_25,p_20,p_19, as.numeric(input$pwarm),as.numeric(input$risk)) })
  dec3 <- reactive({Harvest_Detector_Warm_Rain_Decision(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,as.numeric(input$pwarm),TP(),TN(),as.numeric(input$risk)) })
  dec4 <- reactive({Harvest_Detector_Not_Rain_Decision(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,as.numeric(input$pwarm),TP(),TN(),as.numeric(input$risk))})
  dec5 <- reactive({Harvest_Detector_Warm_Rain_Decision(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,1,Price_25,Price_20,Price_19,p_25,p_20,p_19,as.numeric(input$pwarm),TP(),TN(),as.numeric(input$risk)) })
  dec6 <- reactive({Harvest_Detector_Not_Rain_Decision(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,1,Price_25,Price_20,Price_19,p_25,p_20,p_19,as.numeric(input$pwarm),TP(),TN(),as.numeric(input$risk))})
  
  decspores <- reactive({Spore_Decision_Spore_Only(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,as.numeric(input$pwarm),as.numeric(input$risk),10000) })
  valspores <- reactive({Value_of_Spore_Spore_Only(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,as.numeric(input$pwarm),as.numeric(input$risk))})
  decdata <- reactive({Data_Only_Decision(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,as.numeric(input$pwarm),TP(),TN(),as.numeric(input$risk),1000) })
  valdata <- reactive({Value_of_Detector_Data_Only(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,as.numeric(input$pwarm),TP(),TN(),as.numeric(input$risk)) })
  decdata1 <- reactive({Data_First_Decision(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,as.numeric(input$pwarm),TP(),TN(),as.numeric(input$risk),1000,10000) })
  decspores1 <- reactive({Spore_Decision_Data_First_Detector_Warm(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,as.numeric(input$pwarm),TP(),TN(),as.numeric(input$risk),10000) })
  decspores2 <- reactive({Spore_Decision_Data_First_Detector_Not_Rain(Ordinary,Thin,Sales,Reputation_lose,Botrytis,Reputation_gain,p_Botrytis,Price_25,Price_20,Price_19,p_25,p_20,p_19,as.numeric(input$pwarm),TP(),TN(),as.numeric(input$risk),10000) })
  sens1 <- reactive({sensitivity1(prob, input$risk, input$pwarm)})
  sens2 <- reactive({sensitivity2(prob, input$risk, input$pwarm)})
  sens3 <- reactive({sensitivity3(prob, input$risk, input$pwarm)})
  sens4 <- reactive({sensitivity4(prob, input$pwarm)}) 
  detector <- reactive({predictData(input$file, input$model)})
  
  output$ce <- renderText(
    { 
      if (input$dataav == "nodata" && input$sporesav == "nospores") {
        paste("Certain equivalent of harvest immediately: ", dec1()[2], ", Certain equivalent of not harvest: ", dec1()[3])
      }
      else if (input$dataav == "nodata" && input$sporesav == "aspores") {
        if (decspores() == "Buy spores") {
          paste("Certain equivalent of harvest immediately: ", dec2()[2], ", Certain equivalent of not harvest: ", dec2()[3])
        }
        else {
          paste("Certain equivalent of harvest immediately: ", dec1()[2], ", Certain equivalent of not harvest: ", dec1()[3])
        }
      } 
      else if (input$dataav == "adata" && input$sporesav == "nospores") {
        if (decdata() == "Buy data") {
          if (is.null(input$file)) {
            paste("<b>Case 1: </b> Detector predicts warm rain  &nbsp <b>Certain equivalent of harvest immediately: </b>", dec3()[2]," &nbsp <b>Certain equivalent of not harvest: </b>", dec3()[3], "<br>", "<b>Case 2: </b> Detector predicts no warm rain &nbsp <b>Certain equivalent of harvest immediately:</b>", dec4()[2], "<b>Certain equivalent of not harvest:</b>", dec4()[3])
          }
          else {
            if (detector() == "Warm rain") {
              paste("Detector result: ", detector(), "&nbsp <b>Certain equivalent of harvest immediately: </b>", dec3()[2], " &nbsp <b>Certain equivalent of not harvest: </b>", dec3()[3])
            }
            else {
              paste("Detector result: ", detector(), "&nbsp <b>Certain equivalent of harvest immediately: </b>", dec4()[2], " &nbsp <b>Certain equivalent of not harvest: </b>", dec4()[3])
            }
          }
        }
        else {
          paste("Certain equivalent of harvest immediately: ", dec1()[2], ", Certain equivalent of not harvest: ", dec1()[3])
        }
      }
      else {
        if (decdata1() == "Don't buy data") {
          if (decspores() == "Buy spores") {
            paste("Certain equivalent of harvest immediately: ",dec2()[2], ", Certain equivalent of not harvest: ", dec2()[3])
          }
          else {
            paste("Certain equivalent of harvest immediately: ",dec1()[2], ", Certain equivalent of not harvest: ", dec1()[3])
          }
        }
        else {
          if (decspores() == "Buy spores") {
            if (is.null(input$file)) {
              paste("<b>Case 1: </b> Detector predicts warm rain  &nbsp <b>Certain equivalent of harvest immediately: </b>", dec5()[2]," &nbsp <b>Certain equivalent of not harvest: </b>", dec5()[3], "<br>", "<b>Case 2: </b> Detector predicts no warm rain &nbsp <b>Certain equivalent of harvest immediately:</b>", dec6()[2], "<b>Certain equivalent of not harvest:</b>", dec6()[3])
            }
            else {
              if (detector() == "Warm rain") {
                paste("Detector result: ", detector(), "&nbsp <b>Certain equivalent of harvest immediately: </b>", dec5()[2], " &nbsp <b>Certain equivalent of not harvest: </b>", dec5()[3])
              }
              else {
                paste("Detector result: ", detector(), "&nbsp <b>Certain equivalent of harvest immediately: </b>", dec6()[2], " &nbsp <b>Certain equivalent of not harvest: </b>", dec6()[3])
              }
            }
          }
          else {
            if (is.null(input$file)) {
              paste("<b>Case 1: </b> Detector predicts warm rain  &nbsp <b>Certain equivalent of harvest immediately: </b>", dec3()[2]," &nbsp <b>Certain equivalent of not harvest: </b>", dec3()[3], "<br>", "<b>Case 2: </b> Detector predicts no warm rain &nbsp <b>Certain equivalent of harvest immediately:</b>", dec4()[2], "<b>Certain equivalent of not harvest:</b>", dec4()[3])
            }
            else {
              if (detector() == "Warm rain") {
                paste("Detector result: ", detector(), "&nbsp <b>Certain equivalent of harvest immediately: </b>", dec3()[2], " &nbsp <b>Certain equivalent of not harvest: </b>", dec3()[3])
              }
              else {
                paste("Detector result: ", detector(), "&nbsp <b>Certain equivalent of harvest immediately: </b>", dec4()[2], " &nbsp <b>Certain equivalent of not harvest: </b>", dec4()[3])
              }
              
            }            
          }
        }
      }
    }
  )
  output$decision <- renderText({ 
    if (input$dataav == "nodata" && input$sporesav == "nospores") {
      paste("<b>Harvest Decision:</b> ", dec1()[1])
    }
    else if (input$dataav == "nodata" && input$sporesav == "aspores") {
      if (decspores() == "Buy spores") {
        paste("<b>Harvest Decision:</b>", dec2()[1])
      }
      else {
        paste("<b>Harvest Decision:</b> ", dec1()[1])
      }
      
    }
    else if (input$dataav == "adata" && input$sporesav == "nospores") {
      if (decdata() == "Buy data") {
        if (is.null(input$file)) {
          paste("<b>Case 1: </b> Detector predicts warm rain  &nbsp <b>Harvest Decision:</b>", dec3()[1], "<br>", "<b>Case 2: </b> Detector predicts no warm rain &nbsp <b>Harvest Decision:</b>", dec4()[1])
        }
        else {
          if (detector() == "Warm rain") {
            paste("Detector result: ", detector(), "&nbsp <b>Harvest Decision: </b>", dec3()[1])            
          }
          else {
            paste("Detector result: ", detector(), "&nbsp <b>Harvest Decision: </b>", dec4()[1])            
          }
        }
      }
      else {
        paste("<b>Harvest Decision:</b> ", dec1()[1])
      }
    }
    else {
      if (decdata1() == "Don't buy data") {
        if (decspores() == "Buy spores") {
          paste("<b>Harvest Decision:</b>", dec2()[1])
        }
        else {
          paste("<b>Harvest Decision:</b> ", dec1()[1])
        }
      }
      else {
        if (decspores() == "Buy spores") {
          if (is.null(input$file)) {
            paste("<b>Case 1: </b> Detector predicts warm rain  &nbsp <b>Harvest Decision:</b>", dec5()[1], "<br>", "<b>Case 2: </b> Detector predicts no warm rain &nbsp <b>Harvest Decision:</b>", dec6()[1])
          }
          else {
            if (detector() == "Warm rain") {
              paste("Detector result: ", detector(), "&nbsp <b>Harvest Decision: </b>", dec5()[1])
            }
            else {
              paste("Detector result: ", detector(), "&nbsp <b>Harvest Decision: </b>", dec6()[1])
            }
          }
        }
        else {
          if (is.null(input$file)) {
            paste("<b>Case 1: </b> Detector predicts warm rain  &nbsp <b>Harvest Decision:</b>", dec3()[1], "<br>", "<b>Case 2: </b> Detector predicts no warm rain &nbsp <b>Harvest Decision:</b>", dec4()[1])
          }
          else {
            if (detector() == "Warm rain") {
              paste("Detector result: ", detector(), "&nbsp <b>Harvest Decision: </b>", dec3()[1])
            }
            else {
              paste("Detector result: ", detector(), "&nbsp <b>Harvest Decision: </b>", dec4()[1])
            }
          }
        }
      }
    }
    })

  output$parameters <- renderText({ paste("<b>Fixed parameters:</b>", "<br>", "Ordinary price: ", ordinary, ", Thin price: ", thin, ", sales: ", sales, ", Reputation loss: ", reploss, ", Botrytis price: ", boprice, ", Reputation gain: ", repgain, ", Probability of botrytis: ", pbot, ", Price of 25% sugar: ", price25, ", Price of 20% sugar: ", price20, ", Price of 19% sugar: ", price19, ", Probability of 25% sugar: ", p25, ", Probability of 20% sugar: ", p20, ", Probability of 19% sugar: ", p19) })
  output$data.decision <- renderText({ paste("<b>Suggested decision on buying data: </b>", decdata())})
  output$data.decision1 <- renderText({ paste("<b>Suggested decision on buying data: </b>", decdata1())})
  output$data.value <- renderText({ paste("<b>Value of buying data: </b>", valdata())})
  output$spores.decision <- renderText({ paste("<b>Suggested decision on buying spores: </b>", decspores())})
  output$spores.decision1 <- renderText(
    if (decdata1() == "Buy data") {
      paste("<b>Case 1: </b> Detector predicts warm rain  &nbsp <b>Suggested decision on buying spores:</b>", decspores1(), "<br>", "<b>Case 2: </b> Detector predicts no warm rain &nbsp <b>Suggested decision on buying spores:</b>", decspores2())
    }
    else {
      paste("<b>Suggested decision on buying spores: </b>", decspores())
    }
    )
  output$spores.value <- renderText({ paste("<b>Value of buying spores: </b>", valspores())})
  
  output$senstext <- renderText({ paste("Feel free to adjust the sliders above and see how the sensitivity curves change.") })
  
  output$prediction <- renderText({ 
    if (!is.null(input$file)) {
      paste("Prediction result: ", detector()) 
    }
    else {
      paste("Upload file to get the prediction result of detector")
    }
      })
  
  # Generate plots
  output$plot1 <- renderPlot({
    if (input$param == "pwarm") {
      ggplot() + 
        geom_line(data = sens1(),aes(x = prob, y = ce_Harvest,color = "Blue")) +
        geom_line(data = sens1(),aes(x = prob,y = ce_Not_Harvest,color = "Red")) + 
        scale_color_discrete(name = "Alternatives", labels = c("Harvest Immediately", "Not Harvest")) + xlab("Probability") + ylab("Certain Equivalent")
    }
    
    else if (input$param == "pbot"){
      ggplot() + 
        geom_line(data = sens2(),aes(x = prob, y = ce_Harvest_Botrytis,color = "Blue")) +
        geom_line(data = sens2(),aes(x = prob,y = ce_Not_Harvest_Botrytis,color = "Red")) + 
        scale_color_discrete(name = "Alternatives", labels = c("Harvest Immediately", "Not Harvest")) + xlab("Probability") + ylab("Certain Equivalent")
    }
    
    else if (input$param == "sugar"){
      ggplot() + 
        geom_line(data = sens3(),aes(x = prob[1:51], y = ce_Harvest_Sugar,color = "Blue")) +
        geom_line(data = sens3(),aes(x = prob[1:51],y = ce_Not_Harvest_Sugar,color = "Red")) + 
        scale_color_discrete(name = "Alternatives", labels = c("Harvest Immediately", "Not Harvest")) + xlab("Probability") + ylab("Certain Equivalent")      
    }
    
    else {
      d1 = data.frame(sens4()$a)
      d2 = data.frame(sens4()$b)
      d3 = data.frame(sens4()$c)
      r1 = sens4()$d
      r2 = sens4()$e
      r3 = sens4()$f
      print(length(r1))
      print(length(r3))
      print(d1)
      print(length(d3))
      ggplot() + geom_line(data = d1,aes(x=rho_chang,y=ce_Harvest_rho,color = "Blue")) +
        geom_line(data = d2,aes(x=rho_chang,y=ce_Not_Harvest_rho,color = "Red")) +
        geom_line(data = d3,aes(x=rho_chang,y=ce_Not_Harvest_rho,color = "Red")) +
        scale_color_discrete(name = "Alternatives", labels = c("Harvest Immediately", "Not Harvest")) + xlab("Risk Tolerance") + ylab("Certain Equivalent")      
    }
  })      
    

}


shinyApp(ui = ui, server = server)