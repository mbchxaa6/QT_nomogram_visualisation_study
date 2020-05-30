library(pracma)
library(readr)
library(tibble)
library(signal)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggcyto)

setwd("C:/Users/Alaa/Desktop/Study 4/All raw ECGs/10000 ECGs/Placebo/data/raw ECG with colour/")
mydir = "data"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)

for (f in 1:length(myfiles))
{ 
  
  
  ColoursQuantifier <- read_csv(myfiles[f])
  
  
  time <- ColoursQuantifier$time
  ColouredTime <- ColoursQuantifier$newcolourVector
  
  mv <-  ColoursQuantifier$mv
  
  up <- ColoursQuantifier$UpperColourLimit
  UpperColourLimit <- up[1]
  low <- ColoursQuantifier$LowerColourlimit
  LowerColourlimit <- low[1]
  
  
  colorcodesValues <- NA
  length(colorcodesValues) <- 9
  colorcodesValues[1] <- LowerColourlimit # purple 
  colorcodesValues[2] <- UpperColourLimit - (40*7) #blue
  colorcodesValues[3] <- UpperColourLimit - (40*6) #lime
  colorcodesValues[4] <- UpperColourLimit - (40*5) #green
  colorcodesValues[5] <- UpperColourLimit - (40*4) #yellow
  colorcodesValues[6] <- UpperColourLimit - (40*3) #orange
  colorcodesValues[7] <- UpperColourLimit - (40*2) #dark orange, this QT nomogram
  colorcodesValues[8] <- UpperColourLimit - (40*1) #red
  colorcodesValues[9] <- UpperColourLimit #dark red
  
  

  ################################################################
  
  Average_two_y_points <- 0
  Delat_two_x_points <- 0
  Area_of_trapezoid <- 0 
  
  length(Average_two_y_points) <- length(ColouredTime) 
  length(Delat_two_x_points) <- length(ColouredTime) 
  length(Area_of_trapezoid) <- length(ColouredTime) 
  
  
  
  for ( i in 1:length(ColouredTime))
  { 
    if (ColouredTime[i] < 800)
    { 
      Average_two_y_points[i] = (abs(mv[i]) + abs(mv[i+1]))/2
      Delat_two_x_points[i] = 1 # this alwasys equals to one
      Area_of_trapezoid[i] = Average_two_y_points[i] * Delat_two_x_points[i] #we can remove the delta as it's one
    }
    else Area_of_trapezoid[i] = 0
  }
  
  
  #create a new ECG file
  
  mydata <- cbind(time,mv,ColouredTime,Area_of_trapezoid,UpperColourLimit,LowerColourlimit)
  ECG_with_Area_of_trapezoid <- na.omit(mydata)
  write.csv(ECG_with_Area_of_trapezoid,"C:/Users/Alaa/Desktop/Study 4/ECG_with_Area_of_trapezoid.csv")
  
  ECG_with_Area_of_trapezoid <- read_csv("C:/Users/Alaa/Desktop/Study 4/ECG_with_Area_of_trapezoid.csv")
  View(ECG_with_Area_of_trapezoid)
  
  
  
  ##############################################################################
  #Quntifing area under each colour:
  
  ColouredTime <- ECG_with_Area_of_trapezoid$ColouredTime
  
  
  #area of purple colour
  sum_of_purple_area <- 0
  
  for (x in 1:length(ColouredTime))
  { 
    if (ColouredTime[x] > (colorcodesValues[1]-40) && ColouredTime[x] < colorcodesValues[1])
    { 
      sum_of_purple_area = sum_of_purple_area + Area_of_trapezoid[x] 
    }
  }
  
  sum_of_purple_area
  
  #area of blue colour
  sum_of_blue_area <- 0
  
  for (x in 1:length(ColouredTime))
  { 
    if (ColouredTime[x] > (colorcodesValues[1]) && ColouredTime[x]<= colorcodesValues[2])
    { 
      sum_of_blue_area = sum_of_blue_area + Area_of_trapezoid[x] 
    }
  }
  
  sum_of_blue_area
  
  #area of green colour
  sum_of_green_area <- 0
  
  for (x in 1:length(ColouredTime))
  { 
    if (ColouredTime[x] > (colorcodesValues[2]) && ColouredTime[x]<= colorcodesValues[3])
    { 
      sum_of_green_area = sum_of_green_area + Area_of_trapezoid[x] 
    }
  }
  
  sum_of_green_area
  
  #area of lime colour
  sum_of_lime_area <- 0
  
  for (x in 1:length(ColouredTime))
  { 
    if (ColouredTime[x] > (colorcodesValues[3]) && ColouredTime[x]<= colorcodesValues[4])
    { 
      sum_of_lime_area = sum_of_lime_area + Area_of_trapezoid[x] 
    }
  }
  
  sum_of_lime_area
  
  #area of yellow colour
  sum_of_yellow_area <- 0
  
  for (x in 1:length(ColouredTime))
  { 
    if (ColouredTime[x] > (colorcodesValues[4]) && ColouredTime[x]<= colorcodesValues[5])
    { 
      sum_of_yellow_area = sum_of_yellow_area + Area_of_trapezoid[x] 
    }
  }
  
  sum_of_yellow_area
  
  #area of orange colour
  sum_of_orange_area <- 0
  
  for (x in 1:length(ColouredTime))
  { 
    if (ColouredTime[x] > (colorcodesValues[5]) && ColouredTime[x]<= colorcodesValues[6])
    { 
      sum_of_orange_area = sum_of_orange_area + Area_of_trapezoid[x] 
    }
  }
  
  
  
  #area of dark orange colour
  sum_of_dark_orange_area <- 0
  
  for (x in 1:length(ColouredTime))
  { 
    if (ColouredTime[x] > (colorcodesValues[6]) && ColouredTime[x]<= colorcodesValues[7])
    { 
      sum_of_dark_orange_area = sum_of_dark_orange_area + Area_of_trapezoid[x] 
    }
  }
  
  
  
  #area of red colour
  sum_of_red_area <- 0
  
  for (x in 1:length(ColouredTime))
  { 
    if (ColouredTime[x] > (colorcodesValues[7]) && ColouredTime[x]<= colorcodesValues[8])
    { 
      sum_of_red_area = sum_of_red_area + Area_of_trapezoid[x] 
      
    }
  }
  
  #area of dark red colour
  sum_of_dark_red_area <- 0
  
  for (x in 1:length(ColouredTime))
  { 
    if (ColouredTime[x] > (colorcodesValues[8]) && ColouredTime[x]<= (colorcodesValues[9]))
    { 
      sum_of_dark_red_area = sum_of_dark_red_area + Area_of_trapezoid[x] 
      
    }
  }
  
  
  Area_of_cool_colours = sum_of_purple_area + sum_of_blue_area + sum_of_green_area + sum_of_lime_area
  Area_of_warm_colours = sum_of_yellow_area + sum_of_orange_area + sum_of_dark_orange_area + sum_of_red_area + sum_of_dark_red_area
  
  
  total = Area_of_cool_colours + Area_of_warm_colours
  
  sum_of_purple_area
  sum_of_blue_area
  sum_of_green_area
  sum_of_lime_area
  sum_of_yellow_area
  sum_of_orange_area
  sum_of_dark_orange_area
  sum_of_red_area
  sum_of_dark_red_area
  
  purble <- (sum_of_purple_area/total) * 100
  blue <- (sum_of_blue_area/total) * 100
  green <- (sum_of_green_area/total) * 100
  lime <- (sum_of_lime_area/total) * 100
  yellow <- (sum_of_yellow_area/total) * 100
  ornage <- (sum_of_orange_area/total) * 100
  dark_orange <- (sum_of_dark_orange_area/total) * 100
  red<- (sum_of_red_area/total) * 100
  dark_red <- (sum_of_dark_red_area/total) * 100
  
  percentage_of_warm_colours <- (Area_of_warm_colours/total) * 100
  
  
  if (percentage_of_warm_colours >= 50)
  { outcome <- "Abnormal" }
  if (percentage_of_warm_colours < 50)
  { outcome <- "Normal" }
  
  Human_like_rule <- cbind(myfiles[f],sum_of_purple_area,sum_of_blue_area,sum_of_green_area,sum_of_lime_area,sum_of_yellow_area,sum_of_orange_area,sum_of_dark_orange_area,sum_of_red_area,sum_of_dark_red_area,percentage_of_warm_colours,outcome)
  write.table(Human_like_rule, "C:/Users/Alaa/Desktop/Study 4/Human_like_rule_abs_the_167_PlaceboCases.csv", sep = ",", col.names = !file.exists("myDF.csv"), append = T)
}