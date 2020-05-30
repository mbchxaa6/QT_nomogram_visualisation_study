library(pracma)
library(readr)
library(plyr)
library(tibble)
library(signal)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)



setwd("C:/Users/Alaa/Desktop/Study 4/")
mydir = "data"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)

#dat_csv = ldply(myfiles, read_csv)

for (f in 1:length(myfiles))
{ 
  
#read the file and change the column names 
ECG <- read_csv(myfiles[f], skip = 1)


colnames(ECG) <- c("time", "mv")


#plot the signal
plot(ECG$mv, type="l", col="navy") + grid()

#detect R-peaks
peaks1 <- findpeaks(ECG$mv, nups = 1, ndowns = 1, zero = "0", peakpat = NULL, minpeakheight = -Inf, minpeakdistance = 1, threshold = 0, npeaks = 0, sortstr = FALSE)

##include peaks that have two sustained repeated values  
peaks2 <- findpeaks(ECG$mv, minpeakheight=0.6, peakpat = "[+]{1,}[0]{1,}[-]{1,}")

#comibe all R peaks in one dataframe
peaks <- rbind(peaks1,peaks2)

#plot R peaks using red circels 
points(peaks[, 2], peaks[, 1], pch=20, col="maroon")


#remove unneeded columns 
Rpeaks<-peaks[,-3:-4]

Rpeaks <- Rpeaks[order(Rpeaks[, 2]), ] 

colnames(Rpeaks) <- c("R_mv","time")

ECG_peaks <- as.data.frame(Rpeaks) 
DF_Rpeaks <- as.data.frame(Rpeaks) 

#select R-peaks as any peak latger than 0.6 mv

DF_Rpeaks <- DF_Rpeaks[DF_Rpeaks$R_mv >= 0.6, ] 

# assign the index time of R-peaks in a vector 
R_peaks_time <- DF_Rpeaks[,2]

Diffference <- NA

for (i in 1:length(R_peaks_time))
{ 
  Diffference[i] <- R_peaks_time[i+1] - R_peaks_time[i]
  
  if (!is.null(Diffference[i] < 500) && length(Diffference[i] < 500) == 1 && !is.na(Diffference[i] < 500) &&  Diffference[i] < 500)
  { 
    value <- R_peaks_time[i]
    R_peaks_time <- R_peaks_time[ R_peaks_time != value]
    
  }
}


ECG_peaks_time <- ECG_peaks$time 
time <- ECG$time 
time <- time[-1]

mv <- ECG$mv 

#find the isoelectric line as the mv value of the R-peak time - the previous PR-interval(max value as 220ms)  

second_R_peak_time <- R_peaks_time[4] # take the fourth R-peak as a random/middle value
basline_time <- second_R_peak_time - 220

lift_the_line_value <- mv[basline_time] 
lift_the_line_value <- abs(lift_the_line_value) 

mv <- ECG$mv + lift_the_line_value


#Calculate the average RR-interval and the heart rate (HR)
# first calulcate the RR-intervals

RR_interval <- NA

for (i in 1:length(R_peaks_time)-1)
{
  RR_interval[i] = R_peaks_time[i+1] - R_peaks_time[i]
}

RR_interval <- RR_interval[RR_interval >= 500 & RR_interval <= 1600] 


#take the average RR-interval 
RR <- mean(RR_interval)
RR

#caluclate the heart rate 
HR <- 60000/RR


#QT nomogram
QTnomogram <- read_excel("E:/Study 3 (QT nomogram) stimuli/QTnomogram.xlsx")
HR_nomogram <- QTnomogram$HR
QT_nomogram <- QTnomogram$QT_pred


# find the nearst HR value in the QT nomogram 
index_HR <- which.min(abs(HR_nomogram-HR))
HR_value <- HR_nomogram[index_HR]
QT_value <- QT_nomogram[index_HR]

QT_value <- round(QT_value)
QT_value


#set the lower and upper value of the colour scale based on the QT nomogram
UpperColourLimit <- QT_value + (40*2) #two small squares above the nomogram line
LowerColourlimit <- QT_value - (40*6) #six small squares below the nomogram line 
maximumColourVector <- UpperColourLimit 


colourVector <- NA
length(colourVector) <- 10000 #this equals to the length of ECG signal index for 10-second recording

for (R in 1:length(R_peaks_time))
{ if (R_peaks_time[R] >= 100)
  index <- R_peaks_time[R] - 20 
else index <- R_peaks_time[R]

for (i in 1:maximumColourVector)
{ 
  colourVector[index +i]=i
}

}

newcolourVector <- colourVector[c(1:10000)]



#any time out of the UpperColourLimit (QT nomogram value + 80 ms) (i.e. NA) make it grey 

newcolourVector[is.na(newcolourVector)] <- 800

for (n in 1:length(newcolourVector))
{ 
  if(newcolourVector[n] < LowerColourlimit)
  { newcolourVector[n] <- LowerColourlimit}
  
  if(newcolourVector[n] > UpperColourLimit && newcolourVector[n]< 800)
  {newcolourVector[n] <- UpperColourLimit}
  
}


#start the signal from the first R-peak
first_R_peak <- R_peaks_time[1]
first_R_peak <- first_R_peak -1 
time_image <- time
time_image <- time_image[-1:-first_R_peak]
mv_image <- mv
mv_image <- mv_image[-1:-first_R_peak]



#end the singal at the last R-peak

last_R_peak_index <- length(R_peaks_time)
last_R_peak <- R_peaks_time[last_R_peak_index]
After_last_R_peak <- last_R_peak + 1
After_last_R_peak <- After_last_R_peak - first_R_peak
length <- length(time_image)
time_image <- time_image[-After_last_R_peak:-length]
mv_image <- mv_image[-After_last_R_peak:-length]


length <- length(time_image)


#create a new ECG signal file with the third colour dimension


name <- paste0("C:/Users/Alaa/Desktop/Study 4/data/raw ECG with colour/", myfiles[f])


ECG_with_colourScale <- cbind(time,mv,newcolourVector,UpperColourLimit,LowerColourlimit)

write.csv(ECG_with_colourScale,paste0(name, f,".csv"))

file2 <- read_csv(paste0(name, f,".csv"))

View(file2)

#next script is for visulising the ECG with the colour scale
colorcodesValues <- NA
length(colorcodesValues) <- 9
colorcodesValues[1] <- QT_value - (40*6) # purple. This also equals to the `LowerColourlimit' value
colorcodesValues[2] <- QT_value - (40*5) #blue
colorcodesValues[3] <- QT_value - (40*4) #lime
colorcodesValues[4] <- QT_value - (40*3) #green
colorcodesValues[5] <- QT_value - (40*2) #yellow
colorcodesValues[6] <- QT_value - (40*1) #orange
colorcodesValues[7] <- QT_value #dark orange. This is the QT nomogram value
colorcodesValues[8] <- QT_value + (40*1) #red
colorcodesValues[9] <- QT_value + (40*2) #dark red. This also equals to the `UpperColourLimit' value


#create the pesudo colour vector using spectral codes
myColor <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
myColor_scale_fill <- scale_fill_gradientn(colours = myColor,breaks=c(colorcodesValues[1]+1,colorcodesValues[2],colorcodesValues[3],colorcodesValues[4],colorcodesValues[5],colorcodesValues[6],colorcodesValues[7],colorcodesValues[8],colorcodesValues[9]-1),
                                           labels = c(as.character(unique(colorcodesValues[1])),colorcodesValues[2],colorcodesValues[3],colorcodesValues[4],colorcodesValues[5],colorcodesValues[6],colorcodesValues[7],colorcodesValues[8],as.character(unique(colorcodesValues[9]))),
                                           limits=c(colorcodesValues[1]+1,colorcodesValues[9]-1))


# plot the ECG with the colour scale
p <- ggplot(data=file2, aes(x=time, y=mv, fill=newcolourVector))
p  +  
  #plot vertical solid line for the R-peaks and dahsed line for the QT nomogram value#
  
  geom_vline(xintercept = R_peaks_time[1]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[2]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[3]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[4]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[5]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[6]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[7]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[8]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[9]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[10]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[11]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[12]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[13]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[14]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[15]/1000, size=0.7) +
  geom_vline(xintercept = R_peaks_time[16]/1000, size=0.7) +
  
  geom_vline(xintercept = R_peaks_time[1]/1000 + QT_value/1000 , linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[2]/1000 + QT_value/1000 , linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[3]/1000 + QT_value/1000 , linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[4]/1000 + QT_value/1000 , linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[5]/1000 + QT_value/1000, linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[6]/1000 + QT_value/1000, linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[7]/1000 + QT_value/1000, linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[8]/1000 + QT_value/1000, linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[9]/1000 + QT_value/1000, linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[10]/1000 + QT_value/1000, linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[11]/1000 + QT_value/1000, linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[12]/1000 + QT_value/1000, linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[13]/1000 + QT_value/1000, linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[14]/1000 + QT_value/1000, linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[15]/1000 + QT_value/1000, linetype="dashed", size=0.7) +
  geom_vline(xintercept = R_peaks_time[16]/1000 + QT_value/1000, linetype="dashed", size=0.7) +
  
  scale_y_continuous(lim = c(-0.5, +1)) +
  scale_x_continuous(lim = c(time_image[1] - 0.001, time_image[length]+ 0.001)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_blank(),axis.ticks = element_blank()) +
  

  #plot the coloured ECG + the ECG line graph (signal)
  geom_bar(stat="identity", position ="dodge") + geom_line(size=0.73) + myColor_scale_fill +
  
  theme(axis.text.x= element_blank()) +
  theme(axis.text.y= element_blank()) +
  
  #plot ECG baseline 
  geom_line(data=file2,aes(x=time, y=0), colour="#444444", lwd=0.5) + 
  
  labs(x ="", y="", fill = "") + theme(legend.text=element_blank(),legend.title=element_blank(),legend.position = "none")
  name <- paste0("C:/Users/Alaa/Desktop/Study 4/data/images/", myfiles[f])
  ggsave(paste0(name, f,".png"),  width=32.31, height=6.14)
}