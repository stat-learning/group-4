#######################################################################
# This file contains final code for generating data from recordings
#######################################################################
library(TSA)
library(dplyr)
#######################################################################
# Functions to be defined beforehand
#######################################################################

library(TSA)
library(dplyr)

#####Functions to generate data
#######################################################################
#######################################################################
DataFrame<- function(WaveObject){
  WaveData<- data.frame(WaveObject@left)%>%
    mutate(index= seq(1, length(WaveObject@left)))
  WaveData
}


findMaxima <- function(note){
  sample_rate<-note@samp.rate
  attck<-data.frame(note@left)%>%
    mutate(t=c(1:length(note@left)),
           Reject=FALSE)%>%
    filter(note.left>(.02*max(note.left)))
  for (i in c(2:(length(attck$note.left)-1))){
    if (attck$note.left[i]<attck$note.left[i-1] |
        attck$note.left[i]<attck$note.left[i+1]){
      attck$Reject[i]<-TRUE
    }
  }
  #Rescale data
  attck<-attck%>%
    filter(Reject==FALSE)%>%
    mutate(height=(note.left/max(attck$note.left)))%>%
    mutate(time=(t-min(attck$t))/sample_rate)
  return(attck)
}


#####Functions for attack and decay
#######################################################################
#######################################################################

findDistance<- function(wave){
  Wave<-wave[,1]/max(wave[,1])
  x<-which(Wave==max(Wave))
  origin<- min(which(Wave>.05))
  distance<-x-origin
  distance
}

findCoeff <- function(TimeData){
  TimeData$note.left<-TimeData$note.left/max(TimeData$note.left)
  StartPoint<- min(which(TimeData$note.left>.05))
  Data<-TimeData[StartPoint:nrow(TimeData),]
  #extract coefficient and r^2 from linear model
  m <- lm(log(note.left)~ time, data = Data)
  exp_coef <- m$coefficients[2]
  r_squared <- summary(m)$r.squared
  t_statistic<- summary(m)$coef[2, "t value"]
  amp_coeff <- cbind(exp_coef, r_squared, t_statistic)
  return(amp_coeff)
}


#####Functions for Periodogram Features
#######################################################################
#######################################################################

identifyFrequencies<- function(note){
  p<-periodogram(note@left)
  mainFreq<-data.frame(freq=p$freq)%>%
    mutate(spec=p$spec/max(p$spec),
           freq=freq*note@samp.rate,
           Reject=FALSE)
  #Here we seek all local maxima
  for (i in c(2:(max(mainFreq$freq)-1))){
    if (mainFreq$spec[i]<mainFreq$spec[i-1] |
        mainFreq$spec[i]<mainFreq$spec[i+1]){
      mainFreq$Reject[i]<-TRUE
    }
  }
  mainFreq<-mainFreq%>%
    filter(Reject==FALSE,
           spec>0.01)
  #Selecting only frequencies that are max in their 'neighborhood'
  a<-length(mainFreq$freq)
  for (i in c(1:a)){
    Neighborhood<-filter(mainFreq, freq>mainFreq$freq[i]*.9 &
                           freq<mainFreq$freq[i]*1.10
    )
    if(mainFreq$spec[i]!=max(Neighborhood$spec)){mainFreq$Reject[i]<-TRUE}
  }
  mainFreq<-mainFreq%>%
    filter(Reject==FALSE)%>%
    select(freq, spec)
  
  return(mainFreq)
}



#####Functions for Sine fit and residuals
#######################################################################
#######################################################################

#Loss functions for Sine fit
Dloss_fun<- function(D, A, B, C, x, y){
  r<- y- (A+B* sin(C*x - D))
  sum(r^2)
}
loss_fun<- function(b, x, y){
  r<- y- (b[1]+b[2]* sin(b[3]*x - b[4]))
  sum(r^2)
}

#Function to generate residuals 
Resid_fun<-function(Data, Betas){
  Resid1<- Data-(Betas[1]+ 
                   Betas[2]* sin(Betas[3]*index -Betas[4]))
  Resid1
}


#Sine fit function
#inputs a time-series vector of displacement values and a start point
#outputs a vector of four parameters of sine fit
Sine_Fit<-function(Wave){
  #generating periodogram
  p<-periodogram(Wave, plot=FALSE)
  #exracting fundamental frequency estimate as max value from p
  FundamentalEst<-(p$freq[which.max(p$spec)])*6.25
  #creating data frame within sample min and max
  WaveData<-data.frame(Displacement=Wave,
                       index=seq(1, length(Wave)))
  #estimating amplitude
  AmplitudeEst<-.5*(max(WaveData$Displacement)-min(WaveData$Displacement))
  #estimating midline
  MidlineEst<- mean(c(max(WaveData$Displacement), min(WaveData$Displacement)))
  #optimizing phase shift holding all else constant at esimates
  PhaseShiftOptim<- optim(par= 0, fn=Dloss_fun, 
                          method="Brent",
                          lower=0,
                          upper=1000,
                          A=MidlineEst,
                          B=AmplitudeEst,
                          C=FundamentalEst,
                          x=WaveData$index, 
                          y=WaveData$Displacement)$par
  #optimizing all parameters together
  b_optim<- optim(par= c(MidlineEst, 
                         AmplitudeEst, 
                         FundamentalEst, 
                         PhaseShiftOptim),
                  fn=loss_fun, 
                  x=WaveData$index, 
                  y=WaveData$Displacement)$par
  #Outputing optimal betas
  b_optim
}



#######################################################################
# Final feature extraction function:
#######################################################################
###function extracting every feature from a Wave Object, three pound signs
### indicates a feature to be extracted at end
Feature_Extract<- function(WaveObject){
  Wave1<-DataFrame(WaveObject)
  WaveAmpMax<-findMaxima(WaveObject)
  RelativeFreq<-identifyFrequencies(WaveObject)$spec
  ###
  Attack<-findDistance(Wave1)/(WaveObject@samp.rate)
  
  DecayIndicators<-findCoeff(WaveAmpMax)
  ###
  DecayCoeff<-DecayIndicators[,1]
  ###
  DecayDetermination<-DecayIndicators[,2]
  ###
  DecaySignificance<-DecayIndicators[,3]
  ###
  NumberFrequencies<-length(RelativeFreq)
  ###
  MedianFreqStrength<- median(RelativeFreq)
  ###
  FundamentalStrength<- RelativeFreq[1]
  ###
  FundamentalStrengthRatio<- RelativeFreq[1]/sum(RelativeFreq)
  ###
  FirstStrengthRatio<-RelativeFreq[2]/RelativeFreq[1]
  
  ###Sine fit indicators
  waveRAW<-WaveObject@left
  waveSCALE<-waveRAW/max(waveRAW)
  Note_start<- min(which(waveSCALE>.05))
  R2Values<- rep(NA, 20)
  for (i in 1:20){
    Sine_Start<-Note_start+(i*(1000))
    wave<-waveSCALE[Sine_Start:(Sine_Start+1000)]
    Mean<-mean(wave)
    SS<-sum((wave-Mean)^2)
    Betas<- Sine_Fit(wave)
    Resid1<- Resid_fun(wave, Betas)
    R2Values[i]<-1-(sum(Resid1^2)/SS)
  }
  
  start_point<-Note_start+(which.max(R2Values)*1000)
  
  wave<-waveSCALE[start_point:(start_point+1000)]
  
  Mean<-mean(wave)
  SS<-sum((wave-Mean)^2)
  
  Betas<- Sine_Fit(wave)
  
  Resid1<- Resid_fun(wave, Betas)
  
  Mean1<-mean(Resid1)
  SS1<-sum((Resid1-Mean1)^2)
  Betas1<-Sine_Fit(Resid1) 
  
  Resid2<- Resid_fun(Resid1, Betas1)
  
  Mean2<-mean(Resid2)
  SS2<-sum((Resid2-Mean2)^2)
  Betas2<-Sine_Fit(Resid2) 
  
  Resid3<- Resid_fun(Resid2, Betas2)
  
  #boosted residuals
  index=seq(1, 1001)
  
  BoostedResid<-wave-(Betas[1]+
                        Betas[2]* sin(Betas[3]*index - Betas[4])+
                        Betas1[1]+
                        Betas1[2]* sin(Betas1[3]*index - Betas1[4])+
                        Betas2[1]+
                        Betas2[2]* sin(Betas2[3]*index - Betas2[4]))
  
  ###
  RMSE1<- sqrt(mean(Resid1^2))
  ###
  r_squared1<-1-(sum(Resid1^2)/SS)
  ###
  RMSE2<-sqrt(mean(Resid2^2))
  ###
  r_squared2<-1-(sum(Resid2^2)/SS1)
  ###
  RMSE3<-sqrt(mean(Resid3^2))
  ###
  r_squared3<-1-(sum(Resid3^2)/SS2)
  ###
  RMSEBoosted<-sqrt(mean(BoostedResid^2))
  ###
  r_squaredBoosted<-1-(sum(BoostedResid^2)/SS)
  
  return(c(Attack[1], 
           DecayCoeff, 
           DecayDetermination, 
           DecaySignificance, 
           NumberFrequencies,
           MedianFreqStrength,
           FundamentalStrength,
           FundamentalStrengthRatio,
           FirstStrengthRatio,
           RMSE1,
           r_squared1,
           RMSE2,
           r_squared2,
           RMSE3,
           r_squared3,
           RMSEBoosted,
           r_squaredBoosted
           ))
}


#######################################################################
# Generating Data Set:
#######################################################################

setwd("~/group-4/Data/Web Data/")
path = "~/group-4/Data/Web Data/"
out.file<-""
file.names <- dir(path)

FinalData <- matrix(nrow = length(file.names), ncol = 18)
for(i in 4:7){
  FinalData[i,1]<-file.names[i]
  wave.object<-readMP3(file.names[i])%>%
    mono(which="both")
  FinalData[i,2:18]<-Feature_Extract(wave.object)
  print(i)
}

Instruments<-data.frame(FinalData)
colnames(Instruments) <- c("FileName",
                           "Attack", 
                           "DecayCoeff", 
                           "DecayDetermination", 
                           "DecaySignificance", 
                           "NumberFrequencies",
                           "MedianFreqStrength",
                           "FundamentalStrength",
                           "FundamentalStrengthRatio",
                           "FirstStrengthRatio",
                           "RMSE1",
                           "r_squared1",
                           "RMSE2",
                           "r_squared2",
                           "RMSE3",
                           "r_squared3",
                           "RMSEBoosted",
                           "r_squaredBoosted"
)

#Generating variable for which instrument (response variable)


Instruments<-Instruments%>%
  mutate(instrument=case_when(
    substr(FileName, 1, 4)=="flut"~"Flute",
    substr(FileName, 1, 4)=="guit"~"Guitar",
    substr(FileName, 1, 4)=="mand"~"Mandolin",
    substr(FileName, 1, 4)=="pian"~"Piano",
    substr(FileName, 1, 4)=="ukul"~"Ukulele",
    substr(FileName, 1, 4)=="viol"~"Violin"
  ))
Instruments$instrument<-as.factor(Instruments$instrument)

levels(Instruments$instrument)<-c("Flute", "Guitar", "Mandolin", "Piano", "Ukulele", "Violin")

for(i in 2:18){
  Instruments[,i]<-as.numeric(as.character(Instruments[,i]))
}
View(Instruments)


