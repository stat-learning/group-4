#######################################################################
# This file contains final code for generating data from recordings
#######################################################################


#######################################################################
# Functions to be defined beforehand
#######################################################################


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
    filter(note.left>100)
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
  x<-which(wave[,1]==max(wave[,1]))
  origin<- min(which(wave[,1]>500))
  distance<-x-origin
  distance
}

findCoeff <- function(TimeData){
  StartPoint<- min(which(TimeData$note.left>500))
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
    Neighborhood<-filter(mainFreq, freq>mainFreq$freq[i]-30 &
                           freq<mainFreq$freq[i]+30
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

#Sine fit function
#inputs a time-series vector of displacement values and a start point
#outputs a vector of four parameters of sine fit
Sine_Fit<-function(Wave, StartPoint){
  #generating periodogram
  library(TSA)
  p<-periodogram(Wave, plot=FALSE)
  #exracting fundamental frequency estimate as max value from p
  FundamentalEst<-(p$freq[which.max(p$spec)])*6.25
  #creating data frame within sample min and max
  WaveData<-data.frame(Displacement=Wave[StartPoint:(StartPoint+1000)],
                       index=seq(1, length(Wave[StartPoint:(StartPoint+1000)])))
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
  Attack<-findDistance(Wave1)
  
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
  wave<-Wave1[,1]
  
  start_point<- min(which(wave>500))+5000
  
  index<- seq(1, 1000)
  
  Mean<-mean(wave[start_point:(start_point+5000)])
  SS<-sum((wave[start_point:(start_point+5000)]-Mean)^2)
  
  Betas<- Sine_Fit(wave, StartPoint=start_point)
  
  
  
  Resid1<- wave[start_point:(start_point+1000)]-(Betas[1]+
                                                   Betas[2]* sin(Betas[3]*index - Betas[4]))
  
  Mean1<-mean(Resid1)
  SS1<-sum((Resid1-Mean1)^2)
  
  Betas1<-Sine_Fit(Resid1, StartPoint=1) 
  
  Resid2<- Resid1-(Betas1[1]+
                     Betas1[2]* sin(Betas1[3]*index - Betas1[4]))
  
  Mean2<-mean(Resid2)
  SS2<-sum((Resid2-Mean2)^2)
  
  Betas2<-Sine_Fit(Resid2, StartPoint=1) 
  
  Resid3<- Resid2-(Betas2[1]+
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
           r_squared3
           ))
}


#######################################################################
# Generating Data Set:
#######################################################################

setwd("~/group-4/Data")
path = "~/group-4/Data"
out.file<-""
file.names <- dir(path)

FinalData <- matrix(nrow = length(file.names), ncol = 16)
for(i in 1:length(file.names)){
  FinalData[i,1]<-file.names[i]
  wave.object<-readMP3(file.names[i])%>%
    mono(which="both")
  FinalData[i,2:16]<-Feature_Extract(wave.object)
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
                           "r_squared3"
)

#Generating variable for which instrument (response variable)


Instruments<-Instruments%>%
  mutate(instrument=case_when(
    substr(FileName, 1, 4)=="Flut"~"Flute",
    substr(FileName, 1, 4)=="Guit"~"Guitar",
    substr(FileName, 1, 4)=="Mand"~"Mandolin",
    substr(FileName, 1, 4)=="Pian"~"Piano",
    substr(FileName, 1, 4)=="Ukul"~"Ukulele",
    substr(FileName, 1, 4)=="Viol"~"Violin"
  ))


View(Instruments)


