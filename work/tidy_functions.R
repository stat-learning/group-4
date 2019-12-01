#######################################################################
# This file contains functions for tidying training and test data
#######################################################################




#Maximum filter
note<-readMP3(file.path("~", "group-4", "Data", "UkuleleF4.mp3"))
points<-data.frame(note@left)%>%
  mutate(t=c(1:length(note)))
#Filter out negligible points
maxes<-points%>% 
  filter(note.left>50)
#Find Local Maxima
for (i in c(2:(length(note)-1))){
  if (points$note.left[i]<points$note.left[i-1] | points$note.left[i]<points$note.left[i+1]){
    maxes<-maxes%>%
      filter(maxes$t!=i)
  }
}
#Rescale data into unit square
note_shape<-maxes%>%
  #Rescale units of time to be from 0 to 1
  mutate(time=(1/(max(maxes$t)-min(maxes$t)))*(t-min(maxes$t)))%>%
  #Rescale amplitude to be from 0 to 1
  mutate(height=note.left/max(maxes$note.left))
ggplot(data=note_shape, mapping=aes(x=time, y=height))+
  geom_point()
#^Takes an audio file and finds all the locally maximum data points


#Choose Interval 
note<-readMP3(file.path("~", "group-4", "Data", "UkuleleF4.mp3"))
points<-data.frame(note@left)%>%
  mutate(t=c(1:length(note)))
#Find row with max volume
maximum_volume<-points%>%
  filter(note.left==max(points$note.left))
#make new data frame with 1000 observations after maximum
#1000 is changeable that's just what I thought was good. 
chosen_interval<-points%>%
  filter(t>maximum_volume$t)%>%
  filter(t<(maximum_volume$t+1000))
ggplot(data = chosen_interval, mapping = aes(x=t, y=note.left))+
  geom_point()
#^Takes an audio file and selects an interval of data that occurs after the loudest point in the file

#Function for finding local maxima (inputs wave object, outputs a data frame of local maxima points)
findMaxima <- function(note){
  sample_rate<-note@samp.rate
  points<-data.frame(note@left)%>%
    mutate(t=c(1:length(note)))
  attck<-points%>%
    filter(note.left>50)
  for (i in c(2:(length(points$note.left)-1))){
    if (points$note.left[i]<points$note.left[i-1] | points$note.left[i]<points$note.left[i+1]){
      attck<-attck%>%
        filter(attck$t!=i)
    }
  }
  
#Rescale data
  attck<-attck%>%
    mutate(height=(note.left/max(attck$note.left)))%>%
    mutate(time=(t-min(attck$t))/sample_rate)
  return(attck)
}



#Loss functions for Sine fit
Dloss_fun<- function(D, A, B, C, x, y){
  r<- y- (A+B* sin(C*x - D))
  sum(r^2)
}
loss_fun<- function(b, x, y){
  r<- y- (b[1]+b[2]* sin(b[3]*x - b[4]))
  sum(r^2)
}

#This function takes the list of 
#main frequencies and outputs the 
#median strength. This may or may 
#not be a useful predictor honestly
MedianFreq<- function(note){
  #input must be a wave object
  #output is an number showing 
  #the relative strength of the median
  p<-periodogram(note@left)
  frequencies<-data.frame(freq=p$freq)%>%
    mutate(spec=p$spec/max(periodogram(note@left)$spec))%>%
    mutate(freq=freq*note@samp.rate)
  mainFreq<-frequencies
  
  #Here we seek all local maxima
  for (i in c(2:(max(frequencies$freq)-1))){
    if (frequencies$spec[i]<frequencies$spec[i-1] | frequencies$spec[i]<frequencies$spec[i+1]){
      mainFreq<-mainFreq%>%
        filter(mainFreq$freq!=frequencies$freq[i])
    }
  }  
  
  #Filter out negligible values
  mainFreq<-mainFreq%>%
    filter(spec>0.01)
  
  #Filter out frequencies that are too close together
  a<-length(mainFreq$freq)
  for (i in c(2:length(mainFreq$freq))){
    if (((mainFreq$freq[i]-mainFreq$freq[i-1])^2) <= 20){
      weaker<-min(mainFreq$spec[i], mainFreq$spec[i-1])
      mainFreq<-mainFreq%>%
        filter(spec!=weaker)
      a<-a-1
    }
    
    if (a==i){break}
  }
  
  return(median(mainFreq$spec))
}

#This function returns the ratio between the 
#strengths of the first and second lowest main 
#Frequencies
FreqRatio<- function(note){
  #input must be a wave object
  #output is an number showing 
  #the relative strength of the median
  p<-periodogram(note@left)
  frequencies<-data.frame(freq=p$freq)%>%
    mutate(spec=p$spec/max(periodogram(note@left)$spec))%>%
    mutate(freq=freq*note@samp.rate)
  mainFreq<-frequencies
  
  #Here we seek all local maxima
  for (i in c(2:(max(frequencies$freq)-1))){
    if (frequencies$spec[i]<frequencies$spec[i-1] | frequencies$spec[i]<frequencies$spec[i+1]){
      mainFreq<-mainFreq%>%
        filter(mainFreq$freq!=frequencies$freq[i])
    }
  }  
  
  #Filter out negligible values
  mainFreq<-mainFreq%>%
    filter(spec>0.01)
  
  #Filter out frequencies that are too close together
  a<-length(mainFreq$freq)
  for (i in c(2:length(mainFreq$freq))){
    if (((mainFreq$freq[i]-mainFreq$freq[i-1])^2) <= 20){
      weaker<-min(mainFreq$spec[i], mainFreq$spec[i-1])
      mainFreq<-mainFreq%>%
        filter(spec!=weaker)
      a<-a-1
    }
    
    if (a==i){break}
  }
  
  if (length(mainFreq$freq)>1){
    return(mainFreq$spec[1]/mainFreq$spec[2])
  }
  if (length(mainFreq$freq)==1){return(0)}
}

#######################################
#######################################
#######################################
#######################################
#######################################
#Function for finding main frequencies
identifyFrequencies<- function(note){
  p<-periodogram(note@left)
  frequencies<-data.frame(freq=p$freq)%>%
    mutate(spec=p$spec/max(periodogram(note@left)$spec))%>%
    mutate(freq=freq*note@samp.rate)
  mainFreq<-frequencies
  
  #Here we seek all local maxima
  for (i in c(2:(max(frequencies$freq)-1))){
    if (frequencies$spec[i]<frequencies$spec[i-1] | frequencies$spec[i]<frequencies$spec[i+1]){
      mainFreq<-mainFreq%>%
        filter(mainFreq$freq!=frequencies$freq[i])
    }
  }  
  
  #Filter out negligible values
  mainFreq<-mainFreq%>%
    filter(spec>0.01)
  
  #Filter out frequencies that are too close together
  a<-length(mainFreq$freq)
  for (i in c(2:length(mainFreq$freq))){
    if (((mainFreq$freq[i]-mainFreq$freq[i-1])^2) <= 20){
      weaker<-min(mainFreq$spec[i], mainFreq$spec[i-1])
      mainFreq<-mainFreq%>%
        filter(spec!=weaker)
      a<-a-1
    }
    
    if (a==i){break}
  }
  
  return(mainFreq)
}
#Ok so I think what needs to be done is you 
#have a data frame (I'll call it BigDataFrame)
#with all the observations
#and then you say
#
#BigDataFrame<-BigDataFrame%>%
# mutate(NumberOfFreqs=0)%>%
# mutate(MedianFreq=0)%>%
# mutate(FreqRatio=0)
#
#and then make a for loop and say

#for (each observation):
#freq_data<-identifyFrequencies(observation)
#BigDataFrame$NumberOfFreqs[i]<-length(freq_data$spec)
#BigDataFrame$MedianFreq[i]<-median(freq_data$spec)
#BigDataFrame$FreqRatio[i]<-freq_data$freq[1]/freq_data$freq[2]
#######################################
#######################################
#######################################
#######################################

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

#This function selects 1000 data points out of a wave object
selectInterval<- function(note, plucked){
  #The input should be a wave object (note) 
  #and a TRUE/FALSE value that is TRUE if the 
  #note was plucked and FALSE otherwise
  #The output is a data frame
  sample_rate<-note@samp.rate
  points<-data.frame(note@left)%>%
    mutate(t=c(1:length(note)))
  maximum_volume<-points%>%
    filter(note.left==max(points$note.left))
  sample_rate<-note@samp.rate
  #Select the Interval
  if (plucked==TRUE){
    chosen_interval<-points%>%
      filter(t>maximum_volume$t)%>%
      filter(t<(maximum_volume$t+1000))
  }
  
  if (plucked==FALSE){
    chosen_interval<-points%>%
      filter(t<maximum_volume$t)%>%
      filter(t>(maximum_volume$t-1000))
  }
  
  #Rescale and transform interval
  chosen_interval<-chosen_interval%>%
    mutate(note.left=note.left-mean(chosen_interval$note.left))%>%
    mutate(t=t-min(chosen_interval$t))%>%
    mutate(t=t/sample_rate)
  max_volume<-max(chosen_interval$note.left)
  chosen_interval<-chosen_interval%>%
    mutate(note.left=note.left/max_volume)
  
  return(chosen_interval)
}

#This function calculates how many main frequencies are in the wave object
NumberOfFrequencies<- function(note){
  #input must be a wave object
  #output is an integer
  p<-periodogram(note@left)
  frequencies<-data.frame(freq=p$freq)%>%
    mutate(spec=p$spec/max(periodogram(note@left)$spec))%>%
    mutate(freq=freq*note@samp.rate)
  mainFreq<-frequencies
  
  #Here we seek all local maxima
  for (i in c(2:(max(frequencies$freq)-1))){
    if (frequencies$spec[i]<frequencies$spec[i-1] | frequencies$spec[i]<frequencies$spec[i+1]){
      mainFreq<-mainFreq%>%
        filter(mainFreq$freq!=frequencies$freq[i])
    }
  }  
  
  #Filter out negligible values
  mainFreq<-mainFreq%>%
    filter(spec>0.01)
  
  #Filter out frequencies that are too close together
  a<-length(mainFreq$freq)
  for (i in c(2:length(mainFreq$freq))){
    if (((mainFreq$freq[i]-mainFreq$freq[i-1])^2) <= 20){
      weaker<-min(mainFreq$spec[i], mainFreq$spec[i-1])
      mainFreq<-mainFreq%>%
        filter(spec!=weaker)
      a<-a-1
    }
    
    if (a==i){break}
  }
  
  return(length(mainFreq$freq))
}

###Calculating 1st, 2nd, 3rd order residuals from sine function
wave<-FluteA3@left
start_point<-40000

index<- seq(1, 1000)

Mean<-mean(wave[start_point:(start_point+1000)])
SS<-sum((wave[start_point:(start_point+1000)]-Mean)^2)

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

###Values to extract in final function:
RMSE1<- sqrt(mean(Resid1^2))

r_squared1<-1-(sum(Resid1^2)/SS)

RMSE2<-sqrt(mean(Resid2^2))

r_squared2<-1-(sum(Resid2^2)/SS)

RMSE3<-sqrt(mean(Resid3^2))

r_squared3<-1-(sum(Resid3^2)/SS)



#inputs chosen_interval and outputs matrix of coefficients and residuals
#(needs to be run after selectInterval)
findCoeff <- function(chosen_interval){
  #extract coefficient and r^2 from linear model
  m <- lm(log(note.left)~ 0 + t, data = chosen_interval)
  exp_coef <- m$coefficients
  r_squared <- sum(resid(m)^2)
  amp_coeff <- cbind(exp_coef, r_squared)
  return(amp_coeff)
}

#finds the distance between the max amplitude and the start
findDistance <- function(chosen_interval){
  x <- chosen_interval[ which(chosen_interval$note.left==max(chosen_interval$note.left)), ]
  origin <- chosen_interval[which(chosen_interval$t == min(chosen_interval$t)),]
  distance <- x$t - origin$t
  return(distance)
}




#how loud frequency is in an audio file
violin<-readMP3(file.path("~", "group-4", "Data", "ViolinD4.mp3"))
violin<-mono(object=violin, which="left")
p<-periodogram(violin@left)
frequencies<-data.frame(freq=p$freq)%>%
  mutate(spec=p$spec)%>%
  mutate(freq=freq*violin@samp.rate)
ggplot(data=frequencies, mapping=aes(x=freq, y=spec))+
  geom_point()

# Export data ---------------------------
