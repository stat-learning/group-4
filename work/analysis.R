#######################################################################
# Analysis
#######################################################################

library(tuneR)

#Testing the sine function
FluteA3<-readMP3("FluteA3.mp3")%>%
  mono(which="both")
ViolinA3<-readMP3("ViolinA3.mp3")%>%
  mono(which="both")
UkuleleA4<-readMP3("UkuleleA4.mp3")%>%
  mono(which="both")
FluteC3<-readMP3("FluteC3.mp3")%>%
  mono(which="both")
GuitarG3<-readMP3("GuitarG3.mp3")%>%
  mono(which="both")
MandolinE5<-readMP3("MandolinE5.mp3")%>%
  mono(which="both")
Sine_Fit(FluteA3@left, 40000, 41000)
Sine_Fit(UkuleleA4@left, 18000, 19000)
Sine_Fit(ViolinA3@left, 30000,31000)
Sine_Fit(FluteC3@left, 21000, 22000)
Sine_Fit(GuitarG3@left, 21000, 22000)
Sine_Fit(MandolinE5@left, 21000, 22000)