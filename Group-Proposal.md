---
 output:
   html_document: default
---
#Group Proposal

##Group Members: 
Nolan Anderson Tim Shinners Sarah Maebius

##Topic 1: 
Can we create a program that creates pretty abstract images? We would acquire a set of images (we would probably have multiple small folders) that are “good” and maybe images that are “bad” or “boring” and use that as training data. Ideally our program would identify patterns that correspond to an image being good and learn to avoid patterns that are ugly then produce its own abstract image that would be neat to look at. Models used could include trees to determine a color scheme, and k-nearest neighbors or QDA to try and recreate patterns observed.

##Topic 2: 
Can we predict the next chord of a Beatles song? The goal of this project would be to predict the number (scale degree relative to the key) of the next chord in a Beatles song based on the previous chords. The music theory of “functional harmony” suggests that each chord has a function based on the scale degree which suggests which chords may follow it (for example, a dominant V chord is thought to suggest a resolution to a I chord). If this is the case, we may be able to generate a model which can somewhat accurately classify which chord will be used based on preceding chords. We would code each chord as one of a relatively small number of commonly used chords (I, IImin, IIImin, IV, V, VImin, bVII, IVmin, or II7), and possibly exclude songs which use harmony that is more unconventional or complex to analyze. A good dataset for this project can be found from isophonics at: http://isophonics.net/content/reference-annotations-beatles. The methods used for this could be a number of classification models such as Logistic, LDA, QDA, or regression trees.

##Topic 3: 
Can we predict the instrument being played from a sound recording? Classification, prediction. This model will be trained using data gathered from the shinysense app by recording and listing the names of various instruments. It should then be able to predict an instrument playing from a sound recording.