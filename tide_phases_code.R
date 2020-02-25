#install.packages('data.table')
library(data.table)

#You will need to load a tidal prediction
load("tide.RData")# object 'Bubaque_2016_2020'

#EBB and VLOED
#Here we have a little issue, which you might find interesting to solve.
#My criterion for defining tide phase (ebb vs flow) is whether tide level at the next time point is higher or lower
#However, there are periods around max and min tides, where the oscillation curve plateaus
# We have to assign these 'extremum' points to either of the tide phases
# Keep into account that tide levels can repeat between neighbouring points also outside the maximal or minimal tide levels
# which generates "extremums " also outside the min/max plateaus

tide$tide_next_point<-shift(tide$z.m., n=1, fill=NA, type="lead")
bird$tide_next_point<-approx(x=tide$Time, y=tide$tide_next_point, xout=bird$timestamp)$y
head(bird)
bird$tide_phase<-NA
bird$tide_phase[bird$tide_next_point-bird$tide>0]<-"vloed"
bird$tide_phase[bird$tide_next_point-bird$tide<0]<-"eb"
bird$tide_phase[bird$tide_next_point-bird$tide==0]<-"extremum"