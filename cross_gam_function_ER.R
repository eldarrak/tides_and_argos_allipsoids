cross_gam_wrapper<-function(ndwi, meters, threshold = 0.16, mode=c('Ebb')){#add Flow?{
  # y should be a vector with the tidelevels
# x is the the ndwi vector
length.out=1000
if (threshold < min(ndwi)) { return(99) } else {
if (threshold > max(ndwi)) { return(-99) } else {

Dat<-data.frame(NDWI=ndwi, meters_to_max=meters)
Fun<-suppressWarnings(approxfun(Dat$meters_to_max, Dat$NDWI-threshold, rule=2))
# XX<-data.frame(meters_to_max=seq(min(meters), max(meters), length.out=length.out))	
Roots<-uniroot.all(Fun, interval = range(Dat$meters_to_max), n=600)
Sign<-sign( (Fun(Roots+1e-10))-Fun(Roots))

Up_crossings<-Roots[Sign>0]
Down_crossings<-Roots[Sign<0]

#####
# now, if length of crossings > 1 need to go for gam..
if (length(Up_crossings)>1 | length(Down_crossings)>1) {
    M1<-try(gam(NDWI~s(meters_to_max, bs='cc', k=4), data=Dat))
    Phase<-	sign(
	     predict(M1, newdata=data.frame(meters_to_max=Roots+1e-10)) -
	     predict(M1, newdata=data.frame(meters_to_max=Roots)))
    Up_crossings<-Roots[Sign>0 & Phase>0]
    Down_crossings<-Roots[Sign<0 & Phase<0]
  
}

if (length(Up_crossings)==0 | length(Down_crossings)==0) {
    Res<-ifelse(median(ndwi)<threshold, -99, 99)
	return(Res)
	} else {
#######

if (length(unique(sign(Up_crossings)))>1) {
Up_crossings[Up_crossings>0]<-Up_crossings[Up_crossings>0]-diff(range(Dat$meters_to_max))
} 

if (length(unique(sign(Down_crossings)))>1) {
Down_crossings[Down_crossings<0]<-Down_crossings[Down_crossings<0]+diff(range(Dat$meters_to_max))
} 

Flow <- median(Up_crossings)
Ebb <- median(Down_crossings)

if (mode[1]=='Flow') return(Flow)
if (mode[1]=='Ebb') {

if (Ebb<(-2)) {
Ebb<-Ebb+diff(range(Dat$meters_to_max))
}
return(Ebb)
}
}
}
}
}

