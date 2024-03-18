# ok, on 14 December decided to abandon the cumsum idea and come back to gam AND ALSO 
# to add previous extremum value to solve the edge jumping problem
# ver 0.1 Dec 14 2023
# ver 0.3 8 Jan 2024 added exclude.outside option, so now one can decide what is relevant for the exclusion.

# ver 0.2 Dec 22 2023 decided to also check what is the proportion of wet in the wet pat and dry in the dry part and if it is still below 10%, then decide that this is not significant. 
# use example:
# cyclic_gam_wrapper(brk[100][1,], image_time_pred$meters_to_max, mode=c('Ebb'), max_tide = max_tide, max_range = max_range, plot=T, water.is=1)


cyclic_gam_wrapper<-function(binary, meters, threshold = NA, mode=c('Ebb'), plot=FALSE, max_tide=NA, max_range=NA, water.is=0, k=NA, exclude.outside=c(0.1,0.9)){#add Flood?{
   # y should be a vector with the tidelevels
   # x is the the binary vector
   if(is.na(max_tide)) stop('max_tide value should be provided!')
   if(is.na(max_range)) stop('max_tide value should be provided!')
  water.is<-as.logical(water.is)
  if (0.5 < min(binary)) { return(99) } else {
  if (0.5 > max(binary)) { return(-99) } else {
   Dat<-data.frame(binary=binary, meters_to_max=meters)
   Order<-order(Dat$meters_to_max)
   X<-Dat$meters_to_max[Order]
   Y<-Dat$binary[Order]

   # . run gam over the data
   if (is.na(k)) {
     M1<-try(gam(Y~s(X, bs='cp'), family='binomial'))
   } else {
     M1<-try(gam(Y~s(X, bs='cc', k=k), family='binomial'))
   }
   
   # 3. check if the gam was significant,then get maximum and minimum and these will be the transition points..

   XX<-seq(min(X), max(X), length.out=500)
   YY<-predict(M1, newdata=data.frame(X=XX), type='response')
   #Crossings<-which((YY[-length(YY)]*YY[-1])<0)
   if (is.na(threshold)) {
       threshold=max(YY)-c(max(YY)-min(YY))/2
    }
    Crossings<-which(((YY-threshold)[-length(YY)]*(YY-threshold)[-1])<0)
    Signs<-sign(YY[Crossings+1]-YY[Crossings])
   # now we need to get sign of these crossing
    if (plot) {
      plot(M1)
      plot(Y~X)
      lines(I(as.vector(predict(M1, newdata=data.frame(X=XX), type='response')))~XX, col='red')
      abline(h=threshold)
     }
   if (length(Crossings)<2) {
      if (threshold < mean(Dat$binary) ) { return(99) 
      } else {
      return(-99)}      
   } else { 
      if (threshold >exclude.outside[2]) {return(99) 
      } else {if (threshold <exclude.outside[1]) {return(-99) } else { 
       
      Roots<-XX[Crossings]
      #Signs<-sign(Roots)
      Flow<-Roots[which(Signs==-1)]
      Flow<-Flow[which.min(abs(Flow))]
      #Flow<-Roots[which(Signs==-1)][1]
      #if (Flow>max_range) Flow<- Flow-2*max_range
      Ebb<-Roots[which(Signs==1)]
      Ebb<-Ebb[which.min(abs(Ebb))]
      #Ebb<-Roots[which(Signs==1)][1]
      #if (Ebb>max_range) Ebb<- Ebb-2*max_range
      # now, we want to check the changes, that are closer to the 0 or central value..
      
 if (plot) {
   plot(M1)
   plot(Y~X)
   lines(I(as.vector(predict(M1, newdata=data.frame(X=XX), type='response')))~XX, col='red')
   abline(h=threshold)
   abline(v=XX[Crossings])
   abline(v=c(Ebb), col='red')
   abline(v=c(Flow), col='blue')

  }
      if (mode[1]=='Flow' & !water.is) return(Flow)
      if (mode[1]=='Flow' & water.is) return(Ebb)
      if (mode[1]=='Ebb' & !water.is) return(Ebb)
      if (mode[1]=='Ebb' & water.is) return(Flow)
      }
   }
  }
  }
  }
 }
