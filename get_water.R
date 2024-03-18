# this function estimates whether poitn should be covered by water now (without accounting fro the previous tide levels)
get_water<-function(x, cur_level) {
  if (any(is.na(x))) {return(NA)} else {
    #F<-x[1]
    #E<-x[2]
    if (x[2]>x[1]) {Res<-ifelse(cur_level>=x[1] & cur_level<=x[2], 1, 0)
    } else {Res<-ifelse(cur_level>=x[2] & cur_level <= x[1] , 0, 1)}
    
    return(Res)
  }
}


f_get_water<-compiler::cmpfun(get_water)
