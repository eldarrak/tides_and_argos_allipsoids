# ver. 0.1 13 October 2023
# ver 0.2 20 Dec 2023 added tracking of the levels during the previous maximums.
# ver 0.3 11 Jan 2024 the idea now is to loo into the 48 hours in the past, to find the closest maximum at which the point was flooded.
# and from this point to move forward..
# ver 0.4  March 15 2024 - changed direction for -1, so if direction is -1, then the function returns two values - time since the last submergence and current state
# if the direction == 1 then function returns three values time since, state and time to.

# so the function requires more debugging, the thing to check whether the max that we take at the extremum is positive or negative and then look at the corrections,
# but very likely we are in trouble somwhere earlier.

emergence_fun<-function(fe_local, time_to_predict, tide, direction=c(-1, 0, 1), verbose=FALSE, plot=F) {
    if (direction %in% c(-1, 0, 1)) stop('directions must be -1, 0 or 1')
   #direction = -1 returns time before when state changes
   # direction 0 returns current state (1 - water, 0 - dry)
   # direction 1 returns time when state will change
   if (any(is.na(fe_local))) { 
      if (direction==-1) {Result<-NA } 
         else if (direction==0) { Result<-c(NA, NA)} 
           else {Result=c(NA, NA, NA)}
   } else {
               
   # check the column names: 
   if (!'Time' %in% names(tide)) stop('column Time should be present in the tide data.frame')
   if (!'meters_to_max' %in% names(tide)) stop('column metere_to_max should be present in the tide data.frame')

   tide<- tide |> arrange(Time)
   #  0 - cut the last 48 hours.
   tide_before <- tide |> filter(Time>(time_to_predict - 172800) & Time<= time_to_predict) 
   
  if (plot) tide_before_f<-tide_before
   
   #  1 - extract vector of maximums..
   prev_max_levels_tab<- tide_before |> filter( tide_phase=='extremum' &  tide_before_point > tide_next_point) 
   
   last_extremum<-tide_before |> filter( tide_phase=='extremum')|> last() 
   
   # 2 - find the latest one at which the point was wet
   prev_max_wet_state<-which(sapply(-1*prev_max_levels_tab$meters_to_max, FUN=function(x) f_get_water(fe_local, x)) ==1) |> last()
   # -1 is needed to make jump from negative to positive.
   
  
   if (is.na(prev_max_wet_state)) {
      # the point was never wet during the maximums, so check from the last minimum.
      prev_min_level<-tide_before |> filter( tide_phase=='extremum' &  tide_before_point < tide_next_point) |> last() 
      
      tide_before <- tide_before |> filter(Time>=prev_min_level$Time)
 
   } else {
      prev_min_levels_tab<- tide_before |> filter( tide_phase=='extremum' &  tide_before_point < tide_next_point) |> arrange(Time)

      prev_min_dry_state<-which(sapply(prev_min_levels_tab$meters_to_max, FUN=function(x) f_get_water(fe_local, x)) ==0) |> last()

      if (is.na(prev_min_dry_state) | prev_min_levels_tab$Time[prev_min_dry_state] < prev_max_levels_tab$Time[prev_max_wet_state]) { # the point was never dry during the last minima, so we have to start from the last extremum
      tide_before <- tide_before |> filter(Time>=last_extremum$Time)
       } else { # if the point got dry during some minimum 
      # from this point run loop forward to find the point of change.
      tide_before <- tide_before |> filter(Time>=prev_min_levels_tab$Time[prev_min_dry_state])
      }
      if (nrow(tide_before) ==0) stop('tide_before is NA!')
   }
   # NOW we have to check if the point was ever dry after and if so we have to start from one maximum before this point, if not, then it was basically wet, so we do not need ot run the loop..
   
   Res<-data.frame(Time=as.POSIXct(0), iter_level=numeric(0), numeric(0))
   
   for (i in 2:nrow(tide_before)) {
   # ok, now we do the main loop
      cur_level_iter<-tide_before$meters_to_max[i]
      cur_tide_phase_iter<-tide_before$tide_phase[i]
      if (verbose) {
          cat('cur i', i,  'from', nrow(tide_before), '\n')
          cat('cur_level_iter', cur_level_iter, '\n')
          cat('cur_tide_phase_iter', cur_tide_phase_iter, '\n')
      } 
       
      prev_extr_level_iter<-tide_before |> filter(tide_phase=='extremum' & Time < tide_before$Time[i]) |> last() |> select(meters_to_max)
       if (verbose) cat('prev_extr_level_iter', as.numeric(prev_extr_level_iter)[1], '\n')

      if (tolower(cur_tide_phase_iter) %in% c('ebb', 'eb')) prev_extr_level_iter<--1*prev_extr_level_iter
      
      prev_extr_state_iter<-f_get_water(fe_local, prev_extr_level_iter)
      
      if (verbose) cat('prev_extr_state_iter', as.numeric(prev_extr_state_iter), '\n')
      
   if (tolower(cur_tide_phase_iter) %in% c('ebb', 'eb') & prev_extr_state_iter == 0) { iter_state<-0 ;
   } else {
     if (tolower(cur_tide_phase_iter) %in% c('flow', 'vloed') & prev_extr_state_iter == 1) {iter_state<-1 ;
       } else {
          iter_state<-f_get_water(fe_local, cur_level_iter)
       }
    }
       if (verbose) {
          cat('tide_before$Time[i]', as.character(tide_before$Time[i]), '\n')
          cat('current iter level', tide_before$meters_to_max[i], '\n')
          cat('current iter state', iter_state, '\n')
       }
   # now save the results
   res_cur<-data.frame(tide_before$Time[i], tide_before$meters_to_max[i], iter_state)
   
   Res<-rbind(Res, res_cur)

   }

   names(Res)<-c('Time', 'iter_level' , 'iter_state')
   
   # ok, now, when we have res we can return
   
   cur_state<- Res$iter_state |> last()
   
   # when the state changed
   Time_last_change<-Res |> filter(iter_state != cur_state) |> last() |> select('Time')
   if (is.na(Time_last_change[1,])) Time_last_change[1,]<- Res$Time[1]
   if (direction == 0) {
   Result<-cur_state
   }
   if (direction == -1) {
   
   # now i need to get the difference
   Result<-c(abs(as.numeric(difftime(time_to_predict,  Time_last_change[1,], units='mins'))), cur_state)
   }
   if (direction == 1) {
   # ok, if we want to find point in the future we should move forward for some time.
   tide_future <- tide |> filter(Time>time_to_predict & Time < (time_to_predict + 86400))
   
   Res2<-data.frame(Time=POSIXct(0), iter_level=numeric(0), numeric(0))
   
   for (i in 1:nrow(tide_future)) {
   # ok, now we do the main loop
      cur_level_iter<-tide_future$meters_to_max[i]
      cur_tide_phase_iter<-tide_future$tide_phase[i]
      if (verbose) {
          cat('cur i future', i,  'from', nrow(tide_future), '\n')
          cat('cur_level_iter future', cur_level_iter, '\n')
          cat('cur_tide_phase_iter future', cur_tide_phase_iter, '\n')
      } 
      # only check previous extremums if they are later then the current start.
      #time_last_extremum <- tide |> filter(tide_phase=='extremum' & Time < tide_future$Time[i]) |> last() |> select(Time)
      prev_extr_level_iter<-tide |> filter(tide_phase=='extremum' & Time < tide_future$Time[i]) |> last() |> select(meters_to_max)
      
      if (verbose) cat('prev_extr_level_iter', as.numeric(prev_extr_level_iter)[1], '\n')

      if (tolower(cur_tide_phase_iter) %in% c('ebb', 'eb')) prev_extr_level_iter<--1*prev_extr_level_iter
      
      prev_extr_state_iter<-f_get_water(fe_local, prev_extr_level_iter)
      
      if (verbose) cat('prev_extr_state_iter', as.numeric(prev_extr_state_iter), '\n')
      
   if (tolower(cur_tide_phase_iter) %in% c('ebb', 'eb') & prev_extr_state_iter == 0) { iter_state<-0 ;
   } else {
     if (tolower(cur_tide_phase_iter) %in% c('flow', 'vloed') & prev_extr_state_iter == 1) {iter_state<-1 ;
       } else {
          iter_state<-f_get_water(fe_local, tide_future$meters_to_max[i])
       }
    }
       if (verbose) {
          cat('tide_future$Time[i]', as.character(tide_future$Time[i]), '\n')
          cat('current iter level', tide_future$meters_to_max[i], '\n')
          cat('current iter state', iter_state, '\n')
       }
   # now save the results
   res_cur<-data.frame(tide_future$Time[i], tide_future$meters_to_max[i], iter_state)
   Res2<-rbind(Res2, res_cur)
   if (iter_state != cur_state) break()
   }
   names(Res2)<-c('Time', 'iter_level' , 'iter_state')
 
   Time_next_change <- Res2 |> last() |> select('Time')
   # now i need to get the difference
   Result<-abs(as.numeric(difftime(time_to_predict,  Time_next_change[1,], units='mins')))
   }
   
   if (plot) {
   par(mfrow=c(1,2))
   if (direction==1) tide_before_f<-rbind(tide_before_f, tide_future)
     plot(tide_before_f$Time,tide_before_f$z.m., type='l')
     lines(tide_before$Time,tide_before$z.m., col=' ')
   
     if(direction==1) abline(v=Time_next_change[1,], col='red')
     abline(v=Time_last_change, col='red', lwd=2)
     abline(v=time_to_predict, col='blue')


   plot(tide_before_f$Time,tide_before_f$meters_to_max, type='l')
   abline(h=fe_local)
   if(direction==1) abline(v=Time_next_change[1,], col='red')
   abline(v=Time_last_change, col='red', lwd=2)
   abline(v=time_to_predict, col='blue')

   lines(tide_before$Time,tide_before$meters_to_max, col='green')
   }
   }
   return(Result)
   }
   
   