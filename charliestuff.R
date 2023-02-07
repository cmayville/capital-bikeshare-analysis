
load('data/bikedata.RData')
library(lubridate) #ymd_hms
library(mgcv) #gam

# just stealing drew's nice format stuff
# convert starttime to datetime
starttime_times <- ymd_hms(paste(starttime[,1], starttime[,2], starttime[,3], starttime[,4], starttime[,5], starttime[,6], sep= '-'))

# make dataframe
data <- data.frame(
  "starttime" = starttime_times,
  "duration" = duration,
  "day_of_week" = day_of_week,
  "days_since_Jan1_2010" = days_since_Jan1_2010,
  "bikenum" = bikenum,
  "member" = member,
  "station_start" = station_start,
  "station_end" = station_end
)

# some desc stats
data["route"] <- paste(data$station_start,data$station_end,sep="-")
data['months_since_Jan_2010'] <- 12*(year(data$starttime)-2010)+month(data$starttime)

permute = function(permuations) {
  j = 1
  route.pvalues = 1:length(unique(data$route)) * 0 # dummy
  
  for (selected.route in unique(data$route)) {
    route.data = subset(data, route == selected.route)
    
    
    month.pvalues = 1:max(unique(route.data$months_since_Jan_2010)) * 0 # dummy filled
    for (time in unique(route.data$months_since_Jan_2010)) {
      baseline = mean(route.data$duration[route.data$months_since_Jan_2010 == time])
      
      # permutation time :3
      permuted.means = 1:permuations * 0 # dummy
      for (i in 1:permuations) {
        random.durations = sample(route.data$duration, length(route.data$duration[route.data$months_since_Jan_2010 == time])) 
        permuted.means[i] = mean(random.durations)
      }
      
      month.pvalues[time] = pt((baseline - mean(permuted.means)) / sd(permuted.means), permuations + 1)
      # quick bonferroni correction
      month.pvalues[time] = month.pvalues[time] / length(route.data$duration[route.data$months_since_Jan_2010 == time])
      
    }
    # we could just take the min and bonferroni/something correct on that
    route.pvalues[j] = min(month.pvalues)
    j = j + 1
  }
  
  return(route.pvalues)
  
}

permute.withmodel = function(permutations) {
  j = 1
  route.pvalues = 1:length(unique(data$route)) * 0 # dummy
  
  for (selected.route in unique(data$route)) {
    selected.route = data$route[1]
    route.data = subset(data, route == selected.route)
    
    # train a model
    route.model = gam(duration ~ starttime + bikenum + member, data=route.data)
    

    month.residuals = 1:max(unique(route.data$months_since_Jan_2010)) * 0 # dummy filled
    
    for (time in unique(route.data$months_since_Jan_2010)) {
      baseline = mean(route.model[route.data$months_since_Jan_2010 == time])
      
      permuted.means = 1:permuations * 0 # dummy
      for (i in 1:permuations) {
        random.residuals = sample(route.model$residuals, length(random.data$duration))
        permuted.means[i] = mean(random.residuals)
      }
      
      month.pvalues[time] = pt((baseline - mean(permuted.means)) / sd(permuted.means), permuations + 1)
      month.pvalues[time] = month.pvalues[time] / length(route.data$duration[route.data$months_since_Jan_2010 == time])
    }
    
    route.pvalues[j] = min(month.pvalues)
    j = j + 1
  }
  
  return(route.pvalues)
}






