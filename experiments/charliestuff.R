
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
    
    
    month.pvalues = 1:max(unique(route.data$months_since_Jan_2010)) * Inf # dummy filled # inf for min function
    for (time in unique(route.data$months_since_Jan_2010)) {
      baseline = mean(route.data$duration[route.data$months_since_Jan_2010 == time])
      
      # permutation time
      permuted.means = 1:permuations * 0 # dummy
      for (i in 1:permuations) {
        random.durations = sample(route.data$duration, length(route.data$duration[route.data$months_since_Jan_2010 == time])) 
        permuted.means[i] = mean(random.durations)
      }
      
      month.pvalues[time] = 2 * pnorm(abs((baseline - mean(permuted.means)) / sd(permuted.means)), lower.tail=FALSE)
      # quick bonferroni correction
      month.pvalues[time] = month.pvalues[time] * length(unique(route.data$months_since_Jan_2010))
      
      
    }
    
    route.pvalues[j] = min(month.pvalues)
    j = j + 1
  }
  
  return(route.pvalues)
  
}

no_model = permute(100)
# bonferroni 
no_model.bonferroni = p.adjust(no_model, method="bonferroni")
length(no_model.bonferroni[no_model.bonferroni < 0.05])
# holm
no_model.holm = p.adjust(no_model, method="holm")
length(no_model.holm[no_model.holm < 0.05])


permute.withmodel = function(permutations) {
  j = 1
  route.pvalues = 1:length(unique(data$route)) * 0 # dummy
  
  for (selected.route in unique(data$route)) {
    selected.route = data$route[1]
    route.data = subset(data, route == selected.route)
    
    # train a model
    route.model = gam(duration ~ (starttime) + (bikenum) + (member), data=route.data)
    

    month.pvalues = 1:max(unique(route.data$months_since_Jan_2010)) * Inf # dummy filled
    for (time in unique(route.data$months_since_Jan_2010)) {
      baseline = sum(route.model$residuals[route.data$months_since_Jan_2010 == time]^2, na.rm=TRUE)
      
      permuted.means = 1:permuations * 0 # dummy
      for (i in 1:permuations) {
        random.residuals = sample(na.omit(route.model$residuals), length(random.data$duration))
        permuted.means[i] = sum(random.residuals^2, na.rm=TRUE)
      }
      
      month.pvalues[time] = pnorm(abs((baseline - mean(permuted.means)) / sd(permuted.means)), lower.tail=FALSE)
      month.pvalues[time] = month.pvalues[time] * length(unique(route.data$months_since_Jan_2010))
    }
    
    route.pvalues[j] = min(month.pvalues)
    j = j + 1
  }
  
  return(route.pvalues)
}

with_model = permute.withmodel(100)
# bonferroni 
with_model.bonferroni = p.adjust(with_model, method="bonferroni")
length(with_model.bonferroni[with_model.bonferroni < 0.05])
# holm
with_model.holm = p.adjust(with_model, method="holm")
length(no_model.holm[with_model.holm < 0.05])






