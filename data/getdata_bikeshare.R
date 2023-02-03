data = NULL

for(i in 2010:2011){ # the data goes up to 2017, but the files are extremely large from 2011 onwards - you can decide to just use a subset
	file = paste0('https://s3.amazonaws.com/capitalbikeshare-data/',i,'-capitalbikeshare-tripdata.zip')
	download.file(file,destfile='bikedata.zip')
	unzip('bikedata.zip')
	data = rbind(data,read.csv(paste0(i,'-capitalbikeshare-tripdata.csv')))
}

n = dim(data)[1]

starttime = as.numeric(strsplit(toString(data[,2]),split='[-:, ]')[[1]][-7*(1:n)]) # start time of ride #i
dim(starttime) = c(6,n); starttime = t(starttime) # row i = year/month/date/hour/minute/second for ride #i
duration = data[,1] # duration of the ride in seconds
station_start = data[,4] # station ID where the bike was checked out
station_end = data[,6] # station ID where the bike was returned
member = (data[,9]=='Member') # member (1) or nonmember (0)


bikenum =  as.numeric((strsplit(toString(data[,8]),'[?wW, ]')[[1]][3*(1:n)-1]))
# some are NA, the data is messy for this one

stations = NULL # stations[i,1] = station ID for the i-th station, stations[i,2] = station location for the i-th station
for(i in unique(c(station_start,station_end))){
	if(any(data[,4]==i)){
		ind = min(which(data[,4]==i))
		location = toString(data[ind,5])
	}else{
		ind = min(which(data[,6]==i))
		location = toString(data[ind,7])
	}
	stations = rbind(stations,c(i,location))
}
# note that stations get added to the program over time

days_in_month = rep(c(31,28,31,30,31,30,31,31,30,31,30,31),2) # Jan 2010, ..., Dec 2011

days_since_Jan1_2010 = (starttime[,1]-2010)*365 + cumsum(days_in_month)[starttime[,2]] -
		days_in_month[starttime[,2]] + (starttime[,3]-1)
day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday',
	'Saturday','Sunday')[(days_since_Jan1_2010 + 4)%% 7 + 1]

save('starttime','duration','bikenum','stations','station_start','station_end','member','days_since_Jan1_2010','day_of_week',file='bikedata.RData')
