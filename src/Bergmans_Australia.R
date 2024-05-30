# Bergmann's Rule nice map
require(maps)
require(mapdata)

head(raw_data)
raw_data$CBL
hist(log(raw_data$CBL), breaks = 30)
# Test for normality
log_CBL <- log(raw_data$CBL)
shapiro.test(log_CBL)

shapiro.test(raw_data$CBL)

#map(raw_data[,"Longitude"], raw_data[,"Latitude"])
is.array(raw_data$Latitude)
raw_data[,"Latitude"]
is.data.frame(raw_data)
lat <- as.vector(raw_data$Latitude)
long <- as.vector(raw_data$Longitude)

is.array(lat)

plot(long,lat)
#map(long,lat)
is.numeric(lat)
is.vector(lat)
map(regions = "Australia", xlim = c(110,160), mar = c(1, 1, 1, 1))
points(long,lat, cex = (log_CBL/4)^6, col = hsv(h = (log_CBL-min(log_CBL))/
                                                  (max(log_CBL)-min(log_CBL))))
#example of geographic variation in size consistant with Bergmann's rule

