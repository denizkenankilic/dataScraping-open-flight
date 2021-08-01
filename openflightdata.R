airports <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", header = FALSE)
colnames(airports) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")
head(airports)

install.packages("rworldmap")
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)

points(airports$lon, airports$lat, col = "red", cex = .6)

routes <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat", header=F)
colnames(routes) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")
head(routes)

library(plyr)
departures <- ddply(routes, .(sourceAirportID), "nrow")
names(departures)[2] <- "flights"
arrivals <- ddply(routes, .(destinationAirportID), "nrow")
names(arrivals)[2] <- "flights"

airportD <- merge(airports, departures, by.x = "ID", by.y = "sourceAirportID")
airportA <- merge(airports, arrivals, by.x = "ID", by.y = "destinationAirportID")

library(ggmap)
map <- get_map(location = 'Europe', zoom = 3)

mapPoints <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportD, alpha = .5)

mapPointsLegend <- mapPoints +
  scale_size_area(breaks = sqrt(c(1, 5, 10, 50, 100, 500)), labels = c(1, 5, 10, 50, 100, 500), name = "departing routes")
mapPointsLegend


# create the data set containing both departures and arrivals
airportD$type <- "departures"
airportA$type <- "arrivals"
airportDA <- rbind(airportD, airportA)

# map the data
# map + data points
mapPointsDA <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportDA, alpha = .5)
# adjust the legend
mapPointsLegendDA <- mapPointsDA +
  scale_size_area(breaks = sqrt(c(1, 5, 10, 50, 100, 500)), labels = c(1, 5, 10, 50, 100, 500), name = "routes")
# panels according to type (departure/arrival)
mapPointsFacetsDA <- mapPointsLegendDA +
  facet_grid(. ~ type)
# plot the map
mapPointsFacetsDA
