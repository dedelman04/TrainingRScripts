##12.1

my.plot <-  ggplot(data = diamonds)
my.plot <- my.plot + aes(x=depth, y=carat)+ geom_point() + geom_density2d()
my.plot

##12.2
depth.groups <- cut(diamonds$depth, breaks=40+(0:5)*8)
my.plot <- ggplot(diamonds)
my.plot <- my.plot + aes(price, fill=depth.groups) + geom_density(alpha=.3)
my.plot

##12.3
data<-data.frame(state=row.names(state.x77), murder=state.x77[,5])
data$state <-as.character(data$state)

for (i in 1:nrow(data)) {
  latlon = geocode(data$state[i])
  data$lon[i] = as.numeric(latlon[1])
  data$lat[i] = as.numeric(latlon[2])
}

usa_center = geocode("United States")
USA <- ggmap(get_map(location=usa_center, zoom=4), extent="panel")
USA + ggplot(data=data, aes(lat,lon), alpha=0.4, size=data$murder)
