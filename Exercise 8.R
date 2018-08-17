##8-1
data.frame.z <- merge(data.frame.x, data.frame.y, 
		by.x = c("names", "age"),
		by.y = c("Person_name", "age"), 
		all=TRUE)

##8-2
sbs <- subset(iris, 
	Species=="setosa" & Sepal.Length<median(Sepal.Length),
	select=Sepal.Length:Petal.Width ##select = -Species)

##8-3
my.text<-"Over the last decade, bluetongue virus have spread northwards from the mediterranean area. Initially this was ascribed to climate changes, but it has since been realized that a major contributing factor has been new transmitting vectors, culicoides obsoletus and culicoides pulicaris, which have the ability to aquire and transmit the disease. Recently, schmallenberg virus has emerged in northern europe, transmitted by biting midges as well."

my.lc <-c("bluetongue", "culicoides", "europe", "mediterranean", "northern", "schmallenberg")
my.uc <-c("Bluetongue", "Culicoides", "Europe", "Mediterranean", "Northern", "Schmallenberg")
my.new.text <- my.text
for (i in 1:length(my.lc)) {
	my.new.text<-gsub(my.lc[i], my.uc[i], my.new.text)
}
my.new.text

##8.4
set.seed(885)
my.posixct<-as.POSIXct(sample((60*60*24*365*50):(60*60*24*365*55),20), 
                       origin = as.Date("1960-01-01"))

my.posixct
my.posixct2 <- my.posixct+(60*60*2)+(60*30)+10
df <- data.frame(my.posixct, my.posixct2)
head(df)

