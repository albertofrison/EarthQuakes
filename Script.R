library (tidyverse)

url <- "http://webservices.ingv.it/fdsnws/event/1/query?starttime=1995-01-01T00%3A00%3A00&endtime=2020-01-01T23%3A59%3A59&minmag=3&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000"
quakes <- read.delim(url, header = TRUE, sep ="|")

quakes$Date <- as.Date (quakes$Time, format = "%Y-%m-%d")
quakes$YearDay <- paste(format(quakes$Date, "%d"),format(quakes$Date, "%m"), sep ="-")
quakes$WeekNumber <- lubridate::week(quakes$Date) 


quakes %>%
  ggplot(aes(x = YearDay)) +
  geom_bar()

quakes %>%
  ggplot(aes(x = WeekNumber)) +
  geom_bar()

quakes %>%
  ggplot(aes(x  = as.factor(WeekNumber), y = Magnitude)) +
  geom_boxplot()

quakes %>%
  ggplot(aes(x  = as.factor(MagType), y = Magnitude)) +
  geom_boxplot()

quakes %>%
  ggplot(aes(x  = as.factor(MagType), y = Depth.Km)) +
  geom_boxplot()


  #xlab("")+
  #ylab("Rates") +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle ("INGV data - Earthquakes") 


quakes %>%
  ggplot(aes(x = Magnitude, y =  Depth.Km, color =WeekNumber)) +
  geom_point()


# from here stuff does not work yet

lista_esclusi <-  c("MARE", "Albania", "SLOVENIA", "SVIZZERA", "Slovenia", "Francia", "Serbia", "FRANCIA", "CROAZIA")
`%notin%` <- Negate(`%in%`)

quakes %>%
  filter (EventLocationName %notin% lista_esclusi) %>%
  ggplot(aes(x = Longitude, y =  Latitude)) +
  geom_point()



