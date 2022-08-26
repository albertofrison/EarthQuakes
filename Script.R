#####
# Project to discover more about EarthQuakes in Italy
# Data from INGV
# Created with ♥ by Alberto Frison 
# Created Dec 2021
# Revised Aug 2022

#####
# Libraries
library (tidyverse)

#####
# Data Loading & Wrangling
# EarthQuakes data load from INGV - problem: the systems only returns max 10000 rows, so I had to "trick" the parameters to stay within this limit.

# Option 1 - Year from 2002 to 2019 and Min Magnitude = 2.8
# url_01 <- "http://webservices.ingv.it/fdsnws/event/1/query?starttime=2002-01-01T00%3A00%3A00&endtime=2019-12-31T23%3A59%3A59&minmag=2.8&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000"

# Option -2 Year from 1985 to 2022 and Min Magnitude = 3.1
url_02 <- "http://webservices.ingv.it/fdsnws/event/1/query?starttime=1985-01-01T00%3A00%3A00&endtime=2022-12-31T23%3A59%3A59&minmag=3.1&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000"

# change URL depending on the analysis you want to do
quakes <- read.delim(url_02, header = TRUE, sep ="|")
nrow(quakes) #9962 @ 25 Aug 2022

# we going to work with dates so let's build some categorical parameters to use
quakes$Date <- as.Date (quakes$Time, format = "%Y-%m-%d")
quakes$YearDay <- paste(format(quakes$Date, "%d"),format(quakes$Date, "%m"), sep ="-")
quakes$WeekNumber <- lubridate::week(quakes$Date)
quakes$Year <- as.factor(format(quakes$Date, "%Y"))
quakes$Month <- as.factor(format(quakes$Date, "%m"))


#####
# This whole "dates" section matters only if you want to conduct this witch-hunt analysis, it was fun the first time but not longer...
# update it if you wish...

# ==> now, the task is to understand if - like my mother says - EarthQuakes happen more ofter during holidays... so we have to understand which day is a holiday (In Italy) and which not
# Recurring holidays that happen always the same day every year - easy peasy
holidays <- format(as.Date(paste ("01/01/", c(1985:2020), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y") #Capodanno
holidays <- c(holidays,format(as.Date(paste ("06/01/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Epifania
holidays <- c(holidays,format(as.Date(paste ("25/04/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Liberazione
holidays <- c(holidays,format(as.Date(paste ("01/05/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Festa del Lavoro
holidays <- c(holidays,format(as.Date(paste ("02/06/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Festa della Repubblica
holidays <- c(holidays,format(as.Date(paste ("15/08/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Assunzione aka Ferragosto
holidays <- c(holidays,format(as.Date(paste ("01/11/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Tutti Santi
holidays <- c(holidays,format(as.Date(paste ("08/12/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Immacolata
holidays <- c(holidays,format(as.Date(paste ("25/12/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Natale
holidays <- c(holidays,format(as.Date(paste ("26/12/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Santo Stefano

# Easter holidays that change date every year datasource from here: http://calendario.eugeniosongia.com/datapasqua.htm
# 1985
holidays <- c(holidays, format(as.Date("07/04/1985", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("08/04/1985", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("30/03/1986", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("31/03/1986", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("19/04/1987", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("20/04/1987", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("03/04/1988", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("04/04/1988", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("26/03/1989", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("27/03/1989", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# 1990
holidays <- c(holidays, format(as.Date("15/04/1990", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("16/04/1990", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("31/03/1991", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("01/04/1991", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("19/04/1992", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("20/04/1992", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("11/04/1993", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("12/04/1993", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("03/04/1994", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("04/04/1994", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("16/04/1995", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("17/04/1995", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("07/04/1996", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("08/04/1996", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("30/03/1997", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("31/03/1997", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("12/04/1998", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("13/04/1998", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("04/04/1999", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("05/04/1999", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# 2000
holidays <- c(holidays, format(as.Date("23/04/2000", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("24/04/2000", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("15/04/2001", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("16/04/2001", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("31/03/2002", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
holidays <- c(holidays, format(as.Date("01/04/2002", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("20/04/2003", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("21/04/2003", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("11/04/2004", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("12/04/2004", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("27/03/2005", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("28/03/2005", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("16/04/2006", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("17/04/2006", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("08/04/2007", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("09/04/2007", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("23/03/2008", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("24/03/2008", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("12/04/2009", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("13/04/2009", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
#2010
holidays <- c(holidays, format(as.Date("04/04/2010", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("05/04/2010", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("24/04/2011", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("25/04/2011", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("08/04/2012", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("09/04/2012", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("31/03/2013", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("01/04/2013", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("20/04/2014", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("21/04/2014", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("05/04/2015", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("06/04/2015", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("27/03/2016", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("28/03/2016", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("16/04/2017", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("17/04/2017", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("01/04/2018", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("02/04/2018", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
holidays <- c(holidays, format(as.Date("21/04/2019", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
holidays <- c(holidays, format(as.Date("22/04/2019", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta


# General comment, for each year we identified 12 days of Holidays
# Now, let's create now a dimension that says if an event happened in a Regular Day or in a Holiday Day
quakes$DayType <- as.factor (ifelse (format(quakes$Date,"%d/%m/%Y") %in% format(as.Date(holidays, format = "%d/%m/%Y"), "%d/%m/%Y") , "Holiday", "Working Day" ))

# We can now answer to the first question - how is more likely that an Event happens during a holiday?
# We know that 12 days out of 365 are holidays
12/365 # 3.287%

# how many events happened during a holiday?
summary(quakes$DayType)

sum (quakes$DayType == "Holiday") / nrow (quakes) # [3.77% in for smaller events, 3.68% for greater events]
# at a first look it doesn't look TOO different statistically speaking, but I will revise this once I will have revised hypotesis testing

# what if during holidays events were MORE STRONG?
summary(quakes$Magnitude)

quakes %>%
  ggplot(aes(x  = DayType, y = Magnitude, fill = DayType)) +
  ggtitle("Earthquakes Magnitude by holiday/working day") +
  theme_minimal() +
  theme (legend.position = "none") +
  labs(x = "", y = "Magnitude", subtitle = "Dati INGV - 9.458 events registered in Italy: Years 1985-2019, Magnitude 3.1 min", caption ="https://github.com/albertofrison/EarthQuakes") +
  geom_boxplot() 

# end of witch hunt section


#####
# Some analysis
head (quakes)
summary(quakes)

# Scatterplot Event Depth vs Magnitude
quakes %>%
  ggplot(aes(x = Depth.Km, y = Magnitude)) +
  geom_point ()

# Magnitude Density Plot  per Mag Type
quakes %>%
  filter(!is.na(Depth.Km) & !is.na(Magnitude)) %>%
  #cor(x = as.numeric(Depth.Km), y = as.numeric(Magnitude), use= "pairwise.complete.ob")
  #nrow()
  ggplot() +
  geom_density(aes(x = Magnitude, fill = MagType))

# distribution of events per depending on Magnitude
quakes %>%
  ggplot(aes(x = Magnitude)) +
  ggtitle("Earthquakes Magnitude Distribution") +
  theme_minimal() +
  theme (legend.position = "none") +
  labs(x = "Magnitude", y = "Count", subtitle = paste("Dati INGV -", nrow(quakes)," events registered in Italy: Years 1985-2022 - Magnitude 3.1 min"), caption ="https://github.com/albertofrison/EarthQuakes") +
  geom_histogram(binwidth = 0.1, color = "blue")

# calculate mean on a fancy way - you can check it easily also with summary()
quakes %>%
  pull (Magnitude) %>%
  mean ()

#GEOGRAPHICAL PLOT
# Add to Location some cities
quakes <- quakes %>%
  mutate (Location =  case_when(
    str_detect(EventLocationName,"(TO)") ~  "Turin",
    str_detect(EventLocationName,"(PA)") ~  "Palermo",
    str_detect(EventLocationName,"(RM)") ~  "Rome",
    str_detect(EventLocationName,"(BO)") ~  "Bologna",
    str_detect(EventLocationName,"(VE)") ~  "Venice",
    #str_detect(EventLocationName,"(ME)") ~  "Messina",
    #str_detect(EventLocationName,"(AQ)") ~  "Aquila",
    TRUE ~  "Other"
    )
  )

# Geographical distribution of events, you can see the shape of Italy
quakes %>%
  filter (Magnitude > 3.5) %>% # play here to see Magnitude Impact on the pòot
  ggplot(aes(x = Longitude, y =  Latitude, color = Location)) +
  theme_minimal() +
  scale_color_manual (values = c("#FFDB6D", "#000000", "#C4961A", "#F4EDCA", "#D16103", "#C3D7A4", "#52854C", "#4E84C4" )) +
  ggtitle ("Map of Events (Turin, Bologna, Venice, Rome and Palermo have been colored)") +
  labs(subtitle = paste("Dati INGV -", nrow(quakes)," events registered in Italy: Years 1985-2022 - Magnitude 3.1 min"), caption ="https://github.com/albertofrison/EarthQuakes") +
  theme (legend.position = "bottom") +
  geom_point(size = 1)


# OTHER ANALYSIS
# what about magnitude Type? Not sure what it is
# more on Magnitude Type here http://www.blueplanetheart.it/2017/04/ml-mb-ms-md-mw-perche-esistono-diverse-magnitudo/
quakes$MagType <- as.factor(quakes$MagType)
quakes %>%
  ggplot(aes(x  = MagType, y = Magnitude)) +
  geom_boxplot()

# distribution of events per depending on Depth in KM
quakes %>%
  filter (Depth.Km <300) %>% # if you look very few are beloe 300km
  ggplot(aes(x = Depth.Km) ) +
  geom_histogram(binwidth = 5)

# distribution of events per Year
quakes %>%
  ggplot(aes(x = Year)) +
  geom_bar()

# quakes %>%
#   ggplot(aes(x = Magnitude, color = DayType) ) +
#   geom_density(bw=0.1) +
#   #geom_vline(aes(xintercept=mean(Magnitude), linetype="dashed")) +
#   labs(title = "Earthquakes Magnigudo Density Distribution by Day Type", subtitle = "Dati INGV - 9.458 events registered in Italy: Years 1985-2019, Magnitude 3.1 min", caption ="https://github.com/albertofrison/EarthQuakes") +
#   theme (legend.position = "bottom") 
