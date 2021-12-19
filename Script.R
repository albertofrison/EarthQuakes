library (tidyverse)

# EarthQuakes data load from INGV - problem: the systems only returns max 10000 rows, so I had to "trick" the parameters to stay within this limit. Year from 2002 to 2019 and Min Magnitude = 2.8
url <- "http://webservices.ingv.it/fdsnws/event/1/query?starttime=2002-01-01T00%3A00%3A00&endtime=2019-12-31T23%3A59%3A59&minmag=2.8&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000"
quakes <- read.delim(url, header = TRUE, sep ="|")

# we going to work with dates so let's build some categorical parameters to plot
quakes$Date <- as.Date (quakes$Time, format = "%Y-%m-%d")
quakes$YearDay <- paste(format(quakes$Date, "%d"),format(quakes$Date, "%m"), sep ="-")
quakes$WeekNumber <- lubridate::week(quakes$Date)
quakes$Year <- as.factor(format(quakes$Date, "%Y"))
quakes$Month <- as.factor(format(quakes$Date, "%m"))

# now, the task is to understand if - like my mother says - EarthQuakes happen more ofter during holidays... so we have to understand which day is a holiday (In Italy) and which not
# Holiday Type A recurring holidays that happen always the same day
holidays <- format(as.Date(paste ("01/01/", c(2002:2020), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y") #Capodanno
holidays <- c(holidays,format(as.Date(paste ("06/01/", c(2002:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Epifania
holidays <- c(holidays,format(as.Date(paste ("25/04/", c(2002:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Liberazione
holidays <- c(holidays,format(as.Date(paste ("01/05/", c(2002:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Festa del Lavoro
holidays <- c(holidays,format(as.Date(paste ("02/06/", c(2002:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Festa della Repubblica
holidays <- c(holidays,format(as.Date(paste ("15/08/", c(2002:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Assunzione aka Ferragosto
holidays <- c(holidays,format(as.Date(paste ("01/11/", c(2002:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Tutti Santi
holidays <- c(holidays,format(as.Date(paste ("08/12/", c(2002:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Immacolata
holidays <- c(holidays,format(as.Date(paste ("25/12/", c(2002:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Natale
holidays <- c(holidays,format(as.Date(paste ("26/12/", c(2002:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Santo Stefano

# Holidays Type B, holidays that change date every year datasource from here: http://calendario.eugeniosongia.com/datapasqua.htm
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

# Now, let's create now a variable that says if an event happened in a Regular Day or in a Holiday Day
quakes$DayType <- as.factor (ifelse (format(quakes$Date,"%d/%m/%Y") %in% format(as.Date(holidays, format = "%d/%m/%Y"), "%d/%m/%Y") , "Holiday", "Regular" ))

# We can now answer to the first question - how is more likely that an Event happens during a holiday?
# We know that 12 days out of 365 are holidays
12/365 # 3.287%

# how many events in the last 18 years happened during a holiday?
sum (quakes$DayType == "Holiday") / nrow (quakes) # 3.77%

# 3.28 vs 3.77 doesn't look TOO different statistically speaking, but I will revise this once I will have revised hypotesis testing


# now.. let's do some science
# distribution of events per Year
quakes %>%
  ggplot(aes(x = as.factor(Year))) +
  geom_bar()

# distribution of events per depending on Magnitude
quakes %>%
  ggplot(aes(x = Magnitude)) +
  geom_histogram(binwidth = 0.1)

# what about magnitude Type? Not sure what it is
# more on Magnitude Type here http://www.blueplanetheart.it/2017/04/ml-mb-ms-md-mw-perche-esistono-diverse-magnitudo/
quakes$MagType <- as.factor(quakes$MagType)
quakes %>%
  ggplot(aes(x  = as.factor(MagType), y = Magnitude)) +
  geom_boxplot()

# distribution of events per depending on Depth in KM
quakes %>%
  filter (Depth.Km <300) %>% # if you look very few are beloe 300km
  ggplot(aes(x = Depth.Km) ) +
  geom_histogram(binwidth = 5)

# Geographical distribution of events, you can see the shape of Italy
quakes %>%
  ggplot(aes(x = Longitude, y =  Latitude)) +
  geom_point()

# FROM HERE STUFF DOESN'T WORK YET
 
quakes$Location <- if (quakes$EventLocationName !=1,  ifelse (str_detect(quakes$EventLocationName,"(TO)"), "Torino",1))
quakes$Location <- ifelse (str_detect(quakes$EventLocationName,"(AQ)"), "Aquila",1)

`%notin%` <- Negate(`%in%`)
lista_esclusi <-  c("MARE", "Albania", "SLOVENIA", "SVIZZERA", "Slovenia", "Francia", "Serbia", "FRANCIA", "CROAZIA")
