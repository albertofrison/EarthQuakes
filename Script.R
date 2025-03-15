#####
# Project to discover more about EarthQuakes in Italy
# Data from INGV
# Created with ♥ by Alberto Frison 
# Created December 2021
# Revised August 2022
# Revised on September 2022
# Revised on February 2024
# Revised on March 2024


#####
# 01. Libraries
library (tidyverse) 
library (MASS) # needed for truehist
library(leaflet) # map plotting ()


#####
# 02. Initialization
rm (list = ls())


#####
# 03. LOADING DATA SET
# EarthQuakes data load from INGV - problem: the systems only returns max 10000 rows, so I had to "trick" the parameters to stay within this limit.
# Since each webservices call returns max 10k rows, we need to find a way to load all Events (one year at time?)

# quakes is the dataframe where I store all the data
quakes <- "" # initialize the main dataframe

# This FOR LOOP  loads one month at the time - there are still issues in 2016 / 2017 with more than 10k events in the month
# This will throw an error at the end when it will come to a month in the future - bu the data loaded will still be OK

options(timeout = 240)

for (i in 1985:2025) {
  
  url_m1 <- paste ("http://webservices.ingv.it/fdsnws/event/1/query?starttime=",i,"-01-01","T00%3A00%3A00&endtime=",i,"-01-31","T23%3A59%3A59&minmag=0.0&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000", sep="")
  quakes_url_m1 <- read.delim(url_m1, header = TRUE, sep ="|") # 
  print(paste (i,nrow(quakes_url_m1))) # JANUARY
  quakes <- rbind(quakes,quakes_url_m1) # needed to append one month a time, in order to avoid to have an entire year (last year) completely disregarded due to the running error that will be thrown when we will load the first missing month
  
  url_m2 <- if (i %% 4 == 0) # anni bisestili
    { paste ("http://webservices.ingv.it/fdsnws/event/1/query?starttime=",i,"-02-01","T00%3A00%3A00&endtime=",i,"-02-29","T23%3A59%3A59&minmag=0.0&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000", sep="")
    } else
      { paste ("http://webservices.ingv.it/fdsnws/event/1/query?starttime=",i,"-02-01","T00%3A00%3A00&endtime=",i,"-02-28","T23%3A59%3A59&minmag=0.0&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000", sep="")
      } 
                
  quakes_url_m2 <- read.delim(url_m2, header = TRUE, sep ="|") # 
  print(paste (i,nrow(quakes_url_m2))) # FEBRUARY
  quakes <- rbind(quakes,quakes_url_m2)
  
  url_m3 <- paste ("http://webservices.ingv.it/fdsnws/event/1/query?starttime=",i,"-03-01","T00%3A00%3A00&endtime=",i,"-03-31","T23%3A59%3A59&minmag=0.0&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000", sep="")
  quakes_url_m3 <- read.delim(url_m3, header = TRUE, sep ="|") # 
  print(paste (i,nrow(quakes_url_m3))) # MARCH
  quakes <- rbind(quakes,quakes_url_m3)
  
  url_m4 <- paste ("http://webservices.ingv.it/fdsnws/event/1/query?starttime=",i,"-04-01","T00%3A00%3A00&endtime=",i,"-04-30","T23%3A59%3A59&minmag=0.0&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000", sep="")
  quakes_url_m4 <- read.delim(url_m4, header = TRUE, sep ="|") # 
  print(paste (i,nrow(quakes_url_m4))) # APRIL
  quakes <- rbind(quakes,quakes_url_m4)
  
  url_m5 <- paste ("http://webservices.ingv.it/fdsnws/event/1/query?starttime=",i,"-05-01","T00%3A00%3A00&endtime=",i,"-05-31","T23%3A59%3A59&minmag=0.0&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000", sep="")
  quakes_url_m5 <- read.delim(url_m5, header = TRUE, sep ="|") # 
  print(paste (i,nrow(quakes_url_m5))) # MAY
  quakes <- rbind(quakes,quakes_url_m5)
  
  url_m6 <- paste ("http://webservices.ingv.it/fdsnws/event/1/query?starttime=",i,"-06-01","T00%3A00%3A00&endtime=",i,"-06-30","T23%3A59%3A59&minmag=0.0&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000", sep="")
  quakes_url_m6 <- read.delim(url_m6, header = TRUE, sep ="|") # 
  print(paste (i,nrow(quakes_url_m6))) # JUNE
  quakes <- rbind(quakes,quakes_url_m6)
  
  url_m7 <- paste ("http://webservices.ingv.it/fdsnws/event/1/query?starttime=",i,"-07-01","T00%3A00%3A00&endtime=",i,"-07-31","T23%3A59%3A59&minmag=0.0&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000", sep="")
  quakes_url_m7 <- read.delim(url_m7, header = TRUE, sep ="|") # 
  print(paste (i,nrow(quakes_url_m7))) # JULY
  quakes <- rbind(quakes,quakes_url_m7)
  
  url_m8 <- paste ("http://webservices.ingv.it/fdsnws/event/1/query?starttime=",i,"-08-01","T00%3A00%3A00&endtime=",i,"-08-31","T23%3A59%3A59&minmag=0.0&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000", sep="")
  quakes_url_m8 <- read.delim(url_m8, header = TRUE, sep ="|") # 
  print(paste (i,nrow(quakes_url_m8))) # AUGUST
  quakes <- rbind(quakes,quakes_url_m8)
  
  url_m9 <- paste ("http://webservices.ingv.it/fdsnws/event/1/query?starttime=",i,"-09-01","T00%3A00%3A00&endtime=",i,"-09-30","T23%3A59%3A59&minmag=0.0&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000", sep="")
  quakes_url_m9 <- read.delim(url_m9, header = TRUE, sep ="|") # 
  print(paste (i,nrow(quakes_url_m9))) # SEPTEMBER
  quakes <- rbind(quakes,quakes_url_m9)
  
  url_m10 <- paste ("http://webservices.ingv.it/fdsnws/event/1/query?starttime=",i,"-10-01","T00%3A00%3A00&endtime=",i,"-10-31","T23%3A59%3A59&minmag=0.0&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000", sep="")
  quakes_url_m10 <- read.delim(url_m10, header = TRUE, sep ="|") # 
  print(paste (i,nrow(quakes_url_m10))) # OCTOBER
  quakes <- rbind(quakes,quakes_url_m10)
  
  url_m11 <- paste ("http://webservices.ingv.it/fdsnws/event/1/query?starttime=",i,"-11-01","T00%3A00%3A00&endtime=",i,"-11-30","T23%3A59%3A59&minmag=0.0&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000", sep="")
  quakes_url_m11 <- read.delim(url_m11, header = TRUE, sep ="|") # 
  print(paste (i,nrow(quakes_url_m11))) # NOVEMBER
  quakes <- rbind(quakes,quakes_url_m11)
  
  url_m12 <- paste ("http://webservices.ingv.it/fdsnws/event/1/query?starttime=",i,"-12-01","T00%3A00%3A00&endtime=",i,"-12-31","T23%3A59%3A59&minmag=0.0&maxmag=10&mindepth=-10&maxdepth=1000&minlat=35&maxlat=49&minlon=5&maxlon=20&minversion=100&orderby=time-asc&format=text&limit=10000", sep="")
  quakes_url_m12 <- read.delim(url_m12, header = TRUE, sep ="|") # 
  print(paste (i,nrow(quakes_url_m12))) # DECEMBER
  quakes <- rbind(quakes,quakes_url_m12)
  
}


head (quakes)
today()
nrow(quakes)
# END OF MASSIVE LOAD #446117 ROWS @2025-03-15 - nrow(quakes)
# warning - this type of load excludes some events in some years because the maximum number of rows exported by INGV is 10k - some months have more than 10k events...
# ... so if you want to get also those, you have to change the data load and split each month in two or in weeks or as you wish...


#####
# DATA WRANGLING

# ADDING USEFUL COLUMNS
# we going to work with dates so let's build some categorical parameters to use
quakes$Date <- as.Date (quakes$Time, format = "%Y-%m-%d")
quakes$WeekNumber <- lubridate::week(quakes$Date)
quakes$Year <- as.numeric(format(quakes$Date, "%Y"))
quakes$Month <- as.factor(format(quakes$Date, "%m"))

# DELETING UNUSED ROWS // COLUMNS
quakes <- quakes[-1,] # delete the first row, which is empty
# DELETING UNUSED COLUMNS
quakes <- quakes[,-c(1,2,6,7,8,9,12,14)]

# FORMATTING THE COLUMNS THE RIGHT WAY
quakes$Latitude <- as.numeric (quakes$Latitude)
quakes$Longitude <- as.numeric (quakes$Longitude)
quakes$Depth.Km <- as.numeric (quakes$Depth.Km)
quakes$MagType <- as.factor (quakes$MagType)
quakes$Magnitude <- as.numeric (quakes$Magnitude)

# check again how it looks
head (quakes)

# BACKUP PHASE - IF NEEDED
# LOADING THE DATA WAS LONG AND HARD, LET'S BACK IT UP BEFORE MANIPULATING IY
quakes_backup <- quakes

# IN ALTERNATIVE WE CAN STORE THE DOWNLOAD INTO A CSV
# write.csv(quakes, "data/quakes.csv", row.names=TRUE, quote=FALSE) # a 30MB file...
# quakes <- read.csv("data/quakes.csv", header = TRUE)

# REMOVING THE MESS CAUSED BY THE LOAD
rm (list = c("quakes_url_m1", 
             "quakes_url_m2", 
             "quakes_url_m3",
             "quakes_url_m4",
             "quakes_url_m5",
             "quakes_url_m6",
             "quakes_url_m7",
             "quakes_url_m8",
             "quakes_url_m9",
             "quakes_url_m10",
             "quakes_url_m11",
             "quakes_url_m12"))

rm (list = c("url_m1", 
             "url_m2", 
             "url_m3",
             "url_m4",
             "url_m5",
             "url_m6",
             "url_m7",
             "url_m8",
             "url_m9",
             "url_m10",
             "url_m11",
             "url_m12",
             "i"))


#####
# STARTING THE ANALYSIS
summary(quakes)

################################################################################
# MAGNITUDE DISTRIBUTION
hist(quakes$Magnitude) # distribution of magnitude - VERSION 01
truehist(quakes$Magnitude, h = 0.2, col = "red", xlab = "Magnitude") # distribution of magnitude - VERSION 02


# Creiamo l'istogramma e salviamo i dati - VERSION 03 (CHAT GPT UPGRADED 8=====D )
hist_data <- truehist(quakes$Magnitude, 
                      h = 0.2, 
                      col = rgb(1, 0, 0, 0.5),  
                      border = "darkred",  
                      xlab = "Magnitudine", 
                      ylab = "Frequenza",
                      main = "Distribution of Earthquake Magnitude",
                      cex.lab = 1.2,  
                      cex.axis = 1.1,
                      plot = FALSE)  

# SLIDE 02
truehist(quakes$Magnitude, 
         h = 0.2, 
         col = rgb(1, 0, 0, 0.5),  
         border = "darkred",  
         xlab = "Magnitudine", 
         ylab = "Frequenza",
         main = "Distribution of Earthquake Magnitude",
         cex.lab = 1.2,  
         cex.axis = 1.1  
)

# Sovrapposizione della curva di densità
lines(density(quakes$Magnitude), col = "black", lwd = 2)  

# Aggiunta della barra verticale a Magnitude = 4.4
abline(v = 3.3, col = "blue", lwd = 2, lty = 2)  
abline(v = 4.4, col = "gold", lwd = 2, lty = 2)  


################################################################################
# COMMANDS TO CREATE AND SAVE A PPF

# Creiamo un PDF multipagina per il carousel
pdf("carousel_linkedin.pdf", width = 8, height = 8)

# ora eseguire il codice

# print() oppure truehist() per aggiungere automaticamente una pagina al PDF

dev.off() # salva il PDF
################################################################################


################################################################################
# EVENTS PER YEAR
quakes %>%
  ggplot(aes(x = Year, fill = Month)) +
  geom_bar()

#####
# MAGNITUDE DISTRIBUTION (DENSITY)
quakes %>%
  group_by (MagType) %>%  
    #summarize (n(), mean (Magnitude), mean(Latitude), mean (Longitude)) %>%
    ggplot(aes(x = Magnitude, fill = MagType)) +
    geom_density(alpha = .5) +
    facet_wrap(~MagType)

# MAGNITUDE DISTRIBUTION (HYSTOGRAMS)
quakes %>%
  group_by (MagType) %>%  
    ggplot(aes(x = Magnitude, fill = MagType)) +
    geom_histogram() +
    facet_wrap(~MagType) 
    #labs(title = "ciao")


  
##################------------------------------------------------------------------------------------------------
# MAGNITUDE BOXPLOT
# more on Magnitude Type here http://www.blueplanetheart.it/2017/04/ml-mb-ms-md-mw-perche-esistono-diverse-magnitudo/
#quakes %>%
quakes %>%  
  ggplot(aes(x = MagType, y = Magnitude, fill = MagType)) +
  geom_point()

#####
# DEPTH DENSITY DISTRIBUTION
quakes %>%
  ggplot(aes(x = Depth.Km, fill = MagType)) +
  geom_density(alpha = .5)

# DEPTH BOXPLOT
quakes %>%
  ggplot(aes(x  = MagType, y = Depth.Km, fill = MagType)) +
  geom_boxplot()



################################################################################
# PLOTTING DATA INTO A MAP - WORKING!!!!

head(quakes)

quakes_filtered <- quakes %>% filter(Magnitude >= 3.3)
pal <- colorNumeric(palette = "Spectral", domain = quakes_filtered$Magnitude)

nrow(quakes_filtered)

# SLIDE 01
mappa <- quakes_filtered %>%
  leaflet() %>%
    addTiles() %>%  # Aggiunge la mappa di sfondo
    addCircleMarkers(~Longitude, ~Latitude, stroke = F, fillColor = ~pal(Magnitude), radius = 2, fillOpacity = 1) %>%
  addLegend(
    pal = pal, 
    values = ~Magnitude, 
    title = "Magnitude",
    position = "bottomleft", 
    opacity = 1)

print (mappa) # for the PDF

################################################################################






#####
# Scatterplot Event Depth vs Magnitude
quakes %>%
  ggplot(aes(x = Depth.Km, y = Magnitude)) +
  geom_point ()


# SLIDE 03
# Magnitude Density Plot  per Mag Type
quakes %>%
  filter(!is.na(Depth.Km) & !is.na(Magnitude)) %>%
  ggplot() +
  geom_density(aes(x = Magnitude, fill = MagType), alpha = 0.5)


# distribution of events per depending on Magnitude
quakes %>%
  ggplot(aes(x = as.numeric(Magnitude))) +
  ggtitle("Earthquakes Magnitude Distribution") +
  theme_minimal() +
  theme (legend.position = "none") +
  labs(x = "Magnitude", y = "Count", subtitle = paste("Dati INGV -", nrow(quakes)," events registered in Italy: Years 1985-2022"), caption ="https://github.com/albertofrison/EarthQuakes") +
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
    str_detect(EventLocationName,"(ROME)") ~  "Rome",
    str_detect(EventLocationName,"(BO)") ~  "Bologna",
    str_detect(EventLocationName,"(VE)") ~  "Venice",
    str_detect(EventLocationName,"(ME)") ~  "Messina",
    str_detect(EventLocationName,"(AQ)") ~  "Aquila",
    str_detect(EventLocationName,"(BA)") ~  "Bari",
    str_detect(EventLocationName,"(MI)") ~  "Milan",
    str_detect(EventLocationName,"(FI)") ~  "Florence",
    str_detect(EventLocationName,"(BA)") ~  "Bari",
    TRUE ~  "Other"
    )
  )

# Geographical distribution of events, you can see the shape of Italy
quakes %>%
  filter (Magnitude > 3.5, Location != "") %>% # play here to see Magnitude Impact on the pòot
  ggplot(aes(x = Longitude, y =  Latitude, color = Location)) +
  theme_minimal() +
  #scale_color_manual (values = c("#FFDB6D", "#000000", "#C4961A", "#F4EDCA", "#D16103", "#C3D7A4", "#52854C", "#4E84C4","#4E84CF", "#4E04C4"  )) +
  ggtitle ("Map of Events (Turin, Bologna, Venice, Rome and Palermo have been colored)") +
  labs(subtitle = paste("Dati INGV -", nrow(quakes)," events registered in Italy: Years 1985-2022 - Magnitude 3.1 min"), caption ="https://github.com/albertofrison/EarthQuakes") +
  theme (legend.position = "bottom") +
  geom_point(size = 1)


# OTHER ANALYSIS
# distribution of events per depending on Depth in KM
quakes %>%
  filter (Depth.Km <300) %>% # if you look very few are beloe 300km
  ggplot(aes(x = Depth.Km) ) +
  geom_histogram(binwidth = 5)




# quakes %>%
#   ggplot(aes(x = Magnitude, color = DayType) ) +
#   geom_density(bw=0.1) +
#   #geom_vline(aes(xintercept=mean(Magnitude), linetype="dashed")) +
#   labs(title = "Earthquakes Magnigudo Density Distribution by Day Type", subtitle = "Dati INGV - 9.458 events registered in Italy: Years 1985-2019, Magnitude 3.1 min", caption ="https://github.com/albertofrison/EarthQuakes") +
#   theme (legend.position = "bottom") 







################################################################################
# WITCH HUNT!!!!

# THIS LATEST SECTION WAS CREATED TO COUNTER MY MOTHER'S THEORY THAT "EARTHQUAKES HAPPEN ALWAYS DURING HOLIDAYS" IT WAS FUN DOING IT ONCE
# #####
# # This whole "dates" section matters only if you want to conduct this witch-hunt analysis, it was fun the first time but not longer...
# # update it if you wish...
# 
# # ==> now, the task is to understand if - like my mother says - EarthQuakes happen more ofter during holidays... so we have to understand which day is a holiday (In Italy) and which not
# # Recurring holidays that happen always the same day every year - easy peasy
# holidays <- format(as.Date(paste ("01/01/", c(1985:2020), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y") #Capodanno
# holidays <- c(holidays,format(as.Date(paste ("06/01/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Epifania
# holidays <- c(holidays,format(as.Date(paste ("25/04/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Liberazione
# holidays <- c(holidays,format(as.Date(paste ("01/05/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Festa del Lavoro
# holidays <- c(holidays,format(as.Date(paste ("02/06/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Festa della Repubblica
# holidays <- c(holidays,format(as.Date(paste ("15/08/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Assunzione aka Ferragosto
# holidays <- c(holidays,format(as.Date(paste ("01/11/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Tutti Santi
# holidays <- c(holidays,format(as.Date(paste ("08/12/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Immacolata
# holidays <- c(holidays,format(as.Date(paste ("25/12/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Natale
# holidays <- c(holidays,format(as.Date(paste ("26/12/", c(1985:2019), sep =""), format = "%d/%m/%Y"), "%d/%m/%Y")) #Santo Stefano
# 
# # Easter holidays that change date every year datasource from here: http://calendario.eugeniosongia.com/datapasqua.htm
# # 1985
# holidays <- c(holidays, format(as.Date("07/04/1985", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("08/04/1985", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("30/03/1986", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("31/03/1986", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("19/04/1987", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("20/04/1987", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("03/04/1988", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("04/04/1988", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("26/03/1989", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("27/03/1989", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# # 1990
# holidays <- c(holidays, format(as.Date("15/04/1990", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("16/04/1990", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("31/03/1991", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("01/04/1991", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("19/04/1992", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("20/04/1992", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("11/04/1993", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("12/04/1993", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("03/04/1994", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("04/04/1994", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("16/04/1995", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("17/04/1995", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("07/04/1996", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("08/04/1996", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("30/03/1997", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("31/03/1997", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("12/04/1998", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("13/04/1998", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("04/04/1999", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("05/04/1999", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# # 2000
# holidays <- c(holidays, format(as.Date("23/04/2000", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("24/04/2000", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("15/04/2001", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("16/04/2001", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("31/03/2002", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua (Easter)
# holidays <- c(holidays, format(as.Date("01/04/2002", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("20/04/2003", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("21/04/2003", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("11/04/2004", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("12/04/2004", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("27/03/2005", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("28/03/2005", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("16/04/2006", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("17/04/2006", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("08/04/2007", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("09/04/2007", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("23/03/2008", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("24/03/2008", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("12/04/2009", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("13/04/2009", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# #2010
# holidays <- c(holidays, format(as.Date("04/04/2010", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("05/04/2010", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("24/04/2011", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("25/04/2011", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("08/04/2012", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("09/04/2012", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("31/03/2013", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("01/04/2013", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("20/04/2014", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("21/04/2014", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("05/04/2015", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("06/04/2015", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("27/03/2016", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("28/03/2016", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("16/04/2017", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("17/04/2017", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("01/04/2018", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("02/04/2018", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# holidays <- c(holidays, format(as.Date("21/04/2019", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasqua
# holidays <- c(holidays, format(as.Date("22/04/2019", format = "%d/%m/%Y"),  "%d/%m/%Y")) #Pasquetta
# 
# 
# # General comment, for each year we identified 12 days of Holidays
# # Now, let's create now a dimension that says if an event happened in a Regular Day or in a Holiday Day
# quakes$DayType <- as.factor (ifelse (format(quakes$Date,"%d/%m/%Y") %in% format(as.Date(holidays, format = "%d/%m/%Y"), "%d/%m/%Y") , "Holiday", "Working Day" ))
# 
# # We can now answer to the first question - how is more likely that an Event happens during a holiday?
# # We know that 12 days out of 365 are holidays
# 12/365 # 3.287%
# 
# # how many events happened during a holiday?
# summary(quakes$DayType)
# 
# sum (quakes$DayType == "Holiday") / nrow (quakes) # [3.77% in for smaller events, 3.68% for greater events]
# # at a first look it doesn't look TOO different statistically speaking, but I will revise this once I will have revised hypotesis testing
# 
# # what if during holidays events were MORE STRONG?
# summary(quakes$Magnitude)
# 
# quakes %>%
#   ggplot(aes(x  = DayType, y = Magnitude, fill = DayType)) +
#   ggtitle("Earthquakes Magnitude by holiday/working day") +
#   theme_minimal() +
#   theme (legend.position = "none") +
#   labs(x = "", y = "Magnitude", subtitle = "Dati INGV - 9.458 events registered in Italy: Years 1985-2019, Magnitude 3.1 min", caption ="https://github.com/albertofrison/EarthQuakes") +
#   geom_boxplot() 
# 
# end of witch hunt section
