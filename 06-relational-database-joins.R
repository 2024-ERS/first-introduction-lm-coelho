# Combine cockle data with elevation data using a relational database approach 

# Schiermonnikoog transect

#restore and load library
renv::restore()
library(tidyverse) #ggplot2, dplyr

# clear everything in memory
rm(list=ls())


# load the elevation data and show the first 10 records of the dataset
elevdat<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=1550309563&single=true&output=csv")
elevdat
view(elevdat)

# plot the change in transect  elevation along the transect, using a separate graph for each for each year

elevdat |> #same as using (data= elevdat...)
  ggplot(aes(x=TransectPoint_ID, y=elevation_m)) +
  geom_line(mapping = aes(col=year))


# plot the change in transect  elevation along the transect, using a separate line color for each year 

#to separate colours use FACTOR!
elevdat<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=1550309563&single=true&output=csv") |> 
  dplyr::mutate(year=factor(year)) # MAKE YEAR AS FACTOR !!

elevdat |> #same as using (data= elevdat...)
  ggplot(aes(x=TransectPoint_ID, y=elevation_m)) +
  geom_line(mapping = aes(col=year)) + 
  facet_wrap(~year) #to seperate colours in different plots


# Extract the data for 2017 in a new tibble, keep only variables distance_m and elevation
# omit records where Distance_ID is missing (NA)

elevdat2017 <- elevdat |>
  dplyr::filter(year=="2017") |> #filter only 2017 data
  dplyr::select(TransectPoint_ID, elevation_m) |> #select variables (colums)
  dplyr::filter(!is.na(TransectPoint_ID)) #exclude NA on Transect ID



# read the cockle data 
# keep only the data for 2017, 
# omit observations (Obs_ID) 468 and 1531
# calculate the mean number of cockles and mean size for each distance
cdat2017 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSpormjGiM4uWMTIZYm9Upt5j1Ige_Wu9CrhGGdvjXwxmHP2R6S7JDyEmiGTn3fXihl5s5yBn_sdo8h/pub?gid=1538766002&single=true&output=csv") |>
  dplyr::filter(year=="2017", CockleObs_ID!= 468, CockleObs_ID!=1531) |> #take out outliers
  dplyr::group_by(TransectPoint_ID) |> #make groups of transect point
  dplyr::summarize(n_obs=n(), #using n of observation
                   avg_l=mean(length_mm, na.rm=T), #calculate mean
                   sd_l=sd(length_mm, na.rm=T), #calculate size
                   se_l=sd_l/sqrt(n_obs)) #calculate deviation
print(cdat2017)


# plot (with a line and points)  how the number of cockles changes with distance along the transect
##### merge the cockle and elevation data into a single table you call "combidat"

combidat <- dplyr::left_join(elevdat2017, cdat2017, by="TransectPoint_ID") |>
  dplyr::mutate(n_obs=tidyr::replace_na(n_obs, 0)) #replace NA by 0
#to combine two data sets using the common variable, TransectPoint_ID. Note: longest file first!!


combidat |>
  ggplot(aes(x=elevation_m, y=n_obs)) +
  geom_point() + 
  geom_line() #cockles like low elevation

#fit a linear regression
combidat |>
  ggplot(aes(x=elevation_m, y=n_obs)) +
  geom_point() + 
  geom_smooth(method = "loess") #cockles like low elevation


# using Distance_ID as the common variable between the two tables

# show in a plot how cockle density changes with elevation

# fit a linear regression



# predicted at 0.5 m (x)
# y = b0 + b1x   (b0 is intercept and b1 is the slope, x is elevation, y is no cockles

# show this model as a line in ggplot, with the confidence interval

# fit a better model, using a loess smoother
# show this model in ggplot

##### plot  how the size (as mean length) of cockles changes with  elevation along the transect
# omit the observations where length is NA (because no cockles were measures)
# fit a quadratic model (second-order polynomial)
# show for each point also the standard errors
# add appropriate labels for the x and y axis 

