# Code to estimate the temporal overlap between species 

# package "overlap" to face the temporal dimension in communities

install.packages("overlap")
library(overlap)

? overlap # to open file about information 
data (kerinci)

# excercise: show the first 6 rows of kerinci

head (kerinci) 

# to have information about each column type summary

summary (kerinci)

# convert the time in hour of the day, the number we see is a range from 0 to 1.
# let's create a new field that is the time in radiance

kerinci$Timecirc <- kerinci$Time * 2 * pi 
# $ to assign, * for multiplication, 
# $ was necessary to create a new column

names(kerinci) # see Timecirc now

# now we want to set only the tiger data with symbol "==" to select only tiger path

# [] to select
# the "," to close the query

tiger <- kerinci[kerinci$Sps=="tiger",] 
head (tiger)

# now we do a density plot together
# for the density plot we relate the time to the tigers 
# first we set time for tiger, it is in the coloun Timecirc

tigertime <- tiger$Timecirc # it takes the time only of the tigers 
densityPlot(tigertime) 

# let's do the same for another species and then overlap 
# macaque

# exercise: select the data for the macaque in the kerinci set and assign them to a new object 

macaque <- kerinci[kerinci$Sps=="macaque",] # == means equal, and != means not equal 
head(macaque)

# exercise: select the time for the macaque data and make a density plot 

macaquetime <- macaque$Timecirc
densityPlot(macaquetime)

# now we can overlap the 2 graphs to see how much they're temporarly overlapped
# function overlap

overlapPlot(tigertime, macaquetime) 
