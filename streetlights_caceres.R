# clearing up memory

rm(list = ls())

# setting wp working directory

setwd("~/Dropbox")

# reading data from EU Data portal website

M <- read.csv(url(
  "http://opendata.caceres.es/GetData/GetData?dataset=om:Farola&format=csv"), 
  header=TRUE)

# loading up required packages

library(tidyverse)
library(ggmap)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(party)
library(partykit)
library(caret)
library(rattle)

# renaming column headers from Spanish to English

colnames(M) <- c(
  "url", 
  "functioning", 
  "protection", 
  "lightType", 
  "materialS", 
  "long", 
  "power", 
  "lampsType", 
  "lat", 
  "height", 
  "supportType", 
  "circuits",
  "materialL")

# reading header and looking and summary and structure

head(M)
str(M)
summary(M)


# taking longitude and latitude values in a different data frame to extract map

z <- subset(M, select = c("long", "lat"))


# using another approach using Hadley's ggmap paper approach
# didnt use markers or path arguments

x <- get_googlemap('càceres', zoom=15, scale = 2)
y <- ggmap(x, extent = 'device')

# light Type

y + geom_point(data = M, 
               mapping = aes(x = long, y = lat, color = lightType), 
               size = 0.75)

# lamps Type

y + geom_point(data = M, 
               mapping = aes(x = long, y = lat, color = lampsType), 
               size = 0.75)

# height 

y + geom_point(data = M, 
               mapping = aes(x = long, y = lat, color = height), 
               size = 0.75) + 
  scale_colour_gradient(low = "blue", high = "red", space = "Lab")

# support type 

y + geom_point(data = M, 
               mapping = aes(x = long, y = lat, color = supportType), 
               size = 0.75)

# materialS

y + geom_point(data = M, 
               mapping = aes(x = long, y = lat, color = materialS), 
               size = 0.75)

# materialL

y + geom_point(data = M, 
               mapping = aes(x = long, y = lat, color = materialL), 
               size = 0.75)

# circuits

y + geom_point(data = M, 
               mapping = aes(x = long, y = lat, color = circuits), 
               size = 0.75) +
  scale_colour_gradient(low = "blue", high = "red", space = "Lab")

# functioning

y + geom_point(data = M, 
               mapping = aes(x = long, y = lat, color = functioning), 
               size = 0.75)

# protection

y + geom_point(data = M, 
               mapping = aes(x = long, y = lat, color = protection), 
               size = 0.75)


# decision trees using 'rpart'

form <- as.formula(lightType ~ lampsType + height + supportType + materialL + circuits)
tree_test <- rpart(form, M)
prp(tree_test)
fancyRpartPlot(tree_test)

