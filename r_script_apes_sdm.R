#In this script, I will be building ensemble SDM for great apes
#I will be using speciesoccurrence data from field surveys
#The study area is the Dzanga Sangha Protected Areas of the Central African Republic
#Spatial covariates have been created by myself using various GIS layers
#All datasets used in this script can be obtained by contacting me at ginathyuh@gmail.com


#-------------------------------------------------------------------------------------

#I will start by importing libraries required for the SDM modeling

library(sdm)
library(raster)
library(rgdal)
library(sp)

#The next thing is to setup a working directory where all my datasets are found
work_dir <- 'E:/WWF_data/TNS/analysis/'
setwd(work_dir)

#I will now define the methods that will be used in my SDM analysis
methods1 <- c('gam', 'glm') #I intend to use glms and gams in the ensemble modeling
methods1

#---------Data Analysis section------------------------------

# Here, I will start by loading my species occurrence data from my working directory
# the file containes only the X, Y, and Occurrence fields
# the file Occurrence field has the species name, while the X and Y fields include the long and lat coordinates of each occurrence point

species <- readOGR(".", "chimps1")
species <- readOGR(".", "chimps_nest1")

# I will now load my spatial covariates using either of the below approaches
cov <- '.E:/WWF_data/TNS/analysis/' # I am defining my file path with spatial covariates
cov1 <- list.files(path=cov,pattern='asc$',full.names = T) # this approach directly lists all covariates available in the working directory
cov1

#Another much efficient but time consuming approach is to load covariates directly from the file path
closed_forest<-raster("E:\\WWF_data\\TNS\\analysis\\closed_forest.asc")
open_forest<-raster("E:\\WWF_data\\TNS\\analysis\\open_forest.asc")
slope<-raster("E:\\WWF_data\\TNS\\analysis\\slope.asc")
fires<-raster("E:\\WWF_data\\TNS\\analysis\\fires.asc")
law_enforcement<-raster("E:\\WWF_data\\TNS\\analysis\\law_enforcement.asc")
maximum_temperature<-raster("E:\\WWF_data\\TNS\\analysis\\maximum_temperature.asc")
precipitation<-raster("E:\\WWF_data\\TNS\\analysis\\precipitation.asc")


# Next thing is to Stack the predictors; I will use 2 apparoaces as above
preds <- stack(slope, closed_forest,fires,
                     law_enforcement, 
                     maximum_temperature, 
                     open_forest, precipitation)

preds <- stack(cov1)


# I will now build my SDM models. 
# I will first of all load the chimpas background points as csv files, extracted in arcGIS.
bg1 <- 'E:/WWF_data/TNS/analysis/chimps_background.csv'
bg <- read.csv(bg1)	

# Next, I will train my SDM models using the occurrence and background points.
train <- sdmData(Occurrence~., train=species, predictors = preds, bg=list(bg))

getmethodNames()

# Next, I will carryout an sdm model prediction, using the trained model, and two methods: glm and gams 
# I will conduct a five-fold replication of model runs
m <- sdm(Occurrence ~., data=train, methods=methods1, replication=c('sub'), n=5)

# the next thing is to get the model info, and accuracy, using the receiver operator characteristics (roc)
getModelInfo(m)
roc(m)
roc(m, smooth=T)



# I will now build an ensemble model prediction with the glm and gams methods applied in the sdm predictions above
# the ensemble approach aggregates the two sdm models through a weighted model averaging approach
ensemble  <- ensemble(m,preds, setting=list(id=1:12, method='weighted', stat='AUC', opt=2))
ensemble  <- ensemble(m,preds, setting=list(id=1:12, method='weighted', stat='AUC', opt=2),
                   memory.limit(size=56000)) # Here I am setting a memory size to avoid memory limit issues, since the spatial resolution of my predictions are going to be at 30m
plot(ensemble)

# Next, I will save my ensemble model outputs as raster data in my results folder
writeRaster(ensemble, filename='E:/WWF_data/TNS/analysis/results/chimps_prediction.asc', overwrite=TRUE)
writeRaster(enemble, filename="chimps_prediction.tif", format="GTiff", overite=TRUE)


