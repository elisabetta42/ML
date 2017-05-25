#This script aim is to load the dataset that will then be used for PCA
# Requires

require(gmodels)
require(caret)
require(class)
source('/Users/elisabetta/Desktop/ML/Machine_Learning/PROJECTMD/loadImage.R')
#folder <- "/Users/elisabetta/Desktop/ML/trunk/preProcessed/2017/group"
folder <- "/Users/elisabetta/Desktop/ML/trunk/2017/group"
getAllData <- function(dataList){
  id <- data.frame()
  idList <- list()
  for(i in 1:length(dataList))
  {
    if( length(dataList[[i]]) > 0  ){
      for(j in 1:length(dataList[[i]])){
        
        idTemp <- loadSinglePersonsData(100,i - 1,j,folder)
        idList <- append(idList, list(idTemp))
      }
    }
  }
  return(idList)
}

dataList <- list(
  list(1,2,3) #0
  #list(1,2,3)  #1
  #list(1,2,3), #2
  #list(1,2,3,4), #3
  #list(2,3,4), #4
  #list(1,2,3), #5
  #list(1,2,3), #6
  #list(1,2) #7
  #list(1), #8 #omitted member 2 and 3, as member 2 did not do it correctly
  #list(1),#9
  #list(1), #10
  #list(1,2) #11
  #list() #12 #omitted member 1, did not have corners.txt
)

idList <- getAllData(dataList)

# You can now iterate trough the list
dataset <- idList[1]
dataset <- data.frame(dataset)

for(i in 2:length(idList)){
  idTemp <- idList[i]
  idTemp <- data.frame(idTemp)
  dataset <- rbind(dataset, idTemp)
}

