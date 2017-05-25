#data from all the person with a training set that includes data from all persons
#require(gmodels)
#require(caret)
#
require(class)
#source('/Users/elisabetta/Desktop/ML/Machine_Learning/PROJECTMD/load_dataset.R') 

pers_dep_pca<-prcomp(dataset[2:length(dataset[1,])], retx=TRUE, center=TRUE, scale=TRUE)
pers_dep_data<-pers_dep_pca$x[,1:ncol(dataset)-1] #pca object
pers_dep_label<-dataset[,1] #labels from the original dataset
#bind the pca components with labels
pers_dep<-matrix(nrow = length(dataset[,1]), ncol = ncol(dataset))
#conevrting large matrix into data frame matrix - indexing reasoning
pers_dep <- as.data.frame(matrix(unlist(pers_dep), nrow = length(dataset[,1])))
pers_dep[,1]<-pers_dep_label
pers_dep[,-1]<-pers_dep_data
train_sample_num<-round((nrow(dataset)/100)*50)
test_sample_num<-round((nrow(dataset)/100)*50)
#eliminate the last index from the range to use the right number of people
length<-length(person_index)-1
knn_results<-matrix(nrow = ncol(dataset), ncol = length(person_index)-1) #results using 11 PCAs
#########################run for an increasing number of PCAs with a split 70-30#####################
for(i in 3:ncol(dataset)){
    #shaffle data of the person
    pers_dep<-pers_dep[sample(nrow(pers_dep)),]
    #take a certain number of principal component
    pers_dep_dataset<-pers_dep[,1:i]
    #divide between test and train
    pers_dep_test<-pers_dep[1:(nrow(pers_dep_dataset)/2),]
    pers_dep_train<-pers_dep[(row(pers_dep_dataset)/2+1):(nrow(pers_dep_dataset)),]
    pers_dep_prediction<-knn(train = pers_dep_train[,-1], test = pers_dep_test[,-1], 
                               cl = pers_dep_train[,1], k = 1)
    
    confusion <- confusionMatrix(pers_dep_prediction, pers_dep_test[,1])
    knn_results[i,j] <- confusion$overall['Accuracy']
}

#printing results ordered by accuracy
print(knn_results)
result<-rowMeans(knn_results, na.rm = FALSE, dims = 1)
qplot(unlist(1:length(result)),unlist(result), geom = "line",xlab="Number of principal components",ylab="Accuracy (in percentace)") + ggtitle("Accuracy for increasing number of principal components")
max_pca_number<-which.max(result)
