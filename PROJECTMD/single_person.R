#Machine learning project
#problems:
#Data from single persons
# Data from all persons with a training set that includes data from all persons
# Data from all persons where the training set does not include data from the
#persons in the test set
require(gmodels)
require(caret)
require(class)
source('/Users/elisabetta/Desktop/ML/Machine_Learning/PROJECTMD/load_dataset.R') 
#test on small set to tune parameters, use pca and clustering
#Data from single persons:

#get pca object
single_person_pca <- prcomp(dataset[2:length(dataset[1,])], retx=TRUE, center=TRUE, scale=TRUE)
single_person_data<-single_person_pca$x[,1:ncol(dataset)-1] #pca object
single_person_label<-dataset[,1] #labels from the original dataset
#bind the pca components with labels
single_person<-matrix(nrow = length(dataset[,1]), ncol = ncol(dataset))
#conevrting large matrix into data frame matrix - indexing reasoning
single_person <- as.data.frame(matrix(unlist(single_person), nrow = length(dataset[,1])))
single_person[,1]<-single_person_label
single_person[,-1]<-single_person_data
#create a sequence to access data one person per time
person_index<-seq(0, length(dataset[,1]),4000)
#eliminate the last index from the range to use the right number of people
length<-length(person_index)-1
knn_results<-matrix(nrow = ncol(dataset), ncol = length(person_index)-1) #results using 11 PCAs
#########################run for an increasing number of PCAs#####################
for(i in 3:ncol(dataset)){
      for(j in 1:length){
        lower<-person_index[j]+1 #calculate person raw range - this is the lower range bound
        #for the number of persons split 50 - 50 between training and test set
        #save one person per time in individual person
        individual_person<-single_person[lower:person_index[j+1],1:i]
        #shaffle data of the person
        individual_person<-individual_person[sample(nrow(individual_person)),]
        #divide between test and train
        individual_person_test<-individual_person[1:(nrow(individual_person)/2),]
        individual_person_train<-individual_person[(nrow(individual_person)/2+1):(nrow(individual_person)),]
        individual_prediction<-knn(train = individual_person_train[,-1], test = individual_person_test[,-1], 
                                   cl = individual_person_train[,1], k = 1)
        
        confusion <- confusionMatrix(individual_prediction, individual_person_test[,1])
        knn_results[i,j] <- confusion$overall['Accuracy']
      }
}

#printing results ordered by accuracy
print(knn_results)
result<-rowMeans(knn_results, na.rm = FALSE, dims = 1)
qplot(unlist(1:length(result)),unlist(result), geom = "line",xlab="Number of principal components",ylab="Accuracy (in percentace)") + ggtitle("Accuracy for increasing number of principal components")
max_pca_number<-which.max(result)
########################run with different ks################################################

var_k <-c(3, 5, 10, 15, 20, 30, 40, 50, 100, 150)
vark_results<-matrix(nrow = length(var_k), ncol = length(person_index)-1)
for(i in 1:length(var_k)){
      for(j in 1:length){
        lower<-person_index[j]+1 #calculate person raw range - this is the lower range bound
        #for the number of persons split 50 - 50 between training and test set
        #save one person per time in individual person
        individual_person<-single_person[lower:person_index[j+1],]
        #shaffle data of the person
        individual_person<-individual_person[sample(nrow(individual_person)),]
        #divide between test and train
        individual_person_test<-individual_person[1:(nrow(individual_person)/2),]
        individual_person_train<-individual_person[(nrow(individual_person)/2+1):(nrow(individual_person)),]
        individual_prediction<-knn(train = individual_person_train[,-1], test = individual_person_test[,-1], 
                                   cl = individual_person_train[,1], k = var_k[i])
        
        confusion <- confusionMatrix(individual_prediction, individual_person_test[,1])
        vark_results[i,j] <- confusion$overall['Accuracy']
      }
}
#printing results ordered by accuracy
print(vark_results)
vark_results<-rowMeans(vark_results, na.rm = FALSE, dims = 1)
qplot(unlist(var_k),unlist(vark_results), geom = "line",xlab="Increasing values for the parameter k",ylab="Accuracy (in percentace)") + ggtitle("Accuracy for increasing number of principal components")
###########################################################################################
###########################################################################################

