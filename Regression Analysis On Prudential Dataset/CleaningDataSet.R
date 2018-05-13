dataset <- read.csv(file="Desktop/ADS/ADS Mid term Project/train.csv", header=TRUE, sep=",")

#We could divided dataset based on data type

## Continous data: 13 columns
#"Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
#"Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
#"Family_Hist_5"

## Continous Dataframe
dataset_continous <- dataset[, c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
                                 "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
                                 "Family_Hist_5")]

## Discrete data
#"Id","Medical_History_1", "Medical_History_10", "Medical_History_15", "Medical_History_24", "Medical_History_32", 
#paste("Medical_Keyword_", 1:48, sep="")

## Discrete Dataframe: 6 columns
dataset_discrete <- dataset[,c("Id","Medical_History_1", "Medical_History_10", "Medical_History_15", "Medical_History_24", "Medical_History_32")]


## Categorial data
#paste("Product_Info_", c(1:3,5:7), sep=""),
#paste("Employment_Info_", c(2,3,5), sep=""),
#paste("InsuredInfo_", 1:7, sep=""),
#paste("Insurance_History_", c(1:4,7:9), sep=""),
#"Family_Hist_1",
#paste("Medical_History_", c(2:9, 11:14, 16:23, 25:31, 33:41), sep=""),
#"Response"

## Categorial Dataframe: 109 columns
dataset_categorial <- dataset[,c(paste("Product_Info_", c(1:3,5:7), sep=""),
                                 paste("Employment_Info_", c(2,3,5), sep=""),
                                 paste("InsuredInfo_", 1:7, sep=""),
                                 paste("Insurance_History_", c(1:4,7:9), sep=""),
                                 paste("Medical_Keyword_", 1:48, sep=""),
                                 "Family_Hist_1",
                                 paste("Medical_History_", c(2:9, 11:14, 16:23, 25:31, 33:41), sep=""),
                                 "Response")]




#####################################################################
## Continous data clean #############################################
## Replace NA by mean ###############################################
#####################################################################

#Check NA
summary(dataset_continous)

#Get means from each col
means <- colMeans(dataset_continous,na.rm = TRUE)

#Replace NA by mean
for (i in 1:ncol(dataset_continous)){ 
  dataset_continous[is.na(dataset_continous[, i]), i] <- means[i]
}

#Check NA
summary(dataset_continous)
anyNA(dataset_continous)

#####################################################################
## Discrete data clean ##############################################
## Replace NA by median #############################################
#####################################################################

#Check NA
summary(dataset_discrete)

##To ensure data accuracy, remove four data col with 75% NA
#Medical_History_32, Medical_History_10, Medical_History_15, Medical_History_24
dataset_discrete <- dataset_discrete[,-c(3:6)]
library(matrixStats)
medians <- colMedians(as.matrix(dataset_discrete),na.rm = TRUE)

#Replace NA by median
for (i in 1:ncol(dataset_discrete)){ 
  dataset_discrete[is.na(dataset_discrete[, i]), i] <- medians[i]
}

#Check NA
summary(dataset_discrete)
anyNA(dataset_discrete)

#####################################################################
## Categorial data clean ############################################
## Using 0-to-N transformation ######################################
#####################################################################

#Check NA, no NA found
summary(dataset_categorial)
anyNA(dataset_categorial)

#Check category number for each col
tmp <- c()
for(i in 1:ncol(dataset_categorial)){
  tmp <- c(tmp, length(unique(dataset_categorial[,i])))
}


#For col only with 2 categories, not necessary to transform
data_transform <- dataset_categorial[tmp<=20&tmp!=2]
#Not necessary to transform "Response"
data_transform <-  subset(data_transform,select = -get("Response"))
numTransform <- ncol(data_transform)

#Create a transform Function 
transformFunc <- function(data){
  for(i in 1:numTransform){
    for(level in unique(data[,i])){
      data[paste0(colnames(data)[i],seq = "_",level)]<- ifelse(data[,i] == level,1,0)
    }
  }
  data <- data[,-c(1:numTransform)]
  return(data)
}
data_transform <- transformFunc(data_transform)


#For col with 20+ categories, we consider them as discrete dataset
dataset_discrete <- cbind(dataset_discrete, dataset_categorial[tmp>20])
dataset_categorial <- cbind(dataset_categorial[tmp==2],data_transform,dataset_categorial["Response"])



#####################################################################
## Combine data #####################################################
## Finish data preprocessing ########################################
#####################################################################

cleaned_dataset <- cbind(dataset_discrete,dataset_continous, dataset_categorial)

#Remove meaningless data: Id
cleaned_dataset <- subset(cleaned_dataset,select = -get("Id"))

#Stored cleaned dataset
write.csv(cleaned_dataset,file = "Desktop/ADS/ADS Mid term Project/cleaned_dataset.csv")
