# Reading the dataset
data = read.csv('Desktop/ADS/ADS Mid term Project/cleaned_dataset.csv',header = TRUE)
#testData = read.csv('Desktop/ADS/ADS Mid term Project/cleaned_test_dataset.csv',header = TRUE)

#splitting the data to train and test
trainData <- data[1:45000,]
testData <- data[(45001:59381),]

#Applying linear regression to the train data set
fit_data = lm(Response~., data=trainData )
summary(fit_data)

#Removing insignificant variables with p-values>0.05
fit_data1 = lm(Response~Medical_History_1 + Medical_History_2 + Product_Info_4 + Ins_Age + Ht + Wt + 
                 BMI + Family_Hist_2 + Family_Hist_3 + Family_Hist_4 + Family_Hist_5 + Product_Info_6 + 
                 Employment_Info_3 + Employment_Info_5 + InsuredInfo_2 + InsuredInfo_5 + 
                 InsuredInfo_6 + InsuredInfo_7 + Medical_Keyword_2 + Medical_Keyword_3 + 
                 Medical_Keyword_6 + Medical_Keyword_9 + Medical_Keyword_11 + Medical_Keyword_12 + 
                 Medical_Keyword_15 + Medical_Keyword_16 + Medical_Keyword_18 + Medical_Keyword_19 + 
                 Medical_Keyword_20 + Medical_Keyword_22 + Medical_Keyword_25 + Medical_Keyword_26 + 
                 Medical_Keyword_27 + Medical_Keyword_29 + Medical_Keyword_30 + Medical_Keyword_31 + 
                 Medical_Keyword_33 + Medical_Keyword_34 + Medical_Keyword_37 + Medical_Keyword_38 + 
                 Medical_Keyword_39 + Medical_Keyword_41 + Medical_Keyword_45 + Medical_Keyword_46 + 
                 Medical_History_4 + Medical_History_22 + Medical_History_38 + Product_Info_2_A7 + 
                 Product_Info_2_A6 + Product_Info_2_A5 + Product_Info_2_A4 + Insurance_History_3_1 + 
                 Insurance_History_4_1 + Medical_History_3_2 + Medical_History_7_1 + 
                 Medical_History_11_3 + Medical_History_11_1 + Medical_History_14_3, data=trainData)
summary(fit_data1)

#Removing insignificant variables with p-values>0.05
fit_data2 = lm(Response~Medical_History_1 + Medical_History_2 + Product_Info_4 + Ins_Age + 
                 Ht + Wt + BMI + Family_Hist_2 + Family_Hist_3 + Family_Hist_4 + 
                 Family_Hist_5 + Employment_Info_3 + Employment_Info_5 + InsuredInfo_2 + 
                 InsuredInfo_5 + InsuredInfo_6 + InsuredInfo_7 + Medical_Keyword_2 + 
                 Medical_Keyword_3 + Medical_Keyword_9 + Medical_Keyword_11 + 
                 Medical_Keyword_12 + Medical_Keyword_15 + Medical_Keyword_16 + 
                 Medical_Keyword_18 + Medical_Keyword_19 + Medical_Keyword_22 + 
                 Medical_Keyword_25 + Medical_Keyword_30 + Medical_Keyword_31 + 
                 Medical_Keyword_33 + Medical_Keyword_34 + Medical_Keyword_37 + 
                 Medical_Keyword_38 + Medical_Keyword_45 + Medical_History_4 + 
                 Medical_History_22 + Medical_History_38 + Product_Info_2_A7 + 
                 Product_Info_2_A6 + Product_Info_2_A5 + Product_Info_2_A4 + 
                 Insurance_History_3_1 + Medical_History_3_2 + Medical_History_7_1 + 
                 Medical_History_11_3 + Medical_History_11_1 ,data=trainData)

summary(fit_data2)
#there are no more insignificant variables, so we consider these as our final independent variables
#plotting the  Linear Regression data
#plot(fit_data2)

# predicting the response on test data for linear regression
pred <- predict(fit_data2, testData[-65])
rpred <- round(pred)
summary(rpred)
plot(rpred)

#including prediction column of linear Regression to the file
predictedData <- data.frame(testData[-65],rpred)
write.csv(predictedData, file = "Desktop/ADS/ADS Mid term Project/Result_linearRegression.csv", row.names = FALSE)

meanSquared <- ( mean( (testData$Response - pred)^2 ) )
meanSquared
rmse <- sqrt(mean( (testData$Response - pred)^2 ))
rmse

trainPred <- round(predict(fit_data2, trainData[-65]))
trainAcc<-length(which(trainData$Response==trainPred))/length(trainData$Response) * 100
trainAcc

testAcc<-length(which(testData$Response==rpred))/length(testData$Response) *100
testAcc

#We apply Multinomial logistic regression on these variables

multi_nom = multinom(Response~Medical_History_1 + Medical_History_2 + Product_Info_4 + Ins_Age + 
                       Ht + Wt + BMI + Family_Hist_2 + Family_Hist_3 + Family_Hist_4 + 
                       Family_Hist_5 + Employment_Info_3 + Employment_Info_5 + InsuredInfo_2 + 
                       InsuredInfo_5 + InsuredInfo_6 + InsuredInfo_7 + Medical_Keyword_2 + 
                       Medical_Keyword_3 + Medical_Keyword_9 + Medical_Keyword_11 + 
                       Medical_Keyword_12 + Medical_Keyword_15 + Medical_Keyword_16 + 
                       Medical_Keyword_18 + Medical_Keyword_19 + Medical_Keyword_22 + 
                       Medical_Keyword_25 + Medical_Keyword_30 + Medical_Keyword_31 + 
                       Medical_Keyword_33 + Medical_Keyword_34 + Medical_Keyword_37 + 
                       Medical_Keyword_38 + Medical_Keyword_45 + Medical_History_4 + 
                       Medical_History_22 + Medical_History_38 + Product_Info_2_A7 + 
                       Product_Info_2_A6 + Product_Info_2_A5 + Product_Info_2_A4 + 
                       Insurance_History_3_1 + Medical_History_3_2 + Medical_History_7_1 + 
                       Medical_History_11_3 + Medical_History_11_1 ,data=trainData)

multi_pred <- predict(multi_nom, testData[-65])

trainMultiPred <- (predict(multi_nom, trainData[-65]))
trainAcc<-length(which(trainData$Response==trainMultiPred))/length(trainData$Response) * 100
trainAcc

testAcc<-length(which(testData$Response==multi_pred))/length(testData$Response) *100
testAcc
