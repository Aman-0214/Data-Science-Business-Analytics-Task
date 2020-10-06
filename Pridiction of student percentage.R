#import data
Studentscores <- read.csv("C:/Users/lenovo/Downloads/Scores_hours.csv",header = TRUE,sep = ",")
View(Studentscores)


#Analysis of data
dim(Studentscores)
head(Studentscores)
tail(Studentscores)
str(Studentscores)
summary(Studentscores)
cor(Studentscores)

#Histogram of Hours
hist(Studentscores$Hours)

#Histogram of Scores
hist(Studentscores$Scores)


#Scatter plot is use to visualise the relationship between Hours of study and Scores obtained

plot(x = Studentscores$Hours,y = Studentscores$Scores,
     xlab = "Hours",
     ylab = "Scores",
     xlim = c(0.5,10),
     ylim = c(10,100),		 
     main = "Hours vs Scores")


#Install package
install.packages("Rmisc")
library(ggplot2)
library(Rmisc)

#Boxplot is use to visualise the outliers in dataset.
boxplot <-  ggplot(data = Studentscores, aes(x=Hours, y=Scores)) + 
  scale_y_log10() +
  geom_point(aes(color=Hours), alpha=1.2) +
  geom_boxplot(outlier.size=4, outlier.colour='Blue', alpha=.1)
boxplot

#Linear regression model
Studentscoreslm <- lm(Scores~Hours, data = Studentscores)
Studentscoreslm
summary(Studentscoreslm)

#Prediction of scores while hours is given 9.25 with Simple linear regression method
Hours<- data.frame(Hours=c(9.25))
Prediction_Data <- cbind(Hours, Score= predict(Studentscoreslm,Hours))
Prediction_Data


#Splitting Dataset into TrainingData and TestData
set.seed(19) # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(Studentscores), 0.7*nrow(Studentscores))  
trainingData <- Studentscores[trainingRowIndex, ]  
testData  <- Studentscores[-trainingRowIndex, ] 




#Build linear regression model
#Fit the model on training data and predict Scores on test data
Studentscores.lm <- lm(Scores ~ Hours, data=trainingData)  
Predicted_Scores <- predict(Studentscores.lm, testData)  
summary(Studentscores.lm)

#Calculate prediction accuracy and error rates
actualspreds <- data.frame(cbind(actuals=testData$Scores, predicteds=Predicted_Scores)) # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actualspreds)  
head(actualspreds)

# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(actualspreds, 1, min) / apply(actualspreds, 1, max)) 
min_max_accuracy 

# MAPE Calculation
mape <- mean(abs((actualspreds$predicteds - actualspreds$actuals))/actualspreds$actuals)
mape

# Visualising the Training set results 
ggplot() + geom_point(aes(x = trainingData$Hours,  
                          y = trainingData$Scores), colour = 'blue') +
  geom_line(aes(x = trainingData$Hours, 
                y = predict(Studentscores.lm, newdata = trainingData)), colour = 'green') +
  ggtitle('Scores vs Hours (TrainingData)') +
  xlab('Hours of Study') +
  ylab('Scores') 



# Visualising the Test set results 
ggplot() + geom_point(aes(x = testData$Hours,  
                          y = testData$Scores), colour = 'red') +
  geom_line(aes(x = trainingData$Hours, 
                y = predict(Studentscores.lm, newdata = trainingData)), colour = 'blue') +
  ggtitle('Scores vs Hours (TestData)') +
  xlab('Hours of Study') +
  ylab('Scores')


#Prediction of Score of a given hours of Study
Hours<- data.frame(Hours=c(9.25))
Prediction <- cbind(Hours, Score= predict(Studentscores.lm,Hours))
Prediction
 
