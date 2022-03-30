library(readxl)
mydata = read_excel('Airfoil Self-Noise.xlsx')
#library --> library(readxl)
#In case of multiple sheets in an excel --> df=read_excel("dataset2.xlsx",sheet = "dataset")
str(mydata)
head(mydata)

#check missing data:
colSums(is.na(mydata)) #Number of blank spaces in each colmun
table(is.na(mydata))   #Dont use only 'is.na(mydata)' as it just gives True or False values
dim(mydata)

table(complete.cases(mydata))  #Measures rows only having numeric values
table(is.na(mydata))          #Measures each and every element in table

#Remove missing data (na values):
mydata2 = na.omit(mydata)
dim(mydata2)

#verify again if there's any missing data:
table(complete.cases(mydata2))  #Measures rows only having numeric values
table(is.na(mydata2))  

#Check summary
str(mydata2)
summary(mydata2)

#Draw scatterplot to check relation
#install.packages("scatterplot3d")
#plot(~., data = mydata2)

##Density plot - Check if the response variable is close to normality
plot(density(mydata2$`Frquency(Hz)`), main = 'DensityPlot', xlab = 'FrequencyOfFoil', ylab = 'PlotFrequency')
polygon(density(mydata2$`Frquency(Hz)`), col = 'red')

plot(density(mydata2$Angle_of_Attack), main = 'DensityPlot', xlab = 'Angle_of_Attack', ylab = 'Frequency')
polygon(density(mydata2$Angle_of_Attack), col = 'violet')

plot(density(mydata2$Chord_Length), main = 'DensityPlot', xlab = 'Chord_Length', ylab = 'Frequency')
polygon(density(mydata2$Chord_Length), col = 'blue')

plot(density(mydata2$Free_stream_velocity), main = 'DensityPlot', xlab = 'Free_stream_velocity', ylab = 'Frequency')
polygon(density(mydata2$Free_stream_velocity), col = 'yellow')

plot(density(mydata2$Displacement), main = 'DensityPlot', xlab = 'Displacement', ylab = 'Frequency')
polygon(density(mydata2$Displacement), col = 'green')

plot(density(mydata2$Sound_pressure_level), main = 'DensityPlot', xlab = 'Sound_pressure_level', ylab = 'Frequency')
polygon(density(mydata2$Sound_pressure_level), col = 'blue')

#Scatter plots can help visualize any linear relationships between the dependent (response) variable and independent (predictor) variables.
#Ideally, if you are having multiple predictor variables, a scatter plot is drawn for each one of them against the response,
#along with the line of best as seen below

scatter.smooth(x = mydata2$`Frquency(Hz)`, y = mydata2$Sound_pressure_level)

scatter.smooth(x = mydata2$Angle_of_Attack, y = mydata2$Sound_pressure_level)

scatter.smooth(x = mydata2$Chord_Length, y = mydata2$Sound_pressure_level)

scatter.smooth(x = mydata2$Free_stream_velocity, y = mydata2$Sound_pressure_level)

scatter.smooth(x = mydata2$Displacement, y = mydata2$Sound_pressure_level)

#TO find correlation between all the variables
cor(mydata2)




#Finding outliers and cleaning them
boxplot(mydata2$`Frquency(Hz)`, main = 'Frquency(Hz)', 
        sub = paste('Outlier rows: ', boxplot.stats(mydata$`Frquency(Hz)`)$out))

freqbox = boxplot(mydata2$`Frquency(Hz)`)

freqbox

boxplot(mydata2$Angle_of_Attack, main = 'Angle_of_Attack',
        sub = paste('Outlier rows:', boxplot.stats(mydata2$Angle_of_Attack)$out))

boxplot(mydata2$Chord_Length, main = 'Chord_Length', 
        sub = paste('Outlier rows:', boxplot.stats(mydata2$Chord_Length)$out))

boxplot(mydata2$Free_stream_velocity, main = 'Free_stream_velocity', 
        sub = paste('outlier rows:', boxplot.stats(mydata2$Free_stream_velocity)$out))

boxplot(mydata2$Displacement, main = 'Displacement', 
        sub = paste('Outlier rows:', boxplot.stats(mydata2$Displacement)$out))


#Getting overlapped values in boxplot in R plot


#To remove the values overlapping in R-plot:
#(But its not working)
# install.packages("FField") 
# library(FField) 
# FFieldPtRepDemo()
# 
# 
# #Googled it and found following suggestion on it
# #(But its not working even after installing the given packages):
#   
# install.packages("FField", type = "source")
# library(FField)
# install.packages("ggplot2")
# install.packages("gridExtra")
# FFieldPtRepDemo()

#How to remove outliers in the data:
#In this data we dont have outliers in chord length and velocity.
#So we will calculate outliers in other variables

#(1) Frequency:

IQR(mydata2$`Frquency(Hz)`)       #To find IQR value in the frequency
quantile(mydata2$`Frquency(Hz)`)  #To find Q1, Q3 in the frequency

head(sort(mydata2$`Frquency(Hz)`))  #To check smallest value
tail(sort(mydata2$`Frquency(Hz)`))  #To check largest value


MinCutoffFreq = (800 - 1.5*3200)  #Min = (Q1 - 1.5*IQR)
MinCutoffFreq

MaxCutoffFreq = (4000 + 1.5*3200)   #Max = (Q3 + 1.5*IQR)
MaxCutoffFreq

boxplot(mydata2$`Frquency(Hz)`)$out

#So, we will remove freq>8800 while taking subset.


#(2) Angle of Atttack:

IQR(mydata2$Angle_of_Attack)  #To find IQR value in the angle of attack
quantile(mydata2$Angle_of_Attack) #To find Q1, Q3 in the angle of attack


head(sort(mydata2$Angle_of_Attack)) #To check smallest value
tail(sort(mydata2$Angle_of_Attack)) #To check largest value



MinCutoffAngle = (2.0 - 1.5 * 7.9)   #Min = (Q1 - 1.5*IQR)
MinCutoffAngle
MaxCutOffAngle = (9.9 + 1.5 * 7.9)  #Max = (Q3 + 1.5*IQR)
MaxCutOffAngle


boxplot(mydata2$Angle_of_Attack)$out



#So, we will remove Angle of attack>21.75 while taking subset.


#(3) Displacement:

IQR(mydata2$Displacement) #To find IQR value in the angle of attack
quantile(mydata2$Displacement)  #To find Q1, Q3 in the displacement

head(sort(mydata2$Displacement))
tail(sort(mydata2$Displacement))

MinCutoffDisp = (0.002535110 - 1.5*0.01251269)  #Min = (Q1 - 1.5*IQR)
MinCutoffDisp
MaxCutOffDisp = (0.015047800 + 1.5*0.01251269)  #Max = (Q3 + 1.5*IQR)
MaxCutOffDisp
boxplot(mydata2$Displacement)$out


#So, we will remove Displacement > 0.03381684 while taking subset.

#Following steps will remove all the outliers and will give clean data:

dim(mydata2)
summary(mydata2)

mydata3 = subset(mydata2, mydata2$`Frquency(Hz)`< 8800 & mydata2$Angle_of_Attack < 21.75 & mydata2$Displacement < 0.03381684 )

dim(mydata3) #So number of rows decreased by 248, excel also giving same results by filtering

summary(mydata3)

cor(mydata3)



#Build linear model:

#(1) By considering outliers:
lm1 = lm(Sound_pressure_level ~ `Frquency(Hz)` + Angle_of_Attack + Chord_Length
         + Free_stream_velocity + Displacement, data = mydata2)
summary(lm1)

#(2) Without considering outliers:
lm2 = lm(Sound_pressure_level ~ `Frquency(Hz)` + Angle_of_Attack + Chord_Length
         + Free_stream_velocity + Displacement, data = mydata3)
summary(lm2)

#AIC and BIC
AIC(lm2)
BIC(lm2)

#Now in above models we can check that even if we remove any single variable,adjusted R - squared value decreases. So better to keep all variables.
#R-squared we are getting is low in both models. But the problem is adjusted R-squared value is greater for lm1 than lm2.

plot(lm2) #Residual Plots

#Lets try t check whether we can increase R squared by taking logarithamic value of sound pressure level.

lm3 = lm(log(Sound_pressure_level) ~ ., data = mydata3)
summary(lm3)  #So taking log value of sound pressure, increases adjusted R slightly


#AIC and BIC
AIC(lm3)  
BIC(lm3)


#Predicting Linear Models

#Step 1: Create the training (development) and test (validation) data samples from original data
# Create Training and Test data -

set.seed(100)  # setting seed to reproduce results of random sampling
Index <- sample(1:nrow(mydata3), 0.7*nrow(mydata3))  # row indices for training data
trainData <- mydata3[Index, ]  # model training data
testData  <- mydata3[-Index, ]   # test data

summary(trainData)
summary(testData)

dim(trainData)
dim(testData)

lm4 = lm((Sound_pressure_level) ~ ., data = trainData)
SoundPred = predict(lm4, testData)
SoundPred

#Step 3: Review
summary (lm4)  # model summary
AIC (lm4)  # Calculate akaike information criterion


#Step 4: Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=testData$Sound_pressure_level, predicteds=SoundPred))  # make actuals_predicteds dataframe.
actuals_preds2
correlation_accuracy <- cor(actuals_preds)  # 70.49%
correlation_accuracy

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy 

# => 97.30%, min_max accuracy


mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape

# 2.75% error



#Lets try by replacing outliers with corresponding max values 
#Following steps will replace the outliers with their corresponding max/min values:

mydata4 = mydata2

dim(mydata4)
summary(mydata4)

class(mydata4$Displacement)

mydata4$`Frquency(Hz)`[which(mydata4$`Frquency(Hz)`> 8800) ]=  8800

mydata4$Angle_of_Attack[which(mydata4$Angle_of_Attack> 21.75)] = 21.75

mydata4$Displacement[which(mydata4$Displacement > 0.03381684)] = 0.03381684

dim(mydata4)

lm5 = lm(Sound_pressure_level ~., data = mydata4)
summary(lm5)


#Predicting Linear Models with capped outlier values

#Step 1: Create the training (development) and test (validation) data samples from original data
# Create Training and Test data -

set.seed(100)  # setting seed to reproduce results of random sampling
Index2 <- sample(1:nrow(mydata4), 0.7*nrow(mydata4))  # row indices for training data
trainData2 <- mydata4[Index2, ]  # model training data
testData2  <- mydata4[-Index2, ]   # test data

summary(trainData2)
summary(testData2)

dim(trainData2)
dim(testData2)

lm6 = lm((Sound_pressure_level) ~ ., data = trainData2)
SoundPred2 = predict(lm6, testData2)
SoundPred2

#Step 3: Review
summary (lm6)  # model summary
AIC (lm6)  # Calculate akaike information criterion


#Step 4: Calculate prediction accuracy and error rates
actuals_preds2 <- data.frame(cbind(actuals2=testData2$Sound_pressure_level, predicteds2=SoundPred2))  # make actuals_predicteds dataframe.
actuals_preds2
correlation_accuracy2 <- cor(actuals_preds2)  
correlation_accuracy2
  
min_max_accuracy2 <- mean(apply(actuals_preds2, 1, min) / apply(actuals_preds2, 1, max))  
min_max_accuracy2 

# => 97.31%, min_max accuracy

mape2 <- mean(abs((actuals_preds2$predicteds2 - actuals_preds2$actuals2))/actuals_preds2$actuals2)  
mape2

# => 2.75%, mape  

