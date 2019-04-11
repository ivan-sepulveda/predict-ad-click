# COMPARISON BETWEEN LOGISTIC AND KNN
# Perform the classification below once using Logistic Regression and then using KNN. Which model performs better? Which would you prefer (KNN cannot give you the probability of a user clicking on an ad)?
# The goal of the project is to Predict who is likely going to click on the Ad on a website based on the features of a user. 
# Daily Time Spent on a Site: Time spent by the user on a site in minutes.
# Age:Customer's age in terms of years.
# Area Income: Average income of geographical area of consumer.
# Daily Internet Usage: Average minutes in a day consumer is on the internet.
# Ad Topic Line: Headline of the advertisement.
# City: City of the consumer.
# Male: Whether or not a consumer was male.
# Country: Country of the consumer.
# Timestamp: Time at which user clicked on an Ad or the closed window.
# Clicked on Ad: 0 or 1 is indicated clicking on an Ad.

library(class)
library(caTools)
library(psych)
library(ISOcodes)

knn_data <- read.csv('advertising.csv')
colnames(knn_data)
# Cheching if there's any missing data.
missingData <- knn_data[rowSums(is.na(knn_data)) > 1,]
if(nrow(missingData) > 0){ # If missingData df has 1 or more rows, there's NA values
  warning("One or more of your columns has NA values.")
} else{ # If not, then I clean up the 
    rm(missingData)
  }

knn_data <- knn_data[ , -which(names(knn_data) %in% c("Timestamp"))]
knn_data <- knn_data[ , -which(names(knn_data) %in% c("City"))]
knn_data <- knn_data[ , -which(names(knn_data) %in% c("Country"))]
knn_data <- knn_data[ , -which(names(knn_data) %in% c("Ad.Topic.Line"))]
targetVarColNum = as.numeric(which( colnames(knn_data)=='Clicked.on.Ad' ))
dailyTimeColNum = as.numeric(which( colnames(knn_data)=='Daily.Time.Spent.on.Site' ))
areaIncomeColNum = as.numeric(which( colnames(knn_data)=='Area.Income' ))
dailyInternetColNum = as.numeric(which( colnames(knn_data)=='Daily.Internet.Usage' ))
ageColNum = as.numeric(which( colnames(knn_data)=='Age' ))
scaled_features <- c(dailyTimeColNum, areaIncomeColNum, dailyInternetColNum, ageColNum)

# Splitting the dataset into training and test sets
set.seed(42) # 42 is the randomest of all random numbers 
split = sample.split(knn_data$Clicked.on.Ad, 0.75)
training_set = subset(knn_data, split= TRUE)
test_set = subset(knn_data, split = FALSE)

# Feature Scaling (does not apply to binary values)
training_set[scaled_features] = scale(training_set[scaled_features])
test_set[scaled_features] = scale(training_set[scaled_features])
# Females will be 0, Males will be 1 (data's decision, not mine)



k_values <- list()
acc_values <- list()
largest_k <- 15
best_k <- largest_k
best_accuracy <- 0
current_accuracy <- 0

for(i in 1:largest_k){
  # print(paste("k = ", i))
  y_predict = knn(training_set[,-targetVarColNum], test_set[,-targetVarColNum], cl = training_set[,targetVarColNum], k=i)
  con_matrix = table(test_set[,targetVarColNum], y_predict)
  con_matrix
  true_zero = as.numeric(con_matrix[1, 1])
  false_zero = as.numeric(con_matrix[1, 2])
  true_one = as.numeric(con_matrix[2, 2])
  false_one = as.numeric(con_matrix[2, 1])
  accuracy = (true_one + true_zero)/(true_one + true_zero + false_one + false_zero)
  accuracyPercentage = accuracy*100
  current_accuracy <- accuracyPercentage
  if (current_accuracy > best_accuracy){
    best_accuracy <- current_accuracy
    best_k <-1
  }
  errorPercentage = 100 -accuracy*100
  # print(paste("Accuracy = ", round(accuracyPercentage, digits = 1)))
  # print(paste("Error = ", round(errorPercentage, digits = 1)))
  k_values <- c(k_values, i)
  acc_values <- c(acc_values, accuracy)
  # print(paste("Best k = ", best_k))
}

y_predict = knn(training_set[,-targetVarColNum], test_set[,-targetVarColNum], cl = training_set[,targetVarColNum], k=best_k)
knn_con_matrix = table(test_set[,targetVarColNum], y_predict)
knn_con_matrix
true_zero = as.numeric(knn_con_matrix[1, 1])
false_zero = as.numeric(knn_con_matrix[1, 2])
true_one = as.numeric(knn_con_matrix[2, 2])
false_one = as.numeric(knn_con_matrix[2, 1])
accuracy = (true_one + true_zero)/(true_one + true_zero + false_one + false_zero)
print(paste("By iterating k values of 1 through", largest_k, "I found that optimal k is", best_k))
print(paste("knn Accuracy: ", toString(accuracy*100) ))
print(paste("knn Error: ", toString( 100 - accuracy*100) ))

# Uncomment next three lines to plot different values of k
ta <- table(unlist(k_values), unlist(acc_values))
a <- data.frame(ta)
plot(unlist(k_values),unlist(acc_values))

library(countrycode)
logRegData <- read.csv('advertising.csv')

logRegData <- logRegData[rowSums(is.na(logRegData)) == 0, ]
logRegData$continent <- countrycode(logRegData$Country,
                            origin = "country.name",
                            destination = "continent")
logRegData$continent <- factor(logRegData$continent, 
                               levels = c("Africa", "Antarctica", "Asia", "Europe", "Americas", "Oceania"), 
                               labels = c(1, 2, 3, 4, 5, 6))
# To encode countries, uncomment line below
# logRegData$Country <- countrycode(logRegData$Country, "country.name", "iso3n")
logRegData <- logRegData[rowSums(is.na(logRegData)) == 0, ]
logRegData <- logRegData[ , -which(names(logRegData) %in% c("Country"))]

logRegData <- logRegData[ , -which(names(logRegData) %in% c("Timestamp"))]
logRegData <- logRegData[ , -which(names(logRegData) %in% c("City"))]
logRegData <- logRegData[ , -which(names(logRegData) %in% c("Ad.Topic.Line"))]
# logRegData$Clicked.on.Ad = factor(relevantData$Class, levels=c('Bad', 'Good'), labels = c(0, 1))
targetVarColNum = as.numeric(which( colnames(logRegData)=='Clicked.on.Ad' ))
set.seed(42)
split = sample.split(logRegData$Clicked.on.Ad, 0.75)
training_set = subset(logRegData, split= TRUE)
test_set = subset(logRegData, split = FALSE)
classifier = glm(formula = Clicked.on.Ad ~ ., family = binomial, data=training_set)
prob_predict = predict(classifier, type = 'response', logRegData1 = test_set[-targetVarColNum])
Class_predict = ifelse(prob_predict > 0.5, 1, 0)
logRegConMatrix = table(test_set[, targetVarColNum], Class_predict)
logRegConMatrix
results_matrix = data.matrix(logRegConMatrix)
LR_true_zero = as.numeric(results_matrix[1, 1])
LR_false_zero = as.numeric(results_matrix[1, 2])
LR_true_one = as.numeric(results_matrix[2, 2])
LR_false_one = as.numeric(results_matrix[2, 1])
LR_accuracy = (LR_true_one + LR_true_zero)/(LR_true_one + LR_true_zero + LR_false_one + LR_false_zero)
print("Logistic Regression Confusion/Clarity Matrix)")
logRegConMatrix
print(paste("Logistic Regression Accuracy: ", toString(LR_accuracy*100) ))



print("Logistic Regression Confusion Matrix (AKA Clarity Matrix)")
print("Knn Confusion/Clarity Matrix)")
knn_con_matrix
print(paste("knn Accuracy with k = ", best_k, ":",toString(accuracy*100) ))

# Knn performs better at 100%, but I'd use logistic regression.
# Although knn is always more accurate at k = 1, it is computational expensive
# In addition, Knn can not predit probability.
# The data would have taking extensive time to run if it was a "big data" dataset.
# Therefore, I would stick to logistic regression because it is more scaleable.
# Also, after grouping countries into continents, I got 99.067% accuracy.
