# Comparison Between Logistic Regression and Knn

Packages used included ggplot, caTools, psych, ISOcodes, caret, countrycode, mlbench, and e1071.

Perform the classification below once using Logistic Regression and then using KNN. Which model performs better? Which would you prefer (KNN cannot give you the probability of a user clicking on an ad)?

The goal of the project is to Predict who is likely going to click on the Ad on a website based on the features of a user. Following are the features involved in [this dataset](https://www.kaggle.com/fayomi/advertising) which is obtained from [Kaggle](https://www.kaggle.com). 

Variables

- Daily Time Spent on a Site: Time spent by the user on a site in minutes.
- Age:Customer's age in terms of years.
- Area Income: Average income of geographical area of consumer.
- Daily Internet Usage: Average minutes in a day consumer is on the internet.
- Ad Topic Line: Headline of the advertisement.
- City: City of the consumer.
- Male: Whether or not a consumer was male.
- Country: Country of the consumer.
- Timestamp: Time at which user clicked on an Ad or the closed window.
- Clicked on Ad: 0 or 1 is indicated clicking on an Ad.


KNN Classification Accuracy at k=1: 100%

![alt text](https://raw.githubusercontent.com/ivan-sepulveda/predict-ad-click/master/Knn.png)


Logistic Regression Accuracy: 97.1 %


![alt text](https://raw.githubusercontent.com/ivan-sepulveda/predict-ad-click/master/glmVarImp.png)



GLM Variable Importances

| Feature                  | Weight|
|--------------------------|-------|
| Daily.Internet.Usage     | 9.267 |
| Daily.Time.Spent.on.Site | 9.081 |
| Area.Income              | 7.201 |
| Age                      | 6.629 |
| Location: Americas       | 1.044 |
| Male                     | 1.011 |
| Location: Asia           | 0.164 |
| Location: Oceania        | 0.700 |
| Location: Europe         | 0.090 |
