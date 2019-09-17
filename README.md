# R

# Banking Project

# Problem Statement

A Portugese bank is rolling out term deposit for its customers. They have in the past connected to their customer base through phone calls. Results for these previous campaigns were recorded and have been provided to the current campaign manager to use the same in making this campaign more effective.

Challenges that the manager faces are following:

* Customers have recently started to complain that bank’s marketing staff bothers them with irrelevant product calls and this should        immediately stop

* There is no prior framework for her decide and choose which customer to call and which one to leave alone

She has decided to use past data to automate this decision, instead of manually choosing through each and every customer. Previous campaign data which has been made available to her; contains customer characteristics , campaign characteristics, previous campaign information as well as whether customer ended up subscribing to the product as a result of that campaign or not. Using this she plans to develop a statistical model which given this information predicts whether customer in question will subscribe to the product or not. A successful model which is able to do this, will make her campaign efficiently targeted and less bothering to uninterested customers.

# Objective

To Build a machine learning predictive model and predict which customers should be targeted for rolling out term deposits by bank.

Evaluation Criterion: KS score on test data. larger KS, better Model

# Datasets

We have given you two datasets , bank-full_train.csv and bank-full_test.csv . You need to use data bank-full_train to build predictive model for response variable “y”. bank-full_test data contains all other factors except “y”, you need to predict that using the model that you developed and submit your predicted values in a csv files.

# Data dictionary

Variables : Definition: Type and their categories

Each row represnts characteristic of a single customer . Many categorical data has been coded to mask the data, you dont need to worry about their exact meaning

1 - age (numeric)

2 - job : type of job (categorical: “admin.”,“unknown”,“unemployed”,“management”,“housemaid”,“entrepre neur”,“student”, “blue-collar”, “self-employed”,“retired”,“technician”, “services”)

3 - marital : marital status (categorical: “married”,“divorced”,“single”; note: “divorced” means divorced or widowed)

4 - education (categorical: “unknown”,“secondary”,“primary”,“tertiary”)

5 - default: has credit in default? (binary: “yes”,“no”)

6 - balance: average yearly balance, in euros (numeric)

7 - housing: has housing loan? (binary: “yes”,“no”)

8 - loan: has personal loan? (binary: “yes”,“no”)

Related with the last contact of the current campaign:

9 - contact: contact communication type (categorical: “unknown”,“telephone”,“cellular”)

10 - day: last contact day of the month (numeric))

Direct Marketing Campaign: Details and Phase I Tasks

11 - month: last contact month of year (categorical: “jan”, “feb”, “mar”, . . . , “nov”, “dec”)

12 - duration: last contact duration, in seconds (numeric)

other attributes: 13 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)

14 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)

15 - previous: number of contacts performed before this campaign and for this client (numeric)

16 - poutcome: outcome of the previous marketing campaign (categorical: “unknown”,“other”,“failure”,“success”)

Output variable (desired target):

17 - y - has the client subscribed a term deposit? (binary: “yes”,“no”)

# Methodology

We will build a Logistic regression model to predict the response variable “y” (whether the client subscribed to a term deposit or No.)

Step 1: Imputing NA values in the datasets.

Step 2: Data Preparation: Grouping similar category variables and making dummies.

Step 3: Model Building( LOGISTIC REGRESSION )

Step 4: Finding Cutoff value and Perfomance measurements of the model.(Sensitivity, Specificity, Accuracy)

Step 5: Predict the final output on test dataset.(whether the client subscribe or no to term deposit)

Step 6: Creating confusion matrix and finding how good our model is. (by predicting on test_25 dataset)

# Conclusion
Thus the target number of customers to be focused upon for term deposits by the bank are predicted successfully using logistic regression model with an accuracy of 84.15% using KS method.The KS score examined came out to be: 0.72/1.00.Therfore my model and predictions are very good.
