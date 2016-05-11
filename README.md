# PlayersAnalysis

Task of this analysis is to predict whether a player will pay or not. This was the case of imbalanced class problem (95% Not pay, 5% Pay). Usually, in this scenario, working on a model using entire data may lead to good accuracy, but other metrics like precision and recall are fairly less. One should also look at ROC curve. Undersampling of majority class is one method to work on such type of datasets. Penalized Logistic regression and Random Forests have been used for prediction. Penalty can be either ridge or Lasso, but choosing lasso penalty has an advantage that it does variable selection too.
K-means clustering is used to segment different groupings of players, and then propose some campaigns based on that.

Any suggestions or comments are welcomed.

Garima
