Problem set \#3: Hodgepodge
================
Zhuo Leng

``` r
library(tidyverse)
library(forcats)
library(broom)
library(modelr)
library(stringr)
library(rcfss)
library(haven)
library(car)
library(lmtest)
library(coefplot)
library(RColorBrewer)
library(GGally)
library(Amelia)
library(MVN)
```

Part 1:Regression diagnostics
-----------------------------

Question 1
==========

Test the model to identify any unusual and/or influential observations. Identify how you would treat these observations moving forward with this research. Note you do not actually have to estimate a new model, just explain what you would do. This could include things like dropping observations, respecifying the model, or collecting additional variables to control for this influential effect.

By using bubble plot, we get to know the unusual values with high leverage, discrepancy or influence.We notice that the majority of observations are in lower left corner of the graph.Next step, in order to get more understanding of outliers, we use histograms.

From the histogram, we could classify the point by unusual or influential. Different age people seems have different proportion of outliers. Most of the outliers with high biden score are Democrate(party affiliation) from both genders. The proportion of unusual or influential of male are larger than that of female. Also, republican get the largest proportion of unusual or influential.

Next step: In next step of research, I will try to add party afilliation to my model because different parties seems hace different proportion of unual or influential point. In addition, we will also consider add interaction terms between age and party to control the effect of outlier.

Question 2
==========

Test for non-normally distributed errors. If they are not normally distributed, propose how to correct for them.

From the quantile-comparison plot, graphing for each observation its studentized residual on the yy axis and the corresponding quantile in the tt-distribution on the xx axis. There are observations fall outside this dash line range,so this plot indicate there exist non-normally distributed errors.

From the density plot of the studentized residuals, we can also see that the residuals are skewed. In this case we need to use power and log transformations to correct this problem. I try 2 power transformation first.

From summary table, some varibales are more significant. However, from the quantile-comparison plot, There still observations fall outside this dash line range,and indicate there still exist non-normally distributed errors. Then I could try to use power 3, 4, -2 for examples to test how to make the error of linear model more normally distributed.

Question 3
==========

Test for heteroscedasticity in the model. If present, explain what impact this could have on inference.

From the Homoscedastic varieance of error terms plot, we could know that heteroscedasticity exsit in the model.

From the bptest result, it could verify our from indusction plot above.

We see some mild changes in the estimated parameters, but reductions in the standard errors. We need to use more robust estimation procedure: Huber-White standard errors .

Question 4
==========

Test for multicollinearity. If present, propose if/how to solve the problem.

We can use VIF to take a look at our coefficient inflation factor.From the value of vif, there's no multicollinearity in the model.

Part 2:Interaction terms
------------------------

Question 1
==========

Evaluate the marginal effect of age on Joe Biden thermometer rating, conditional on education. Consider the magnitude and direction of the marginal effect, as well as its statistical significance.

We could see the marginal effect of age conditional on education from the plot. And also from the Hypothesis test we could know from the p-value which is under 0.05, the marginal effect is significant.

Question 2
==========

Evaluate the marginal effect of education on Joe Biden thermometer rating, conditional on age. Consider the magnitude and direction of the marginal effect, as well as its statistical significance.

We could see the marginal effect of education conditional on age from the plot. And also from the Hypothesis test we could know from the p-value which is under 0.05, the marginal effect is significant. \#\# part 3 Missing data \#\# \# Question \# This time, use multiple imputation to account for the missingness in the data. Consider the multivariate normality assumption and transform any variables as you see fit for the imputation stage. Calculate appropriate estimates of the parameters and the standard errors and explain how the results differ from the original, non-imputed model.

By testing the multivariate normality, I use MVN tests to do that. From the p-value of the test, we could know the data is not multivariate normality. Also, after plotting qq plot, from the shape of plot, we could first try square root or log to tranform the model. Below, I try square root first.

After transforming, the result is still not mutivariate normality although the result has been inproved.

We could see from the table, after inputing, the model has not change significantly. I think mainly because the missing data in the dataset is not that much, so although impute the missing value, the model still similar. What's more, the data is not multivatiate normal, so maybe it will affect the result.
