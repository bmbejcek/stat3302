# Stat 3302 Project: 2001 Barry Bonds Plate Appearances

INTRODUCTION
------------

This project focuses on exploratory data analysis, model building, selection, and diagnostics for 2001 Barry Bonds Plate Appearances. Last updated: March 2016.

EXPLORATORY DATA ANALYSIS
-------------------------

Our initial steps in the exploratory data analysis phase was to familiarize ourselves with the data. After this process, we started to look at general trends in the data. One of the first models we fit was with Bonds' probability of getting on base vs. if there are other individuals present on base. This model produced a statistically significant relationship.

MODEL BUIDLING PROCESS
-------

After the EDA process concluded, we built a generalized linear model based on statistically significant findings. We continued to alter the model based on test error from cross-validated sets until we landed at an optimal model:
Let Yi be whether Bonds gets on base for at bat i in the 2001 season. Assume Yi ~ Bern(pi) where pi = prob that Bonds gets
on base in at bat i and logit pi = β0 + β1xi + β2yi + β3zi where xi = 1 if someone is on base and 0 otherwise, yi = 1 / appearance and zi = game number.


AUTHORS
-------
* Brett Bejcek
* Kyle Voytovich
