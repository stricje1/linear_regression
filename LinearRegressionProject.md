---
title: "Motor Trend Magazine MPG Study"
author: "Jeffrey Strickland"
date: "1/13/2022"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE, fig.width=6.5, fig.height=4, scipen = 1000000)
```

I work for Motor Trend, a magazine about the automobile industry, as an analyst. I am at a data set of a collection of cars, they are interested in exploring. They want to understand the relationship between a set of variables and miles per gallon (MPG) (outcome). They are particularly interested in the following two questions:
  
1. “Is an automatic or manual transmission better for MPG”
2. "Quantify the MPG difference between automatic and manual transmissions"  

## Data Processing

The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models).

```{r}
library(datasets)
data(mtcars)
help(mtcars)
```

## Data Description
The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models).

It consists of 32 observations on 11 variables.

1. [, 1] mpg  Miles/(US) gallon
2. [, 2] cyl  Number of cylinders
3. [, 3] disp Displacement (cu.in.)
4. [, 4] hp Gross horsepower
5. [, 5] drat Rear axle ratio
6. [, 6] wt Weight (lb/1000)
7. [, 7] qsec 1/4 mile time
8. [, 8] vs Engine (0 = V-shaped, 1 = straight)
9. [, 9] am Transmission (0 = automatic, 1 = manual)
10. [,10] gear Number of forward gears
11. [,11] carb Number of carburetors

## Exploratory Data Analysis (EDA)

The data summaries for automatic and manual transmissions are listed in Appendix A.

```{r}
library(dplyr)
my_quantile <- function(x, probs) {
  tibble(x = quantile(x, probs), probs = probs)
}

mtcars %>%
  group_by(am) %>%
  summarise(xbar = mean(mpg), sd = sd(mpg))
```

More

```{r}
mtcars %>%
  group_by(am) %>%
  summarise(my_quantile(mpg, c(0.25, 0.5, 0.75)))
```

## Data Preparation

To perform this analysis, we need to transform the class of some variables. The function factor() is used to encode a vector as a factor.

```{r}
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <-c("Automatic", "Manual")
```

## 1. Is an automatic or manual transmission better for MPG?

Here we quantify the MPG difference between automatic and manual transmissions. First, we generate a boxplot of MPG by transmission types at Appendix B. The boxplot shows that vehicles with manual transmissions may achieve better gas mileage than vehicles with automatic transmissions.

### Hypothesis Test

Now we address Question 1 statistically. The null hypothesis we are testing is:

  Ho: There is no different in mpg between automatic and manual transmissions. 
  
We conduct a t-test to test this hypothesis.

```{r}
t_test<-t.test(mtcars$mpg~mtcars$am, conf.level = 0.95)
df <- data.frame(
  "Welch Two Sample t-test"=rep(c('t-test p-value', 'Lower CI', 'Upper CI', 'Automatic Estimate', 'Manual Estimate',''), times=1),
  Value=rep(c(sprintf("%.6f",  t_test$p.value), sprintf("%.6f", t_test$conf.int),
    sprintf("%.6f", t_test$estimate), times=1))
)
knitr::kable(df)
```

Based on the results, p-value = 0.001374< 0.05, we reject the null hypothesis that there is no difference in MPG, and conclude that manual transmission is better than automatic transmission for MPG, with assumption that all other conditions remain unchanged.

## 2. Quantify the MPG difference between automatic and manual transmissions

Here we try to quantify the MPG difference between transmission types, and find if there are other variables that account for the MPG differences.

First, do a multivariate linear regression with all variables. We use the step function in R for a step-wise regression, where the choice of predictor is carried out automatically by comparing certain criterion, for example, Akaike information criterion (AIC).

```{r}
stepmodel = step(lm(data = mtcars, mpg ~ .), trace=0, steps=10000)
summary(stepmodel)
```
To further optimize the model, we can examine mpg ~ wt + qsec correlation with am.

```{r}
model <- lm(mpg~ factor(am):wt + factor(am):qsec,data=mtcars)
summary(model)
```
The results suggests that the best model includes weight and qsec. The Adjusted R-squared increased from 0.83 to 0.88. The adjusted R-squared increases when the new term improves the model more than would be expected by chance. Thus, the model improved by adding the factors. Now we can make the following conclusions:


* when the weight increased by 1000 lbs, the mpg decreased by -3.176 for automatic transmission cars, and -6.09 for manual transmission cars
* with increasing car weight we should choose automatic transmission cars for better gas mileage
* when the acceleration speed dropped, and 1/4 mile time increased (by 1 sec), the mpg factor increased by 0.834 miles for automatic transmission cars, and 1.446 miles for manual transmission cars
* with lower acceleration speed and same weight, manual transmission cars get bettwer gas mileage mpg

Residual plots seems to be randomly scattered, and some transformation may be needed for linearity (Appendix C). The Normal Q-Q plot shows the residuals are approximately normally distributed. Both scale-location and residual vs leverage show no issues.

## Conclusion

The hypothesis tests in part 1 showed that manual transmission vehicles are better than automatic transmission vehicles with no other factors considered. The factors in the optimized model quantifies the difference between automatic and manual transmissions, based on vehicle weight and acceleration, shows that manual transmission vehicles provide better gas mileage (increased mpg).

## Appendix A

For automatic:
```{r}  
  summary(mtcars[mtcars$am==0,])
```

For manual:
```{r}  
  summary(mtcars[mtcars$am==1,])
```

## Appendix B

```{r}
boxplot(mpg~am, data = mtcars,
        xlab = "Transmission",
        ylab = "Miles per Gallon",
        main = "MPG by Transmission Type")
```

## Appendix C
Residuals

```{r}
par(mfrow = c(2,2))
plot(model)
```

## Appendix D

```{r}
require(graphics)
pairs(mtcars, main = "mtcars data", gap = 1/4)
coplot(mpg ~ am | as.factor(vs), data = mtcars,
       panel = panel.smooth, rows = 1)
```
