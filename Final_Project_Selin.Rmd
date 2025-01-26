---
title: "Final Project"
author: "Tina Zheng, Selin Altiparmak, Feifan Liu, Jaaji, Tu"
date: "2023-12-12"
output: pdf_document
---

```{r}
library(dplyr)
library(ggplot2)
library(knitr)
library(moderndive)
library(rsm)
library(rockchalk)
library(infer)
library(corrplot)
library(outliers)
library(car)
library(tidyverse)
library(psych)
library(performance)

```



```{r}
all <-read.csv("all.csv", header=T)
#all <- all %>% mutate(baby2=ifelse(chdperwoman<=2, 1, 0))
glimpse(all)
table(all$continent)
summary(all)
#Structure of the dataset
str(all)
```

## data cleaning and pre-processing
```{r}

#Remove country variable
all<-all[,-1]

#Convert continent variable in to factor
all$continent<-as.factor(all$continent)

#check the missing values
sapply(all, function(x) sum(is.na(x)))

#Remove Outlier

quartiles <- quantile(all$lifeexp, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(all$lifeexp)
 
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
 
all_clean <- subset(all, all$lifeexp > Lower & all$lifeexp < Upper)
dim(all_clean) # no outliers

```

```{r}
#Correlation plots
num <- all_clean[sapply(all_clean, is.numeric)]

corr_matrix <- cor(num)
corr_matrix

scatter_matrix <- pairs(num)
corrplot(corr_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", addCoef.col = "black")
# corrplot(correlation_matrix, method = "color")
```

## Attempt 1: Using scatterplots to analyze the correlations between one characteristic variable with life expectancy.
#Child Mortality vs Life Expectancy
From the scatterplot, we see that there seems to be a negative linear relationship between Child mortality and Life expectancy indicating that there is a negative correlation.
#Income vs Life Expectancy
From the scatterplot, we see that there seems to be a positive relationship between Income and Life expectancy indicating that there is a positive correlation.
#GDP Capita vs Life Expectancy
From the scatterplot, we see that there seems to be a positive relationship between GDP Capita and Life expectancy indicating that there is a positive correlation.
#Population Density vs Life Expectancy
From the scatterplot, we see that there seems to be little or no correlation between Population Density and Life expectancy.
#Health Spend vs Life Expectancy
From the scatterplot, we see that there seems to be a positive relationship between Health Spend and Life expectancy indicating that there is a positive correlation.
#CO2 vs Life Expectancy
From the scatterplot, we see that there seems to be a positive relationship between CO2 and Life expectancy indicating that there is a positive correlation.
#Water vs Life Expectancy
From the scatterplot, we see that there seems to be a positive relationship between Water and Life expectancy indicating that there is a positive correlation.
#CHD per Woman vs Life Expectancy
From the scatterplot, we see that there seems to be a negative linear relationship between CHD per Woman and Life expectancy indicating that there is a negative correlation.
#Murder vs Life Expectancy
From the scatterplot, we see that there seems to be little or no correlation between Murder and Life expectancy.
#Side by Side Boxplot of Life Expectancy by Continent.
From the side by side boxplot, we see that there are outliers in the continents Africa and America. Oceania has the highest average life expectancy while Africa has the lowest average life expectancy.


```{r}
ggplot(data=all_clean, aes(x=childmort, y=lifeexp, size=population, color = continent))+ geom_point()
ggplot(data=all_clean, aes(x=income, y=lifeexp,size=population, color = continent)) + geom_point()
ggplot(data=all_clean, aes(x=gdpcapita, y=lifeexp, size=population, color = continent)) + geom_point()
ggplot(data=all_clean, aes(x=popdensity, y=lifeexp, size=population, color = continent)) + geom_point()
ggplot(data=all_clean, aes(x=healthspend, y=lifeexp, size=population, color = continent)) + geom_point()
ggplot(data=all_clean, aes(x=co2, y=lifeexp, size=population, color = continent)) + geom_point()
ggplot(data=all_clean, aes(x=water, y=lifeexp, size=population, color = continent)) + geom_point()
ggplot(data=all_clean, aes(x=chdperwoman, y=lifeexp, size=population, color = continent)) + geom_point()
ggplot(data=all_clean, aes(x=murder, y=lifeexp, size=population, color = continent)) + geom_point()
ggplot(data=all_clean, aes(x=continent, y=lifeexp, size=population, color = continent)) + geom_boxplot()

```

#Attempt 2: based on out positive and negative corraleation plots for life expectancy we would try to create the best simple regression model variables to fit our model, and plot it consecutively.

Out of these single regression models all_clean these variables are significant(Child Mortality,Chdperwoman,Income,Gdp Per Capita,Healthspend,co2,Water. According to our Population and Population Density, and Murder variables are not significant single regression models to use for our final model.

```{r}
get_regression_table(childmort_model)
child_points <- get_regression_points(childmort_model)
ggplot(child_points, aes(x=childmort, y=residual)) + geom_point()
get_regression_summaries(childmort_model)

ggplot(all_clean,aes(x=childmort, y=lifeexp)) + geom_point() + labs(x="Child Mortality", y="Life Expectancy") + geom_smooth(method="lm",se=FALSE)
cor(x = all_clean$lifeexp, y = all_clean$childmort)
```

```{r}
chdperwoman_model <-lm(lifeexp ~ chdperwoman,data=all_clean)
get_regression_table(chdperwoman_model)
chdperwoman_points <- get_regression_points(chdperwoman_model)
ggplot(chdperwoman_points, aes(x=chdperwoman, y=residual)) + geom_point()
get_regression_summaries(chdperwoman_model)

ggplot(all_clean,aes(x=chdperwoman, y=lifeexp)) + geom_point() + labs(x="Chdperwoman", y="Life Expectancy") + geom_smooth(method="lm",se=FALSE)
cor(x = all_clean$lifeexp, y = all_clean$chdperwoman)
```

```{r}
population_model <-lm(lifeexp ~ population,data=all_clean)
get_regression_table(population_model)

population_points <- get_regression_points(chdperwoman_model)
ggplot(population_points, aes(x=chdperwoman, y=residual)) + geom_point()
get_regression_summaries(population_model)

ggplot(all_clean,aes(x=population, y=lifeexp)) + geom_point() + labs(x="Population", y="Life Expectancy") + geom_smooth(method="lm",se=FALSE)
cor(x = all_clean$lifeexp, y = all_clean$population)
```

```{r}
income_model <-lm(lifeexp ~ income,data=all_clean)
get_regression_table(income_model)

income_points <- get_regression_points(income_model)
ggplot(income_points, aes(x = income, y=residual)) + geom_point()
get_regression_summaries(income_model)

ggplot(all_clean,aes(x=income, y=lifeexp)) + geom_point() + labs(x="Income", y="Life Expectancy") + geom_smooth(method="lm",se=FALSE)
cor(x = all_clean$lifeexp, y = all_clean$income)
```

```{r}
popdensity_model <-lm(lifeexp ~ popdensity,data=all_clean)
get_regression_table(popdensity_model)

income_points <- get_regression_points(popdensity_model)
ggplot(income_points, aes(x = popdensity, y=residual)) + geom_point()
get_regression_summaries(popdensity_model)

ggplot(all_clean,aes(x=popdensity, y=lifeexp)) + geom_point() + labs(x="Income", y="Life Expectancy") + geom_smooth(method="lm",se=FALSE)
cor(x = all_clean$lifeexp, y = all_clean$popdensity)
```


```{r}
gdpcapita_model <-lm(lifeexp ~ gdpcapita,data=all_clean)
get_regression_table(gdppercapita_model)

gdpcapita_points <- get_regression_points(gdpcapita_model)
ggplot(gdpcapita_points, aes(x = gdpcapita, y=residual)) + geom_point()
get_regression_summaries(gdpcapita_model)

ggplot(all_clean,aes(x=gdpcapita, y=lifeexp)) + geom_point() + labs(x="Gdp Capita", y="Life Expectancy") + geom_smooth(method="lm",se=FALSE)
cor(x = all_clean$lifeexp, y = all_clean$gdpcapita)

```



```{r}
water_model <-lm(lifeexp ~ water,data=all_clean)
get_regression_table(gdppercapita_model)

water_points <- get_regression_points(water_model)
ggplot(water_points, aes(x = water, y=residual)) + geom_point()

ggplot(all_clean,aes(x=water, y=lifeexp)) + geom_point() + labs(x="Water", y="Life Expectancy") + geom_smooth(method="lm",se=FALSE)
cor(x = all_clean$lifeexp, y = all_clean$water)

```

```{r}
co2_model <-lm(lifeexp ~ co2,data=all_clean)
get_regression_table(co2_model)

co2_points <- get_regression_points(co2_model)
ggplot(co2_points, aes(x = co2, y=residual)) + geom_point()

ggplot(all_clean,aes(x=co2, y=lifeexp)) + geom_point() + labs(x="Co2", y="Life Expectancy") + geom_smooth(method="lm",se=FALSE)
cor(x = all_clean$lifeexp, y = all_clean$co2)

```

```{r}
health_spend_model <-lm(lifeexp ~ healthspend,data=all_clean)
get_regression_table(health_spend_model)

health_spend_points <- get_regression_points(health_spend_model)
health_spend_points

ggplot(all_clean,aes(x=co2, y=lifeexp)) + geom_point() + labs(x="Co2", y="Life Expectancy") + geom_smooth(method="lm",se=FALSE)
cor(x = all_clean$lifeexp, y = all_clean$healthspend)
```
```{r}
murder_model <-lm(lifeexp ~ murder,data=all_clean)
get_regression_table(murder_model)

murder_points <- get_regression_points(murder_model)
murder_points

ggplot(all_clean,aes(x=co2, y=lifeexp)) + geom_point() + labs(x="Co2", y="Life Expectancy") + geom_smooth(method="lm",se=FALSE)
cor(x = all_clean$lifeexp, y = all_clean$murder)
```
## EDA

```{r}
# Descriptive summaries
describe(all_clean[,-12])

#explore the target variable
ggplot(all_clean, aes(x = lifeexp)) +
  geom_histogram( fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Life Expectancy",
       x = "Life Expectancy",
       y = "Frequency")




```

## develop the models

```{r}

model_01 <- lm(lifeexp ~ ., data = all_clean)
get_regression_table(model_01)
get_regression_summaries(model_01)

# Check for multicollinearity
vif_model_01 <- vif(model_01)
vif_model_01
# If VIF values are high, consider removing correlated predictors

# Residual Analysis for  Model 01
residuals_01 <- residuals(model_01)
plot(model_01)
```

* income has a relatively high GVIF of 14.37, indicating potential multicollinearity.
* childmort, chdperwoman, and continent also have moderately high GVIF values.
* Removing them from the model and do further analysis.

```{r}
model_02 <- lm(lifeexp ~ childmort + gdpcapita + healthspend + co2 + water + popdensity + murder + continent, data = all_clean)
get_regression_table(model_02)
get_regression_summaries(model_02)

vif_02 <- vif(model_02)
vif_02
```

```{r}
life_expectancy_new <- mutate(all_clean,
                            childmort_sq = childmort^2,
                            healthspend_sq = healthspend^2)

model_03 <- lm(lifeexp ~ childmort + gdpcapita + healthspend + co2 + water + popdensity + murder + continent+childmort_sq+healthspend_sq, data = life_expectancy_new)
get_regression_table(model_03)
get_regression_summaries(model_03)

vif_03 <- vif(model_03)
vif_03
 
residuals_03 <- residuals(model_03)
plot(model_03)
```


```{r}
model_04 <- lm(lifeexp ~ childmort*healthspend  + water  + murder + continent+childmort_sq, data = life_expectancy_new)
get_regression_table(model_04)
get_regression_summaries(model_04)

vif_04 <- vif(model_04)
vif_04

residuals_04 <- residuals(model_04)
plot(model_04)
```




## Model Performance

```{r}
performance <- compare_performance(model_01, model_02, model_03,model_04)
as.data.frame(performance)
```

# The best model  created is model_03  with R Square Adjusted 87.62%.