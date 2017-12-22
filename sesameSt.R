---
title: "Methods and Data Analysis 6"
author: "Vivek Sriram"
date: "11/15/2017"
output: pdf_document
---

## The Sesame St Problem

Did you watch the television show Sesame Street growing up? Its goals were to help young children learn academic skills and, in particular, to help economically disadvantaged children catch up to economically advantaged children on those skills. I will analyze whether or not Sesame Street achieved its goals in the early 1970s, soon after it was created. I will use a logistic regression to examine whether or not encouragement to watch Sesame Street results in higher odds of watching Sesame Street at least once. 

As always, I'll start off by importing my data and taking a look at its format.
```{r}
library(arm)
library(pROC)
sesame <- read.csv("/Users/viveksriram/Desktop/sesame.txt")
summary(sesame)
dim(sesame)
```

My prompt tells me to create a new variable that equals zero for all individuals who never watch Sesame Street and equals one for all other individuals. I'm asked to use the viewcat variable to do this. One thing to note in this scenario is that viewcat, which tells me the frequency of viewing for a child from a scale of 1 to 4, doesn't actually provide for the possibility that a child never watches Sesame Street. The lowest ranking of 1 indicates that a child rarely watches the show. For the sake of my intepretation, I'll consider a viewcat of 1 to represent a child who doesn't really watch Sesame Street, and a viewcat of 2, 3, or 4, representing a child watching the show once or twice a week, three to five times a week, or more than 5 times a week, respectively, to represent a child who watches Sesame Street a lot. I'll call my new variable "viewInd".  

```{r}
sesame$viewInd = rep(1, length(sesame$viewcat))
sesame$viewInd[sesame$viewcat==1] = 0
summary(sesame$viewInd)
```
I see that the average value of my new viewInd variable is 0.775, meaning that the percentage of individuals in my data set who "never watch Sesame Street" is 1-0.775 = 22.5%

Now I'll perform some exploratory analysis. I'll consider solely the predictors that originally existed in my model, meaning I'm going to disregard the three variables made by Professor Reiter: numbers, letters and num.let. It makes sense to disregard these values because they are combinations of other predictors in my data set, and using them over my original predictors would just give me less information.  

I'll disregard the "id" predictor variable, because I know ahead of time that all the ID values are distinct and that it won't impact the probability a child watches Sesame Street.

I'm also told not to control any of the post-test variables in my data set, so I can exclude all of those variables from my model.

Finally, I'm told that I can pick which pre-test variables I want to include in my model. I expect there to be notable interaction effects across my pre-test variables, so it definitely makes sense for me to drop some of them. Doing so should improve interpretation and simplify my model. To decide which pre-test variables to include, I'll use my past experience with Sesame Street. Based on what I remember of the show, I'd expect knowledge of classification skills, knowledge of letters, and knowledge of numbers to be sufficient in terms of test predictors for watching Sesame Street. I'm just going to go ahead and assume that these variables sufficiently encompass knowledge of forms, relational terms, body parts, and peabody score. If I wanted to be more rigorous with my model generation, I'd probably include all of the above variables in my initial model, and then perform backward and foward selection on these predictors and their interaction effects to produce a best fit model. However, since I've been specifically told in the prompt that I can include whatever pretest predictors I want, I'm going to settle with the choice I made :) Later on, I might check if adding in all the pre-test scores has any beneficial impact.

In any case, the final set of predictors I've decided to make my initial model with are:
site, sex, age, setting, viewenc, prelet, prenumb, preclasf, and peabody 

Time to perform some exploratory analysis on these variables to see if I can find any non-linear trends. I'll make boxplots and tables for my categorical predictors, and binned plots for my continuous predictors:
```{r}
table(sesame$viewInd,sesame$viewenc)
table(sesame$viewInd,sesame$site)
table(sesame$viewInd,sesame$sex)
table(sesame$viewInd,sesame$setting)

boxplot(sesame$viewenc,sesame$viewInd)
boxplot(sesame$site,sesame$viewInd)
boxplot(sesame$sex,sesame$viewInd)
boxplot(sesame$setting,sesame$viewInd)

binnedplot(x= sesame$age, y=sesame$viewInd, xlab = "age", ylab = "Watches Sesame St", main = "Binned Age and Watch cases") 
binnedplot(x= sesame$prelet, y=sesame$viewInd, xlab = "prelet", ylab = "Watches Sesame St", main = "Binned Prelet and Watch cases") 
binnedplot(x= sesame$prenumb, y=sesame$viewInd, xlab = "prenumb", ylab = "Watches Sesame St", main = "Binned Prenumb and Watch cases") 
binnedplot(x= sesame$preclasf, y=sesame$viewInd, xlab = "preclasf", ylab = "Watches Sesame St", main = "Binned Preclasf and Watch cases") 
binnedplot(x= sesame$peabody, y=sesame$viewInd, xlab = "peabody", ylab = "Watches Sesame St", main = "Binned Peabody and Watch cases") 
```

Looking through my binned plots, I fail to see any significant non-linear patterns comparing my predictors to my indicator. There are a couple of interesting trends, such as what looks like sinusoidal behavior for my pretest plots (particularly for prenumb, preclasf, and peabody). Considering the nature of my data, however, it doesn't make sense for me to fit a sinusoidal transformation to this predictor. It's more likely that my limited amount of data is simply causing me to see chance patterns in the data. Overall, it looks like we don't have to worry about non-linear transformations for any of my continuous predictors in my model. One thing to note is that for my binned age plot, I don't seem to have any relationship between age and watching the show. I'll keep the age predictor in for now, but it might make my model better to drop it later.

Looking at boxplots and tables for my categorical variables, I also don't notice any concerning trends in my data. It seems like I can be relatively content without transforming any of my categorical predictors for this model. One interesting point to note is that it seems that if a child wasn't encouraged to watch Sesame Street, the likelihood he or she didn't watch it is pretty high.

Now it's time for me to look for interaction effects across the predictors in my data set. I'll start off by looking for interaction effects with my main variable of interest, viewenc.
```{r}
par(mfcol=c(2,1))

boxplot(sesame$viewInd, sesame$sex[sesame$viewenc==1], xlab = "Watches (not encouraged)", ylab = "Sex")
boxplot(sesame$viewInd, sesame$sex[sesame$viewenc==2], xlab = "Watches (encouraged)", ylab = "Sex")

boxplot(sesame$viewInd, sesame$setting[sesame$viewenc==1], xlab = "Watches (not encouraged)", ylab = "Setting")
boxplot(sesame$viewInd, sesame$setting[sesame$viewenc==2], xlab = "Watches (encouraged)", ylab = "Setting")

#first plot for viewenc = 1
binnedplot(sesame$site[sesame$viewenc==1], y=sesame$viewInd[sesame$viewenc==1], xlab = "Site", ylab = "Watches Sesame St", main = "Binned Site and Watches Show (not encouraged)") 
#next the plot for viewenc = 2
binnedplot(sesame$site[sesame$viewenc==2], y=sesame$viewInd[sesame$viewenc==2], xlab = "Site", ylab = "Watches Sesame St", main = "Binned Site and Watches Show (encouraged)") 

#first plot for viewenc = 1
binnedplot(sesame$age[sesame$viewenc==1], y=sesame$viewInd[sesame$viewenc==1], xlab = "Age", ylab = "Watches Sesame St", main = "Binned Age and Watches Show (not encouraged)") 
#next the plot for viewenc = 2
binnedplot(sesame$age[sesame$viewenc==2], y=sesame$viewInd[sesame$viewenc==2], xlab = "Age", ylab = "Watches Sesame St", main = "Binned Age and Watches Show (encouraged)") 

#first plot for viewenc = 1
binnedplot(sesame$prelet[sesame$viewenc==1], y=sesame$viewInd[sesame$viewenc==1], xlab = "PreLet", ylab = "Watches Sesame St", main = "Binned PreLet and Watches Show (not encouraged)") 
#next the plot for viewenc = 2
binnedplot(sesame$prelet[sesame$viewenc==2], y=sesame$viewInd[sesame$viewenc==2], xlab = "PreLet", ylab = "Watches Sesame St", main = "Binned PreLet and Watches Show (encouraged)") 

#first plot for viewenc = 1
binnedplot(sesame$prenumb[sesame$viewenc==1], y=sesame$viewInd[sesame$viewenc==1], xlab = "prenumb", ylab = "Watches Sesame St", main = "Binned prenumb and Watches Show (not encouraged)") 
#next the plot for viewenc = 2
binnedplot(sesame$prenumb[sesame$viewenc==2], y=sesame$viewInd[sesame$viewenc==2], xlab = "prenumb", ylab = "Watches Sesame St", main = "Binned prenumb and Watches Show (encouraged)") 

#first plot for viewenc = 1
binnedplot(sesame$preclasf[sesame$viewenc==1], y=sesame$viewInd[sesame$viewenc==1], xlab = "preclasf", ylab = "Watches Sesame St", main = "Binned preclasf and Watches Show (not encouraged)") 
#next the plot for viewenc = 2
binnedplot(sesame$preclasf[sesame$viewenc==2], y=sesame$viewInd[sesame$viewenc==2], xlab = "preclasf", ylab = "Watches Sesame St", main = "Binned preclasf and Watches Show (encouraged)")

#first plot for viewenc = 1
binnedplot(sesame$peabody[sesame$viewenc==1], y=sesame$viewInd[sesame$viewenc==1], xlab = "peabody", ylab = "Watches Sesame St", main = "Binned peabody and Watches Show (not encouraged)") 
#next the plot for viewenc = 2
binnedplot(sesame$peabody[sesame$viewenc==2], y=sesame$viewInd[sesame$viewenc==2], xlab = "peabody", ylab = "Watches Sesame St", main = "Binned peabody and Watches Show (encouraged)") 
```

Great, as far as I can tell, there doesn't seem to be any interaction between viewenc and my other predictors. This was my main interaction of concern before I fit my regression model. Just for kicks, I can also try to see if there's an interaction between sex and any of my variables.

```{r}
par(mfcol=c(2,1))

boxplot(sesame$viewInd, sesame$setting[sesame$sex==1], xlab = "Watches (male)", ylab = "Setting")
boxplot(sesame$viewInd, sesame$setting[sesame$sex==2], xlab = "Watches (female)", ylab = "Setting")

#first plot for sex = 1
binnedplot(sesame$site[sesame$sex==1], y=sesame$viewInd[sesame$sex==1], xlab = "Site", ylab = "Watches Sesame St", main = "Binned Age and Watches Show (Male)") 
#next the plot for sex = 2
binnedplot(sesame$site[sesame$sex==2], y=sesame$viewInd[sesame$sex==2], xlab = "Site", ylab = "Watches Sesame St", main = "Binned Age and Watches Show (Female)") 

#first plot for sex = 1
binnedplot(sesame$age[sesame$sex==1], y=sesame$viewInd[sesame$sex==1], xlab = "Age", ylab = "Watches Sesame St", main = "Binned Age and Watches Show (Male)") 
#next the plot for sex = 2
binnedplot(sesame$age[sesame$sex==2], y=sesame$viewInd[sesame$sex==2], xlab = "Age", ylab = "Watches Sesame St", main = "Binned Age and Watches Show (Female)") 

#first plot for sex = 1
binnedplot(sesame$prelet[sesame$sex==1], y=sesame$viewInd[sesame$sex==1], xlab = "PreLet", ylab = "Watches Sesame St", main = "Binned PreLet and Watches Show (Male)") 
#next the plot for sex = 2
binnedplot(sesame$prelet[sesame$sex==2], y=sesame$viewInd[sesame$sex==2], xlab = "PreLet", ylab = "Watches Sesame St", main = "Binned PreLet and Watches Show (Female)") 

#first plot for sex = 1
binnedplot(sesame$prenumb[sesame$sex==1], y=sesame$viewInd[sesame$sex==1], xlab = "prenumb", ylab = "Watches Sesame St", main = "Binned prenumb and Watches Show (Male)") 
#next the plot for sex = 2
binnedplot(sesame$prenumb[sesame$sex==2], y=sesame$viewInd[sesame$sex==2], xlab = "prenumb", ylab = "Watches Sesame St", main = "Binned prenumb and Watches Show (Female)") 

#first plot for sex = 1
binnedplot(sesame$preclasf[sesame$sex==1], y=sesame$viewInd[sesame$sex==1], xlab = "preclasf", ylab = "Watches Sesame St", main = "Binned preclasf and Watches Show (Male)") 
#next the plot for sex = 2
binnedplot(sesame$preclasf[sesame$sex==2], y=sesame$viewInd[sesame$sex==2], xlab = "preclasf", ylab = "Watches Sesame St", main = "Binned preclasf and Watches Show (Female)") 

#first plot for sex = 1
binnedplot(sesame$peabody[sesame$viewenc==1], y=sesame$viewInd[sesame$viewenc==1], xlab = "peabody", ylab = "Watches Sesame St", main = "Binned peabody and Watches Show (not encouraged)") 
#next the plot for sex = 2
binnedplot(sesame$preclasf[sesame$viewenc==2], y=sesame$viewInd[sesame$viewenc==2], xlab = "peabody", ylab = "Watches Sesame St", main = "Binned peabody and Watches Show (encouraged)") 
```

Once again, my comparison for plots across genders fails to show me anything concerning. It looks like there aren't any significant interaction effects between sex and my other predictors.

At this point, I feel fairly ready to fit my logistic regression model. I'm going to go ahead and fit my model with all the predictors I mentioned above. I'll be using no transformations, and I'll start off by using no interactions effects (Later on, I'll perform forward selection on interaction effects). I'll also go ahead and mean-center my continuous predictors to improve interpretation. Furthermore, I need to convert my site variable into a series of 4 indicator variables to be able to properly interpet my results. Finally, I'll use dummy variables to convert viewenc, setting, and sex into reasonable indicators for my model. Note that I'm making it so that encouraging a child to watch Sesame Street is 1, while not encouraging a child is 0, being male is 0 and being female is 1, and watching from home is 0 while watching from school is 1.

```{r}
sesame$age.c = sesame$age - mean(sesame$age)
sesame$prelet.c = sesame$prelet - mean(sesame$prelet)
sesame$prenumb.c = sesame$prenumb - mean(sesame$prenumb)
sesame$preclasf.c = sesame$preclasf - mean(sesame$preclasf)
sesame$peabody.c = sesame$peabody - mean(sesame$peabody)

#sample size
n = nrow(sesame)
#create series of 4 indicator variables for my 5 sites
sesame$site2 = rep(0, n)
sesame$site2[sesame$site == 2] = 1
sesame$site3 = rep(0, n)
sesame$site3[sesame$site == 3] = 1
sesame$site4 = rep(0, n)
sesame$site4[sesame$site == 4] = 1
sesame$site5 = rep(0, n)
sesame$site5[sesame$site == 5] = 1

sesame$viewencDummy = rep(0, n)
sesame$viewencDummy[sesame$viewenc == 1] = 1
sesame$settingDummy = rep(0, n)
sesame$settingDummy[sesame$setting == 2] = 1
sesame$sexDummy = rep(0, n)
sesame$sexDummy[sesame$sex == 2] = 1
```

```{r}
sesamereg = glm(viewInd ~ viewencDummy + age.c + settingDummy + sexDummy + site2 + site3 + site4 + site5 + prelet.c + prenumb.c + preclasf.c + peabody.c, data = sesame, family = binomial)
summary(sesamereg)
```

We can see that I've done a fairly good job with my initial model. However, I expect that there would be strong interaction effects across my pre-test scores. I'll perform some forward selection with interaction effects for these variables to see how useful they are.

```{r}
sesamereg = glm(viewInd ~ viewencDummy + age.c + settingDummy + sexDummy + site2 + site3 + site4 + site5 + prelet.c + prenumb.c + preclasf.c + peabody.c + prelet.c*prenumb.c, data = sesame, family = binomial)
summary(sesamereg)
```

Hmm, not very useful. My AIC went up and my interaction effect has a very high p-value.

```{r}
sesamereg = glm(viewInd ~ viewencDummy + age.c + settingDummy + sexDummy + site2 + site3 + site4 + site5 + prelet.c + prenumb.c + preclasf.c + peabody.c + prelet.c*preclasf.c, data = sesame, family = binomial)
summary(sesamereg)
```

Looks like I have the same result here. I'll try my last possible interaction effect.

```{r}
sesamereg = glm(viewInd ~ viewencDummy + age.c + settingDummy + sexDummy + site2 + site3 + site4 + site5 + prelet.c + prenumb.c + preclasf.c + peabody.c + prenumb.c*preclasf.c, data = sesame, family = binomial)
summary(sesamereg)
```
Still no better. Looks like trying to include an interaction effect for my pre-test predictors wasn't particularly useful.

Let me perform some backward selection on my original model to see if I can improve things by dropping some of my predictors. Note that I'm asked in my prompt to keep the predictors "site," "setting," "age," and "sex" in my model. In other words, I'll just try to get rid of my pre-test scores. I'll start with prelet.c because it has the highest p-value of my pretest predictors.

```{r}
sesamereg = glm(viewInd ~ viewencDummy + age.c + settingDummy + sexDummy + site2 + site3 + site4 + site5 + prenumb.c + preclasf.c + peabody.c, data = sesame, family = binomial)
summary(sesamereg)
```

Well would you look at that! Dropping prelet improved my model by reducing the AIC score. Let me try dropping prenumb too, because it also seems to have a high p-value.

```{r}
sesamereg = glm(viewInd ~ viewencDummy + age.c + settingDummy + sexDummy + site2 + site3 + site4 + site5 + preclasf.c + peabody.c, data = sesame, family = binomial)
summary(sesamereg)
```

Even better! Finally, let me see what happens if I drop preclasf.c

```{r}
sesamereg = glm(viewInd ~ viewencDummy + age.c + settingDummy + sexDummy + site2 + site3 + site4 + site5 + peabody.c, data = sesame, family = binomial)
summary(sesamereg)
```

Okay, my AIC value went up. It looks like it might be important to keep this predictor in my model.
Let me perform a change of variance test between my second-to-last model and my first model to see if I have a significantly better outcome.

```{r}
sesameregFull = glm(viewInd ~ viewencDummy + age.c + settingDummy + sexDummy + site2 + site3 + site4 + site5 + prelet.c + prenumb.c + preclasf.c + peabody.c, data = sesame, family = binomial)
sesameregEdited = glm(viewInd ~ viewencDummy + age.c + settingDummy + sexDummy + site2 + site3 + site4 + site5 + preclasf.c + peabody.c, data = sesame, family = binomial)
anova(sesameregFull, sesameregEdited, test= "Chisq")
```

Well that's interesting... my change of variance test tells me that there isn't a significant difference between my original model and the final model I selected. I expect that even if I had included all of my pre-test variables in my initial model, I would have found that they don't provide a significant benefit to my model.

Just out of interest, let me see what would have happened if I included all of my pretest variables.

```{r}
sesame$preform.c = sesame$preform - mean(sesame$preform)
sesame$prerelat.c = sesame$prerelat - mean(sesame$prerelat)
sesame$prebody.c = sesame$prebody - mean(sesame$prebody)

sesameregFull = glm(viewInd ~ viewencDummy + age.c + settingDummy + sexDummy + site2 + site3 + site4 + site5 + prelet.c + prenumb.c + preclasf.c + preform.c + prerelat.c + prebody.c + peabody.c, data = sesame, family = binomial)
summary(sesameregFull)
```

Yep, my model doesn't look any better or worse. Overall, it appears that including multiple predictors from the pre-test category doesn't much help my cause, and neither does including them and potential interaction effects. For the sake of simplicity as well as ease of interpretation, I'm going to go with the smallest model I found that doesn't have a significantly larger AIC value compared to my model with all of my pretest predictors. In other words, here's my final model:
```{r}
sesamereg = glm(viewInd ~ viewencDummy + age.c + settingDummy + sexDummy + site2 + site3 + site4 + site5 + preclasf.c + peabody.c, data = sesame, family = binomial)
summary(sesamereg)
confint(sesamereg)
```
My model is as follows:  
log(Watches the Show/Doesn't Watch the Show) = 4.43297 + 2.62441 x viewenc - 0.07866 x age.c - 0.24778 x setting - 0.48283 x sex - 0.65164 x site2 - 0.82167 x site3 - 2.44351 x site4 - 2.62704 x site5 + 0.18849 x preclasf.c + 0.04513 x peabody

My 95% confidence intervals for each of my predictors are as follows:    
viewenc: (1.698331578, 3.671612137)  
age.c: (-0.165761862, 0.003878586)  
setting: (-1.182043179, 0.696266163)  
sex: (-1.319768562, 0.326384314)  
site2: (-2.226271780, 0.977660291)  
site3: (-2.056515296, 0.357558055)  
site4: (-3.732987559, -1.276063522)  
site5: (-4.397934102, -0.924063099)  
preclasf.c: (0.074109150, 0.312869523)  
peabody.c: (0.005799212, 0.087931074)  

Let me go ahead and interpet my results. I'll start off by calculating relevant values:
```{r}
#average values
mean(sesame$age); mean(sesame$preclasf); mean(sesame$peabody)

#intercept
exp(7.78799)

print("viewenc")
exp(2.62441); exp(1.698331578); exp(3.671612137)

print("age")
exp(-0.07866); exp(-0.165761862); exp(0.003878586)

print("setting")
exp(-0.24778); exp(-1.182043179); exp(0.696266163)

print("sex")
exp(-0.48283); exp(-1.319768562); exp(0.326384314)

print("site2")
exp(-0.65164); exp(-2.226271780); exp(0.977660291)

print("site3")
exp(-0.82167); exp(-2.056515296); exp(0.357558055)

print("site4")
exp(-2.44351); exp(-3.732987559); exp(-1.276063522)

print("site5")
exp(-2.62704); exp(-4.397934102); exp(-0.924063099)

print("preclasf")
exp(0.18849); exp(0.074109150); exp(0.312869523)

print("peabody")
exp(0.04513); exp(0.005799212); exp(0.087931074)
```

Interpretations:
For a typical child from my data set, with age 51.525 months, a pretest score on classification of 12.23333, a peabody score of 46.46667, who is not encouraged to watch Sesame Street, watches the show at home and not at school, is male, and is a 3 to 5 year old disadvantaged child from inner city areas in various parts of the country, the odds that he watches Sesame Street are 2411.466

Holding all else constant, increasing the age of a child by 1 month will change the odds they watch Sesame Street by a multiplicative factor of 0.9243542 (95% confidence interval of [0.847248, 1.003886])

Holding all else constant, increasing the pre-test classification score of a child by 1 point will change the odds they watch Sesame Street by a multiplicative factor of 1.207425 (95% confidence interval of [1.076924, 1.367343])

Holding all else constant, increasing the peabody score of a child by 1 point will change the odds they watch Sesame Street by a multiplicative factor of 1.046164 (95% confidence interval of [1.005816, 1.091913])

Holding all else constant, considering a child who watches Sesame Street at school vs. at home will change the odds they watch Sesame Street by a multiplicative factor of 0.7805316 (95% confidence interval of [0.3066516, 2.006248])

Holding all else constant, considering a child who is a girl vs. who is a boy will change the odds they watch Sesame Street by a multiplicative factor of 0.6170347 (95% confidence interval of [0.2671971, 1.385948])

Holding all else constant, considering four year old advantaged suburban children compared to 3 to 5 year old disadvantaged children from inner city areas in various parts of the country will change the odds they watch Sesame Street by a multiplicative factor of 0.5211903 (95% confidence interval of [0.1079301, 2.658229])

Holding all else constant, considering advantaged rural children compared to 3 to 5 year old disadvantaged children from inner city areas in various parts of the country will change the odds they watch Sesame Street by a multiplicative factor of 0.4396967 (95% confidence interval of [0.1278989, 1.429834])

Holding all else constant, considering disadvantaged rural children compared to 3 to 5 year old disadvantaged children from inner city areas in various parts of the country will change the odds they watch Sesame Street by a multiplicative factor of 0.08685545 (95% confidence interval of [0.2823351, 0.2791339])

Holding all else constant, considering disadvantaged Spanish speaking children compared to 3 to 5 year old disadvantaged children from inner city areas in various parts of the country will change the odds they watch Sesame Street by a multiplicative factor of 0.07229213 (95% confidence interval of [0.01230273, 0.3969031])

Now for the important result:  
Holding all else constant, encouraging a child to watch Sesame Street will change the odds they watch Sesame Street by a multiplicative factor of 13.79643 (95% confidence interval of [5.464822, 39.31524])

As a last step to interpet the results of my model, I can produce graphs that demonstrate how the probabilities of survival change given various values for my continuous predictors.

```{r}
#create some values in line with those in the data, going from centered to raw scale.
summary(sesame$peabody)
summary(sesame$preclasf)
summary(sesame$age)
```

```{r}
#create some values in line with those in the data, going from centered to raw scale.
samplepeabody.c = seq(from = 10, to = 100, by = 10) 
samplepeabody = samplepeabody.c + mean(sesame$peabody)
samplepreclasf.c = seq(from = 0, to = 24, by = 3) 
samplepreclasf = samplepreclasf.c + mean(sesame$preclasf)
sampleage.c = seq(from = 35, to = 70, by = 5) 
sampleage = sampleage.c + mean(sesame$age)

#peabody
logitpredvalue = 4.43297 + 0.04513*samplepeabody.c
predprobpeabody = exp(logitpredvalue) / (1 + exp(logitpredvalue))
plot(y=predprobpeabody, x= samplepeabody, pch= 3, xlab = "Peabody", ylab = "Predicted probability", main = "Peabody vs. Predicted Probability")

#preclasf
logitpredvalue = 4.43297 + 0.18849*samplepreclasf.c
predprobpreclasf = exp(logitpredvalue) / (1 + exp(logitpredvalue))
plot(y=predprobpreclasf, x= samplepreclasf, pch= 3, xlab = "Preclasf", ylab = "Predicted probability", main = "Preclasf vs. Predicted Probability")

#age
logitpredvalue = 4.43297 - 0.07866*sampleage.c 
predprobage = exp(logitpredvalue) / (1 + exp(logitpredvalue))
plot(y=predprobage, x= sampleage, pch= 3, xlab = "Age", ylab = "Predicted probability", main = "Age vs. Predicted Probability")
```

I now have a clear way to visualize how changing my continous predictors affects the probability that a child watches Sesame Street.

Now that I've generated my model and interpeted my results, let me look at my binned residuals and my ROC curve to evaluate my satisfaction with my model.
```{r}
rawresid = sesame$viewInd - fitted(sesamereg)

#average residuals for my mean-centered continous variables
binnedplot(x=sesame$peabody.c, y = rawresid, xlab = "Peabody centered", ylab = "Residuals", main = "Binned residuals versus Peabody")
binnedplot(x=sesame$preclasf.c, y = rawresid, xlab = "Preclasf centered", ylab = "Residuals", main = "Binned residuals versus Preclasf")
binnedplot(x=sesame$age.c, y = rawresid, xlab = "Age centered", ylab = "Residuals", main = "Binned residuals versus Age")

#roc curve
roc(sesame$viewInd, fitted(sesamereg), plot=T, legacy.axes=T)
```

Looking through my binned residual plots, I really can't see any trends in my residuals. Indeed, they all fit within the appropriate bounds of each of their respective residual plots. Furthermore, looking at my ROC curve, I see that it veers pretty far from my line. Indeed, I have an area of 0.8874 under my ROC curve, which is quite high. It looks like I can be fairly satisfied with the assumptions behind my logistic regression fit.

Outside of my binned residual plots and my ROC curve, I have a couple things to note in terms of confidence in my model:  
1) The size of my data set: I don't have a huge amount of data, meaning that I might be missing out on some trends that actually have a huge influence on the likelihood a child watches Sesame Street. For instance, I assumed a linear relationship between all of my predictors and my outcome variable based upon my initial exploratory analysis, but this may not be the case!  
2) My method of predictor selection: despite the fact that I made use of p-values and AIC scores, overall, my method to pick which predictors I include in my model was a little bit arbitrary. Because of my methodology, it would be wise to take my fitted logistic regression model with a grain of salt.

Once again, however, I've determined the following:    
Holding all else constant, encouraging a child to watch Sesame Street will change the odds they watch Sesame Street by a multiplicative factor of 13.79643 (95% confidence interval of [5.464822, 39.31524]).

In terms of policy recommendations for the creators of Sesame Street, I would indeed suggest that they try to encourage children to watch the show. It does indeed appear that encouraging children to watch Sesame Street gets them to do so at least once!
