## Homework 4 Submission ## 

library(readr)
library(Metrics)
library(performance)
library(marginaleffects)

## Question 1:

################################################################################

## 1a)

data <- read_csv("mistletoes.csv")

mod.pois <- glm(Seedlings~Treatment, family = poisson(link = "log"), data = data)                               

summary(mod.pois)
coef(mod.pois) # examining the coefficients in logistic space


MAE <- performance::mae(mod.pois) # Assessing model fit with mean absolute error
MAE

# The average difference between the actual Y values and model estimate (Yhat)  
# is about 146 seedling individuals. This indicates a rough estimation accuracy 
# by the poisson model. 

################################################################################

## 1b)

predictions(mod.pois, newdata=data.frame(Treatment=c("parasitized", "unparasitized")))


plot_predictions(mod.pois, condition="Treatment")


# According to this model, misteltoe has a strong effect on seedling density. 
# The mean seedling count is 13.1 for unparasitized trees, and 308.2 for 
# parasitized trees. These were derived from the Treatmentunparasitized and
# (Intercept) coefficients, respectively. The 95% confidence interval is larger 
# for parasitized trees than unparasitized trees.

################################################################################

## 1c) 

mod.pois.yr <- glm(Seedlings~Year + Treatment, family = poisson(link = "log"), data = data)

summary(mod.pois.yr)
coef(mod.pois.yr) # examining the coefficients in logistic space

MAE2 <- performance::mae(mod.pois.yr) 
MAE2 # mod.poi.yr produces a slightly lower mean absolute error

plot_predictions(mod.pois.yr, condition=c("Year",
                                          "Treatment"))
exp(coef(mod.pois.yr)["(Intercept)"])
exp(coef(mod.pois.yr)["Year2012"])
exp(coef(mod.pois.yr)["Treatmentunparasitized"])

# The number of seedlings in the unparasitized treatments stayed relatively 
# similar between 2011 and 2012. The number of seedlings in the parasitized 
# treatments was significantly higher in 2012. This could be due to more 
# favorable conditions for the parasite or better conditions for seedling
# germination due to moist soil conditions. 

################################################################################

## Question 2:

################################################################################

## 2a)

treemortality <- read.csv("treemortality.csv")

mod.binom = glm(mortality~thinning, family = binomial(link = "logit"), data = treemortality)
summary(mod.binom)

MAE3 <- performance::mae(mod.binom) 
MAE3

plot_predictions(mod.binom, condition=c("thinning"))
 
#####  

mod.binom2 = glm(mortality~thinning + treesize, family = binomial(link = "logit"), data = treemortality)
summary(mod.binom2)

MAE4 <- performance::mae(mod.binom2) 
MAE4

plot_predictions(mod.binom2, condition=c("treesize",
                                         "thinning"))
coef(mod.binom2)
plogis(coef(mod.binom2)["(Intercept)"])
plogis(coef(mod.binom2)["thinning"])
plogis(coef(mod.binom2)["treesize"])

# Thinning treatments reduce the probability of tree mortality in the Ponderosa 
# forests assessed in this experiment. Thinning decreased Ponderosa mortality by 
# roughly 13%. Tree size had an even greater effect on survival, decreasing 
# mortality by about 49%. Thinning had an effect on tree mortality likely by 
# lowering fuel connectivity, resulting in less intense fires. Larger Ponderosa
# trees would be less susceptible to burning due to fire resistent bark, which
# is thicker in larger individual trees. 

################################################################################

## 2b)

# Yes, they do need to incorporate the effect of thinning. They may have 
# randomized their post-fire collection, however the managers implementing the
# thinning treatments did not. Therefore there could be a bias in tree size 
# between the thinned and unthinned sites. 

################################################################################

## 2c) 

mod.binom3 = glm(mortality~thinning + treesize + roaddist + slope, family = binomial(link = "logit"), data = treemortality)
summary(mod.binom3)

coef(mod.binom3)
plogis(coef(mod.binom3)["(Intercept)"])
plogis(coef(mod.binom3)["thinning"])
plogis(coef(mod.binom3)["treesize"])
plogis(coef(mod.binom3)["roaddist"])
plogis(coef(mod.binom3)["slope"])

MAE5 <- performance::mae(mod.binom3) 
MAE5 # By far the lowest MAE of the 3 binomial GLM's. This indicates this model 
     # has the best predicton power

plot_predictions(mod.binom3, condition=c("treesize",
                                         "thinning",
                                         "roaddist",
                                         "slope"))

# Thinning still has a negative effect on tree mortality. However, Slope and 
# distance from roads has a strong positive effect on tree mortality. Looking at
# The collection of plot predictions assessing the 4 parameters, the third row
# demonstrates the effect of road distance on the thinned and unthinned curves,
# moving from left to right. The effect of slope moving from top to bottom is
# even stronger. 
