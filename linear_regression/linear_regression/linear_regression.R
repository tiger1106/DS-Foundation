


#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
#setwd("~/Desktop/Rstatistics")
#setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])   # What is this step about??
str(states.info)

#look at last few labels
tail(states.info)


## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##Start by examining the data to check for problems.
# summary of expense and csat columns, all rows

sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)


# correlation between expense and csat

cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod)) 

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1,3)) # "which" argument optional



## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))

summary(sat.voting.mod)

sat.mod <- update(sat.mod, data=na.omit(states.data))
summary(sat.mod)

# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model

list.files("dataSets")

# read the states data
states.en<- readRDS("dataSets/states.rds") 
summary(states.en)
str(states.en)
library(dplyr)
states.en %>% select(energy,metro)
cor(states.en$energy,states.en$metro)  # correlation is giving NA
states.en %>% ggplot(aes(x=metro,y=energy))+geom_point()

EnergyUse<- lm(energy~metro,data = states.en)

##   2. Print and interpret the model `summary'

summary(EnergyUse)

par(mfrow=c(1,2))

plot(EnergyUse)

abline(EnergyUse)


# R square value is 0.1154 and adjusted R squared is 0.097 which is not very significant as R square value is small


##   3. `plot' the model to look for deviations from modeling assumptions

plot(EnergyUse,which=c(1,3))

str(states.en)

EnergyUse<- lm(energy~metro+pop,data = states.en)

summary(EnergyUse)

par(mfrow=c(1,2))

plot(EnergyUse)

# Multiple R-squared:  0.1155,	Adjusted R-squared:  0.07788 

EnergyUse<- lm(energy~metro+density,data = states.en)

#Multiple R-squared:  0.1397,	Adjusted R-squared:  0.1031   # Adjusted R sqre increased but both metro and denstiy are insignificant

summary(EnergyUse)
par(mfrow=c(1,2))

plot(EnergyUse)


##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income, data=states.data) 

#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table
  summary(sat.expense.by.percent)
  par(mfrow=c(2,2))
  plot(sat.expense.by.percent)
  # R sq = 0.3002 with only income is statistically significant parameter.

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,data=states.data) 
#Show the results
summary(sat.region)
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=1),   data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

EnergyUse<- lm(energy~metro,data = states.en)
summary(EnergyUse)

# adding Interation term

EnergyUse<- lm(energy~metro+pop*density,data = states.en)
summary(EnergyUse)  # Multiple R sq = 0.1162 and none of the paramters are significant
coef(summary(EnergyUse)) # show regression coefficients table
par(mfrow=c(2,2))
plot(EnergyUse)

# Added Interactions are not significant to the model.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

EnergyUse<- lm(energy~metro+region,data = states.en)
summary(EnergyUse)
EnergyUse.region<- lm(energy~region,data = states.en)
summary(EnergyUse.region)

# r sq has improved, Metro and N east are statistically significant.
coef(summary(EnergyUse.region)) # show regression coefficients table
par(mfrow=c(2,2))
plot(EnergyUse.region)
coef(summary(lm(energy ~ C(region, base=4),   data=states.data)))
EnergyUse.region<- lm(energy~C(region,base=4),data = states.en)
summary(EnergyUse.region)

# R sq remain same