                            ## Importing the nessecery libraries
library(dplyr)
library(readr)
library(readxl)
library(psych)
library(ggplot2)
library(Amelia)
                            ## Importing the data
data = read_xlsx('LOWBWT.xlsx')
                            ##Eye-balling the dataset
head(data)
dim(data)
## Our dataset consists of 6 columns and a 100 rows. That means we are dealing with 6 variables and 100 observations
## Every variable represents a different clinical fact
## sbp = systolic blood preassure
## sex = gender
## tox = The experience of pre-eclampsia
## grmhem = The experience of brain haimorrage
## gestage = The duration of pregnancy
## apgar5 = The apgar scale at 5 minutes after birth
str(data)
any(is.na(data))
sum(is.na(data))
colSums(is.na(data))
missmap(data,main = 'Missing Map',col = c('yellow','black' ),legend = F)
## sex and tox are dichotomus variables so lets convert them
data$sex = as.factor(data$sex)
levels(data$sex) = c(' Male',' Female')
data$tox = as.factor(data$tox)
levels(data$tox) = c(' No',' Yes')
str(data)
data$grmhem = as.factor(data$grmhem)
levels(data$grmhem) = c('Without hemorrhage','Hemorrhage')
## apgar5 is a catigorical ordinal variable so lets handle it like that 
data$apgar5 = as.factor(data$apgar5)
                            ## Discriptives statiticsa and visualization
## Systolic blood preassure is a continues variale so we are going to describe it and visualize it with
## box plot and histogram
describe(data$sbp)
describeBy(data$sbp, group = data$grmhem, mat = T)
sbp_box = ggplot(data,aes(y = sbp, x = factor(grmhem))) + geom_boxplot(aes(fill = factor(grmhem))) + theme_light()
print(sbp_box)
sbp_hist = ggplot(data, aes(x = sbp)) + geom_histogram(aes(fill = 'red')) + theme_light()
print(sbp_hist)


## Gender is a nominal (dichotomus) variable so we are going to calculate only percentages and counts
## The visulization will include pie chart and bar chart

data %>% count(sex)

data %>%
  count(sex) %>%
  mutate(percentage = round(n / sum(n) * 100))

table(data$grmhem,data$sex)

library(dplyr)

data %>% group_by(grmhem, sex) %>% summarise(count = n()) %>% group_by(grmhem) %>% mutate(proportion = count / sum(count))

ggplot(data,aes(x = sex, fill = sex)) + geom_bar() + theme_light()
ggplot(data,aes(x = sex, fill = sex)) + geom_bar() +coord_polar() + theme_light()
proportions = table(data$sex) / length(data$sex)
pie(proportions,labels = sprintf("%s %.1f%%", names(proportions), proportions * 100), main = 'Males and Females percentages')

## toxemia is a dichotous variable so we are going to calclate the percentages and counts
## The visualization will incude pie chart and bar chart
data %>% count(tox)
## As we see there are 21 woomen who had experienced pre-eclampsia

data %>%
  count(tox) %>%
  mutate(percentage = round(n / sum(n) * 100))
## the exact percentage is 21%

table(data$grmhem,data$tox)
## One wooman who had ecperienced pre-eclampsia have delived a baby with a brain hemorrhage
## Fourteen woomen with no experience of pre-eclampsia hace delivered a baby with brain hemorrhage


data %>% group_by(grmhem, sex) %>% summarise(count = n()) %>% group_by(grmhem) %>% mutate(proportions = count / sum(count))
## Here we cam see the percentages of males and females grouped by the hemorrhagic event
## Above babies who had experienced a brain hemorrhage event 26% was males and 74% was females

ggplot(data, aes(x = tox, fill = tox)) + geom_bar() + theme_light()
proportions_tox = table(data$tox) / length(data$tox)
pie(proportions_tox, labels = sprintf("%s %.1f%%", names(proportions_tox),proportions_tox*100), main = 'Percentages of pre-eclampsia')

## gestage is numerical continues variable so we are going to describe it and visualize it with histogram and boxplot
describe(data$gestage)
## Again we describe the variable grouped by the hemorrhagic event
describeBy(data$gestage, group = data$grmhem, mat = T)
## Fifteen babies who had brain hemorrhage hada a mean gestational age of 28.1 weeks

ggplot(data, aes(gestage)) + geom_histogram(aes(fill = 'red')) + theme_light()
ggplot(data, aes(x = factor(grmhem),y = gestage )) + geom_boxplot(aes(fill = factor(grmhem))) + theme_light()

## apgar5 is a catigorical ordinal variable so we will take only the counts and percentages
data %>% count(apgar5)
## The most common apgare score in 9 with 24 observations and the less common is 2 with only one observation

data%>% count(apgar5) %>% mutate(percentage =round(n / sum(n) * 100 ))
## The percentage of each apgar5 score

data %>% group_by(grmhem, apgar5) %>% summarise(count = n()) %>% group_by(grmhem) %>% mutate(proportions = count / sum(count))
## The percentage of each apgar5 score grouped by the hemorrhagic event
## 20% of babies who had scales with 6 fibaly had hemorrhage and 35% of babies with score 3 and 4 have never experience the event 

ggplot(data,aes(x = apgar5)) + geom_bar(fill = 'lightblue') + theme_light()
proportions_apgar = table(data$apgar5) / length(data$apgar5)
pie(proportions_apgar, labels = sprintf("%s %.1f%%", names(proportions_apgar),proportions_apgar*100), main = 'Percentages of apgar score')                                

## At this point we can check for correlations between numeric variables
num_col = sapply(data, is.numeric)
corr_data = cor(data[,num_col])
print(corr_data)
## Lets visualize them
library(corrplot)
library(corrgram)
corrplot(corr_data, method = 'color')
corrgram(data, order = TRUE,
         lower.panel = panel.shade,
         upper.panel = panel.pie,
         text.panel = panel.txt)
                                  ##Logistic regression
## We want to see the odds of a baby experiencing a brain hemorrhage event based on the above described variables
#First of all we need to import the appropriate libraries for the analysis#
library(epiDisplay)
#Assumptions#
#Binary Outcome
#Linearity in logistic scale
#No multicolliniarity of independent Variables
#After that we import the dataset, check for its structure and type of variables as well as the existence of missing values#
data = read_excel('LOWBWT.xlsx')
View(data)
str(data)
colSums(is.na(data))




#Univariate logistic regression models#
model_sbp = glm(grmhem  ~ sbp, data = data, family = binomial)
summary(model_sbp)
logistic.display(model_sbp)
exp(model_sbp$coefficients)
#As we see the coefficient has a negative correlation with brain hemorrhage in a ligistic scale
#Showing us the change in the log of odds in Dependent Variable for every unit of change in Independent Variable#
#In the physical scale the spb coefficient shows us that for every one unit increase in sbp the probability
#of experiencing a brain hemorrhage event is decreasing by 5%#
#The intercept is not interpretable in this case because shows as the probability of experiencing the event 
#when the blood pressure is 0#
#The log likelihood is a model of comparison that shows us how well the model fits the data. Higher values
#indicate better fitting#
#The AIC is also a model that measures the goodness of fit. Lower values indicate a better model#

#We will change the type of variables sex and tox into factor for the shake of presentation
data$sex = as.factor(data$sex)
levels(data$sex) = c(' Male',' Female')
model_sex = glm(grmhem ~ sex, data = data, family=binomial)
summary(model_sex)
logistic.display(model_sex)
exp(model_sex$coefficients)
#Analizing the sex variable we can see that it has a positive correlation with the brain henorrhage event
#In the physical scale the sex coefficient show as that female babies are 2.4 times more prone in experiencing a
#brain hemorrhage event than male babies, but the results are not statistically significant as well as 
#the p-value is greater that 0.05 and the confidence interval includes the number one#
#Here the intercept is interpretable and show us the odds of a male baby experiencing a brain hemorrhage event#
data$tox = as.factor(data$tox)
levels(data$tox) = c(' No',' Yes')
model_tox = glm(grmhem ~ tox, data = data, family = binomial)
summary(model_tox)
logistic.display(model_tox)
exp(model_tox$coefficients)


model_gest = glm(grmhem ~ gestage, data = data, family = binomial)
summary(model_gest)
logistic.display(model_gest)
exp(model_gest$coefficients)

model_apgar = glm(grmhem ~ apgar5, data = data, family = binomial)
summary(model_apgar)
logistic.display(model_apgar)
exp(model_apgar$coefficients)


#Multivariate regression model#
#Full model without p_value ristrictions#
Multi_v_model = glm(grmhem  ~ ., data =data, family = binomial)
summary(Multi_v_model)
logistic.display(Multi_v_model)
exp(Multi_v_model$coefficients)
#In the full multivariate logistic regression model we can see the estimated coefficients for every IV
#without applying a threshold of p-value lower than 0.1%. The estimated coefficients shows us the odds of
#experiencing the event in a particular variable, adjusting all the other variables. Here seems that non of 
#the estimated coefficients is statistically significant. Thus we will perform another regression by only 
#including the statistically significant variables from the univariate analysis. Then, we are going to 
#compare the models in terms of goodness of fit#


#Reduced model after p_value thresshold of 0.1#
Multi_v_model_reduced = glm(grmhem ~ sbp + apgar5, data = data, family = binomial)
summary(Multi_v_model_reduced)
logistic.display(Multi_v_model_reduced)
exp(Multi_v_model_reduced$coefficients)
#In the reduced model we can see the estimated coefficients of sbp and apgar5 variables.The coefficients 
#shows us the odds of experiencing a brain hemorrhage event for every unit increase in a IV adjusting the
#other IV's#



#Models' comparison#
# Anova and Likelihood ratio test #
# H0 there is no improvement in model fit when additional parameters are included in the model #
anova(Multi_v_model,Multi_v_model_reduced, test = 'Chisq')
#The p-value is greater from 0.05, indicating that the full model does not fit sifnificantly better
#the data than the reduced model#

#Likelihood ratio test#
# H0 there is no improvement in model fit when additional parameters are included in the model #
lrtest(Multi_v_model, Multi_v_model_reduced)


#Multicolliniarity diagnostics#
library(car)
vif(Multi_v_model_reduced)
#VIF is approximately 1 so there is no concern of multicolliniarity#

#Stepwise model selection#
#Backward selection#
Mod_sel_back = step(glm(grmhem ~ ., data = data, family = binomial()), direction = 'backward')
#In backward selection we are trying to detect the IV's that creates the best fitting model via 
#including every IV and reducing them step-wisely based on lowest upcoming AIC#
#We end up with the same IVs as by hand model selection#

#Forward selection#
Mod_sel_forward = step(glm(grmhem ~ 1, data = data, family= binomial()),
                       direction = 'forward',
                       scope =  ~ sbp + sex + tox + gestage + apgar5)
#In backward selection we are trying to detect the IV's that creates the best fitting model starting
#from the point that the model has no IVs. It calculates the AIC based only on in the intercept and 
#includes only the IV that induce a better model according to the calculated AIC#
#Here we end up with the same IVs as by hand model selection#


#Fake patients prediction
patient_table =data.frame(sbp = c(40,35,80),apgar5 = c(5,2,9))
rownames(patient_table) = c('Patient_1','Patient_2','Patient_3')
predicted_probs = predict(Multi_v_model_reduced,patient_table, type = 'response')
print(predicted_probs)
patient_table$brnhem_probs = round(predicted_probs,2)
View(patient_table)
#At this time we have created a model that fits the data well and explained the estimated coefficient for each variable
#This model could calculate the probabilities for upcoming patients based on their systolic blood pressure and apgar scale
#The patient table showed that for patient_1 the probability of experiencing the event is 21%, for patient_2 39%
#and for patient_3 only 2%


#Results tables creation#
library(gtsummary)
library(gt)


uni_v_reg = tbl_uvregression(data[c('sbp','sex','tox','grmhem','gestage','apgar5')],
                             method = glm,
                             y = grmhem,
                             method.args = list(family = binomial),
                             exponentiate = TRUE,
                             conf.level = 0.95)
uni_v_reg

multi_v_reg = tbl_regression(Multi_v_model, exponentiate = TRUE) 
multi_v_reg


multi_v_reg_reduced = tbl_regression(Multi_v_model_reduced, exponentiate = TRUE)
multi_v_reg_reduced

tbl_merge(tbls = list(uni_v_reg,multi_v_reg,multi_v_reg_reduced),
          tab_spanner = c('Univariate', 'Multivariate','Multivariate Reduced'))

tbl_merge(tbls = list(uni_v_reg,multi_v_reg_reduced),
          tab_spanner = c('Univariate','Multivariate'))

#Tables extraction
library(ggpubr)
library(gridExtra)

table_plot = ggtexttable(patient_table, rows = rownames(patient_table), theme = ttheme("classic"))
ggsave("patient_table.png", table_plot, width = 6, height = 3)

mini_data_table = head(data)
data_plot = ggtexttable(mini_data_table, rows = rownames(mini_data_table),theme = ttheme('classic'))
ggsave('mini_data_table.png',data_plot, width = 6, height = 3)
         