# Factors of Crime and Incarceration: A Cross-Section Study
# Shukrit Guha

library(AER)
library(car)
library(sandwich)
library(lmtest)
library(xtable)
library(stargazer)

# Function for outputting regression tables
xtable.coeftest <- function (x, caption = NULL, label = NULL, align =     NULL, digits = NULL, 
                             display = NULL, ...) 
{
  class(x) <- "matrix"
  li<-list(coef=x)
  return(xtable:::xtable.summary.lm(li, caption = caption, label = label, 
                                    align = align, digits = digits, display = display))
}

######################################### LOAD AND EXAMINE DATA ###################################################

# Assign working directory and read in pre-cleaned dataset
setwd("C:/Users/shukr/Desktop/NYU_Classes/SEM 1/Applied Stats & Metrics/Project")
Incarc_data <- read.csv(file="Gartman, Guha, & Haguette Compiled Data.csv", header = TRUE)

Incarc_data <- Incarc_data[-c(51:53),-c(21:25)]
state.names = Incarc_data$ï..State
Incarc_data = Incarc_data[,-1]
rownames(Incarc_data) <- state.names
View(Incarc_data)

# quick overview of dataset (not all data utilized in final model)
summary(Incarc_data)
str(Incarc_data)

############################ APPLY STEP REGRESSION TO ASCERTAIN MOST SIGNIFICANT VARIABLES ###############################

attach(Incarc_data)

# Start with regressing incarceration on all explanatory variables
reg_1 = lm(log(Incarc_rate) ~ log(GDPPC) + log(Gini) + log(HSD) + Health + log(Poverty) +log(LFP) + log(Unemp)
           +log(V_part) +log(W_gap) + D_cont + G_cont + Corrup + Conserv +log(Religion) + log(Violent.Crime.Rate),
           data = Incarc_data)

summary(step(reg_1, direction = "backward"))
reg_1 = lm(log(Incarc_rate) ~ Health + Conserv +log(Violent.Crime.Rate), data = Incarc_data)
summary(reg_1)
# Health, Conservative Advantage and Violent Crime are significant.

# It might be that Incarceration affects health outcomes, so reverse regress with health as dependent
check_reg = lm(Health ~ log(GDPPC) + log(Gini) + log(HSD) + log(Incarc_rate) + log(Poverty) +log(LFP) + log(Unemp)
               +log(V_part) +log(W_gap) + D_cont + G_cont + Corrup + Conserv +log(Religion) + log(Violent.Crime.Rate),
               data = Incarc_data)

summary(step(check_reg, direction = "backward"))
check_reg = lm(Health ~ log(Incarc_rate) + log(Poverty) + log(Unemp) + D_cont + Conserv + log(Violent.Crime.Rate),
               data = Incarc_data)
summary(check_reg)
# No, Incarceration is not significant in explaining Health. Only Poverty, Unemployment and the Conservative advantage
# are significant (all at 1% level). Moving on..

####

# Regress crime rates on all explantory variables:

# Violent Crime
reg_2 = lm(log(Violent.Crime.Rate) ~ log(GDPPC) + log(Gini) + log(HSD) + Health + log(Poverty) +log(LFP) + log(Unemp)
               +log(V_part) +log(W_gap) + D_cont + G_cont + Corrup + Conserv +log(Religion), data = Incarc_data)

summary(step(reg_2, direction = "backward"))
reg_2 = lm(log(Violent.Crime.Rate) ~ Health + Conserv + log(Religion), data = Incarc_data)
summary(reg_2)
# Health, Conservative advantage and Religion are significant.

# Property Crime
reg_3 = lm(log(Property.Crime.Rate) ~ log(GDPPC) + log(Gini) + log(HSD) + Health + log(Poverty) +log(LFP) + log(Unemp)
                +log(V_part) +log(W_gap) + D_cont + G_cont + Corrup + Conserv +log(Religion), data = Incarc_data)

summary(step(reg_3, direction = "backward"))
reg_3 = lm(log(Property.Crime.Rate) ~ log(LFP) + Corrup + G_cont + log(Religion) , data = Incarc_data)
summary(reg_3)
# LFP, Gun Control, Corruption and Religion are significant.

# All crime
All_crime_reg = lm(log(Total.Crime.Rate) ~ log(GDPPC) + log(Gini) + log(HSD) + Health + log(Poverty) +log(LFP) + log(Unemp)
               +log(V_part) +log(W_gap) + D_cont + G_cont + Corrup + Conserv +log(Religion), data = Incarc_data)

summary(step(third_reg, direction = "backward"))
# Drug Control, Gun Control and Religion are significant.

####

cor(data.frame(Health, Incarc_rate, Violent.Crime.Rate))

## NOTE:
# Health is the most significant variable in explaining both violent crime and incarceration, but Violent crime 
# is significant in explaining incarceration. Health is also highly correlated with both. 
# Health maybe is endogenously affecting both Incarceration and Violent Crime.

# Take health out of fourth_reg and find most significant variables
reg_4 = lm(log(Incarc_rate) ~ log(GDPPC) + log(Gini) + log(HSD) + log(Poverty) +log(LFP) + log(Unemp)
               +log(V_part) +log(W_gap) + D_cont + G_cont + Corrup +Conserv + log(Religion) + log(Violent.Crime.Rate),
               data = Incarc_data)

summary(step(reg_4, direction = "backward"))
reg_4 = lm(log(Incarc_rate) ~ log(LFP) + Conserv + log(Violent.Crime.Rate), data = Incarc_data)
summary(reg_4)
# LFP replaces Health as significant variable.

############################### FIT INCARC MODEL AND TEST IV FOR VIOLENT CRIME ##########################################

# From reg_2 we determined that health is the most significant variable in explaining Violent crime.
# Thus, we want to test if Violent Crime is endogenous first, using Health as its instrument.

# Revisit reg_4
summary(reg_4)
# Violent crime and labor force participation significant at 1% level. Conservative advantage at 0.1%.

# Fit ivreg
library(AER)

?ivreg

fit1 = ivreg(log(Incarc_rate) ~ log(LFP) +Conserv +log(Violent.Crime.Rate) | log(LFP) +Conserv +Health,
             data = Incarc_data, model = T)

summary(fit1, diagnostics = T)
# Weak instruments test infers that Health is a good instrument for Violent Crime (at 1% level)
# The Wu-Hausman test signifies that violent Crime is endogenous at 5% level - IV estimator is consistent over OLS
# Notice, that the significance of Violent Crime has reduced from the OLS model i.e. from 1% to 5%.
# The same is true for conservative advantage, which is now at 1% level of significance. The labor force
# participation rate is no longer significant, even at the 10% level.

# Also notice that the coefficeint estimate for Violent Crime has increased in value. Thus, the effect of 
# Health (through Violent Crime) on the Incarceration Rate is higher than that of Violent Crime by itself.

# Possible economic interpretation:
# Health outcomes may be indirectly aggravating imprisonment of offenders by increasing the risk of violent crimes. 
# However, it may also be that health is itself endogenous. We investigate this in the next section.

## NOTE: 
# We do not include Conserv as an instrument of Violent Crime despite its significance in explaining Violent Crime 
# (Equaltion (2)), because doing so leads to Overidentification. How? -
# We first use step-wise selection to reselect the best variables to use after keeping Health and Conserv out.
# We then use those variables + Violent crime in the model and instrument for Violent Crime using Health and Conserv
# But doing so reveals a significant Sargan Test (The model is inferred as overidentified)

#################################### CHECK IF HEALTH IS ENDOGENOUS TOO #############################################

# Because of health's high significance in explaining both incarceration and Violent Crime, we suspected that Health
# itself might be an endogenous variable. That is, something could be causing both Health to be low and 
# Incarceration to be high (Endogeneity). This is further justified by the use of some common variables in 
# calculating the index (such as violent crime, high-school graduation, children in poverty etc.). 
# So we select the best set of predictors for instrumenting Health from:

health_reg = lm(Health ~ log(GDPPC) + log(Gini) + log(HSD) + log(Poverty) +log(LFP) + log(Unemp)
                +log(V_part) +log(W_gap) + D_cont + G_cont + Corrup + Conserv +log(Religion), data = Incarc_data)

summary(step(health_reg, direction = "backward"))
# Poverty, Unemployment and Conservative Advantage.

reg_5 = lm(Health ~ log(Poverty) + log(Unemp) + Conserv, data = Incarc_data)

# Assuming Health is Endogenous, we fit the ivreg instrumenting Health with all three significant variables i.e.
# Poverty, Unemployment and the Conservative Advantage. 
library(AER)
summary(lm(log(Incarc_rate)~ log(Violent.Crime.Rate)+Health +Conserv, data = Incarc_data))

fit2 = ivreg(log(Incarc_rate) ~ log(Violent.Crime.Rate) +Health +Conserv | log(Violent.Crime.Rate) +Conserv 
                     +log(Poverty) + log(Unemp), data = Incarc_data)

summary(fit2, diagnostics = T)
# They are good instruments; no endogeneity or overidentification so OLS is better than IV, but 
# health remains significant at the 0.1% level!

###################### DIAGNOSTIC TEST RESULTS ##############################################

# Now that we know that Violent crime is endogenous with respect to Incarceration (using Health as an instrument)
# Lets examine Equation (1) simple linear model of Incarc on Health, Conserv and Violent Crime

# Final regression for Incarceration regression
Incarc_reg = lm(log(Incarc_rate) ~ Health + Conserv + log(Violent.Crime.Rate), data = Incarc_data)
summary(Incarc_reg)
# Health is expectedly most significant, then Conserv and Violent Crime

# test for Multicollinearity
library(car)
vif(Incarc_reg)
# All values much below 10, highest being Health at 2.26

# Heteroscedasticity
library(lmtest)
bptest(Incarc_reg)
# p-val = 0.08246 : (there exists some heteroscedasticity), so correct standard errors and test coefficients

library(sandwich)

?vcovHC
?coeftest

t = coeftest(Incarc_reg, vcov = vcovHC(Incarc_reg))
xtable.coeftest(t)
# Only Health remains significant, Violent Crime drops out.

## THUS, correcting for heteroscedastic errors immediately drop Violent Crime and Conserv out of the model.
## Correcting their standard errors reveal their insignificance in explaining Incarceration, while Health 
## remains significant.

# Autocorrelation in errors
dwtest(Incarc_reg)
# p-value of 0.3385, fail to reject null of normally distributed errors. 

# Multicollinearity
vif(reg_1)
vif(reg_2)
vif(reg_3)
vif(reg_4)
vif(reg_5)
# No serious multicollineariy

######################################## CONCLUSIONS ######################################################

# Its easy to see that Health is undoubtedly the single biggest explanation to incarceration and Violent Crime.
# In the first section, we suspected health to be influencing Incarceration through its effect on Violent Crime because
# Health was most significant in explaining both - through best subset selection. Our results indicated that Violent
# Crime truly was endogenous, and that Health was a good instrument for Violent Crime. In the IV regression,
# the significance of Violent Crime and Conserv are both reduced by the inclusion of Health as an instrument, but the
# estimates for Violent crime increases by more than twofold (from ~0.29 in OLS to ~0.77 in IV).

# We fitted a linear model specified by reg_1 to obtain the same results as the subset selection. However, 
# applying the Breusch-Pagan test to our fitted model, we found hetereoscedatic errors. Correcting for them and 
# testing the coefficients again for significance revealed that conserv and Violent Crime dropped out, leaving 
# only Health behind.

# Lastly, we suspected that health might be an endogenous variable itself - such that other factors might be influencing
# both health and incarceration, making it look like health is affecting incarceration in some way.
# We tested this hypothesis by using LFP and Unemp as instruments for Health in an IV regression with the same model
# specification as in Incarc_reg. Health was not endogenous, though its significance reduced when accounting for
# the LFP and Unemployment rate in the State. Thus, the OLS model was better than the IV model.

## ------------------------------- OUTPUTTING REG TABLES AND SUMMARY STATS ---------------------------

# ?vcovHC
# 
# 
# rob.reg1 <- coeftest(reg_1, function(x) vcovHC(x, type="HC0"))
# rob.reg2  <- coeftest(reg_2, function(x) vcovHC(x, type="HC0"))
# rob.reg3 <- coeftest(reg_3, function(x) vcovHC(x, type="HC0"))
# rob.reg4  <- coeftest(reg_4, function(x) vcovHC(x, type="HC0"))
# rob.reg5  <- coeftest(reg_5, function(x) vcovHC(x, type="HC0"))
# 
# stargazer(reg_1, reg_2, reg_3, reg_4, reg_5, type = "text", 
#           se = list(rob.reg1[,"Std. Error"], rob.reg2[,"Std. Error"], rob.reg3[,"Std. Error"], rob.reg4[,"Std. Error"],
#                     rob.reg5[,"Std. Error"]), out = "1234_GGH.tex")
# 
# 
# rob.fit1 <- coeftest(fit1, function(x) vcovHC(x, type="HC0"))
# rob.fit2  <- coeftest(fit2, function(x) vcovHC(x, type="HC0"))
# summ.fit1 <- summary(fit1, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
# summ.fit2 <- summary(fit2, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
# 
# stargazer(fit1, fit2, type = "text", 
#           se = list(rob.fit1[,"Std. Error"], rob.fit2[,"Std. Error"]), 
#           add.lines = list(c(rownames(summ.fit1$diagnostics)[1], 
#                              round(summ.fit1$diagnostics[1, "p-value"], 2), 
#                              round(summ.fit2$diagnostics[1, "p-value"], 2)), 
#                            c(rownames(summ.fit1$diagnostics)[2], 
#                              round(summ.fit1$diagnostics[2, "p-value"], 2), 
#                              round(summ.fit2$diagnostics[2, "p-value"], 2))),
#           out = "iv_GGH.tex")
# 
# 
# ?stargazer


# m = round(apply(Incarc_data,2, FUN = mean),3)
# s.d = round(apply(Incarc_data,2, FUN = sd),3)
# 
# rbind(m,s.d)

