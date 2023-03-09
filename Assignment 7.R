#
pacman::p_load(
  tidyverse,    # data management + ggplot2 graphics
  dplyr,        # select
  ggplot2,      # ggplot2 graphics
  MASS,         # step wise model , linear discriminant, proportional odds, negative binomial
  survival,     # survival analysis
  ranger,       # time-to-event models with the large data 
  ggfortify,    # handsome, one-line survival plots
  read_excel,

  skimr,        # get overview of data
  tidymodels,   # for tidy modelling
  survey,       # for survey functions
  srvyr,        # for tidy survey work
  lubridate,    # for converting date character to date format
  tidyquant,    # for tidy time series functions
  patchwork,    # easily combine ggplot rasters
  plyr,         # for seeing the frequency
  freqtables,   # for frequency table
  corrplot,     # for plotting the correlation 
  glue,
  ggpubr,
  car,          # omparison of the regression coefficients
  lmtest,       # package is used to conduct the Wald test
  mice,         # multiple imputation 
  pROC,         # ROC curve
  
  caret,        # for easy machine learning workflow
  gmodels,      # for cross tab percentage
  readxl        #to read xlsx
)
options (scipen=999)

UIS_data <- read_excel("C:/Users/humay/Dropbox/HRM/HRM 753/Assignments/Assignment 7/UIS data.xlsx")

save.image("C:/Users/humay/Dropbox/HRM/HRM 753/Assignments/Assignment 7/UIS data.RData")

#here the censor variable indicate (censor=1) the return to drug use.
#censor=1 is return to drug use (so event occurring)
#censor=0 otherwise (so no event, lost to follow up or something like actual censor)

#Distribution of the study variables
sum (is.na (UIS_data$age))



#1.	Describe graphically the distribution of time to drug relapse.

hist(UIS_data$time)



#2.	Estimate the survival function, the cumulative incidence function, and the hazard function.


# Kaplan Meier Survival Curve
library (survival)
library(survminer) #publication-ready survival plots

km_fit <- survfit(Surv(time, censor)~1, data=UIS_data)

ggsurvplot(km_fit) # KM plot 


#survival function
ggsurvplot(km_fit, conf.int=T, pval=F, risk.table=T,
           legend="none", legend.labs="", legend.title="",
           title="Kaplan-Meier Curve for return to drug use",
           ggtheme = theme(plot.title = element_text(hjust = 0.5)),
           risk.table.height=.15)

#cumulative incidence 
library (tidycmprsk)
cuminc(Surv(time, censor) ~ 1, data = UIS_data)
cuminc(Surv(time, censor) ~ 1, data = UIS_data) %>% 
  ggcuminc() + 
  labs(
    x = "Days"
  ) + 
  add_confidence_interval() +
  add_risktable()


#Cumulative Hazard functions
ggsurvplot(km_fit, conf.int=T, pval=F, risk.table=T,
           legend="none", legend.labs="", legend.title="",
           title="Kaplan-Meier Curve for return to drug use",
           ggtheme = theme(plot.title = element_text(hjust = 0.5)),  fun = "cumhaz",
           risk.table.height=.15)


# Fit Cox Model
cox <- coxph(Surv(time, censor) ~ 1 , data = UIS_data)
summary(cox)




#3.	Test whether treatment is effective, in the following ways

  #a.	Overall

#we look at survival curves by treatment.
km.fit.treat <- survfit(Surv(time, censor) ~ treat, data=UIS_data)

ggsurvplot(km.fit.treat, conf.int=F, pval=T, risk.table=F,
           legend.title="", legend.labs =
             c("No", "Yes"),
           title="Kaplan-Meier Curve for return to drug use by long treatment",
           ggtheme = theme(plot.title = element_text(hjust = 0.5)),  
           risk.table.height=.15)



  #3.b.	Stratified by age group (<30, 30-35, >35)
UIS_data$age
UIS_data$age.cat= ifelse (UIS_data$age>35, ">35",
                          ifelse(UIS_data$age>29, "30-35", "<30"
                          )
  )
UIS_data$age.cat

# Fit Kaplan-Meier model stratified by age group and treatment
km.fit.teat.age <- survfit(Surv(time, censor) ~  treat +age.cat, data=UIS_data)

ggsurvplot(km.fit.teat.age)

ggsurvplot(km.fit.teat.age, conf.int=F, pval=T, risk.table=F, 
           legend.labs=c("Treat=No,<30", 
                         "Treat=No,30-35", 
                         "Treat=Yes,>35",
                         "Treat=No,>35",
                         "Treat=Yes,<30",
                         "Treat=Yes,30-35"), 
           legend.title="", 
           title="Kaplan-Meier Curve for long treatment effectiveness by age", 
           ggtheme = theme(plot.title = element_text(hjust = 0.5)),
           risk.table.height=.15) #confidence intervals, show the p-value for the log-rank test


  #c.	Stratified by number of prior drug treatments (0 or 1, 2 or more)

UIS_data$ndrugtx.cat= ifelse (UIS_data$ndrugtx=="0" | UIS_data$ndrugtx=="1" , "0/1",
                               "2 or more"
                              )
UIS_data$ndrugtx.cat


km.fit.treat.ndrugtx <- survfit(Surv(time, censor) ~ treat+ndrugtx.cat, data=UIS_data)

ggsurvplot(km.fit.treat.ndrugtx)

ggsurvplot(km.fit.treat.ndrugtx, conf.int=F, pval=T, risk.table=F, 
           legend.labs=c("Treat=No, prior.drug=0 or 1", 
                         "Treat=No, prior.drug=2 or more", 
                         "Treat=Yes, prior.drug=0 or 1", 
                         "Treat=Yes, prior.drug=2 or more"), 
           legend.title="", 
           title="Kaplan-Meier Curve for long treatment effectiveness by prior drug use", 
           ggtheme = theme(plot.title = element_text(hjust = 0.5)),
           risk.table.height=.15) #confidence intervals, show the p-value for the log-rank test


  #d.	Stratified by heroine-cocaine use (heroin and cocaine use, either heroin or cocaine use, neither heroin nor cocaine use)

km.treat.fit.herco <- survfit(Surv(time, censor) ~ treat+herco, data=UIS_data)


ggsurvplot(km.treat.fit.herco)

ggsurvplot(km.treat.fit.herco, conf.int=F, pval=T, risk.table=F, 
           legend.labs=c("Treat=No,Heroin+cocaine", 
                         "Treat=No,None", 
                         "Treat=Yes,Heroin/cocaine", 
                         "Treat=No,Heroin/cocaine",
                         "Treat=Yes,Heroin/cocaine",
                         "Treat=Yes,None"), 
           legend.title="", 
           title="Kaplan-Meier Curve for long treatment effectiveness by heroin and cocaine use", 
           ggtheme = theme(plot.title = element_text(hjust = 0.5)),
           risk.table.height=.15) #confidence intervals, show the p-value for the log-rank test

#4.	How would you study the intervention effect accounting for the effect of age, number of prior drug treatments, and heroin or cocaine use?
typeof (UIS_data$treat)
typeof (UIS_data$herco)

UIS_data$herco.fct = as.factor(UIS_data$herco)
UIS_data$herco.fct

# Fit Cox Model
fit.cox <- coxph(Surv(time, censor)~treat + age.cat  + ndrugtx.cat+herco.fct, data = UIS_data)
fit.cox
summary (fit.cox)

hazard_ratios= exp(coef(fit.cox))
ci=exp(confint(fit.cox))
hazard_ratios
ci

#being female (sex=2) reduces the hazard by a factor of 0.59, or 41%. Being female is associated with good prognostic.

