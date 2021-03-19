#### mise en place ####

# remove all objects from workspace
rm(list=ls())

# install & load packages
install.packages("metafor")
install.packages("robumeta")
install.packages("devtools")
install_github("jepusto/clubSandwich")
install.packages("vioplot")

library(metafor)
library(robumeta)
library(devtools)      # to download clubSandwich
library(clubSandwich)
library(vioplot)
library(dplyr)
library(grid)

# load data from file
neet <- read.csv("~/Dropbox/PEPP/NEET meta-analysis/metaanalysis/neet 20201027.csv")





#### fit RVE random-effects model with correlated effects weights ####


# create violin plot to visualize the plot
str(neet)

vioplot(neet$OR_ln, col="grey", names="NEET mental health associations")
title(ylab="log OR")


# rho assumed = 0.8; sensitivity analyses could vary rho from 0.1 to 0.9 
# small = TRUE option applies the small sample correction to df 
re.all<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
            modelweights = "CORR", rho = 0.8, small=TRUE, data=neet) 
print(re.all)




#### testing using distress as example ####
dis <- filter(neet, Distress==1 & is.na(Repeated_sample)==TRUE & is.na(rep_dis)==TRUE & !is.na(OR_ln))
dis
View(dis)

dis.full <- filter(neet, Distress==1 & !is.na(OR_ln))
dis.full
View(dis.full)

vioplot(dis.full$OR_ln, col="grey", names="NEET-MH association")

rve.dis.full<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
             modelweights = "CORR", rho = 0.8, small=TRUE, data=dis.full) 
print(rve.dis.full)

# forest plot
# 1 default
forest.robu(rve.dis.full, es.lab = "Mental.health.measures", study.lab = "Study",
            "OR_ln" = effect.size, # optional column
            "Weight" = r.weights)        # optional column

# 2 
forest.robu(rve.dis.full, es.lab = "Mental.health.measures", study.lab = "Study",
            "log OR" = effect.size, "Weight" = r.weights, "OR" = OR)

View(neet)



#### mental health measure ####

# creating deduplicated subsets for main analysis
dis <- filter(neet, Distress==1 & is.na(Repeated_sample)==TRUE & is.na(rep_dis)==TRUE & !is.na(OR_ln))
dis

mood <- filter(neet, Mood==1 & Gender=="all" & is.na(Repeated_sample) & !is.na(OR_ln))
mood

anx <- filter(neet, Anxiety==1 & Gender=="all" & is.na(Repeated_sample) & !is.na(OR_ln))
anx

beh <- filter(neet, Behavioral==1 & Gender=="all" & is.na(Repeated_sample) & is.na(rep_beh))
beh

sui <- filter(neet, Suicidal==1 & Gender=="all" & is.na(Repeated_sample))
sui

# binding together
mh <- bind_rows(dis, mood, anx, beh, sui)
mh

# rve random effects model
mh.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                   modelweights = "CORR", rho = 0.8, small=TRUE, data=mh) 
print(mh.rve)

exp(0.273)

exp(0.0704)
exp(0.476)

# forest plot
forest.robu(mh.rve, es.lab = "Mental.health.measures", study.lab = "Study",
            "log OR" = effect.size, # optional column
            "Weight" = r.weights)   # optional column

#### substance use measure ####

# creating deduplicated subsets for main analysis
alc <- filter(neet, Alcohol==1 & Gender=="all" & is.na(Repeated_sample) & !is.na(OR_ln))
alc

can <- filter(neet, Cannabis==1 & Gender=="all" & is.na(Repeated_sample))
can

drg <- filter(neet, Drug==1 & Gender=="all" & is.na(Repeated_sample) & is.na(rep_drg) & !is.na(OR_ln))
drg

# binding together
su <- bind_rows(alc, can, drg)
su

# rve random effects model
su.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
             modelweights = "CORR", rho = 0.8, small=TRUE, data=su) 
print(su.rve)

exp(0.359)

exp(0.0794) 
exp(0.638)

# forest plot
forest.robu(su.rve, es.lab = "Mental.health.measures", study.lab = "Study",
            "log OR" = effect.size, # optional column
            "Weight" = r.weights)   # optional column
dev.off()

# alternative plot, includes N
forest.robu(su.rve, es.lab = "Mental.health.measures", study.lab = "Study",
            "log OR" = effect.size, "Weight" = r.weights, "n" = N)
dev.off()

# shows all the model output 
str(su.rve)  

#### overall measure ####

# creating deduplicated subsets for main analysis
any <- filter(neet, Any==1 & Gender=="all" & is.na(rep_any))
any

# binding together
all <- bind_rows(mh, su, any)
all

# rve random effects model
all.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
             modelweights = "CORR", rho = 0.8, small=TRUE, data=all) 
print(all.rve)

exp(0.321)        

exp(0.144) 
exp(0.497)

# forest plot
forest.robu(all.rve, es.lab = "Mental.health.measures", study.lab = "Study",
            "log OR" = effect.size, # optional column
            "Weight" = r.weights)   # optional column

