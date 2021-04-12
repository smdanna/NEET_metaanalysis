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



#### inspect the data ####
# create violin plot to visualize the plot
str(neet)
View(neet)

vioplot(neet$OR_ln, col="grey", names="NEET mental health associations")
title(ylab="log OR")



# fit RVE random-effects model with correlated effects weights

# rho assumed = 0.8; sensitivity analyses could vary rho from 0.1 to 0.9 
# small = TRUE option applies the small sample correction to df 

## mental health measures -------------------------------------------------------------------------------

# creating deduplicated subsets for main analysis
dis <- filter(neet, Distress==1 & is.na(Repeated_sample)==TRUE & is.na(rep_dis)==TRUE & !is.na(OR_ln))
dis

mood <- filter(neet, Mood==1 & Gender=="all" & is.na(Repeated_sample) & !is.na(OR_ln))
mood

anx <- filter(neet, Anxiety==1 & Gender=="all" & is.na(Repeated_sample) & !is.na(OR_ln))
anx

beh <- filter(neet, Behavioral==1 & Gender=="all" & is.na(Repeated_sample) & is.na(rep_beh))
beh

# binding together into one measure
mh <- bind_rows(dis, mood, anx, beh)
mh

# rve random effects model with correlated effects weights
mh.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                   modelweights = "CORR", rho = 0.8, small=TRUE, data=mh) 
print(mh.rve)

# exponentiate to obtain OR and CI
exp(0.243)

exp(0.0564)
exp(0.43)

# show more of what's going on 
summary(mh.rve)
str(mh.rve)  

# forest plot
forest.robu(mh.rve, es.lab = "Mental.health.measures", study.lab = "Study",
            "log OR" = effect.size, # optional column
            "Weight" = r.weights)   # optional column
dev.off()

# mh _ directionality ####

dis.dir <- filter(neet, Distress==1 & Gender=="all")
dis.dir
mood.dir <- filter(neet, Mood==1 & Gender=="all")
mood.dir
anx.dir <- filter(neet, Anxiety==1 & Gender=="all")
anx.dir
beh.dir <- filter(neet, Behavioral==1 & Gender=="all")
beh.dir

mh.dir <- bind_rows(dis.dir, mood.dir, anx.dir, beh.dir)

# Temporality.of.measures                      
# MH-->NEET 

mh.dir.mh_neet <- filter(mh.dir, Temporality.of.measures=="MH-->NEET")
mh.dir.mh_neet

# rve random effects model
mh.dir.mh_neet.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
             modelweights = "CORR", rho = 0.8, small=TRUE, data=mh.dir.mh_neet) 
print(mh.dir.mh_neet.rve)

# exponentiate to obtain OR and CI
exp(0.282)

exp(0.00997)
exp(0.554)

# forest plot 
# DOESNT WORK; HAVE TO TWEAK
forest.robu(mh.dir.mh_neet.rve, es.lab = "Mental.health.measures", study.lab = "Study",
            "log OR" = effect.size, # optional column
            "Weight" = r.weights)   # optional column
dev.off()

# Temporality.of.measures
# NEET-->MH

mh.dir.neet_mh <- filter(mh.dir, Temporality.of.measures=="NEET-->MH")
mh.dir.neet_mh

# rve random effects model
mh.dir.neet_mh.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
             modelweights = "CORR", rho = 0.8, small=TRUE, data=mh.dir.neet_mh) 
print(mh.dir.neet_mh.rve)

# exponentiate to obtain OR and CI
exp(0.276)

exp(-1.59)
exp(2.14)

# forest plot
# HAVE TO TWEAK
forest.robu(mh.rve, es.lab = "Mental.health.measures", study.lab = "Study",
            "log OR" = effect.size, # optional column
            "Weight" = r.weights)   # optional column
dev.off()

# mh _ age ####

dis.age <- filter(neet, Distress==1 & Gender=="all" & is.na(Age_at_interview)==FALSE)
mood.age <- filter(neet, Mood==1 & Gender=="all" & is.na(rep_mood.age) & is.na(Age_at_interview)==FALSE)
anx.age <- filter(neet, Anxiety==1 & Gender=="all" & is.na(rep_anx.age) & is.na(Age_at_interview)==FALSE)
beh.age <- filter(neet, Behavioral==1 & Gender=="all" & is.na(rep_beh.age) & is.na(Age_at_interview)==FALSE)

mh.age <- bind_rows(dis.age, mood.age, anx.age, beh.age)
mh.age

# Age_at_interview
# <18

mh.age.under18 <- filter(mh.age, Age_at_interview=="<18")
mh.age.under18

# rve random effects model
mh.age.under18.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                         modelweights = "CORR", rho = 0.8, small=TRUE, data=mh.age.under18) 
print(mh.age.under18.rve)

# exponentiate to obtain OR and CI
exp(0.182)

exp(-0.519)
exp(0.882)

# Age_at_interview
# >=18

mh.age.18plus <- filter(mh.age, Age_at_interview==">=18")
mh.age.18plus

# rve random effects model
mh.age.18plus.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                         modelweights = "CORR", rho = 0.8, small=TRUE, data=mh.age.18plus) 
print(mh.age.18plus.rve)

# exponentiate to obtain OR and CI
exp(0.247)

exp(-0.126)
exp(0.62)

# mh _ gender ####

dis.gen <- filter(neet, Distress==1 & Gender!="all" & is.na(Repeated_sample))
mood.gen <- filter(neet, Mood==1 & Gender!="all")
anx.gen <- filter(neet, Anxiety==1 & Gender!="all" & is.na(Repeated_sample))
beh.gen <- filter(neet, Behavioral==1 & Gender!="all" & is.na(Repeated_sample))

mh.gen <- bind_rows(dis.gen, mood.gen, anx.gen, beh.gen)
mh.gen

# Gender
# girl

mh.gen.girl <- filter(mh.gen, Gender=="girl")
mh.gen.girl

# rve random effects model
mh.gen.girl.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                         modelweights = "CORR", rho = 0.8, small=TRUE, data=mh.gen.girl) 
print(mh.gen.girl.rve)

# exponentiate to obtain OR and CI
exp(0.109)

exp(-0.22)
exp(0.438)

# Gender
# boy

mh.gen.boy <- filter(mh.gen, Gender=="boy")
mh.gen.boy

# rve random effects model
mh.gen.boy.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                        modelweights = "CORR", rho = 0.8, small=TRUE, data=mh.gen.boy) 
print(mh.gen.boy.rve)

# exponentiate to obtain OR and CI
exp(0.0661)

exp(-0.294)
exp(0.426)

## substance use measures -------------------------------------------------------------------------------

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

# exponentiate to obtain OR and CI
exp(0.359)

exp(0.0794) 
exp(0.638)

# forest plot
forest.robu(su.rve, es.lab = "Mental.health.measures", study.lab = "Study",
            "log OR" = effect.size, # optional column
            "Weight" = r.weights)   # optional column
dev.off()

# forest plot, includes N
forest.robu(su.rve, es.lab = "Mental.health.measures", study.lab = "Study",
            "log OR" = effect.size, "Weight" = r.weights, 
            "n" = N)
dev.off()

# su _ directionality ####

alc.dir <- filter(neet, Alcohol==1 & Gender=="all")
can.dir <- filter(neet, Cannabis==1 & Gender=="all")
drg.dir <- filter(neet, Drug==1 & Gender=="all")

su.dir <- bind_rows(alc.dir, can.dir, drg.dir)

# Temporality.of.measures                      
# MH-->NEET 

su.dir.mh_neet <- filter(su.dir, Temporality.of.measures=="MH-->NEET")
su.dir.mh_neet

# rve random effects model
su.dir.mh_neet.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                         modelweights = "CORR", rho = 0.8, small=TRUE, data=su.dir.mh_neet) 
print(su.dir.mh_neet.rve)

# exponentiate to obtain OR and CI
exp(0.151)

exp(-0.95)
exp(1.25)

# Temporality.of.measures
# NEET-->MH

su.dir.neet_mh <- filter(su.dir, Temporality.of.measures=="NEET-->MH")
su.dir.neet_mh

# rve random effects model
su.dir.neet_mh.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                         modelweights = "CORR", rho = 0.8, small=TRUE, data=su.dir.neet_mh) 
print(su.dir.neet_mh.rve)

# exponentiate to obtain OR and CI
exp(0.199)

exp(-0.799)
exp(1.2)


# su _ age ####

alc.age <- filter(neet, Alcohol==1 & Gender=="all" & is.na(rep_alc.age) & is.na(Age_at_interview)==FALSE)
can.age <- filter(neet, Cannabis==1 & Gender=="all" & is.na(Age_at_interview)==FALSE)
drg.age <- filter(neet, Drug==1 & Gender=="all" & is.na(rep_drg.age) & is.na(Age_at_interview)==FALSE)

su.age <- bind_rows(alc.age, can.age, drg.age)
su.age

# Age_at_interview
# <18

su.age.under18 <- filter(su.age, Age_at_interview=="<18")
su.age.under18

# rve random effects model
su.age.under18.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                         modelweights = "CORR", rho = 0.8, small=TRUE, data=su.age.under18) 
print(su.age.under18.rve)

# exponentiate to obtain OR and CI
exp(0.851)

exp(-6.19)
exp(7.89)

# Age_at_interview
# >=18

su.age.18plus <- filter(su.age, Age_at_interview==">=18")
su.age.18plus

# rve random effects model
su.age.18plus.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                        modelweights = "CORR", rho = 0.8, small=TRUE, data=su.age.18plus) 
print(su.age.18plus.rve)

# exponentiate to obtain OR and CI
exp(0.335)

exp(-0.253)
exp(0.922)


# su _ gender ####

alc.gen <- filter(neet, Alcohol==1 & Gender!="all")
can.gen <- filter(neet, Cannabis==1 & Gender!="all")
drg.gen <- filter(neet, Drug==1 & Gender!="all" & is.na(Repeated_sample))

su.gen <- bind_rows(alc.gen, can.gen, drg.gen)
su.gen

# Gender
# girl

su.gen.girl <- filter(su.gen, Gender=="girl")
su.gen.girl

# rve random effects model
su.gen.girl.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                      modelweights = "CORR", rho = 0.8, small=TRUE, data=su.gen.girl) 
print(su.gen.girl.rve)

# exponentiate to obtain OR and CI
exp(0.0961)

exp(-8.58)
exp(8.77)

# Gender
# boy

su.gen.boy <- filter(su.gen, Gender=="boy")
su.gen.boy

# rve random effects model
su.gen.boy.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                     modelweights = "CORR", rho = 0.8, small=TRUE, data=su.gen.boy) 
print(su.gen.boy.rve)

# exponentiate to obtain OR and CI
exp(0.359)

exp(-3.46)
exp(4.18)


## all measures -------------------------------------------------------------------------------

# creating deduplicated subsets for main analysis
any <- filter(neet, Any==1 & Gender=="all" & is.na(rep_any))
any

sui <- filter(neet, Suicidal==1 & Gender=="all" & is.na(Repeated_sample))
sui

# binding together
all <- bind_rows(mh, su, any, sui)
all

# rve random effects model
all.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
             modelweights = "CORR", rho = 0.8, small=TRUE, data=all) 
print(all.rve)

# exponentiate to obtain OR and CI
exp(0.321)        

exp(0.144) 
exp(0.497)

# forest plot
forest.robu(all.rve, es.lab = "Mental.health.measures", study.lab = "Study",
            "log OR" = effect.size, # optional column
            "Weight" = r.weights)   # optional column
dev.off()

# all _ directionality ####

any.dir <- filter(neet, Any==1 & Gender=="all" & is.na(rep_any.dir))
sui.dir <- filter(neet, Suicidal==1 & Gender=="all")

all.dir <- bind_rows(mh.dir, su.dir, any.dir, sui.dir)

# Temporality.of.measures                      
# MH-->NEET 

all.dir.mh_neet <- filter(all.dir, Temporality.of.measures=="MH-->NEET")
all.dir.mh_neet

# rve random effects model
all.dir.mh_neet.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                         modelweights = "CORR", rho = 0.8, small=TRUE, data=all.dir.mh_neet) 
print(all.dir.mh_neet.rve)

# exponentiate to obtain OR and CI
exp(0.328)

exp(0.0335)
exp(0.623)

# Temporality.of.measures
# NEET-->MH

all.dir.neet_mh <- filter(all.dir, Temporality.of.measures=="NEET-->MH")
all.dir.neet_mh

# rve random effects model
all.dir.neet_mh.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                         modelweights = "CORR", rho = 0.8, small=TRUE, data=all.dir.neet_mh) 
print(all.dir.neet_mh.rve)

# exponentiate to obtain OR and CI
exp(0.294)

exp(-0.0852)
exp(0.673)

# all _ age ####

any.age  <- filter(neet, Any==1 & Gender=="all" & is.na(rep_any))
sui.age <- filter(neet, Suicidal==1 & Gender=="all" & is.na(rep_sui.age) & is.na(Age_at_interview)==FALSE)

all.age <- bind_rows(mh.age, su.age, any.age, sui.age)
all.age

# Age_at_interview
# <18

all.age.under18 <- filter(all.age, Age_at_interview=="<18")
all.age.under18

# rve random effects model
all.age.under18.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                         modelweights = "CORR", rho = 0.8, small=TRUE, data=all.age.under18) 
print(all.age.under18.rve)

# exponentiate to obtain OR and CI
exp(0.288)

exp(-0.433)
exp(1.01)

# Age_at_interview
# >=18

all.age.18plus <- filter(all.age, Age_at_interview==">=18")
all.age.18plus

# rve random effects model
all.age.18plus.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                        modelweights = "CORR", rho = 0.8, small=TRUE, data=all.age.18plus) 
print(all.age.18plus.rve)

# exponentiate to obtain OR and CI
exp(0.28)

exp(-0.189)
exp(0.75)


# all _ gender ####

any.gen <- filter(neet, Any==1 & Gender!="all")
sui.gen <- filter(neet, Suicidal==1 & Gender!="all")

all.gen <- bind_rows(mh.gen, su.gen, any.gen, sui.gen)
all.gen

# Gender
# girl

all.gen.girl <- filter(all.gen, Gender=="girl")
all.gen.girl

# rve random effects model
all.gen.girl.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                      modelweights = "CORR", rho = 0.8, small=TRUE, data=all.gen.girl) 
print(all.gen.girl.rve)

# exponentiate to obtain OR and CI
exp(0.178)

exp(-0.117)
exp(0.474)

# Gender
# boy

all.gen.boy <- filter(all.gen, Gender=="boy")
all.gen.boy

# rve random effects model
all.gen.boy.rve<-robu(formula = OR_ln ~ 1, var.eff.size=SE, studynum = Study, 
                     modelweights = "CORR", rho = 0.8, small=TRUE, data=all.gen.boy) 
print(all.gen.boy.rve)

# exponentiate to obtain OR and CI
exp(0.147)

exp(-0.226)
exp(0.52)
