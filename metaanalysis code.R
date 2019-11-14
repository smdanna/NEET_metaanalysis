#### Mise en place, testing ####
#
# Will be using this tutorial for analyses:
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/ 
#
# Original data that Gen sent me in August 2019
# > neet <- read.csv("~/Dropbox/PEPP/NEET/neet.csv")
# > View(neet)
#
# using the instructions from the tutorial by Mathias Harrer (url above), we will use: 
# "Author"  will be "study_no"
# "TE"      will be "OR_ln"
# "seTE"    will be "SE"

# checking out the variables in the NEET dataframe, named "neet" for simplicity
summary(neet)
str(neet)

# fixing a value in the dataframe
neet$study_no <- as.character(neet$study_no)  # making this column into characters (were factors before)
neet[12,2] <- "bania 2019 6"  # adding the value that was missing
View(neet)

# turns out that several of the variables I created are read as factor variables, which sucks, 
# because I want numeric variables. let's transform them in here again, even though I did the 
# same thing in Excel before importing the data

# re-deriving OR_ln in R
neet$OR_ln <- log(neet$OR)
View(neet)
class(neet$OR_ln)
head(neet$OR_ln)

# re-deriving SE [ (upper_ln - lower_ln) / 3.92 ]
class(neet$SE)
neet$SE <- ((neet$upper_ln-neet$lower_ln)/3.92)
class(neet$SE)

str(neet)
# okay all the variables I need to use are now numeric
# now onto Chapter 4 Pooling Effect Sizes

#installing and loading meta package
install.packages("meta")
library(meta)
library(metafor)

# Parameter     Function
# TE	          This tells R to use the TE column to retrieve the effect sizes for each study
# seTE	        This tells R to use the seTE column to retrieve the standard error for each study
# data=	        After =, paste the name of your dataset here
# studlab=paste()	    This tells the function were the labels for each study are stored. If you named the spreadsheet columns as advised, this should be studlab=paste(Author)
# comb.fixed=	  Whether to use a fixed-effects-model
# comb.random=	Whether to use a random-effects-model. This has to be set to TRUE
# method.tau=	  Which estimator to use for the between-study variance
# hakn=	        Whether to use the Knapp-Hartung method
# prediction=	  Whether to print a prediction interval for the effect of future studies based on present evidence
# sm=	          The summary measure we want to calculate. We can either calculate the mean difference (MD) or Hedges’ g (SMD)

meta.test <- metagen(OR_ln,
                  SE,
                  data=neet,
                  studlab=paste(study_no),
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  method.tau = "DL",
                  hakn = FALSE,
                  prediction=FALSE,
                  sm="OR")
meta.test

# this is the default plot
forest(meta.test)

# saving default plot
svg(file='forestplot_default.svg', width = 9, height = 27) 
forest.jama<-forest(meta.test)
dev.off() 

# saving default plot as pdf
pdf(file='forestplot_default.pdf', width = 9, height = 27) 
forest.jama<-forest(meta.test)
dev.off() 

# into JAMA layout
forest(meta.test,
       layout = "JAMA",
       text.predict = "95% PI",
       col.predict = "black")

# saving JAMA forest plot
svg(file='forestplot_JAMA.svg', height = 27) 
forest.jama<-forest(meta.test,
                    layout = "JAMA",
                    text.predict = "95% PI",
                    col.predict = "black")
dev.off() 

# SUBGROUP ANALYSIS by mental health measure

MHmeasure.subgroup<-update.meta(meta.test, 
                             byvar=MH_measure, 
                             comb.random = TRUE, 
                             comb.fixed = FALSE)
MHmeasure.subgroup

# saving default plot as SVG
# svg(file='forestplot_default_subgrp.svg', width = 9, height = 36) 
# forest.subgrp<-forest(MHmeasure.subgroup)
# dev.off() 

# saving default plot as PDF
pdf(file='forestplot_default_subgrp.pdf', width = 9, height = 36) 
forest.subgrp<-forest(MHmeasure.subgroup)
dev.off() 

# SUBGROUP ANALYSIS by MH measure ONLY where Subgroup=all and not NA (should only be 1 per study)

neet.sub.all <- neet[which(neet$Subgroup=="all" & !is.na(neet$OR_ln)),]
View(neet.sub.all)


meta.MH.all <- metagen(OR_ln,
                     SE,
                     data=neet.sub.all,
                     studlab=paste(study_no),
                     comb.fixed = FALSE,
                     comb.random = TRUE,
                     method.tau = "DL",
                     hakn = FALSE,
                     prediction=FALSE,
                     sm="OR")
meta.MH.all

subgroup.MH.all<-update.meta(meta.MH.all, 
                                byvar=MH_measure, 
                                comb.random = TRUE, 
                                comb.fixed = FALSE)
subgroup.MH.all

# saving default plot
pdf(file='forestplot_subgroup_MH_all.pdf', width = 9, height = 24) 
forest.subgrp<-forest(subgroup.MH.all)
dev.off() 






#### Refined data cleaning, 21 Oct 2019 ####

# first, remove some objects from the environment so shit doesn't get confusing
rm(gender.subgroup, meta.MH.all, meta.test, MHmeasure.subgroup, neet.sub.all, res,
     stratif.subgroup, subgroup.MH.all)

# These are the updated data that Gen and I decided on 18 October 2019.
# There should no longer be any repeated samples pooled together!

neet2 <- read.csv("~/Dropbox/PEPP/NEET meta-analysis/neet updated.csv")
View(neet2)

# using the instructions from the tutorial by Mathias Harrer (url at top), we will use: 
# "Author"  will be "study_no"
# "TE"      will be "OR_ln"
# "seTE"    will be "SE"

# checking out the variables in the updated NEET dataframe
summary(neet2)
str(neet2)

# fixing a value in the dataframe
#neet$study_no <- as.character(neet$study_no)  # making this column into characters (were factors before)
#neet[12,2] <- "bania 2019 6"  # adding the value that was missing
#View(neet)

# turns out that several of the variables I created are read as factor variables, which sucks, 
# because I want numeric variables. let's transform them in here again, even though I did the 
# same thing in Excel before importing the data

# re-deriving OR_ln in R
neet2$OR_ln <- log(neet2$OR)
View(neet2)
class(neet2$OR_ln)
head(neet2$OR_ln)
str(neet2)

# re-deriving lower_ln and upper_ln
class(neet2$LowerCI)
neet2$lower_ln <- log(neet2$LowerCI)
neet2$upper_ln <- log(neet2$UpperCI)
str(neet2)
View(neet2)

# re-deriving SE [ (upper_ln - lower_ln) / 3.92 ]
class(neet2$SE)
neet2$SE <- ((neet2$upper_ln-neet2$lower_ln)/3.92)
class(neet$SE)
View(neet2)
str(neet2)

# making LowerCI_sens numeric

neet2$LowerCI_sens <- as.numeric(neet2$LowerCI_sens)
class(neet2$LowerCI_sens)
str(neet2)

#### remove all blank rows
# first, create backup of neet database which is revised by Gen (decisions made about which studies
# to retain in the meta-analyses)
neet.OctRevisions <- neet2
View(neet.OctRevisions)

# select non-blank rows (the hard way because no NAs in blank rows, unfortunately)
neet2 <- neet.OctRevisions[c(2:5,7:12,14:22,24:34,36:42,44:48,50:53,55:62,64:72,74:76,78:92,94:95,
                             97:98,100:103,105:112,114:115,117:120),]
View(neet2)
summary(neet2)

# all variables I need to use are now numeric, now: Chapter 4 Pooling Effect Sizes

#installing and loading meta package
install.packages("meta")
library(meta)
library(metafor)

# Parameter     Function
# TE	          This tells R to use the TE column to retrieve the effect sizes for each study
# seTE	        This tells R to use the seTE column to retrieve the standard error for each study
# data=	        After =, paste the name of your dataset here
# studlab=paste()	    This tells the function were the labels for each study are stored. If you named the spreadsheet columns as advised, this should be studlab=paste(Author)
# comb.fixed=	  Whether to use a fixed-effects-model
# comb.random=	Whether to use a random-effects-model. This has to be set to TRUE
# method.tau=	  Which estimator to use for the between-study variance
# hakn=	        Whether to use the Knapp-Hartung method
# prediction=	  Whether to print a prediction interval for the effect of future studies based on present evidence
# sm=	          The summary measure we want to calculate. We can either calculate the mean difference (MD) or Hedges’ g (SMD)

# this is a sort of bullshit analysis, just running to see if the command works. these studies are
# not all supposed to be pooled together, obviously
meta.test <- metagen(OR_ln,
                     SE,
                     data=neet2,
                     studlab=paste(study_no),
                     comb.fixed = FALSE,
                     comb.random = TRUE,
                     method.tau = "DL",
                     hakn = FALSE,
                     prediction=FALSE,
                     sm="OR")
meta.test


# this is the default plot
forest(meta.test)

# saving default plot
svg(file='forestplot_default.svg', width = 9, height = 27) 
forest.jama<-forest(meta.test)
dev.off() 

# saving default plot as pdf
pdf(file='forestplot_default.pdf', width = 9, height = 27) 
forest.jama<-forest(meta.test)
dev.off() 

# into JAMA layout
forest(meta.test,
       layout = "JAMA",
       text.predict = "95% PI",
       col.predict = "black")

# saving JAMA forest plot
svg(file='forestplot_JAMA.svg', height = 27) 
forest.jama<-forest(meta.test,
                    layout = "JAMA",
                    text.predict = "95% PI",
                    col.predict = "black")
dev.off() 




#### Analyses: Mental health measure  ####################################################################



# DISTRESS ================================================================================================
#
# distress = 1, gender = all, repeated sample = 0

# how many studies total (overlapping, NA ORs)
nrow(neet2[which(neet2$Distress==1 & neet2$Gender=="all"),])

MHdis.Gall.rep0 <- neet2[which(neet2$Distress==1 & neet2$Gender=="all" & is.na(neet2$Repeated_sample)),]
View(MHdis.Gall.rep0)
head(MHdis.Gall.rep0)
#              Study     Country         study_no  Mental.health.measures Temporality.of.measures
# 2     Baggio, 2015 Switzerland    baggio 2015 1   General mental health               MH-->NEET
# 7      Bania, 2019      Norway     bania 2019 1      Emotional problems               MH-->NEET
# 14     Basta, 2019      Greece     basta 2019 1  Psychological distress              MH<-->NEET
# 32    Bynner, 2002          UK    bynner 2002 1  Psychological distress               NEET-->MH
# 74      Hale, 2018          UK      hale 2018 1  Psychological distress               MH-->NEET
# 78 Henderson, 2017      Canada henderson 2017 1 Internalizing disorders              MH<-->NEET

meta.MHdis.Gall.rep0 <- metagen(OR_ln,
                     SE,
                     data=MHdis.Gall.rep0,
                     studlab=paste(Study),
                     comb.fixed = FALSE,
                     comb.random = TRUE,
                     method.tau = "DL",
                     hakn = FALSE,
                     prediction=FALSE,
                     sm="OR")
meta.MHdis.Gall.rep0

forest(meta.MHdis.Gall.rep0)







# DISTRESS - directionality --------------------------------------------------------------------------------

meta.MHdis.Gall.rep0.dir<-update.meta(meta.MHdis.Gall.rep0, 
                                 byvar=Temporality.of.measures, 
                                 comb.random = TRUE, 
                                 comb.fixed = FALSE)
meta.MHdis.Gall.rep0.dir

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHdis.Gall.rep0.dir)







# DISTRESS - geography ------------------------------------------------------------------------------------

meta.MHdis.Gall.rep0.geo<-update.meta(meta.MHdis.Gall.rep0, 
                                      byvar=Country, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHdis.Gall.rep0.geo

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHdis.Gall.rep0.geo)






# DISTRESS - gender ----------------------------------------------------------------------------------------

MHdis.Gbg.rep0 <- neet2[which(neet2$Distress==1 & neet2$Gender!="all" & is.na(neet2$Repeated_sample)),]
View(MHdis.Gbg.rep0)
head(MHdis.Gbg.rep0)

# Study Country      study_no Mental.health.measures Temporality.of.measures
# 8   Bania, 2019  Norway  bania 2019 2     Emotional problems               MH-->NEET
# 9   Bania, 2019  Norway  bania 2019 3     Emotional problems               MH-->NEET
# 15  Basta, 2019  Greece  basta 2019 2 Psychological distress              MH<-->NEET
# 16  Basta, 2019  Greece  basta 2019 3 Psychological distress              MH<-->NEET
# 33 Bynner, 2002      UK bynner 2002 2 Psychological distress               NEET-->MH
# 34 Bynner, 2002      UK bynner 2002 3 Psychological distress               NEET-->MH

meta.MHdis.Gbg.rep0 <- metagen(OR_ln,
                                SE,
                                data=MHdis.Gbg.rep0,
                                studlab=paste(Study),
                                comb.fixed = FALSE,
                                comb.random = TRUE,
                                method.tau = "DL",
                                hakn = FALSE,
                                prediction=FALSE,
                                sm="OR")
meta.MHdis.Gbg.rep0


meta.MHdis.Gbg.rep0<-update.meta(meta.MHdis.Gbg.rep0, 
                                      byvar=Gender, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHdis.Gbg.rep0

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHdis.Gbg.rep0)







  


# MOOD ====================================================================================================
#
# mood = 1, gender = all, repeated sample = 0

# how many studies total (even if overlapping, even if no OR given)
View(neet2[which(neet2$Mood==1 & neet2$Gender=="all"),])
# there are 11 studies

MHmood.Gall.rep0 <- neet2[which(neet2$Mood==1 & neet2$Gender=="all" & is.na(neet2$Repeated_sample)),]
View(MHmood.Gall.rep0)
head(MHmood.Gall.rep0)
#                    Study     Country       study_no         Mental.health.measures
# 3           Baggio, 2015 Switzerland  baggio 2015 2            Depressive symptoms
# 24          Benjet, 2012      Mexico  benjet 2012 1                  Mood disorder
# 36         Gariepy, 2018      Canada gariepy 2018 1            Depressive disorder
# 44  Goldman-Mellor, 2016          UK goldman 2016 1            Depressive disorder
# 100          O'Dea, 2014   Australia    odea 2014 1                  Mood disorder
# 105          Power, 2015     Ireland   power 2015 1 Depressive disorder (lifetime)

meta.MHmood.Gall.rep0 <- metagen(OR_ln,
                                SE,
                                data=MHmood.Gall.rep0,
                                studlab=paste(Study),
                                comb.fixed = FALSE,
                                comb.random = TRUE,
                                method.tau = "DL",
                                hakn = FALSE,
                                prediction=FALSE,
                                sm="OR")
meta.MHmood.Gall.rep0







# MOOD - directionality ---------------------------------------------------------------------------------------

meta.MHmood.Gall.rep0.dir<-update.meta(meta.MHmood.Gall.rep0, 
                                      byvar=Temporality.of.measures, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHmood.Gall.rep0.dir

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHmood.Gall.rep0.dir)






# MOOD - geography ------------------------------------------------------------------------------------

meta.MHmood.Gall.rep0.geo<-update.meta(meta.MHmood.Gall.rep0, 
                                      byvar=Country, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHmood.Gall.rep0.geo

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHmood.Gall.rep0.geo)





# MOOD - gender -----------------------------------------------------------------------------------------------

MHmood.Gbg.rep0 <- neet2[which(neet2$Mood==1 & neet2$Gender!="all" & is.na(neet2$Repeated_sample)),]
View(MHmood.Gbg.rep0)
head(MHmood.Gbg.rep0)
# <0 rows>











# ANXIETY ====================================================================================================
#
# anxiety = 1, gender = all, repeated sample = 0

View(neet2[which(neet2$Anxiety==1 & neet2$Gender=="all"),])
nrow(neet2[which(neet2$Anxiety==1 & neet2$Gender=="all"),])
# 11 studies total

MHanx.Gall.rep0 <- neet2[which(neet2$Anxiety==1 & neet2$Gender=="all" & is.na(neet2$Repeated_sample)),]
View(MHanx.Gall.rep0)
head(MHanx.Gall.rep0)
#                    Study   Country       study_no       Mental.health.measures Temporality.of.measures
# 17           Basta, 2019    Greece   basta 2019 4             Anxiety symptoms              MH<-->NEET
# 25          Benjet, 2012    Mexico  benjet 2012 2             Anxiety disorder              MH<-->NEET
# 38         Gariepy, 2018    Canada gariepy 2018 3 Generalized anxiety disorder              MH<-->NEET
# 45  Goldman-Mellor, 2016        UK goldman 2016 2 Generalized anxiety disorder              MH<-->NEET
# 101          O'Dea, 2014 Australia    odea 2014 2             Anxiety disorder              MH<-->NEET
# 106          Power, 2015   Ireland   power 2015 2  Anxiety disorder (lifetime)              MH<-->NEET

meta.MHanx.Gall.rep0 <- metagen(OR_ln,
                                 SE,
                                 data=MHanx.Gall.rep0,
                                 studlab=paste(Study),
                                 comb.fixed = FALSE,
                                 comb.random = TRUE,
                                 method.tau = "DL",
                                 hakn = FALSE,
                                 prediction=FALSE,
                                 sm="OR")
meta.MHanx.Gall.rep0



# ANXIETY - directionality ---------------------------------------------------------------------------------

meta.MHanx.Gall.rep0.dir<-update.meta(meta.MHanx.Gall.rep0, 
                                       byvar=Temporality.of.measures, 
                                       comb.random = TRUE, 
                                       comb.fixed = FALSE)
meta.MHanx.Gall.rep0.dir

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHanx.Gall.rep0.dir)





# ANXIETY - geography ------------------------------------------------------------------------------------

meta.MHanx.Gall.rep0.geo<-update.meta(meta.MHanx.Gall.rep0, 
                                       byvar=Country, 
                                       comb.random = TRUE, 
                                       comb.fixed = FALSE)
meta.MHanx.Gall.rep0.geo

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHanx.Gall.rep0.geo)






# ANXIETY - gender -----------------------------------------------------------------------------------------

MHanx.Gbg.rep0 <- neet2[which(neet2$Anxiety==1 & neet2$Gender!="all" & is.na(neet2$Repeated_sample)),]
View(MHanx.Gbg.rep0)
head(MHanx.Gbg.rep0)

# Study Country     study_no Mental.health.measures Temporality.of.measures
# 18 Basta, 2019  Greece basta 2019 5       Anxiety symptoms              MH<-->NEET
# 19 Basta, 2019  Greece basta 2019 6       Anxiety symptoms              MH<-->NEET

meta.MHanx.Gbg.rep0 <- metagen(OR_ln,
                               SE,
                               data=MHanx.Gbg.rep0,
                               studlab=paste(Study),
                               comb.fixed = FALSE,
                               comb.random = TRUE,
                               method.tau = "DL",
                               hakn = FALSE,
                               prediction=FALSE,
                               sm="OR")
meta.MHanx.Gbg.rep0


meta.MHanx.Gbg.rep0<-update.meta(meta.MHanx.Gbg.rep0, 
                                 byvar=Gender, 
                                 comb.random = TRUE, 
                                 comb.fixed = FALSE)
meta.MHanx.Gbg.rep0

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHanx.Gbg.rep0)







# BEHAVIOURAL ================================================================================================
#
# behavioural = 1, gender = all, repeated sample = 0

# total number of studies, including overlapping and NA ORs
nrow(neet2[which(neet2$Behavioral==1 & neet2$Gender=="all"),])


MHbeh.Gall.rep0 <- neet2[which(neet2$Behavioral==1 & neet2$Gender=="all" & is.na(neet2$Repeated_sample)),]
View(MHbeh.Gall.rep0)
head(MHbeh.Gall.rep0)
#                    Study   Country         study_no        Mental.health.measures
# 10           Bania, 2019    Norway     bania 2019 4              Conduct problems
# 27          Benjet, 2012    Mexico    benjet 2012 4           Behavioral disorder
# 48  Goldman-Mellor, 2016        UK   goldman 2016 5              Conduct disorder
# 83       Henderson, 2017    Canada henderson 2017 6       Externalizing disorders
# 118        Rodwell, 2018 Australia   rodwell 2018 2 Disruptive behaviour disorder

meta.MHbeh.Gall.rep0 <- metagen(OR_ln,
                                SE,
                                data=MHbeh.Gall.rep0,
                                studlab=paste(Study),
                                comb.fixed = FALSE,
                                comb.random = TRUE,
                                method.tau = "DL",
                                hakn = FALSE,
                                prediction=FALSE,
                                sm="OR")
meta.MHbeh.Gall.rep0


# BEHAVIOURAL - directionality --------------------------------------------------------------------------------

meta.MHbeh.Gall.rep0.dir<-update.meta(meta.MHbeh.Gall.rep0, 
                                      byvar=Temporality.of.measures, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHbeh.Gall.rep0.dir

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHbeh.Gall.rep0.dir)






# BEHAVIOURAL - geography ------------------------------------------------------------------------------------

meta.MHbeh.Gall.rep0.geo<-update.meta(meta.MHbeh.Gall.rep0, 
                                      byvar=Country, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHbeh.Gall.rep0.geo

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHbeh.Gall.rep0.geo)







# BEHAVIOURAL - gender ----------------------------------------------------------------------------------------

MHbeh.Gbg.rep0 <- neet2[which(neet2$Behavioral==1 & neet2$Gender!="all" & is.na(neet2$Repeated_sample)),]
View(MHbeh.Gbg.rep0)
head(MHbeh.Gbg.rep0)

# Study Country          study_no  Mental.health.measures Temporality.of.measures
# 11     Bania, 2019  Norway      bania 2019 5        Conduct problems               MH-->NEET
# 12     Bania, 2019  Norway      bania 2019 6        Conduct problems               MH-->NEET
# 84 Henderson, 2017  Canada  henderson 2017 7 Externalizing disorders              MH<-->NEET
# 85 Henderson, 2017  Canada  henderson 2017 8 Externalizing disorders              MH<-->NEET
# 86 Henderson, 2017  Canada  henderson 2017 9 Externalizing disorders              MH<-->NEET
# 87 Henderson, 2017  Canada henderson 2017 10 Externalizing disorders              MH<-->NEET

meta.MHbeh.Gbg.rep0 <- metagen(OR_ln,
                               SE,
                               data=MHbeh.Gbg.rep0,
                               studlab=paste(Study),
                               comb.fixed = FALSE,
                               comb.random = TRUE,
                               method.tau = "DL",
                               hakn = FALSE,
                               prediction=FALSE,
                               sm="OR")
meta.MHbeh.Gbg.rep0


meta.MHbeh.Gbg.rep0<-update.meta(meta.MHbeh.Gbg.rep0, 
                                 byvar=Gender, 
                                 comb.random = TRUE, 
                                 comb.fixed = FALSE)
meta.MHbeh.Gbg.rep0

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHbeh.Gbg.rep0)










# ALCOHOL =====================================================================================================
#
# alcohol = 1, gender = all, repeated sample = 0

# number of total studies
nrow(neet2[which(neet2$Alcohol==1 & neet2$Gender=="all"),])
# 8

MHalc.Gall.rep0 <- neet2[which(neet2$Alcohol==1 & neet2$Gender=="all" & is.na(neet2$Repeated_sample)),]
View(MHalc.Gall.rep0)
head(MHalc.Gall.rep0)
#                      Study     Country         study_no          Mental.health.measures
# 4             Baggio, 2015 Switzerland    baggio 2015 3                  Binge drinking
# 39           Gariepy, 2018      Canada   gariepy 2018 4            Alcohol use disorder
# 46    Goldman-Mellor, 2016          UK   goldman 2016 3              Alcohol dependence
# 67  Gutierrez-Garcia, 2019      Mexico gutierrez 2019 4            Alcohol use disorder
# 102            O'Dea, 2014   Australia      odea 2014 3            Alcohol use disorder
# 107            Power, 2015     Ireland     power 2015 3 Alcohol use disorder (lifetime)

meta.MHalc.Gall.rep0 <- metagen(OR_ln,
                                SE,
                                data=MHalc.Gall.rep0,
                                studlab=paste(Study),
                                comb.fixed = FALSE,
                                comb.random = TRUE,
                                method.tau = "DL",
                                hakn = FALSE,
                                prediction=FALSE,
                                sm="OR")
meta.MHalc.Gall.rep0



# ALCOHOL - directionality --------------------------------------------------------------------------------------

meta.MHalc.Gall.rep0.dir<-update.meta(meta.MHalc.Gall.rep0, 
                                      byvar=Temporality.of.measures, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHalc.Gall.rep0.dir

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHalc.Gall.rep0.dir)






# ALCOHOL - geography ------------------------------------------------------------------------------------

meta.MHalc.Gall.rep0.geo<-update.meta(meta.MHalc.Gall.rep0, 
                                      byvar=Country, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHalc.Gall.rep0.geo

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHalc.Gall.rep0.geo)






# ALCOHOL - gender ---------------------------------------------------------------------------------------------

MHalc.Gbg.rep0 <- neet2[which(neet2$Alcohol==1 & neet2$Gender!="all" & is.na(neet2$Repeated_sample)),]
View(MHalc.Gbg.rep0)
head(MHalc.Gbg.rep0)

# <0 rows>









# CANNABIS ====================================================================================================
#
# cannabis = 1, gender = all, repeated sample = 0

# total number of studies, including overlapping and NA ORs
nrow(neet2[which(neet2$Cannabis==1 & neet2$Gender=="all"),])
# 5

MHcan.Gall.rep0 <- neet2[which(neet2$Cannabis==1 & neet2$Gender=="all" & is.na(neet2$Repeated_sample)),]
View(MHcan.Gall.rep0)
head(MHcan.Gall.rep0)
#                    Study     Country       study_no Mental.health.measures Temporality.of.measures
# 5           Baggio, 2015 Switzerland  baggio 2015 4  Cannabis use disorder               MH-->NEET
# 40         Gariepy, 2018      Canada gariepy 2018 5      Cannabis disorder              MH<-->NEET
# 47  Goldman-Mellor, 2016          UK goldman 2016 4    Cannabis dependence              MH<-->NEET
# 103          O'Dea, 2014   Australia    odea 2014 4  Cannabis use disorder              MH<-->NEET
# 119        Rodwell, 2018   Australia rodwell 2018 3  Cannabis use disorder               MH-->NEET

meta.MHcan.Gall.rep0 <- metagen(OR_ln,
                                SE,
                                data=MHcan.Gall.rep0,
                                studlab=paste(Study),
                                comb.fixed = FALSE,
                                comb.random = TRUE,
                                method.tau = "DL",
                                hakn = FALSE,
                                prediction=FALSE,
                                sm="OR")
meta.MHcan.Gall.rep0





# CANNABIS - directionality ----------------------------------------------------------------------------------

meta.MHcan.Gall.rep0.dir<-update.meta(meta.MHcan.Gall.rep0, 
                                      byvar=Temporality.of.measures, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHcan.Gall.rep0.dir

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHcan.Gall.rep0.dir)





# CANNABIS - geography ------------------------------------------------------------------------------------

meta.MHcan.Gall.rep0.geo<-update.meta(meta.MHcan.Gall.rep0, 
                                      byvar=Country, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHcan.Gall.rep0.geo

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHcan.Gall.rep0.geo)





# CANNABIS - gender ------------------------------------------------------------------------------------------

MHcan.Gbg.rep0 <- neet2[which(neet2$Cannabis==1 & neet2$Gender!="all" & is.na(neet2$Repeated_sample)),]
View(MHcan.Gbg.rep0)
head(MHcan.Gbg.rep0)

# <0 rows>







# DRUG ========================================================================================================
#
# drug = 1, gender = all, repeated sample = 0

# how many total of these studies (overlapping, NA ORs)
View(neet2[which(neet2$Drug==1 & neet2$Gender=="all"),])
# 8 studies

MHdrg.Gall.rep0 <- neet2[which(neet2$Drug==1 & neet2$Gender=="all" & is.na(neet2$Repeated_sample)),]
View(MHdrg.Gall.rep0)
head(MHdrg.Gall.rep0)
# Study Country          study_no       Mental.health.measures Temporality.of.measures
# 20           Basta, 2019  Greece      basta 2019 7                     Drug use              MH<-->NEET
# 26          Benjet, 2012  Mexico     benjet 2012 3       Substance use disorder              MH<-->NEET
# 41         Gariepy, 2018  Canada    gariepy 2018 6            Drug use disorder              MH<-->NEET
# 52  Goldman-Mellor, 2016      UK    goldman 2016 8        Harmful substance use               MH-->NEET
# 88       Henderson, 2017  Canada henderson 2017 11       Substance use disorder              MH<-->NEET
# 108          Power, 2015 Ireland      power 2015 4 Drug use disorder (lifetime)              MH<-->NEET

meta.MHdrg.Gall.rep0 <- metagen(OR_ln,
                                SE,
                                data=MHdrg.Gall.rep0,
                                studlab=paste(Study),
                                comb.fixed = FALSE,
                                comb.random = TRUE,
                                method.tau = "DL",
                                hakn = FALSE,
                                prediction=FALSE,
                                sm="OR")
meta.MHdrg.Gall.rep0


# DRUG - directionality ---------------------------------------------------------------------------------------

meta.MHdrg.Gall.rep0.dir<-update.meta(meta.MHdrg.Gall.rep0, 
                                      byvar=Temporality.of.measures, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHdrg.Gall.rep0.dir

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHdrg.Gall.rep0.dir)






# DRUG - geography ------------------------------------------------------------------------------------

meta.MHdrg.Gall.rep0.geo<-update.meta(meta.MHdrg.Gall.rep0, 
                                      byvar=Country, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHdrg.Gall.rep0.geo

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHdrg.Gall.rep0.geo)







# DRUG - gender -----------------------------------------------------------------------------------------------

MHdrg.Gbg.rep0 <- neet2[which(neet2$Drug==1 & neet2$Gender!="all" & is.na(neet2$Repeated_sample)),]
View(MHdrg.Gbg.rep0)
head(MHdrg.Gbg.rep0)

# Study Country          study_no Mental.health.measures Temporality.of.measures
# 21     Basta, 2019  Greece      basta 2019 8               Drug use              MH<-->NEET
# 22     Basta, 2019  Greece      basta 2019 9               Drug use              MH<-->NEET
# 89 Henderson, 2017  Canada henderson 2017 12 Substance use disorder              MH<-->NEET
# 90 Henderson, 2017  Canada henderson 2017 13 Substance use disorder              MH<-->NEET
# 91 Henderson, 2017  Canada henderson 2017 14 Substance use disorder              MH<-->NEET
# 92 Henderson, 2017  Canada henderson 2017 15 Substance use disorder              MH<-->NEET

meta.MHdrg.Gbg.rep0 <- metagen(OR_ln,
                               SE,
                               data=MHdrg.Gbg.rep0,
                               studlab=paste(Study),
                               comb.fixed = FALSE,
                               comb.random = TRUE,
                               method.tau = "DL",
                               hakn = FALSE,
                               prediction=FALSE,
                               sm="OR")
meta.MHdrg.Gbg.rep0


meta.MHdrg.Gbg.rep0<-update.meta(meta.MHdrg.Gbg.rep0, 
                                 byvar=Gender, 
                                 comb.random = TRUE, 
                                 comb.fixed = FALSE)
meta.MHdrg.Gbg.rep0

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHdrg.Gbg.rep0)













# SUICIDAL ====================================================================================================
#
# suicidal = 1, gender = all, repeated sample = 0

# how many total 
View(neet2[which(neet2$Suicidal==1 & neet2$Gender=="all"),])


MHsui.Gall.rep0 <- neet2[which(neet2$Suicidal==1 & neet2$Gender=="all" & is.na(neet2$Repeated_sample)),]
View(MHsui.Gall.rep0)
head(MHsui.Gall.rep0)
#                    Study Country       study_no       Mental.health.measures Temporality.of.measures
# 31          Benjet, 2012  Mexico  benjet 2012 8             Suicidal attempt              MH<-->NEET
# 42         Gariepy, 2018  Canada gariepy 2018 7            Suicidal ideation              MH<-->NEET
# 53  Goldman-Mellor, 2016      UK goldman 2016 9          Suicidal behaviours               MH-->NEET
# 110          Power, 2015 Ireland   power 2015 6 Suicidal attempts (lifetime)              MH<-->NEET

meta.MHsui.Gall.rep0 <- metagen(OR_ln,
                                SE,
                                data=MHsui.Gall.rep0,
                                studlab=paste(Study),
                                comb.fixed = FALSE,
                                comb.random = TRUE,
                                method.tau = "DL",
                                hakn = FALSE,
                                prediction=FALSE,
                                sm="OR")
meta.MHsui.Gall.rep0



# SUICIDAL - directionality ----------------------------------------------------------------------------------

meta.MHsui.Gall.rep0.dir<-update.meta(meta.MHsui.Gall.rep0, 
                                      byvar=Temporality.of.measures, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHsui.Gall.rep0.dir

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHsui.Gall.rep0.dir)







# SUICIDAL - geography ------------------------------------------------------------------------------------

meta.MHsui.Gall.rep0.geo<-update.meta(meta.MHsui.Gall.rep0, 
                                      byvar=Country, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHsui.Gall.rep0.geo

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHsui.Gall.rep0.geo)








# SUICIDAL - gender -------------------------------------------------------------------------------------------

MHsui.Gbg.rep0 <- neet2[which(neet2$Suicidal==1 & neet2$Gender!="all" & is.na(neet2$Repeated_sample)),]
View(MHsui.Gbg.rep0)
head(MHsui.Gbg.rep0)

# <0 rows>









# ANY ========================================================================================================
#
# any = 1, gender = all, repeated sample = 0

# how many studies total
View(neet2[which(neet2$Any==1 & neet2$Gender=="all"),])
# 4 studies

MHany.Gall.rep0 <- neet2[which(neet2$Any==1 & neet2$Gender=="all" & is.na(neet2$Repeated_sample)),]
View(MHany.Gall.rep0)
head(MHany.Gall.rep0)
#             Study   Country       study_no       Mental.health.measures Temporality.of.measures
# 28   Benjet, 2012    Mexico  benjet 2012 5                 Any disorder              MH<-->NEET
# 111   Power, 2015   Ireland   power 2015 7        Any lifetime disorder              MH<-->NEET
# 115   Power, 2015   Ireland  power 2015 10 Any diagnosis in adolescence               MH-->NEET
# 117 Rodwell, 2018 Australia rodwell 2018 1      Common mental disorders               MH-->NEET

# Gen says to only use Power 2015's "Any lifetime disorder" here and "Any dx in adol" for directionality
MHany.Gall.rep0 <- MHany.Gall.rep0[c(1,2,4),]
View(MHany.Gall.rep0)
head(MHany.Gall.rep0)
# Study   Country       study_no  Mental.health.measures Temporality.of.measures
# 28   Benjet, 2012    Mexico  benjet 2012 5            Any disorder              MH<-->NEET
# 111   Power, 2015   Ireland   power 2015 7   Any lifetime disorder              MH<-->NEET
# 117 Rodwell, 2018 Australia rodwell 2018 1 Common mental disorders               MH-->NEET

meta.MHany.Gall.rep0 <- metagen(OR_ln,
                                SE,
                                data=MHany.Gall.rep0,
                                studlab=paste(Study),
                                comb.fixed = FALSE,
                                comb.random = TRUE,
                                method.tau = "DL",
                                hakn = FALSE,
                                prediction=FALSE,
                                sm="OR")
meta.MHany.Gall.rep0


# ANY - directionality ----------------------------------------------------------------------------------------

MHany.Gall.rep0 <- neet2[which(neet2$Any==1 & neet2$Gender=="all" & is.na(neet2$Repeated_sample)),]
View(MHany.Gall.rep0)
head(MHany.Gall.rep0)
#             Study   Country       study_no       Mental.health.measures Temporality.of.measures
# 28   Benjet, 2012    Mexico  benjet 2012 5                 Any disorder              MH<-->NEET
# 111   Power, 2015   Ireland   power 2015 7        Any lifetime disorder              MH<-->NEET
# 115   Power, 2015   Ireland  power 2015 10 Any diagnosis in adolescence               MH-->NEET
# 117 Rodwell, 2018 Australia rodwell 2018 1      Common mental disorders               MH-->NEET



# Gen says to only use Power 2015's "Any dx in adol" for directionality
MHany.Gall.rep0 <- MHany.Gall.rep0[c(1,3,4),]
View(MHany.Gall.rep0)
head(MHany.Gall.rep0)

# Study   Country       study_no       Mental.health.measures Temporality.of.measures
# 28   Benjet, 2012    Mexico  benjet 2012 5                 Any disorder              MH<-->NEET
# 115   Power, 2015   Ireland  power 2015 10 Any diagnosis in adolescence               MH-->NEET
# 117 Rodwell, 2018 Australia rodwell 2018 1      Common mental disorders               MH-->NEET


meta.MHany.Gall.rep0 <- metagen(OR_ln,
                                SE,
                                data=MHany.Gall.rep0,
                                studlab=paste(Study),
                                comb.fixed = FALSE,
                                comb.random = TRUE,
                                method.tau = "DL",
                                hakn = FALSE,
                                prediction=FALSE,
                                sm="OR")
meta.MHany.Gall.rep0


meta.MHany.Gall.rep0.dir<-update.meta(meta.MHany.Gall.rep0, 
                                      byvar=Temporality.of.measures, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHany.Gall.rep0.dir

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHany.Gall.rep0.dir)






# ANY - geography ------------------------------------------------------------------------------------

#going back to "any lifetime disorder" for subgroup analysis by geography

MHany.Gall.rep0 <- neet2[which(neet2$Any==1 & neet2$Gender=="all" & is.na(neet2$Repeated_sample)),]
View(MHany.Gall.rep0)
head(MHany.Gall.rep0)
#             Study   Country       study_no       Mental.health.measures Temporality.of.measures
# 28   Benjet, 2012    Mexico  benjet 2012 5                 Any disorder              MH<-->NEET
# 111   Power, 2015   Ireland   power 2015 7        Any lifetime disorder              MH<-->NEET
# 115   Power, 2015   Ireland  power 2015 10 Any diagnosis in adolescence               MH-->NEET
# 117 Rodwell, 2018 Australia rodwell 2018 1      Common mental disorders               MH-->NEET



#going back to "any lifetime disorder"
MHany.Gall.rep0 <- MHany.Gall.rep0[c(1,2,4),]
View(MHany.Gall.rep0)
head(MHany.Gall.rep0)

#             Study   Country       study_no  Mental.health.measures     Temporality.of.measures              
# 28   Benjet, 2012    Mexico  benjet 2012 5            Any disorder              MH<-->NEET
# 111   Power, 2015   Ireland   power 2015 7   Any lifetime disorder              MH<-->NEET
# 117 Rodwell, 2018 Australia rodwell 2018 1 Common mental disorders               MH-->NEET




meta.MHany.Gall.rep0.geo<-update.meta(meta.MHany.Gall.rep0, 
                                      byvar=Country, 
                                      comb.random = TRUE, 
                                      comb.fixed = FALSE)
meta.MHany.Gall.rep0.geo

# plot so can see full details of subgroup analyses (namely, the studies in each group)
forest(meta.MHany.Gall.rep0.geo)






# ANY - gender -----------------------------------------------------------------------------------------------

MHany.Gbg.rep0 <- neet2[which(neet2$Any==1 & neet2$Gender!="all" & is.na(neet2$Repeated_sample)),]
View(MHany.Gbg.rep0)
head(MHany.Gbg.rep0)

# <0 rows>


#### Checking results #######################################################################################

write.csv(neet2,'neet2.csv')
