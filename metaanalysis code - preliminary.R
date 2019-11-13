####  First analyses, 26-27 Aug 2019 ----
# Rudimentary analyses pulled together for Gen's CAPE 2019 poster
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

# checking out the variables in the NEET dataframe, named neet for simplicity
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

#### Refining analyses, 21 Oct 2019 ----

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







#### Analyses: Direction of association  ----
# only looking at longitudinal studies, as therefore disregarding associational MH<-->NEET
# ACTUALLY, PRETTY SURE THIS IS NOT WHAT WE NEED TO BE RUNNING. DISREGARD THIS SECTION


# MH-->NEET
#
# temporality = MH-->NEET, gender = all, repeated sample = 0, OR != NA

Tmh.Gall.rep0 <- neet2[which(neet2$Temporality.of.measures=="MH-->NEET" & neet2$Gender=="all" 
                             & is.na(neet2$Repeated_sample) & !is.na(neet2$OR_ln)),]
View(Tmh.Gall.rep0)
head(Tmh.Gall.rep0)
# Study     Country       study_no Mental.health.measures Temporality.of.measures
# 2          Baggio, 2015 Switzerland  baggio 2015 1  General mental health               MH-->NEET
# 3          Baggio, 2015 Switzerland  baggio 2015 2    Depressive symptoms               MH-->NEET
# 5          Baggio, 2015 Switzerland  baggio 2015 4  Cannabis use disorder               MH-->NEET
# 7           Bania, 2019      Norway   bania 2019 1     Emotional problems               MH-->NEET
# 10          Bania, 2019      Norway   bania 2019 4       Conduct problems               MH-->NEET
# 52 Goldman-Mellor, 2016          UK goldman 2016 8  Harmful substance use               MH-->NEET

meta.Tmh.Gall.rep0 <- metagen(OR_ln,
                              SE,
                              data=Tmh.Gall.rep0,
                              studlab=paste(Study),
                              comb.fixed = FALSE,
                              comb.random = TRUE,
                              method.tau = "DL",
                              hakn = FALSE,
                              prediction=FALSE,
                              sm="OR")
meta.Tmh.Gall.rep0



# NEET-->MH
#
# temporality = NEET-->MH, gender = all, repeated sample = 0, OR != NA
View(neet2[which(neet2$Temporality.of.measures=="NEET-->MH" & neet2$Gender=="all"),])

Tnt.Gall.rep0 <- neet2[which(neet2$Temporality.of.measures=="NEET-->MH" & neet2$Gender=="all" 
                             & is.na(neet2$Repeated_sample) & !is.na(neet2$OR_ln)),]
View(Tnt.Gall.rep0)
head(Tnt.Gall.rep0)
# Study     Country       study_no Mental.health.measures Temporality.of.measures
# 2          Baggio, 2015 Switzerland  baggio 2015 1  General mental health               MH-->NEET
# 3          Baggio, 2015 Switzerland  baggio 2015 2    Depressive symptoms               MH-->NEET
# 5          Baggio, 2015 Switzerland  baggio 2015 4  Cannabis use disorder               MH-->NEET
# 7           Bania, 2019      Norway   bania 2019 1     Emotional problems               MH-->NEET
# 10          Bania, 2019      Norway   bania 2019 4       Conduct problems               MH-->NEET
# 52 Goldman-Mellor, 2016          UK goldman 2016 8  Harmful substance use               MH-->NEET

meta.Tmh.Gall.rep0 <- metagen(OR_ln,
                              SE,
                              data=Tmh.Gall.rep0,
                              studlab=paste(Study),
                              comb.fixed = FALSE,
                              comb.random = TRUE,
                              method.tau = "DL",
                              hakn = FALSE,
                              prediction=FALSE,
                              sm="OR")
meta.Tmh.Gall.rep0

