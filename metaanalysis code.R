#### https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/ ----
# 
# > neet <- read.csv("~/Dropbox/PEPP/NEET/neet.csv")
# >   View(neet)
#
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
