
# creating forest plots for Lancet Psychiatry
# main analyses only:
#      Mood
#      Anxiety
#      Behavioural
#      Alcohol use
#      Cannabis use
#      Drug use
#      Any disorder 
#      Suicidal behaviours
#      Psychological distress


#### Mise en place ###################################################

# installing and loading packages
install.packages("meta")
library(meta)
library(metafor)
library(dplyr)

# load data from file
neet <- read.csv("~/Dropbox/PEPP/NEET meta-analysis/metaanalysis/neet 20200619.csv")
View(neet)
str(neet)

#### Creating composite dataframe, pooling estimates ##################

single <- bind_rows(mood, anx, beh, alc, can, drg, any, sui, dis)
single
summary(single)

single$MH_measure <- as.character(single$MH_measure)
summary(single$MH_measure)

View(single)

# this didn't work

#levels(single$MH_measure) <- c("mood", "anxiety", "behavioral", "alcohol", "cannabis", "drug", "any", "suicide", "distress")
#levels(single$MH_measure)
#single$MH_measure <- as.numeric(single$MH_measure)
#single$MH_measure <- as.factor(single$MH_measure)
#str(single$MH_measure)
#summary(single$MH_measure)

single$MH_measure[single$MH_measure == "depression"] <- "Mood"
single$MH_measure[single$MH_measure == "anxiety"] <- "Anxiety"
single$MH_measure[single$MH_measure == "behavioral"] <- "Behavioural"
single$MH_measure[single$MH_measure == "alcohol"] <- "Alcohol use"
single$MH_measure[single$MH_measure == "cannabis"] <- "Cannabis use"
single$MH_measure[single$MH_measure == "drug"] <- "Drug use"
single$MH_measure[single$MH_measure == "any"] <- "Any disorder"
single$MH_measure[single$MH_measure == "suicide"] <- "Suicidal behaviours"
single$MH_measure[single$MH_measure == "distress"] <- "Psychological distress"


#### Generating and saving plot #########################################


single

meta.single <- metagen(OR_ln,
                        SE,
                        data=single,
                        studlab=paste(Study),
                        comb.fixed = FALSE,
                        comb.random = TRUE,
                        method.tau = "DL",
                        hakn = FALSE,
                        prediction=FALSE,
                        sm="OR")

meta.single<-update.meta(meta.single,
                          byvar=MH_measure, 
                          comb.random = T, 
                          comb.fixed = F)

meta.single

p<-forest(meta.single,
       sortvar = Study,
       overall = F,
       overall.hetstat = F,
       #       print.subgroup.labels #A logical indicating whether subgroup label should be printed.
       #       bylab #A character string with a label for the grouping variable.
       print.byvar = F, #logical, whether the name of the grouping variable should be printed in front of the group labels.
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)



svg(file = 'single_8x18.svg', width = 8, height = 18) 
forest(meta.single,
       sortvar = Study,
       overall = F,
       overall.hetstat = F,
#       print.subgroup.labels #A logical indicating whether subgroup label should be printed.
#       bylab #A character string with a label for the grouping variable.
       print.byvar = F, #logical, whether the name of the grouping variable should be printed in front of the group labels.
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)
dev.off() 


#### testing plots and saving ############################################

# https://www.rdocumentation.org/packages/meta/versions/4.9-9/topics/forest
# ?meta::forest

forest(meta.mood, 
       studlab = T,
       comb.random=TRUE,
       overall = T,
       xlim = c(0.5,14.0),                    # set x axis limits
       text.random = "Random effects model",  # change text
       col.random = "black",
       type.random = "diamond",
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                            # remove "Odds Ratio" text above plot
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       just = "left", just.addcols = "left", just.studlab = "left",
       col.study = "black",
       col.square = "grey",
       col.square.lines = "grey",
       col.inside = "black",
       col.diamond = "white",
       hetstat = TRUE,
       overall.hetstat=T, 
       print.I2 = T,
       print.tau2=F)

# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/saving-the-forest-plots.html

# SVG with dimensions
svg(file='plot9-27.svg', width = 9, height = 27) 
forest.jama<-forest(meta.test)
dev.off() 
