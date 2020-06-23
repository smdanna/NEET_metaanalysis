
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

#### Creating composite dataframes, pooling estimates ##################
# "single" dataframe is created in "generating single forest plot.R"
single <- read.csv("~/Dropbox/PEPP/NEET meta-analysis/metaanalysis/neet single plot 20200623.csv")
single
summary(single)


single1 <- filter(single, MH_measure == "Mood"| MH_measure == "Anxiety" | MH_measure == "Behavioural")
single1

single2 <- filter(single, MH_measure == "Alcohol use" | MH_measure == "Cannabis use" | MH_measure == "Drug use")
single2

single3 <- filter(single, MH_measure == "Any disorder"| MH_measure == "Suicidal behaviours" | MH_measure == "Psychological distress")
single3


View(single)


#### Generating and saving plot #########################################

#### · single1 #### 

single1

meta.single1 <- metagen(OR_ln,
                        SE,
                        data=single1,
                        studlab=paste(Study),
                        comb.fixed = FALSE,
                        comb.random = TRUE,
                        method.tau = "DL",
                        hakn = FALSE,
                        prediction=FALSE,
                        sm="OR")

meta.single1<-update.meta(meta.single1,
                          byvar=MH_measure, 
                          comb.random = T, 
                          comb.fixed = F)

meta.single1

forest(meta.single1,
       xlim = c(0.1,10),
       sortvar = Study,
       overall = F,
       overall.hetstat = F,
       print.byvar = F, #logical, whether name of grouping var should be printed in front of group labels
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)

pdf(file = 'single1_8x7.pdf', width = 8, height = 7)
forest(meta.single1,
       xlim = c(0.1,10),
       sortvar = Study,
       overall = F,
       overall.hetstat = F,
       print.byvar = F, #logical, whether name of grouping var should be printed in front of group labels
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

#### · single2 #### 

single2

meta.single2 <- metagen(OR_ln,
                       SE,
                       data=single2,
                       studlab=paste(Study),
                       comb.fixed = FALSE,
                       comb.random = TRUE,
                       method.tau = "DL",
                       hakn = FALSE,
                       prediction=FALSE,
                       sm="OR")

meta.single2<-update.meta(meta.single2,
                         byvar=MH_measure, 
                         comb.random = T, 
                         comb.fixed = F)

meta.single2

forest(meta.single2,
          xlim = c(0.1,10),
          sortvar = Study,
          overall = F,
          overall.hetstat = F,
          print.byvar = F, #logical, whether name of grouping var should be printed in front of group labels
          leftcols=c("studlab", "N"),
          leftlabs = c("Study","n"),
          smlab = "",                           
          rightcols = c("effect.ci"),
          rightlabs = c("Odds Ratio [95% CI]"),
          col.square = "black",
          col.inside = "black",
          col.diamond = "white",
          print.tau2=F)



pdf(file = 'single2_8x7.pdf', width = 8, height = 7) 
forest(meta.single2,
       xlim = c(0.1,10),
       sortvar = Study,
       overall = F,
       overall.hetstat = F,
       print.byvar = F, #logical, whether name of grouping var should be printed in front of group labels
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

#### · single3 #### 

single3

meta.single3 <- metagen(OR_ln,
                       SE,
                       data=single3,
                       studlab=paste(Study),
                       comb.fixed = FALSE,
                       comb.random = TRUE,
                       method.tau = "DL",
                       hakn = FALSE,
                       prediction=FALSE,
                       sm="OR")

meta.single3<-update.meta(meta.single3,
                         byvar=MH_measure, 
                         comb.random = T, 
                         comb.fixed = F)

meta.single3

forest(meta.single3,
          xlim = c(0.1,10),
          sortvar = Study,
          overall = F,
          overall.hetstat = F,
          print.byvar = F, #logical, whether name of grouping var should be printed in front of group labels
          leftcols=c("studlab", "N"),
          leftlabs = c("Study","n"),
          smlab = "",                           
          rightcols = c("effect.ci"),
          rightlabs = c("Odds Ratio [95% CI]"),
          col.square = "black",
          col.inside = "black",
          col.diamond = "white",
          print.tau2=F)



pdf(file = 'single3_8x7.pdf', width = 8, height = 7) 
forest(meta.single3,
       xlim = c(0.1,10),
       sortvar = Study,
       overall = F,
       overall.hetstat = F,
       print.byvar = F, #logical, whether name of grouping var should be printed in front of group labels
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
