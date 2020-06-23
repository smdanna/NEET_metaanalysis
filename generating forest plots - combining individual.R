
# creating forest plots for Lancet Psychiatry
# main analyses only:
#      mood
#      anxiety
#      behaviour
#      alcohol
#      cannabis
#      drug use
#      any 
#      suicidal
#      psychiatric distress


#### Mise en place ###########################################

# installing and loading packages
install.packages("meta")
library(meta)
library(metafor)
library(dplyr)

# load data from file
neet <- read.csv("~/Dropbox/PEPP/NEET meta-analysis/metaanalysis/neet 20200619.csv")
View(neet)
str(neet)

#### Pooling estimates ###########################################

#### · mood #### 

mood <- filter(neet, Mood==1 & Gender=="all" & is.na(Repeated_sample) & !is.na(OR_ln))

mood

meta.mood <- metagen(OR_ln,
                     SE,
                     data=mood,
                     studlab=paste(Study),
                     comb.fixed = FALSE,
                     comb.random = TRUE,
                     method.tau = "DL",
                     hakn = FALSE,
                     prediction=FALSE,
                     sm="OR")

meta.mood

#### · anxiety #### 

anx <- filter(neet, Anxiety==1 & Gender=="all" & is.na(Repeated_sample) & !is.na(OR_ln))

anx

meta.anx <- metagen(OR_ln,
                    SE,
                    data=anx,
                    studlab=paste(Study),
                    comb.fixed = FALSE,
                    comb.random = TRUE,
                    method.tau = "DL",
                    hakn = FALSE,
                    prediction=FALSE,
                    sm="OR")

meta.anx

#### · behavioural #### 

beh <- filter(neet, Behavioral==1 & Gender=="all" & is.na(Repeated_sample) & is.na(rep_beh))

beh

meta.beh <- metagen(OR_ln,
                    SE,
                    data=beh,
                    studlab=paste(Study),
                    comb.fixed = FALSE,
                    comb.random = TRUE,
                    method.tau = "DL",
                    hakn = FALSE,
                    prediction=FALSE,
                    sm="OR")

meta.beh

#### · alcohol #### 

alc <- filter(neet, Alcohol==1 & Gender=="all" & is.na(Repeated_sample) & !is.na(OR_ln))

alc

meta.alc <- metagen(OR_ln,
                    SE,
                    data=alc,
                    studlab=paste(Study),
                    comb.fixed = FALSE,
                    comb.random = TRUE,
                    method.tau = "DL",
                    hakn = FALSE,
                    prediction=FALSE,
                    sm="OR")

meta.alc

#### · cannabis #### 

can <- filter(neet, Cannabis==1 & Gender=="all" & is.na(Repeated_sample))

can

meta.can <- metagen(OR_ln,
                    SE,
                    data=can,
                    studlab=paste(Study),
                    comb.fixed = FALSE,
                    comb.random = TRUE,
                    method.tau = "DL",
                    hakn = FALSE,
                    prediction=FALSE,
                    sm="OR")

meta.can

#### · drug #### 

drg <- filter(neet, Drug==1 & Gender=="all" & is.na(Repeated_sample) & is.na(rep_drg) & !is.na(OR_ln))

drg

meta.drg <- metagen(OR_ln,
                    SE,
                    data=drg,
                    studlab=paste(Study),
                    comb.fixed = FALSE,
                    comb.random = TRUE,
                    method.tau = "DL",
                    hakn = FALSE,
                    prediction=FALSE,
                    sm="OR")

meta.drg

#### · any problem #### 

any <- filter(neet, Any==1 & Gender=="all" & is.na(rep_any))

any

meta.any <- metagen(OR_ln,
                    SE,
                    data=any,
                    studlab=paste(Study),
                    comb.fixed = FALSE,
                    comb.random = TRUE,
                    method.tau = "DL",
                    hakn = FALSE,
                    prediction=FALSE,
                    sm="OR")

meta.any

#### · suicidal #### 

sui <- filter(neet, Suicidal==1 & Gender=="all" & is.na(Repeated_sample))

sui

meta.sui <- metagen(OR_ln,
                    SE,
                    data=sui,
                    studlab=paste(Study),
                    comb.fixed = FALSE,
                    comb.random = TRUE,
                    method.tau = "DL",
                    hakn = FALSE,
                    prediction=FALSE,
                    sm="OR")

meta.sui

#### · distress #### 

dis <- filter(neet, Distress==1 & is.na(Repeated_sample)==TRUE & is.na(rep_dis)==TRUE & !is.na(OR_ln))

dis

meta.dis <- metagen(OR_ln,
                    SE,
                    data=dis,
                    studlab=paste(Study),
                    comb.fixed = FALSE,
                    comb.random = TRUE,
                    method.tau = "DL",
                    hakn = FALSE,
                    prediction=FALSE,
                    sm="OR")

meta.dis

#### Generating and saving plots #########################################

#### · mood #### 

forest(meta.mood, 
       text.random = "Mood (random effects)", 
       type.random = "diamond",
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)


svg(file = 'mood_8x268_xlim.svg', width = 8, height = 2.68) 
plot.mood <- forest(meta.mood,
                    text.random = "Mood (random effects)",
                    xlim = c(0.1,10), 
                    type.random = "diamond",
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

#### · anxiety #### 

forest(meta.anx, 
       text.random = "Anxiety (random effects)", 
       type.random = "diamond",
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)


svg(file = 'anx_8x245_xlim.svg', width = 8, height = 2.45) 
plot.anx <- forest(meta.anx,
                    text.random = "Anxiety (random effects)",
                    xlim = c(0.1,10),
                    type.random = "diamond",
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

#### · behavioural #### 

forest(meta.beh, 
       text.random = "Behavioural (random effects)",
       xlim = c(0.1,10), 
       type.random = "diamond",
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)


svg(file = 'beh_8x29_xlim.svg', width = 8, height = 2.9) 
plot.beh <- forest(meta.beh,
                    text.random = "Behavioural (random effects)",
                    xlim = c(0.1,10),
                    type.random = "diamond",
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

#### · alcohol #### 

forest(meta.alc, 
       text.random = "Alcohol use (random effects)", 
       type.random = "diamond",
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)


svg(file = 'alc_8x245_xlim.svg', width = 8, height = 2.45) 
plot.alc <- forest(meta.alc,
                    text.random = "Alcohol use (random effects)",
                    xlim = c(0.1,10),
                    type.random = "diamond",
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

#### · cannabis #### 

forest(meta.can, 
       text.random = "Cannabis use (random effects)", 
       type.random = "diamond",
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)


svg(file = 'can_8x268_xlim.svg', width = 8, height = 2.68) 
plot.can <- forest(meta.can,
                    text.random = "Cannabis use (random effects)",
                    xlim = c(0.1,10),
                    type.random = "diamond",
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

#### · drug #### 

forest(meta.drg, 
       text.random = "Drug use (random effects)", 
       type.random = "diamond",
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)


svg(file = 'drg_8x245_xlim.svg', width = 8, height = 2.45) 
plot.drg <- forest(meta.drg,
                    text.random = "Drug use (random effects)",
                    xlim = c(0.1,10),
                    type.random = "diamond",
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

#### · any problem #### 

forest(meta.any, 
       text.random = "Any disorder (random effects)", 
       type.random = "diamond",
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)

svg(file = 'any_8x2_xlim.svg', width = 8, height = 2) 
plot.any <- forest(meta.any,
                    text.random = "Any disorder (random effects)",
                    xlim = c(0.1,10),
                    type.random = "diamond",
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

#### · suicidal #### 

forest(meta.sui, 
       text.random = "Suicidal behaviours (random effects)", 
       type.random = "diamond",
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)


svg(file = 'sui_8x223_xlim.svg', width = 8, height = 2.23) 
plot.sui <- forest(meta.sui,
                    text.random = "Suicidal behaviours (random effects)",
                    xlim = c(0.1,10),
                    type.random = "diamond",
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

#### · distress #### 

forest(meta.dis, 
       text.random = "Psychological distress (random effects)", 
       type.random = "diamond",
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)


svg(file = 'dis_8x29_xlim.svg', width = 8, height = 2.9) 
plot.dis <- forest(meta.dis,
                    text.random = "Psychological distress (random effects)",
                    xlim = c(0.1,10),
                    type.random = "diamond",
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

#### generating plots in single figure #### 

par(mfrow=c(2,2))
forest(meta.mood, 
       text.random = "Mood (random effects)", 
       type.random = "diamond",
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)
forest(meta.anx, 
       text.random = "Anxiety (random effects)", 
       type.random = "diamond",
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)
forest(meta.beh, 
       text.random = "Behavioural (random effects)",
       xlim = c(0.1,10), 
       type.random = "diamond",
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)
forest(meta.alc, 
       text.random = "Alcohol use (random effects)", 
       type.random = "diamond",
       leftcols=c("studlab", "N"),
       leftlabs = c("Study","n"),
       smlab = "",                           
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       col.square = "black",
       col.inside = "black",
       col.diamond = "white",
       print.tau2=F)

#### testing plots and saving #### 

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
