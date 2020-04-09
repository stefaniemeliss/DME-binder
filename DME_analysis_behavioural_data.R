# this r script summarises the behavioural analysis and creates the plots for the publication

##################################################################  
############################ set ups  ############################ 
##################################################################  

# clear workspace
rm(list = ls())

# load libraries and functions
library(ggplot2)
library(psych)
library(reshape2)
library(osfr)
library(dplyr)
library(lme4)
library(lmerTest)
library(car)
source("anovakun_482.txt") # anovakun package retrieved from http://riseki.php.xdomain.jp/index.php?ANOVA君%2FANOVA君の使い方


# download datasets from OSF
#osfr::osf_auth("PleaseUnCommentAndPasteTheCodeFromTheEmailHere") # Authenticate osfr with a personal access token
osfr::osf_retrieve_file("https://osf.io/qgbd6/") %>% # retrieve file from OSF
  osfr::osf_download(conflicts = "overwrite") # and download it into project

# read in data set
df <- read.csv("Behaviour.csv")

# subset data set fMRI == 1: These are the 51 participants in the final fMRI sample
df <- df[df$fMRI == 1,]

# recode variable capturing between group mannipulation
df$cond <- ifelse(df$cond == "C", "No-reward",
                  ifelse(df$cond == "R", "Reward",
                         ifelse(df$cond == "G", "Gambling", NA)))

# ordering data in the same way as in SPM
df$orderedcond <- rep(c(1, 3, 2), each =17)
df <- df[with(df, order(df$orderedcond, df$scan)),]
df$orderedcond <- as.factor(df$orderedcond)
df$orderedcond <- ifelse(df$orderedcond == "1", "No-reward",
                         ifelse(df$orderedcond == "2", "Reward",
                                ifelse(df$orderedcond == "3", "Gambling", NA)))

##################################################################################
############################ behavioural assessments  ############################ 
##################################################################################

# a1-a10: 10 questions after session 1 (asked inside the scanner)
# b1-b10: same 10 questions after session 2 (asked inside the scanner)
# c1-c10: combined ratings for both sessions [ created with ci <- (ai + bi)/2 ]
# 1. I am glad to know that the next one is easy
# 2. I am disappointed to know that the next one is easy --> used to create c2r [recoded df$c2r <- car::recode(df$c2, "1=5; 2=4; 3=3; 4=2; 5=1") ]
# 3. I am glad to know that the next one is moderately difficult
# 4. I am disappointed to know that the next one is moderately difficult --> used to create c4r [recoded df$c4r <- car::recode(df$c4, "1=5; 2=4; 3=3; 4=2; 5=1") ]
# 5. I am glad to know that the next one is very difficult
# 6. I am disappointed to know that the next one is very difficult --> used to create c6r [recoded df$c6r <- car::recode(df$c6, "1=5; 2=4; 3=3; 4=2; 5=1") ]
# 7. I am glad to know that the next one is a watchstop trial
# 8. I am disappointed to know that the next one is a watchstop trial --> used to create c8r [recoded df$c8r <- car::recode(df$c8, "1=5; 2=4; 3=3; 4=2; 5=1") ]
# 9. I understand the rule of the experiment
# 10. I am satisfied with the results so far

# POST QUESTIONS ASKED AT THE END OF THE EXPERIMENT

# intrinsic motivation after the scanning
#post1.	It was fun to do the easy task
#post2.	It was boring to do the easy task (reversely coded)
#post3.	It was enjoyable to do the easy task
#post4.	It was fun to do the moderately difficult task
#post5.	It was boring to do the moderately difficult task (reversely coded)
#post6.	It was enjoyable to do the moderately difficult task
#post7.	It was fun to do the very difficult task
#post8.	It was boring to do the very difficult task (reversely coded)
#post9.	It was enjoyable to do the very difficult task
#post10.	It was fun to do the watchstop task
#post11.	It was boring to do the watchstop task (reversely coded)
#post12.	It was enjoyable to do the watchstop task

# calculate the score of intrinsic motivation for each level of chance of success
df$mot_high <- (8 + df$post1 - df$post2 + df$post3)/3 # high chance of success
df$mot_mod <- (8 + df$post4 - df$post5 + df$post6)/3 # moderate chance of success
df$mot_low <- (8 + df$post7 - df$post8 + df$post9)/3 # extremely-low chance of success
df$mot_ws <- (8 + df$post10 - df$post11 + df$post12)/3 # watch stop

# calculate Cronbach's alpha for the items for capturing intrinsic motivation for each level of success
mot_high <- df[,c("post1", "post2", "post3")] # high chance of success
psych::alpha(mot_high, keys = "post2") # post2 reversely coded
mot_mod <- df[,c("post4", "post5", "post6")] # moderate chance of success
alpha(mot_mod, keys = "post5") # post5 reversely coded
mot_low <- df[,c("post7", "post8", "post9")] # extremely-low chance of success
alpha(mot_low, keys = "post8") # post8 reversely coded
mot_ws <- df[,c("post10", "post11", "post12")] # watch stop
alpha(mot_ws, keys = "post11") # post11 reversely coded

# difficulty
#post13.	The easy task was difficult
#post14.	The moderately difficult task was difficult
#post15.	The very difficult task was difficult

# Others 
#post16.	The experiment was sleepy
#post17.	I concentrated on the experiment
#post18.	I was unable to focus on the experiment
#post19.	To be honest, I was totally demotivated
#post20.	I really did not like the experiment

# post happiness
#post21.	I felt happy when I see the cue of the easy task
#post23.	I felt happy when I see the cue of the moderately difficult task
#post25.	I felt happy when I see the cue of the very difficult task
#post27   I felt happy when I see the cue of the watchstop task

# post dissappointment
#post22.	I got dissappointed when I see the cue of the easy task
#post24.	I got dissappointed when I see the cue of the moderately difficult task
#post26.	I got dissappointed when I see the cue of the very difficult task
#post28.	I got dissappointed when I see the cue of the watchstop task

# calculate rewarding value: recode disappointment items and combine them with happiness
df$post22_r <- car::recode(df$post22, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1" )
df$post24_r <- car::recode(df$post24, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1" )
df$post26_r <- car::recode(df$post26, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1" )
df$post28_r <- car::recode(df$post28, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1" )

# calculate the correlations between the items
cor(df$post21, df$post22_r) # extremely-low chance of success
cor(df$post23, df$post24_r) # moderate chance of success
cor(df$post25, df$post26_r) # high chance of success
cor(df$post27, df$post28_r) # watch stop

# calculate the score of reward value for each level of chance of success
df$val_high <- (df$post21 + df$post22_r)/2 # high chance of success
df$val_mod <- (df$post23 + df$post24_r)/2 # moderate chance of success
df$val_low <- (df$post25 + df$post26_r)/2 # extremely-low chance of success
df$val_ws <- (df$post27 + df$post28_r)/2 # watch stop

# Post29: I felt dizzy during the experiment

# post comparison happiness
#"which cue did you feel more positive?"
#postc1: Easy -2, -1, 0, 1, 2  Medium
#postc2: Difficult -2, -1, 0, 1, 2  Easy
#postc3: Easy -2, -1, 0, 1, 2  WS
#postc4: Medium -2, -1, 0, 1, 2  Difficult
#postc5: WS -2, -1, 0, 1, 2  Medium
#postc6: Difficult -2, -1, 0, 1, 2  WS

# post comparison motivation "which did you feel more motivated"? Gambling condition did not receive these items as "motivation" does not make sense to this condition.
#"which cue did you feel more motivated about?"
#postc7: Easy -2, -1, 0, 1, 2  Medium
#postc8: Difficult -2, -1, 0, 1, 2  Easy
#postc9: Easy -2, -1, 0, 1, 2  WS
#postc10: Medium -2, -1, 0, 1, 2  Difficult
#postc11: WS -2, -1, 0, 1, 2  Medium
#postc12: Difficult -2, -1, 0, 1, 2  WS

# sleepr: How long do you regularly sleep?
# sleepd: How long did you sleep last night
# Easy/Mid/Dif/IAT: forget about them
# Score: Total score that participants obtained


######################################################################   
########################## CONTRAST ESTIMATES ######################## 
######################################################################   

# the contrast estimates come from the 2017_DME/GLM/GLM1/2nd-level/ANOVA/TwoWay/SWWS_/SPM.mat file
# ROIs created in marsbar (Build ROI --> point coordinate --> peak voxel [-9 5 -8] and [9 5 -8]
# ./points_coordinate_mm_-9_5_-8_roi.mat & ./points_coordinate_mm_9_5_-8_roi.mat
# extracted ROI data saved in file ./extracted_point_coordinates_-9_5_-8_and_9_5_-8_mres.mat
# txt.files created with ./save_extracted_contrast_estimates.m


# download datasets from OSF
#osfr::osf_auth("PleaseUnCommentAndPasteTheCodeFromTheEmailHere") # Authenticate osfr with a personal access token
# left peak
osfr::osf_retrieve_file("https://osf.io/dyvrf/") %>% # retrieve file from OSF
  osfr::osf_download(conflicts = "overwrite") # and download it into project
# right peak
osfr::osf_retrieve_file("https://osf.io/9mzsq/") %>% # retrieve file from OSF
  osfr::osf_download(conflicts = "overwrite") # and download it into project

# read in values from .txt file 
left <- read.delim("peak_leftStriatum_contrastEstimates.txt", header = F, sep = "\t")
right <- read.delim("peak_rightStriatum_contrastEstimates.txt", header = F, sep = "\t")

# add values to data set 
df$left_low <- left$V1 # extremely-low chance of success
df$left_mod <- left$V2 # moderate chance of success
df$left_high <- left$V3 # high chance of success

df$right_low <- right$V1 # extremely-low chance of success
df$right_mod <- right$V2 # moderate chance of success
df$right_high <- right$V3 # high chance of success

#######################################################################
########################## 3  x 3  MIXED ANOVA ######################## 
#######################################################################

# between factor group: no-reward, reward, or gambling
# within-factor chance of success: high chance, moderate chance, or extremely-low chance

# DEPENDENT VARIABLE 1: ratings of intrinsic motivation #
# create data frame in long format
df_mot <- melt(df, id.vars = c("id", "cond"), measure.vars = c("mot_low", "mot_mod", "mot_high") )
names(df_mot) <- c("id", "cond", "measurement", "rating")
df_mot$chance_success <- ifelse(df_mot$measurement == "mot_low", "extremely-low",
                               ifelse(df_mot$measurement == "mot_mod", "moderate",
                                      ifelse(df_mot$measurement == "mot_high", "high",NA)))
# specify anovakun
anovakun_mot <- df_mot
anovakun_mot <- anovakun_mot[,c("id", "cond", "chance_success", "rating")]
anovakun(anovakun_mot, "AsB", 3, 3, long = T, geta = T)

# DEPENDENT VARIABLE 2: ratings of rewarding value #
# create data frame in long format
df_val <- melt(df, id.vars = c("id", "cond"), measure.vars = c("val_low", "val_mod", "val_high") )
names(df_val) <- c("id", "cond", "measurement", "rating")
df_val$chance_success <- ifelse(df_val$measurement == "val_low", "extremely-low",
                                ifelse(df_val$measurement == "val_mod", "moderate",
                                       ifelse(df_val$measurement == "val_high", "high",NA)))
# specify anovakun
anovakun_val <- df_val
anovakun_val <- anovakun_val[,c("id", "cond", "chance_success", "rating")]
anovakun(anovakun_val, "AsB", 3, 3, long = T, geta = T)



########################################################################
############################ Trend Analysis ############################ 
########################################################################

# DEPENDENT VARIABLE 1: ratings of intrinsic motivation #
# compute mean of intrinsic motivation for each group for each level of chance of success
df_mot %>% 
  group_by(cond, chance_success) %>% 
  summarise_all(mean)
# compute mean of intrinsic motivation for all groups for each level of chance of success
tapply(df_mot$rating, df_mot$chance_success, mean)

# define variable contrast: 
# examining the orthogonal linear and the quadratic effects of chance of success for each group. 
df_mot$contrast <- 
  # no-reward group: resulting in a decrease in intrinsic motivation as chance of success increases
  ifelse(df_mot$cond == "No-reward" & df_mot$chance_success == "extremely-low", 1,
         ifelse(df_mot$cond == "No-reward" & df_mot$chance_success == "moderate", 0,
                ifelse(df_mot$cond == "No-reward" & df_mot$chance_success == "high", -1, 
                       # reward group: resulting in a quadratic relationship between intrinsic motivation and chance of success
                       ifelse(df_mot$cond == "Reward" & df_mot$chance_success == "extremely-low", -1,
                              ifelse(df_mot$cond == "Reward" & df_mot$chance_success == "moderate", 2,
                                     ifelse(df_mot$cond == "Reward" & df_mot$chance_success == "high", -1, 
                                            # resulting in an increase in intrinsic motivation as chance of success increases in gambling group
                                            ifelse(df_mot$cond == "Gambling" & df_mot$chance_success == "extremely-low", -1,
                                                   ifelse(df_mot$cond == "Gambling" & df_mot$chance_success == "moderate", 0,
                                                          ifelse(df_mot$cond == "Gambling" & df_mot$chance_success == "high", 1, NA 
                                                          )))))))))

# compute LME with the defined contrast as predictor for intrinsic motivation across all groups
trend_mot <- lmer(rating ~ 1 + contrast + (1 + contrast | id), data = df_mot, REML = F)
summary(trend_mot)

# compute LME with the defined contrast as predictor for intrinsic motivation for each group individually
# no-reward
df_mot_c <- subset(df_mot, df_mot$cond == "No-reward")
trend_mot_c <- lmer(rating ~ 1 + contrast + (1 + contrast | id), data = df_mot_c, REML = F)
summary(trend_mot_c)
# reward
df_mot_r <- subset(df_mot, df_mot$cond == "Reward")
trend_mot_r <- lmer(rating ~ 1 + contrast + (1 + contrast | id), data = df_mot_r, REML = F)
summary(trend_mot_r)
# gambling
df_mot_g <- subset(df_mot, df_mot$cond == "Gambling")
trend_mot_g <- lmer(rating ~ 1 + contrast + (1 + contrast | id), data = df_mot_g, REML = F)
summary(trend_mot_g)


# DEPENDENT VARIABLE 2: ratings of rewarding value #
# compute mean of reward value for each group for each level of chance of success
df_val %>% 
  group_by(cond, chance_success) %>% 
  summarise_all(mean)
# compute mean of reward value for all groups for each level of chance of success
tapply(df_val$rating, df_val$chance_success, mean)

# define variable contrast: 
# examining the orthogonal linear effects  of chance of success for each group
df_val$contrast <- 
  # no-reward group: resulting in a decrease in rewarding value as chance of success increases
  ifelse(df_val$cond == "No-reward" & df_val$chance_success == "extremely-low", 1,
         ifelse(df_val$cond == "No-reward" & df_val$chance_success == "moderate", 0,
                ifelse(df_val$cond == "No-reward" & df_val$chance_success == "high", -1, 
                       # reward group: resulting in an increase in rewarding value as chance of success increases
                       ifelse(df_val$cond == "Reward" & df_val$chance_success == "extremely-low", -1,
                              ifelse(df_val$cond == "Reward" & df_val$chance_success == "moderate", 0,
                                     ifelse(df_val$cond == "Reward" & df_val$chance_success == "high", 1, 
                                            # gambling group: resulting in an increase in rewarding value as chance of success increases
                                            ifelse(df_val$cond == "Gambling" & df_val$chance_success == "extremely-low", -1,
                                                   ifelse(df_val$cond == "Gambling" & df_val$chance_success == "moderate", 0,
                                                          ifelse(df_val$cond == "Gambling" & df_val$chance_success == "high", 1, NA 
                                                          )))))))))

# compute LME with the defined contrast as predictor for rewarding value across all groups
trend_val <- lmer(rating ~ 1 + contrast + (1 + contrast | id), data = df_val, REML = F)
summary(trend_val)

# compute LME with the defined contrast as predictor for rewarding value for each group individually
# no-reward
df_val_c <- subset(df_val, df_val$cond == "No-reward")
trend_val_c <- lmer(rating ~ 1 + contrast + (1 + contrast | id), data = df_val_c, REML = F)
summary(trend_val_c)
# reward
df_val_r <- subset(df_val, df_val$cond == "Reward")
trend_val_r <- lmer(rating ~ 1 + contrast + (1 + contrast | id), data = df_val_r, REML = F)
summary(trend_val_r)
# gambling
df_val_g <- subset(df_val, df_val$cond == "Gambling")
trend_val_r <- lmer(rating ~ 1 + contrast + (1 + contrast | id), data = df_val_g, REML = F)
summary(trend_val_r)


#################################################################################  
############################ code to create Figure 2 ############################ 
#################################################################################  

# define variables used in the graph
varWithin <- "Chance of success" #fill
levelWithinReordered <- c("High", "Moderate", "Extremely-low")
groupNames <- c("No-reward", "Reward", "Gambling")
title <- c("(A) Intrinsic motivation", "(B) Rewarding value", "(C) Activation pattern at (9 5 -8) and (-9 5 -8)")
xLab <- "Experimental group"
yLab <- c("Rating", "Contrast estimate SW - WS")

titleSize <- 16
axisSize <- 12

## (A) INTRINSIC MOTIVATION ##
# get descriptes of the ratings of intrinsic motivation for each group
mot <- describeBy(df[, c( "mot_high", "mot_mod", "mot_low")], group=df$cond)
# combine the ratings for each group in a data frama
graph_mot <- as.data.frame(rbind(mot$`No-reward`, mot$Reward, mot$Gambling))
graph_mot$cond <- rep(groupNames, each = 3)
graph_mot$vars <- as.factor(graph_mot$vars)
levels(graph_mot$vars) <- levelWithinReordered

# use ggplot to create a bar graph including SE
outg_A <- ggplot(graph_mot, aes(cond, mean, fill = vars)) + theme_classic()
outg_A <- outg_A + geom_bar(stat="identity", position="dodge") + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(0.9)) + 
  scale_x_discrete(limits=groupNames) + labs(x=xLab, y=yLab[1], fill = varWithin, title = title[1]) +
  theme(axis.text=element_text(size=axisSize), axis.title=element_text(size=axisSize, face="bold"), title=element_text(size = titleSize, face="bold"), legend.title = element_text(size=axisSize), legend.text = element_text(size = axisSize)) + 
  coord_cartesian(ylim = c(1, 7)) + scale_fill_brewer(palette = 14)
outg_A
ggsave("Figure2A.jpeg")


## (B) REWRADING VALUE ##
# get descriptes of the ratings of rewarding value for each group
val <- describeBy(df[, c("val_high", "val_mod", "val_low")], group=df$cond)
# combine the ratings for each group in a data frama
outposgraphValue <- as.data.frame(rbind(val$`No-reward`, val$Reward, val$Gambling))
outposgraphValue$cond <-rep(groupNames, each = 3)
outposgraphValue$vars <- as.factor(outposgraphValue$vars)
levels(outposgraphValue$vars) <-  levelWithinReordered

# use ggplot to create a bar graph including SE
outg_B <- ggplot(outposgraphValue, aes(cond, mean, fill = vars)) + theme_classic()
outg_B <- outg_B + geom_bar(stat="identity", position="dodge") + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(0.9)) + 
  scale_x_discrete(limits=groupNames) + labs(x=xLab, y=yLab[1], fill = varWithin, title = title[2]) +
  theme(axis.text=element_text(size=axisSize), axis.title=element_text(size=axisSize, face="bold"), title=element_text(size = titleSize, face="bold"), legend.title = element_text(size=axisSize), legend.text = element_text(size = axisSize)) + 
  coord_cartesian(ylim = c(1, 7)) + scale_fill_brewer(palette = 14)
outg_B
ggsave("Figure2B.jpeg")

## (C) ACTIVATION PATTERN ##

# computing average between left and right peak voxel
df$avg_low <- (df$left_low + df$right_low)/2
df$avg_mod <- (df$left_mod + df$right_mod)/2
df$avg_high <- (df$left_high + df$right_high)/2

# creating data frame for plotting purposes
peak <- describeBy(df[, c("avg_high", "avg_mod", "avg_low" )], group=df$cond)

graph_peak <- as.data.frame(rbind(peak$`No-reward`, peak$Reward, peak$Gambling))
graph_peak$cond <- rep(groupNames, each = 3)

graph_peak$vars <- as.factor(graph_peak$vars)
levels(graph_peak$vars) <- levelWithinReordered

# plotting contrast estimates
outg_C <- ggplot(graph_peak, aes(cond, mean, fill = vars))  + theme_classic()
outg_C <- outg_C + geom_bar(stat="identity", position="dodge") + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(0.9)) +
  scale_x_discrete(limits=groupNames) + labs(x=xLab, y=yLab[2], fill = varWithin, title = title[3]) +
  theme(axis.text=element_text(size=axisSize), axis.title=element_text(size=axisSize, face="bold"), title=element_text(size = titleSize, face="bold"), legend.title = element_text(size=axisSize), legend.text = element_text(size = axisSize)) + 
  coord_cartesian(ylim = c(-1, 8)) + scale_fill_brewer(palette = 14)
outg_C
ggsave("Figure2C.jpeg")

