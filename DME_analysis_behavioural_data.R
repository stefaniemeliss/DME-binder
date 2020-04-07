# this r script summarises the behavioural analysis and creates the plots for the publication
rm(list = ls())

#### READ IN DATA SET ####

setwd("~/Dropbox/Reading/DME_2014/Behavior")

library(ggplot2)
library(car)
library(psych)
library(ggm)
library(reshape2)
source("anovakun_482.txt")



df <- read.table("DME_behavioural_activation.txt",  header = T)

# fMRI == 1: These are the 51 participants in the final fMRI sample
df <- df[df$fMRI == 1,]

#recode cond
df$cond <- ifelse(df$cond == "C", "No-reward",
                  ifelse(df$cond == "R", "Reward",
                         ifelse(df$cond == "G", "Gambling", NA)))

#ordering data in the same way as in SPM
df$orderedcond <- rep(c(1, 3, 2), each =17)
df <- df[with(df, order(df$orderedcond, df$scan)),]
df$orderedcond <- as.factor(df$orderedcond)
df$orderedcond <- recode(df$orderedcond, " 1 = 'No-reward'; 2 = 'Reward'; 3 = 'Gambling' ")

# a1-a10: 10 questions after session 1 (asked inside the scanner)
# b1-b10: same 10 questions after session 2 (asked inside the scanner)
# c1-c10: combined ratings for both sessions
# 1. I am glad to know that the next one is easy
# 2. I am disappointed to know that the next one is easy --> used to create c2r
# 3. I am glad to know that the next one is moderately difficult
# 4. I am disappointed to know that the next one is moderately difficult --> used to create c4r
# 5. I am glad to know that the next one is very difficult
# 6. I am disappointed to know that the next one is very difficult --> used to create c6r
# 7. I am glad to know that the next one is a watchstop trial
# 8. I am disappointed to know that the next one is a watchstop trial --> used to create c8r
# 9. I understand the rule of the experiment
# 10. I am satisfied with the results so far

# POST QUESTIONS
# intrinsic motivation after the scanning
#1.	It was fun to do the easy task
#2.	It was boring to do the easy task
#3.	It was enjoyable to do the easy task
#4.	It was fun to do the moderately difficult task
#5.	It was boring to do the moderately difficult task
#6.	It was enjoyable to do the moderately difficult task
#7.	It was fun to do the very difficult task
#8.	It was boring to do the very difficult task
#9.	It was enjoyable to do the very difficult task
#10.	It was fun to do the watchstop task
#11.	It was boring to do the watchstop task
#12.	It was enjoyable to do the watchstop task
# difficulty
#13.	The easy task was difficult
#14.	The moderately difficult task was difficult
#15.	The very difficult task was difficult
# Others 
#16.	The experiment was sleepy
#17.	I concentrated on the experiment
#18.	I was unable to focus on the experiment
#19.	To be honest, I was totally demotivated
#20.	I really did not like the experiment
# post happiness
#21.	I felt happy when I see the cue of the easy task
#23.	I felt happy when I see the cue of the moderately difficult task
#25.	I felt happy when I see the cue of the very difficult task
#27   I felt happy when I see the cue of the watchstop task
# post dissappointment
#22.	I got dissappointed when I see the cue of the easy task
#24.	I got dissappointed when I see the cue of the moderately difficult task
#26.	I got dissappointed when I see the cue of the very difficult task
#28.	I got dissappointed when I see the cue of the watchstop task
# calculate rewarding value: recode disappointment items and combine them with happiness
df$post22_r <- car::recode(df$post22, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1" )
df$post24_r <- car::recode(df$post24, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1" )
df$post26_r <- car::recode(df$post26, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1" )
df$post28_r <- car::recode(df$post28, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1" )
df$rewardvalue_e <- (df$post21 + df$post22_r)/2
df$rewardvalue_m <- (df$post23 + df$post24_r)/2
df$rewardvalue_h <- (df$post25 + df$post26_r)/2
df$rewardvalue_ws <- (df$post27 + df$post28_r)/2
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
# sleepr: How long do you regularly sleep?
# sleepd: How long did you sleep last night
# Easy/Mid/Dif/IAT: forget about them
# Score: Total score that participants obtained

#### 3 (group: no-reward, reward, or gambling) x 3 (chance of success: high chance, moderate chance, or extremely-low chance) mixed ANOVA ####

# intrinsic motivation
dfPos_im <- melt(df, id.vars = c("id", "cond"), measure.vars = c("pos_im_h", "pos_im_m", "pos_im_e") )
names(dfPos_im) <- c("id", "cond", "measurement", "rating")
dfPos_im$chance_success <- gl(3,51,153, label = c("extremely-low", "moderate", "high"))
#anovakun
Pos_imanovakun <- dfPos_im
Pos_imanovakun$measurement <- NULL
Pos_imanovakun <- Pos_imanovakun[,c("id", "cond", "chance_success", "rating")]
anovakun(Pos_imanovakun, "AsB", 3, 3, long = T, geta = T)

# rewarding value
dfRewardvalue <- melt(df, id.vars = c("id", "cond"), measure.vars = c("rewardvalue_h", "rewardvalue_m", "rewardvalue_e") )
names(dfRewardvalue) <- c("id", "cond", "measurement", "rating")
dfRewardvalue$chance_success <- gl(3,51,153, label = c("extremely-low", "moderate", "high"))
#anovakun
Rewardvalueanovakun <- dfRewardvalue
Rewardvalueanovakun$measurement <- NULL
Rewardvalueanovakun <- Rewardvalueanovakun[,c("id", "cond", "chance_success", "rating")]
anovakun(Rewardvalueanovakun, "AsB", 3, 3, long = T, geta = T)

##### trend analysis ##### 
library(lme4)
library(lmerTest)
library(dplyr)

# intrinsic motivation
dfPos_im %>% 
  group_by(cond, chance_success) %>% 
  summarise_all(mean)
tapply(dfPos_im$rating, dfPos_im$chance_success, mean)

dfPos_im$contrast <- ifelse(dfPos_im$cond == "No-reward" & dfPos_im$chance_success == "extremely-low", 1,
                            ifelse(dfPos_im$cond == "No-reward" & dfPos_im$chance_success == "moderate", 0,
                                   ifelse(dfPos_im$cond == "No-reward" & dfPos_im$chance_success == "high", -1, 
                                          ifelse(dfPos_im$cond == "Reward" & dfPos_im$chance_success == "extremely-low", -1,
                                                 ifelse(dfPos_im$cond == "Reward" & dfPos_im$chance_success == "moderate", 2,
                                                        ifelse(dfPos_im$cond == "Reward" & dfPos_im$chance_success == "high", -1, 
                                                               ifelse(dfPos_im$cond == "Gambling" & dfPos_im$chance_success == "extremely-low", -1,
                                                                      ifelse(dfPos_im$cond == "Gambling" & dfPos_im$chance_success == "moderate", 0,
                                                                             ifelse(dfPos_im$cond == "Gambling" & dfPos_im$chance_success == "high", 1, NA 
                                                                             )))))))))


trendIntMot <- lmer(rating ~ 1 + contrast + (1 + contrast | id), data = dfPos_im, REML = F)
summary(trendIntMot)

# reward value
dfRewardvalue %>% 
  group_by(cond, chance_success) %>% 
  summarise_all(mean)
tapply(dfRewardvalue$rating, dfRewardvalue$chance_success, mean)

dfRewardvalue$contrast <- ifelse(dfRewardvalue$cond == "No-reward" & dfRewardvalue$chance_success == "extremely-low", 1,
                            ifelse(dfRewardvalue$cond == "No-reward" & dfRewardvalue$chance_success == "moderate", 0,
                                   ifelse(dfRewardvalue$cond == "No-reward" & dfRewardvalue$chance_success == "high", -1, 
                                          ifelse(dfRewardvalue$cond == "Reward" & dfRewardvalue$chance_success == "extremely-low", -1,
                                                 ifelse(dfRewardvalue$cond == "Reward" & dfRewardvalue$chance_success == "moderate", 0,
                                                        ifelse(dfRewardvalue$cond == "Reward" & dfRewardvalue$chance_success == "high", 1, 
                                                               ifelse(dfRewardvalue$cond == "Gambling" & dfRewardvalue$chance_success == "extremely-low", -1,
                                                                      ifelse(dfRewardvalue$cond == "Gambling" & dfRewardvalue$chance_success == "moderate", 0,
                                                                             ifelse(dfRewardvalue$cond == "Gambling" & dfRewardvalue$chance_success == "high", 1, NA 
                                                                             )))))))))


trendRewardValue <- lmer(rating ~ 1 + contrast + (1 + contrast | id), data = dfRewardvalue, REML = F)
summary(trendRewardValue)

#### code to create Figure 2 ####
setwd("/Users/stefaniemeliss/Dropbox/Reading/DME_2014/Publication/paper/")

varWithin <- "Chance of success"
levelWithin <- c("Extremely-low","Moderate","High")
levelWithinReordered <- c("High", "Moderate", "Extremely-low")
groupNames <- c("No-reward", "Reward", "Gambling")
xLab <- "Experimental group"
title <- c("(A) Intrinsic motivation", "(B) Rewarding value", "(C) Activation pattern at (9 5 -8) and (-9 5 -8)")
yLab <- c("Rating", "Contrast estimate SW - WS")

## (A) INTRINSIC MOTIVATION
outposIntMot <- describeBy(df[, c( "pos_im_e", "pos_im_m", "pos_im_h")], group=df$cond)

outposgraphIntMot <- as.data.frame(rbind(outposIntMot$`No-reward`, outposIntMot$Reward, outposIntMot$Gambling))
outposgraphIntMot$cond <- rep(groupNames, each = 3)
outposgraphIntMot$vars <- as.factor(outposgraphIntMot$vars)
levels(outposgraphIntMot$vars) <- levelWithinReordered



outg_A <- ggplot(outposgraphIntMot, aes(cond, mean, fill = vars)) + theme_classic()
outg_A <- outg_A + geom_bar(stat="identity", position="dodge") + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(0.9)) + 
  scale_x_discrete(limits=groupNames) + labs(x=xLab, y=yLab[1], fill = varWithin, title = title[1]) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), title=element_text(size =20, face="bold"), legend.title = element_text(size=20), legend.text = element_text(size = 20)) + coord_cartesian(ylim = c(1, 7)) + scale_fill_brewer(palette = 14)
outg_A
ggsave("Figure2A.jpeg")


## (B) REWRADING VALUE
outposValue <- describeBy(df[, c("rewardvalue_e", "rewardvalue_m", "rewardvalue_h")], group=df$cond)

outposgraphValue <- as.data.frame(rbind(outposValue$`No-reward`, outposValue$Reward, outposValue$Gambling))
outposgraphValue$cond <-rep(groupNames, each = 3)
outposgraphValue$vars <- as.factor(outposgraphValue$vars)
levels(outposgraphValue$vars) <-  levelWithinReordered

outg_B <- ggplot(outposgraphValue, aes(cond, mean, fill = vars)) + theme_classic()
outg_B <- outg_B + geom_bar(stat="identity", position="dodge") + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(0.9)) + 
  scale_x_discrete(limits=groupNames) + labs(x=xLab, y=yLab[1], fill = varWithin, title = title[2]) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), title=element_text(size =20, face="bold"), legend.title = element_text(size=20), legend.text = element_text(size = 20)) + coord_cartesian(ylim = c(1, 7)) + scale_fill_brewer(palette = 14)
outg_B
ggsave("Figure2B.jpeg")

## (C) ACTIVATION PATTERN

# computing average between left and right peak voxel
df$avgContrastEstimate_D <- (df$leftContrastEstimate_D + df$rightContrastEstimate_D)/2
df$avgContrastEstimate_M <- (df$leftContrastEstimate_M + df$rightContrastEstimate_M)/2
df$avgContrastEstimate_E <- (df$leftContrastEstimate_E + df$rightContrastEstimate_E)/2

# creating data frame for plotting purposes
outposAvgContrastEstimate <- describeBy(df[, c("avgContrastEstimate_E", "avgContrastEstimate_M", "avgContrastEstimate_D" )], group=df$cond)

outposgraphAvgContrastEstimate <- as.data.frame(rbind(outposAvgContrastEstimate$`No-reward`, outposAvgContrastEstimate$Reward, outposAvgContrastEstimate$Gambling))
outposgraphAvgContrastEstimate$cond <- rep(groupNames, each = 3)

outposgraphAvgContrastEstimate$vars <- as.factor(outposgraphAvgContrastEstimate$vars)
levels(outposgraphAvgContrastEstimate$vars) <- levelWithinReordered

# plotting contrast estimates
outg_C <- ggplot(outposgraphAvgContrastEstimate, aes(cond, mean, fill = vars))  + theme_classic()
outg_C <- outg_C + geom_bar(stat="identity", position="dodge") + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(0.9)) +
 scale_x_discrete(limits=groupNames) + labs(x=xLab, y=yLab[2], fill = varWithin, title = title[3]) +
 theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), title=element_text(size =20, face="bold"), legend.title = element_text(size=20), legend.text = element_text(size = 20)) + coord_cartesian(ylim = c(-1, 10)) + scale_fill_brewer(palette = 14)
outg_C
ggsave("Figure2C.jpeg")

gridExtra::grid.arrange(outg_A, outg_B, outg_C, ncol=2)











