setwd("/Users/manu/Documents/RProject/SourceBias")
options(scipen=999)
options(digits= 3)


##Simple LME: checking different models
library(lme4)
library(lmerTest)
library(ggplot2)
library(pastecs)
library(MuMIn)
library(glmmTMB)
library(afex)
library(lsmeans)
library(lattice)
library(multcomp)
library(GPArotation)
library(psych)
library(corpcor)
library(ggpubr)
library(psych)
library(dplyr)

#check consumers data and clean
IDsd<- aggregate(dataCONS[,c("Like","Mquality","Authentic","Fit","Agree1","Agree2","Agree3")],by=list(dataCONS$subject),sd,na.rm=TRUE)
IDnumberofANDs<- aggregate(dataCONS[,c("ANDnumber")],by=list(dataCONS$subject),sum,na.rm=TRUE)

#correlation matrix
##visualizing the correlation matrix
library(corrplot)
res.prof<- cor(dataPROF[21:25], use="complete.obs")
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

res.cons<- cor(dataCONS[21:25], use="complete.obs")
res.both<- cor(dataBOTH[23:27], use="complete.obs")


#PCA
subsetPCA<- dataBOTH[c(21:25)]
subsetPCAMatrix<-cor(subsetPCA) #calculate correlation matrix
round(subsetPCAMatrix,2)# display matrix round to 2 decimals
cortest.bartlett(subsetPCA) #Bartlett test
cortest.bartlett(subsetPCAMatrix, n= 1404) #Bartlett test from correlation Matrix
KMO(subsetPCA) #KMO test
det(subsetPCAMatrix) #determinant of the correlation matrix
pc1<- principal(subsetPCA,nfactors=5,rotate="none")
## repeat only with 4 items and 1 factor and generate scores
subsetPCA4items<-dataBOTH[c(21:24)]
cortest.bartlett(subsetPCA4items)
KMO(subsetPCA4items)
pc2<- principal(subsetPCA4items,nfactors=1,rotate="none",scores=TRUE)
dataBOTHPCA<- cbind(dataBOTH, pc2$scores)
sink("PCAscores.txt")
print(pc2$scores)
sink()

#reliability analysis
items.prof <- select(dataPROF, 21,22,23,24,25)
alpha(items.prof)

items.cons <- select(dataCONS, 21,22,23,24,25)
alpha(items.cons)

items.cons2 <- select(dataCONS, 27,28,29)
alpha(items.cons2)

items.both <- select(dataBOTH, 23,24,25,26,27)
alpha(items.both)
# setting up for an ANOVA with effects coding
library(car)
groupBOTH<-as.factor(dataBOTH$Group)
sourceBOTH<-as.factor(dataBOTH$Origin)
subjectBOTH<-as.factor(dataBOTH$subject)
trackBOTH<- as.factor(dataBOTH$song)

#Model with source and group
contrasts(groupBOTH)<-contr.sum
contrasts(sourceBOTH)<-contr.sum
GeneralModel <- lmer(PCA ~ groupBOTH*sourceBOTH+(1|subjectBOTH)+(1|trackBOTH)+(1|BrandGroup),data=dataBOTH)
Anova(GeneralModel,type="III")
r.squaredGLMM(GeneralModel)

#Proessionals
groupPROF<-as.factor(dataPROF$Group)
sourcePROF<-as.factor(dataPROF$Origin)
subjectPROF<-as.factor(dataPROF$subject)
trackPROF<- as.factor(dataPROF$song)
PCAscore<- as.numeric(dataPROF$PCA)
contrasts(sourcePROF)<-contr.sum
#Like
m2PROF <- lmer(PCAscore ~ sourcePROF+(1|subjectPROF)+(1|trackPROF)+(1|BrandGroup),data=dataPROF)
Anova(m2PROF,type="III")

m3PROF <- lmer(PCAscore ~ sourcePROF*BrandGroup+(1|subjectPROF)+(1|trackPROF),data=dataPROF)
Anova(m3PROF,type="III")

summary(m2PROF, split=list( cond=list(AvB=1, CvAB=2, CtrlvElse=3),dose=list(Linear=1, Quad=2, Cubic=3)))
a= step(m2PROF)
plot(a)
r.squaredGLMM(m2PROF)
#if I want to use post hoc bonferroni
postPROF <- glht(m2PROF, lsm(pairwise ~ sourcePROF))
Holmpost = summary(postPROF, test=adjusted("holm"))
#Money professionals
m2moPROF <- lmer(Money ~ sourcePROF+(1|subjectPROF)+(1|trackPROF)+(1|BrandGroup),data=dataPROF)
Anova(m2moPROF,type="III")
summary(m2moPROF)
b=step(m2moPROF)
plot(b)
r.squaredGLMM(m2moPROF)
#Consumers
groupCONS<-as.factor(dataCONS$Group)
sourceCONS<-as.factor(dataCONS$Origin)
subjectCONS<-as.factor(dataCONS$subject)
trackCONS<- as.factor(dataCONS$song)
PCAscores<- as.numeric(dataCONS$PCA)
contrasts(sourceCONS)<-contr.sum
##PCA
m2CONS <- lmer(PCA ~ sourceCONS+(1|subjectCONS)+(1|trackCONS)+(1|BrandGroup),data=dataCONS)
Anova(m2CONS,type="III")
summary(m2CONS)
r.squaredGLMM(m2CONS)

m2CONS <- lmer(PCA ~ sourceCONS*BrandGroup+(1|subjectCONS)+(1|trackCONS),data=dataCONS)
Anova(m2CONS,type="III")
##Money
m2moCONS <- lmer(Money ~ sourceCONS+(1|subjectCONS)+(1|trackCONS)+(1|BrandGroup),data=dataCONS)
Anova(m2moCONS,type="III")
summary(m2av4CONS)
r.squaredGLMM(m2moCONS)
#agreeeeees
m.agree1 <- lmer(Agree1 ~ sourceCONS+(1|subjectCONS)+(1|trackCONS),data=dataCONS)
m.agree2 <- lmer(Agree2 ~ sourceCONS+(1|subjectCONS)+(1|trackCONS),data=dataCONS)
m.agree3 <- lmer(Agree3 ~ sourceCONS+(1|subjectCONS)+(1|trackCONS),data=dataCONS)

Anova(m.agree1,type="III")
summary(m2av4CONS)
r.squaredGLMM(m2moCONS)


#graphs - line
lineMoney1<-ggplot(X1RareCasesEliminated,aes(Origin,Money, colour=Group))
lineMoney1 + stat_summary(fun.y=mean,geom="point")+stat_summary(fun.y=mean,geom="line", aes(group=Group))+stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2)

lineAV41<-ggplot(X1RareCasesEliminated,aes(Origin,AV4, colour=Group))
lineAV41 + stat_summary(fun.y=mean,geom="point")+stat_summary(fun.y=mean,geom="line", aes(group=Group))+stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2)

h1<-ggplot(dataPROF, aes(AV4)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "xxx", y = "Density")
h1 + stat_function(fun = dnorm, args = list(mean = mean(dataPROF$AV4, na.rm = TRUE), sd = sd(dataPROF$AV4, na.rm = TRUE)), colour = "red", size = 1)

#EXPLORATION
groupBOTH<-as.factor(dataBOTH$Group)
sourceBOTH<-as.factor(dataBOTH$Origin)
subjectBOTH<-as.factor(dataBOTH$subject)
trackBOTH<- as.factor(dataBOTH$song)

#Model with source and group
contrasts(groupBOTH)<-contr.sum
contrasts(sourceBOTH)<-contr.sum
contrasts(trackBOTH)<-contr.sum
m2explorePCA <- lmer(PCA ~ groupBOTH*trackBOTH+(1|subjectBOTH),data=dataBOTH) #best fit
Anova(m2explorePCA,type="III")
r.squaredGLMM(m2explorePCA)

m2exploreMoney <- lmer(Money ~ groupBOTH*trackBOTH+(1|subjectBOTH),data=dataBOTH) #best fit
Anova(m2exploreMoney,type="III")
r.squaredGLMM(m2exploreMoney)

#PLOT BOTH
plotBOTHaesthetic= summarySE(dataBOTH,measurevar="Aesthetic",groupvars=c("Origin","Group"))
Aesthetic.plot= ggplot(plotBOTHaesthetic, aes(x=Origin, y=Aesthetic, group = Group, shape=Group, linetype=Group))+ 
    geom_errorbar(aes(ymin=Aesthetic-se, ymax=Aesthetic+se), width=.1, 
                  position=position_dodge(0.005)) +
    geom_line() +
    geom_point()+
    ylim(-1,1)+
    labs(x="Music Source", y = "Aesthetic Evaluation (liking, quality, authenticity, fit)")+
    theme_classic()

plotBOTHmoney= summarySE(dataBOTH,measurevar="Money",groupvars=c("MusicSource","Group"))
Money.plot= ggplot(plotBOTHmoney, aes(x=MusicSource, y=Money, group = Group, shape=Group, linetype=Group))+ 
    geom_errorbar(aes(ymin=Money-se, ymax=Money+se), width=.1, 
                  position=position_dodge(0.005)) +
    geom_line() +
    geom_point()+
    ylim(1,7)+
    labs(x="Music Source", y = "Expected Cost")+
    theme_classic()

#plots with tracks and no source
plotExploreAesthetic= summarySE(dataBOTH,measurevar="Aesthetic",groupvars=c("song","Group"))
Aesthetic.plot= ggplot(plotExploreAesthetic, aes(x=song, y=Aesthetic, group = Group, shape=Group, linetype=Group))+ 
    geom_errorbar(aes(ymin=Aesthetic-se, ymax=Aesthetic+se), width=.1, 
                  position=position_dodge(0.005)) +
    geom_line() +
    geom_point()+
    ylim(-1,1)+
    labs(x="Music Excerpt", y = "Aesthetic Evaluation (liking, quality, authenticity, fit)")+
    theme_classic()
Aesthetic.plot

plottingMoney<- summarySE(dataBOTH,measurevar="Money",groupvars=c("groupBOTH","trackBOTH"))
Money.plot= ggplot(plottingMoney, aes(x=trackBOTH, y=Money, group = groupBOTH, shape=groupBOTH, linetype=groupBOTH))+ 
    geom_errorbar(aes(ymin=Money-se, ymax=Money+se), width=.1, 
                  position=position_dodge(0.005)) +
    geom_line() +
    geom_point()+
    ylim(1,6)+
    ggtitle("Professionals vs. Consumers)") +
    labs(x="Music Track", y = "Expected Cost")+
    theme_classic()

ggarrange(PCA.plot,Money.plot, ncol=1, nrow=2)

#BARPLOTS
##Figure 1: music source effects
a <- ggplot(data=plotBOTHaesthetic, aes(x=Origin, y=Aesthetic, fill=Group)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
    geom_errorbar(aes(ymin=Aesthetic-se, ymax=Aesthetic+se), width=.2,
                  position=position_dodge(.9)) +
    ylim(-1,1) +
    labs(x="Music Source", y = "Aesthetic Evaluation (z-scores)")+
    theme_classic()
a + scale_fill_brewer(palette="Greys")

##Figure 2: differences between groups in the nine music tracks
track= as.factor(plotExploreAesthetic$song)
b <- ggplot(data=plotExploreAesthetic, aes(x=track, y=Aesthetic, fill=Group)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
    geom_errorbar(aes(ymin=Aesthetic-se, ymax=Aesthetic+se), width=.2,
                  position=position_dodge(.9)) +
    ylim(-1,1) +
    labs(x="Music Excerpt", y = "Aesthetic Evaluation (z-scores)")+
    theme_classic()
ae.plot <- b + scale_fill_brewer(palette="Greys")

trackm= as.factor(plottingMoney$song)
c <- ggplot(data=plottingMoney, aes(x=trackm, y=Money, fill=Group)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
    geom_errorbar(aes(ymin=Money-se, ymax=Money+se), width=.2,
                  position=position_dodge(.9)) +
    labs(x="Music Excerpt", y = "Expected Cost")+
    theme_classic()
money.plot <- c + scale_fill_brewer(palette="Greys")  + coord_cartesian(ylim=c(1,7))

library(ggpubr)
ggarrange(ae.plot,money.plot, ncol=1, nrow=2)



#Final question on awarness:

by(dataBOTH$EffectLabel, dataBOTH$Group, stat.desc, basic = FALSE, norm = TRUE) 
ind.t.test<- t.test(EffectLabel ~ Group, data = dataBOTH, paired = FALSE)
t<-ind.t.test$statistic[[1]] #create a variable that contains the value of t
df <- ind.t.test$parameter[[1]] #create a variable that contains the degrees of freedom
r <- sqrt(t^2/(t^2+df)) #calculate effect size
r

install.packages("devtools")
library(devtools)
install_github("kassambara/easyGgplot2")
library(easyGgplot2)
library(plyr)
library(plyr);
library(dplyr)

by(dataBOTH$EffectLabel, dataBOTH$Group, stat.desc, basic = FALSE, norm = TRUE) 
Indep.t.test<-t.test(EffectLabel ~ Group, data = dataBOTH, paired = FALSE)
Indep.t.test

t<-Indep.t.test$statistic[[1]] #create a variable that contains the value of t
df <- Indep.t.test$parameter[[1]] #create a variable that contains the degrees of freedom 
r <- sqrt(t^2/(t^2+df)) #calculate effect size
r

mu <- ddply(dataBOTH, "Group", summarise, grp.mean=mean(EffectLabel, na.rm=TRUE))

p <- ggplot(dataBOTH) + geom_density(aes(x = EffectLabel, fill = Group), alpha = 0.2) +
    geom_vline(data=mu, aes(xintercept=grp.mean, color=Group),  linetype="dashed")
p

#boxplot
p <- ggplot(dataBOTH, aes(x=Group, y=EffectLabel, fill= Group)) + 
        scale_fill_grey(start=0.8, end=0.5) +
        geom_boxplot() 
p

p + theme(axis.text.x = element_text(size=14)) +
    scale_x_discrete(name = "", labels=c("Consumers" = "Non-professionals", "Professioanls" = "Professionals")) + 
    scale_y_continuous(name = "Awareness of source effects",
                       breaks= c(1,2,3,4,5,6)) +
    theme_classic() + 
    theme(axis.text=element_text(size=14),axis.title=element_text(size=12)) +
    theme(legend.position = "none")
    
    


