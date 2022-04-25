# Rstats
R stats for fly data 

#preparations
install.packages("readxl")
library(readxl)

install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)

#GRADIENT DATA - load data
dat <- read_excel('/Users/hd475/Documents/experiments/Heat shock/gradient_pilot.xlsx')
View(dat)

#stats number of clones (ratio)
dat = na.omit(dat)
compare_means(data=dat, Ratio~HS, method = "t.test", paired = FALSE)
compare_means(data=dat, Ratio~HS, method = "wilcox.test", paired = FALSE)

#progeny number
dat2 <- read_excel('/Users/hd475/Documents/experiments/Heat shock/progeny.xlsx')
View(dat2)

#stats for progeny number (EdU):

compare_means(data=dat2, progeny~HS, method = "t.test", paired = FALSE)

t.test(progeny ~ HS, data=dat2, var.equal=TRUE) #same results as in compare_means t.test, with var.equal it is a student's t-test

compare_means(data=dat2, progeny~HS, method = "wilcox.test", paired = FALSE)

#plot progeny number
ggplot(dat2, aes(y=progeny, x=HS, group=HS, fill=HS))+theme_classic()+theme(legend.position="none")+
  theme(axis.title.y = element_text(size = 12, face=2))+theme(axis.title.x = element_text(size = 12, face=2))+ scale_fill_brewer(palette='Set3')+
  labs(y="Progeny number", x="Heat shock (min)")+geom_boxplot()+geom_jitter()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#with stats
ggplot(dat2, aes(y=progeny, x=HS, group=HS, fill=HS))+theme_classic()+theme(legend.position="none")+ylim(0,15)+
  theme(axis.title.y = element_text(size = 12, face=2))+theme(axis.title.x = element_text(size = 12, face=2))+ scale_fill_brewer(palette='Set3')+
  labs(y="Progeny number EdU", x="Heat shock (min)")+geom_boxplot()+geom_jitter()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_compare_means(method='wilcox.test',  aes(label = ..p.signif..), label.x = 1.5, label.y = 14)


#AMOUNT OF CLONES
dat <- read_excel(file.choose())
#directly pick up file: 
dat <- read_excel('/Users/hd475/Documents/experiments/Heat shock/NB counting.xlsx')
View(dat)

#change order:
dat = na.omit(dat)
HSrelevel <- fct_relevel(dat$HS, 'none', '1 min', '5 min', '10 min', '20 min', '20-60-20', '45 min', '60 min', '90 min', '60-30-60', '120 min', '180 min')

#works with less comparisons (for graph)
my_comparisons3 <- list(c('none', '45 min'), c('none', '60 min'), c('none', '90 min'))
ggplot(dat, aes(y=Ratio, x=HSrelevel, group=HSrelevel, fill=HSrelevel))+theme_classic()+theme(legend.position="none")+
  theme(axis.title.y = element_text(size = 12, face=2))+theme(axis.title.x = element_text(size = 12, face=2))+ scale_fill_brewer(palette='Set3')+
  labs(y="Ratio Clones (%)", x="Heat shock (min)")+geom_boxplot()+geom_jitter()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_compare_means(comparison=my_comparisons3, method="t.test",  aes(label = ..p.signif..))

##REGRESSION

#stats: liner model
fit <- lm(Ratio ~ HSrelevel, data=dat)
summary(fit)
#for coefficients 
coef(fit)

#linear mixed effect regression with number of clones as random factor
library(nlme)
mod.lme <- lme(Ratio ~ HSrelevel, random = ~1|clones, data = dat)
mod.lme
anova(mod.lme)
#results: same as in ANOVA and also linear regression 
mod.nlme <- nlme(Ratio ~ HSrelevel, random = ~1|clones, data = dat) #might not work, at least one random factor is needed 

#to calculate the paired comparison of betas in regression 
install.packages("emmeans")
library(emmeans)

marginal = emmeans(mod.lme,
                   ~ HSrelevel)
pairs(marginal,
      adjust="bonferroni")
#same as in ANVOA (see below)

##ANOVA
res <- aov(Ratio ~ HSrelevel, data=dat)
summary(res)

library(car)
Anova(res, type=3) # same as summary(res)

#assumptions: 
#1. normal distribution 
shapiro.test(res$residuals)
# result W = 0.97385, p-value = 0.1177 = we do not reject H0 that the distribution is normally distributed = we can do ANVOA!

#2. normality of residuals: 
leveneTest(Ratio ~ HSrelevel,
           data = dat
)
# result:     Df F value Pr(>F)
#       group 12  1.4115 0.1845
#             63                = we do not reject H0 that the residuals are normally distributed = we can do ANOVA

#3. indepentend smaples = yes, all independent brains

#signfifcant = multiple comparison
#Bonferroni
pairwise.t.test(dat$Ratio, HSrelevel, p.adj="bonferroni")
post_t.test <- glht(res,
                  linfct = mcp(HSrelevel = "bonferroni")
)

summary(post_t.test)

#TukeyHSD: compare all groups = decrease in power
TukeyHSD(res)

post_test <- glht(res,
                  linfct = mcp(HSrelevel = "Tukey")
)

summary(post_test)

#Dunnett's test: comparisons within a reference group = increase in power
library(multcomp)
post_test <- glht(res,
                  linfct = mcp(HSrelevel = "Dunnett")
)
summary(post_test)
# 'none' is the reference category because we used the releveled HS variable 
# significant in none vs 45, 60, 90, 60-30-60, 120, 180

#add in graph
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparison4 <- list(c('none', '45 min'), c('none', '60 min'), c('none', '90 min'), c('none', '60-30-60'), c('none', '120'), c('none', '180'))
my_comparison5 <- list(c('none', '60 min'), c('none', '90 min'), c('none', '60-30-60'), c('none', '120'), c('none', '180'))
my_comparison6 <- list(c('none', '60 min'), c('none', '90 min'))


#not enough y observations for t-test, only works with 2 as in my_comparison6
ggplot(dat, aes(y=Ratio, x=HSrelevel, group=HSrelevel, fill=HSrelevel))+theme_classic()+theme(legend.position="none")+
  theme(axis.title.y = element_text(size = 12, face=2))+theme(axis.title.x = element_text(size = 12, face=2))+ 
  scale_fill_brewer(palette='Set3')+
  labs(y="Ratio Clones (%)", x="Heat shock (min)")+
  geom_boxplot()+geom_jitter()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_compare_means(aes(label = paste0(..method.., ", p-value = ", ..p.format..)),
                     method = method1, label.y = max(dat[, i], na.rm = TRUE)
  )+ stat_compare_means(comparisons = my_comparison6, method = method2, label = "p.format")
  
##NON PARAMETRIC

# Kruskal Wallis (rank sum test) as non-parametric alternative to ANOVA (with non paired data)- not needed here
kruskal.test(Ratio ~ HSrelevel, data = dat)

#for follow up pairwise comparison non-parametric
pwc <- dat %>%
  wilcox_test(Ratio ~ HS, paired = FALSE, p.adjust.method = "bonferroni")
View(pwc)
