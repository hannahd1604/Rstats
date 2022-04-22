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
  
#continue line 82 of 'analysis clones'
