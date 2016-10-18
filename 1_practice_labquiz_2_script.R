library(tidyverse)
library(apaTables)
library(haven)
library(dplyr)
#NEVER!!! LOAD!!! library(psych)

#Load data
raw_data <- read_csv(file="raw_data.csv")

#Fix data
raw_data <- read_csv(file="raw_data.csv",na=c("","NA","-999"))

#Labelling categorical data 
categorical_variables <- select(raw_data,sex)
categorical_variables$sex <- as.factor(categorical_variables$sex)
levels(categorical_variables$sex) <- list("Male"=1,"Female"=2)
sex <- categorical_variables$sex

#Creating scales 
na_affect_items <- select(raw_data,afraid,angry,anxious,ashamed)
pa_affect_items <- select(raw_data,delighted,elated,enthusiastic,excited)
Neuroticism <- select(raw_data,Neuroticism)
Extraversion <- select(raw_data,Extraversion)
#don't need to create items for N and E b/c only one column. 
##i.e., one score per participant

#Checking for out of range values
psych::describe(na_affect_items)
#na affect range = 0-3
psych::describe(pa_affect_items)
#pa affect range = 0-3
psych::describe(Neuroticism)
#Neuro range = 0-24
psych::describe(Extraversion)
#extra range = 0-24

#Correcting out of range values
is_bad_value <- na_affect_items<0 | na_affect_items>3
na_affect_items[is_bad_value]<-NA 
is_bad_value <- pa_affect_items<0 | pos_affect_items>3
pa_affect_items[is_bad_value] <- NA
is_bad_value <- Neuroticism<0 | Neuroticism>24
Neuroticism[is_bad_value] <- NA
is_bad_value <- Extraversion<0 | Extraversion>24
Extraversion[is_bad_value] <- NA

#Calculating scores
##Check keys = FALSE disables automatic re-keying since you've already manually corrected bad values
neg_affect <- psych::alpha(as.data.frame(na_affect_items),check.keys=FALSE)$scores
pos_affect <- psych::alpha(as.data.frame(pa_affect_items),check.keys=FALSE)$scores

#Creating analytic data sets
##Select option is used to select only needed columns
##Alternative 1: to select wanted variables is to simply -sex
##e.g., analytic_data_male <- select(analytic_data_male, -sex)
##Alternative 2: analytic_data_male <- analytic_data %>% filter(sex=="Male") %>% select(-sex)
##Clarification: %>% means "and then" i.e., next step 
analytic_data <- cbind(categorical_variables,pos_affect,neg_affect,Neuroticism,Extraversion)
analytic_data_male <- filter(analytic_data,sex=="Male")
analytic_data_male <- select(analytic_data_male,pos_affect,neg_affect,Neuroticism,Extraversion)
analytic_data_female <- filter(analytic_data,sex=="Female")
analytic_data_female <- select(analytic_data_female,pos_affect,neg_affect,Neuroticism,Extraversion)

#Save data sets
write_csv(analytic_data,path="analytic_data.csv")
write_csv(analytic_data_male,path="analytic_data_male.csv")
write_csv(analytic_data_female,path="analytic_data_female.csv")

#Creating APA style correlation tables 
apa.cor.table(analytic_data,filename="Table_1_Overall.doc",table.number=1)
apa.cor.table(analytic_data_male,filename="Table_2_Male.doc",table.number=2)
apa.cor.table(analytic_data_female,filename="Table_3_Female.doc",table.number=3)

#Creating correlation graphs 
##Save figures as .tiff by exporting as image and selecting TIFF
##BETTER WAY: psych::pairs.panels(analytic_data, lm="TRUE") so it uses smooth lines instead of curved lines
psych::pairs.panels(analytic_data)
psych::pairs.panels(analytic_data_male)
psych::pairs.panels(analytic_data_female)

#Creating Neuroticism score histogram for Females
##Removing binwidth will get R to set default bin width 
my.hist <- ggplot(analytic_data_female,aes(Neuroticism))
my.hist <- my.hist + geom_histogram(aes(y=..count..),binwidth = 0.75,fill="black",color="black")
my.hist <- my.hist + labs(title="Neuroticism Histogram",x="Neuroticism",y="Frequency")
my.hist <- my.hist + coord_cartesian(xlim=c(0,25), ylim = c(0,1200))
my.hist <- my.hist + theme_classic()
my.hist <- my.hist + theme(axis.line.x = element_line(colour = "black", size=0.5,linetype = 'solid'),
                           axis.line.y=element_line(colour = "black", size=0.5, linetype='solid'))
my.hist <- my.hist + scale_x_continuous( breaks = seq(0,25,by=5) )
my.hist <- my.hist + scale_y_continuous( breaks = seq(0,1200,by=300),expand=c(0,0))
print(my.hist)
ggsave("Figure_4_Neuroticism_Histogram_Female.tiff", plot=my.hist, width=6,height=6)

#Creating Negative Affect score histogram for Females
my.hist.neg <- ggplot(analytic_data_female,aes(neg_affect))
my.hist.neg <- my.hist.neg + geom_histogram(aes(y=..count..),binwidth = 0.10,fill="black",color="black")
my.hist.neg <- my.hist.neg + labs(title="Negative Affect Histogram",x="Negative Affect",y="Frequency")
my.hist <- my.hist + coord_cartesian(xlim=c(0,3), ylim = c(0,1600))
my.hist.neg <- my.hist.neg + theme_classic()
my.hist.neg <- my.hist.neg + theme(axis.line.x = element_line(colour = "black", size=0.5,linetype = 'solid'),
                                   axis.line.y=element_line(colour = "black", size=0.5, linetype='solid'))
my.hist.neg <- my.hist.neg + scale_x_continuous (breaks = seq(0,3,by=1))
my.hist.neg <- my.hist.neg + scale_y_continuous (breaks = seq(0,1600,by=300), expand=c(0,0))
print(my.hist.neg)
ggsave(filename="Figure_5_Negative-Affect_Histogram_Female.tiff", plot=my.hist.neg, width = 6,height = 6)
                                                
#Creating scatterplot of negative affect against neuroticism scores for females 
my.plot.neg.neur <- qplot(neg_affect,Neuroticism,data=analytic_data_female)
my.plot.neg.neur <- my.plot.neg.neur + geom_smooth(method = "lm", se = FALSE, color='black')
my.plot.neg.neur <- my.plot.neg.neur + theme_classic()
my.plot.neg.neur <- my.plot.neg.neur + theme(axis.line.x = element_line(colour='black',size=0.5,linetype='solid'),axis.line.y = element_line(colour='black',size=0.5,linetype='solid'))
my.plot.neg.neur <- my.plot.neg.neur + labs(title="",x="Negative Affect",y="Neuroticism")
my.plot.neg.neur <- my.plot.neg.neur + coord_cartesian(xlim=c(0,3),ylim=c(0,24))
print(my.plot.neg.neur)
ggsave(filename="Figure_6_NA_Neuroticism_Scatter.tiff", plot=my.plot.neg.neur,width = 6,height = 6)
                                                
                                                