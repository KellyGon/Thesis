rm(list=ls())

#library(rmarkdown)
#rmarkdown::render('C:/Users/Kelly/OneDrive - Fundacao Getulio Vargas - FGV/Paper Gender Inequality UNICAMP/Code/competition.R')

#' ---
#' title: "Competition"
#' author: Kelly G.
#' output:
#'   html_document:
#'     toc: true
#'     number_sections: true
#' ---


#path:
setwd("/Users/Kelly/OneDrive - Fundacao Getulio Vargas - FGV/Paper Gender Inequality UNICAMP")


#' ## Library

library(foreign)
library(ggplot2)
library(dplyr)
library(reshape2)
library(kdensity)
library(tidyverse)
library(expss)
library(stringr)
library(knitr)
library(printr)
library(data.table)
library(tidyverse)
library(stargazer)
library(sandwich)
library(plm)
library(clubSandwich)
library(fixest)
library(lfe)


#' ## Opening

df_first<-read.dta("/Users/Kelly/OneDrive - Fundacao Getulio Vargas - FGV/Paper Gender Inequality UNICAMP/Dataset/Dataset/final/final_1st.dta")

df<-read.dta("/Users/Kelly/OneDrive - Fundacao Getulio Vargas - FGV/Paper Gender Inequality UNICAMP/Dataset/Dataset/final/final_withRounds.dta")

df$enroll.numeric<-0
df$enroll.numeric[df$enroll=='yes'] = 1
df$enroll.numeric[is.na(df$enroll)] = NA

df.2004.2006 <-
  df %>%  subset(year=="2004" | year=="2006")

#' ### Datasets used
#' Number of students
number.students<-df_first %>% group_by(year,career_choice) %>% count()

#' Slots
slots<-df_first %>% subset(enroll=='yes') %>% group_by(year,career_choice) %>% count()

#' Students by seats
slots.number.students<-merge(slots,number.students,by=c("year", "career_choice"))
names(slots.number.students)[names(slots.number.students) == 'n.x'] <- 'Slots'
names(slots.number.students)[names(slots.number.students) == 'n.y'] <- 'Students'
slots.number.students$students_by_seat<-slots.number.students$Students/slots.number.students$Slots

#' Cutoff
cutoff.score.carreer.year<-df %>% subset(convoc==1) %>% group_by(year, career_choice) %>% 
  summarise(across(c(wNPO1,wNPO1_aa), min))
names(cutoff.score.carreer.year)[names(cutoff.score.carreer.year) == 'wNPO1'] <- 'wNPO_cutoff'
names(cutoff.score.carreer.year)[names(cutoff.score.carreer.year) == 'wNPO1_aa'] <- 'wNPO1_aa_cutoff'

cutoff.score.year <- cutoff.score.carreer.year %>% group_by(year) %>% 
  summarise(across(c(wNPO_cutoff,wNPO1_aa_cutoff), mean,  na.rm = TRUE))

cutoff.score.year.medicine <- cutoff.score.carreer.year %>%
  subset(career_choice==75 | career_choice==15) %>% 
  group_by(year) %>% 
  summarise(across(c(wNPO_cutoff,wNPO1_aa_cutoff), mean,  na.rm = TRUE))

#' Merge cutoff
#' 
df.merge.cutoff.carreer.year <-merge(df,cutoff.score.carreer.year,by=c("year", "career_choice"))
df.merge.cutoff.carreer.year <-merge(number.students,df.merge.cutoff.carreer.year,by=c("year", "career_choice"))


df.merge.cutoff.carreer.year<-df.merge.cutoff.carreer.year %>%
  mutate(threshold = ifelse(wNPO1_aa >= wNPO1_aa_cutoff, 1, 0))

df.merge.cutoff.carreer.year <- subset(df.merge.cutoff.carreer.year, 
                                       select = c("year", "career_choice","threshold",
                                                  "wNPO1_aa_cutoff", "wNPO1_aa",
                                                  "aa_year", "aa_policy",
                                                  "all_public","minority_all_public",
                                                  "convoc","vis_min","n","sex") )

df.merge.cutoff.carreer.year$difference.cutoff<-NA
df.merge.cutoff.carreer.year<-transform(df.merge.cutoff.carreer.year, 
                                        difference.cutoff=wNPO1_aa-wNPO1_aa_cutoff)























#' ## Density Plots
#' ### All period

#' #### Score pre and pos
#Pre bonus
before.bonus<- subset(df,  aa_year==0)

#Pos bonus
after.bonus<- subset(df,  aa_year==1)

d.without.bonus<-density(before.bonus$wNPO1, na.rm = TRUE)
d.with.bonus<-density(after.bonus$wNPO1_aa, na.rm = TRUE)
file=str_c("Code/Results/imgs/","dist_score_all.tiff")
tiff(file=file, width=6, height=4, units="in", res=100)
par(mar=c(7.0, 3.0, 1.5, 1.5))
plot(d.without.bonus, col = "blue", main="Distributions of Admission Examination Final Scores")
lines(d.with.bonus, col = "red")
legend("bottom",
       c("Pre bonus Score", "Pos bonus Score"), 
       col=c("blue", "red"), 
       lwd=2, lty=c(1,1,1),
       xpd = TRUE, horiz = TRUE, inset = c(-0.3,-0.45))
dev.off()



#' #### By group
#' 
#' Students that receive bonus 
bonifed.without.bonus<-subset(df,all_public==1 | vis_min==1 )
bonifed.without.bonus$bonus<-1

#' Students that don't receive  bonus
non.bonifed <-subset(df, all_public==0 & vis_min==0)
non.bonifed$bonus<-0

#' Plots
d.without.bonus<-density(bonifed.without.bonus$wNPO1, na.rm = TRUE)
d.with.bonus<-density(bonifed.without.bonus$wNPO1_aa, na.rm = TRUE)
d.no.bonus<-density(non.bonifed$wNPO1, na.rm = TRUE)

file=str_c("Code/Results/imgs/","dist_score_group.tiff")
tiff(file=file, width=6, height=4, units="in", res=100)
par(mar=c(7.0, 3.0, 1.5, 1.5))
plot(d.without.bonus, col = "blue", main="Distributions of Admission Examination Final Scores")
lines(d.with.bonus, col = "red")
lines(d.no.bonus, col = "black")
legend("bottom",
       c("Private school", "Public school Pre bonus", "Public school Pos bonus"), 
       col=c("black","blue", "red"), 
       lwd=2, lty=c(1,1,1),
       xpd = TRUE, horiz = FALSE,  inset = c(-0.3,-0.45))

dev.off()


#ggplot(bonifed.without.bonus, aes(x=wNPO1, color=all_public)) + 
  # geom_density(aes(group=all_public))+
  # labs(x= "Padronized Score")


#' ## Only after the politics
#' Students bonified without bonus 
bonifed<-subset(df,aa_policy=="30 points" | aa_policy=="40 points" )

#' Students without bonus
non.bonifed <-subset(df, aa_policy=="no" & aa_year==1)

#' Plots
d.without.bonus<-density(bonifed$wNPO1, na.rm = TRUE)
d.with.bonus<-density(bonifed$wNPO1_aa, na.rm = TRUE)
d.no.bonus<-density(non.bonifed$wNPO1, na.rm = TRUE)

par(mar=c(7.0, 3.0, 1.5, 1.5))
plot(d.without.bonus, col = "blue", main="Distributions of Admission Examination Final Scores")
lines(d.with.bonus, col = "red")
lines(d.no.bonus, col = "black")
legend("bottom",
       c("Private school", "Public school without bonus", "Public school with bonus"), 
       col=c("black","blue", "red"), 
       lwd=2, lty=c(1,1,1),
       xpd = TRUE, horiz = TRUE,  inset = c(-0.3,-0.45))

#' # Cutoff score
#' ## All students
d <- melt(cutoff.score.year, id.vars="year")
p<-ggplot(d, aes(year,value, col=variable)) + 
  geom_line()+
  geom_point()+
  scale_color_manual(labels = c("Cutoff without bonus", "Cutoff with bonus"), 
                     values = c("red", "blue"))+ labs(color = "")

file=str_c("Code/Results/imgs/","competition_cutoff_all.tiff")
tiff(file=file, width=6, height=4, units="in", res=100)
p+theme(legend.position="bottom")+ ggtitle("Cutoff Score - Mean") +
  xlab("Year") + ylab("Score")
dev.off()

#' 
#' ## Only Medicine students

d <- melt(cutoff.score.year.medicine, id.vars="year")
p<-ggplot(d, aes(year,value, col=variable)) + 
  geom_line()+
  geom_point() +
  scale_color_manual(labels = c("Cutoff without bonus", "Cutoff with bonus"), 
                     values = c("red", "blue"))+ labs(color = "")
p+theme(legend.position="bottom")+ ggtitle("Cutoff Score - Medicine students") +
  xlab("Year") + ylab("Score")

#ggplot(data=cutoff.score.year.medicine, aes(x=year, y=wNPO1, group=1)) +
  # geom_line()+
  # geom_point()




















#' # Difference of score to cutoff
#' ## All students
d.concentration.cutoff.with.bonus<-df.merge.cutoff.carreer.year %>% subset(aa_year==1) 
d.concentration.cutoff.with.bonus<- density(d.concentration.cutoff.with.bonus$difference.cutoff, na.rm = TRUE) 

d.concentration.cutoff.without.bonus<-df.merge.cutoff.carreer.year %>% subset(aa_year==0) 
d.concentration.cutoff.without.bonus<- density(d.concentration.cutoff.without.bonus$difference.cutoff, na.rm = TRUE) 

file=str_c("Code/Results/imgs/","competition_diffence_cutoff_all.tiff")
tiff(file=file, width=6, height=4, units="in", res=100)

par(mar=c(7.0, 3.0, 1.5, 1.5))
plot(d.concentration.cutoff.with.bonus, col = "blue",xlim=c(-100,100), 
     main="Distributions of Difference of Final Scores to cutoff - All students")
lines(d.concentration.cutoff.without.bonus, col = "red")
colors <- c("red", "blue")
labels <- c("Before bonus", "After bonus")
abline(v=0, col="black",lty=2, lwd=1)
legend("bottom",
       labels, 
       col=colors, 
       lwd=2, lty=c(1,1),
       xpd = TRUE, horiz = TRUE,  inset = c(-0.3,-0.45))
dev.off()




#' ## Bonified students
d.concentration.cutoff.with.bonus<-df.merge.cutoff.carreer.year %>% subset(aa_year==1 & (all_public==1 | vis_min==1))
d.concentration.cutoff.with.bonus<- density(d.concentration.cutoff.with.bonus$difference.cutoff, na.rm = TRUE) 

d.concentration.cutoff.without.bonus<-df.merge.cutoff.carreer.year %>% 
  subset(aa_year==0 & (all_public==1 | vis_min==1))
d.concentration.cutoff.without.bonus<- density(d.concentration.cutoff.without.bonus$difference.cutoff, na.rm = TRUE) 

file=str_c("Code/Results/imgs/","competition_diffence_cutoff_bonus.tiff")
tiff(file=file, width=6, height=4, units="in", res=100)
par(mar=c(7.0, 3.0, 1.5, 1.5))
plot(d.concentration.cutoff.with.bonus, col = "blue",xlim=c(-100,100), 
     main="Distributions of Difference of Final Scores to cutoff - Students with bonus")
lines(d.concentration.cutoff.without.bonus, col = "red")
colors <- c("red", "blue")
labels <- c("Before bonus", "After bonus")
abline(v=0, col="black",lty=2, lwd=1)
legend("bottom",
       labels, 
       col=colors, 
       lwd=2, lty=c(1,1),
       xpd = TRUE, horiz = TRUE,  inset = c(-0.3,-0.45))

dev.off()


#' ## Non-bonified students
#' 
d.concentration.cutoff.with.bonus<-df.merge.cutoff.carreer.year %>% 
  subset(aa_year==1 & (all_public==0 & vis_min==0))
d.concentration.cutoff.with.bonus<- density(d.concentration.cutoff.with.bonus$difference.cutoff, na.rm = TRUE) 

d.concentration.cutoff.without.bonus<-df.merge.cutoff.carreer.year %>% 
  subset(aa_year==0 & (all_public==0 & vis_min==0))
d.concentration.cutoff.without.bonus<- density(d.concentration.cutoff.without.bonus$difference.cutoff, na.rm = TRUE) 

file=str_c("Code/Results/imgs/","competition_diffence_cutoff_nonbonus.tiff")
tiff(file=file, width=6, height=4, units="in", res=100)
par(mar=c(7.0, 3.0, 1.5, 1.5))
plot(d.concentration.cutoff.with.bonus, col = "blue",xlim=c(-100,100), 
     main="Distributions of Difference of Final Scores to cutoff - Students without bonus")
lines(d.concentration.cutoff.without.bonus, col = "red")
colors <- c("red", "blue")
labels <- c("Before bonus", "After bonus")
abline(v=0, col="black",lty=2, lwd=1)
legend("bottom",
       labels, 
       col=colors, 
       lwd=2, lty=c(1,1),
       xpd = TRUE, horiz = TRUE,  inset = c(-0.3,-0.45))

dev.off()


#' # Number of students around cutoff
#' ## All students
#+ echo=FALSE, results='asis'
for(i in seq(15, 90, by =15)){
  
  count.around.cutoff<-df.merge.cutoff.carreer.year %>% 
    subset(difference.cutoff<i & difference.cutoff>-i)
  count.around.cutoff<-count.around.cutoff %>% count(year,career_choice)
  count.around.cutoff<-merge(count.around.cutoff,number.students,by=c("year", "career_choice"))
  count.around.cutoff$n <- count.around.cutoff$n.x/count.around.cutoff$n.y
 
  #Mean
  #Proportion of students
  count.around.cutoff.table<-count.around.cutoff %>% 
    subset(select=c('year','career_choice','n')) %>% 
    group_by(year) %>% 
    summarise(across(n, mean, na.rm=TRUE))
  p<-ggplot(count.around.cutoff.table, aes(year, n)) + 
    geom_line()+
    geom_point()+
    geom_vline(xintercept=2005)
  p<-p+ ggtitle(str_c("Proportion of students around cutoff per year - All students"),i) +
    xlab("Year") + ylab("Students")
  print(p)
  cat("\n")
  
  #Number of students
  count.around.cutoff.table<-count.around.cutoff %>% 
    subset(select=c('year','career_choice','n.x')) %>% 
    group_by(year) %>% 
    summarise(across(n.x, mean, na.rm=TRUE))
  p<-ggplot(count.around.cutoff.table, aes(year, n.x)) + 
    geom_line()+
    geom_point()+
    geom_vline(xintercept=2005)
  p<-p+ ggtitle(str_c("Number of students around cutoff per year - All students"),i) +
    xlab("Year") + ylab("Students")
  print(p)
  cat("\n")
  
  # print(knitr::kable(
  #   count.around.cutoff.table,
  #   col.names = c('Year', str_c('Number of students in bandwidth - ', i))
  # ))
  # cat("\n")
  
  
  #Only medicine
  count.around.cutoff.medicine <- count.around.cutoff %>%
    subset(select=c('year','career_choice','n','n.x')) %>% 
    subset(career_choice==75 | career_choice==15) %>% 
    group_by(year)
  count.around.cutoff.medicine$career_choice[count.around.cutoff.medicine$career_choice==15]<-
    "Medicine Unicamp"
  count.around.cutoff.medicine$career_choice[count.around.cutoff.medicine$career_choice==75]<-
    "Famerp Unicamp"
  
  p<-ggplot(count.around.cutoff.medicine, aes(year, n.x, group=career_choice,color=career_choice)) + 
    geom_line()+
    geom_point()+
    geom_vline(xintercept=2005)
  p<-p+ ggtitle(str_c("Number of students around cutoff per year - Medicine students"),i) +
    xlab("Year") + ylab("Students")
  print(p)
  cat("\n")
  
  p<-ggplot(count.around.cutoff.medicine, aes(year, n, group=career_choice,color=career_choice)) + 
    geom_line()+
    geom_point()+
    geom_vline(xintercept=2005)
  p<-p+ ggtitle(str_c("Proportion of students around cutoff per year - Medicine students"),i) +
    xlab("Year") + ylab("Students")
  print(p)
  cat("\n")
  
  #Top 5 courses - top 5 careers: 15 75 34 6 11 
  count.around.cutoff.medicine <- count.around.cutoff %>%
    subset(select=c('year','career_choice','n','n.x')) %>% 
    subset(career_choice==75 | career_choice==15 |
             career_choice==34 | career_choice==6 |
             career_choice==11 ) %>% 
    group_by(year)
  count.around.cutoff.medicine$career_choice[count.around.cutoff.medicine$career_choice==15]<-
    "Medicine Unicamp"
  count.around.cutoff.medicine$career_choice[count.around.cutoff.medicine$career_choice==75]<-
    "Famerp Engineering"
  count.around.cutoff.medicine$career_choice[count.around.cutoff.medicine$career_choice==34]<-
    "Computer Engineering"
  count.around.cutoff.medicine$career_choice[count.around.cutoff.medicine$career_choice==6]<-
    "Biological Science"
  count.around.cutoff.medicine$career_choice[count.around.cutoff.medicine$career_choice==11]<-
    "Engineering"
  
  p<-ggplot(count.around.cutoff.medicine, aes(year, n.x, group=career_choice,color=career_choice)) + 
    geom_line()+
    geom_point()+
    geom_vline(xintercept=2005)
  p<-p+ ggtitle(str_c("Number of students around cutoff per year - Medicine students"),i) +
    xlab("Year") + ylab("Students") 
  print(p)
  cat("\n")
  
  p<-ggplot(count.around.cutoff.medicine, aes(year, n, group=career_choice,color=career_choice)) + 
    geom_line()+
    geom_point()+
    geom_vline(xintercept=2005)
  p<-p+ ggtitle(str_c("Proportion of students around cutoff per year - Medicine students"),i) +
    xlab("Year") + ylab("Students")
  print(p)
  cat("\n")
  
}


#' ## Regression: Number of students - Difference of score to cutoff 
#' 



treat1 <-
  c("female", "aa_year", "all_public")

treat2 <-
  c("female", "aa_year", "all_public", "female:aa_year", "female:all_public", "aa_year:all_public",
    "female:aa_year:all_public")

control_family <- c(
  "fa_manual_occ",
  "fa_medium_occ",
  "fa_top_occ",
  "mo_manual_occ",
  "mo_medium_occ",
  "mo_top_occ",
  "mother_univ",
  "father_univ"
)

fe1 <- "| career_choice"
fe2 <- "| career_choice + code"
cluster<- "| code"

note.latex<-c("Personal characteristics consist of a quartic function of age,",
              "gender, and a previous university attendance indicator variable,", 
              "while the parental education and occupation controls each consists",
              "of 16 dummy variables (8 per parent). Municipalities are the ones ",
              "of the applicant's residence. Cluster-robust standard errors at the",
              "municipality are shown in parentheses.")

for(i in seq(15, 105, by =15)){
  df.merge.cutoff.carreer.year$around_cutoff<-
    as.numeric(df.merge.cutoff.carreer.year$difference.cutoff<i 
               & df.merge.cutoff.carreer.year$difference.cutoff>-i)
  
  df.merge.cutoff.carreer.year.2004.2006 <-
    df.merge.cutoff.carreer.year %>%  subset(year=="2004" | year=="2006")
  
  formula <- paste('around_cutoff ~', 
                   paste(c( treat1, "enem", control_family), collapse = "+"),
                   fe2, "| 0", cluster )
  
  reg.1_all<-felm(formula(formula), data=df.merge.cutoff.carreer.year)
  
  reg.1_2004<-felm(formula(formula), data=df.merge.cutoff.carreer.year.2004.2006)
  
  formula <- paste('around_cutoff ~', 
                   paste(c( treat2, "enem", control_family), collapse = "+"),
                   fe2, "| 0", cluster )
  
  reg.2_all<-felm(formula(formula), data=df.merge.cutoff.carreer.year)
  
  reg.2_2004<-felm(formula(formula), data=df.merge.cutoff.carreer.year.2004.2006)
  
  stargazer(
    reg.1_all,
    reg.2_all,
    type = "latex",
    omit = c("Constant", control_family),
    dep.var.labels = c("Around Cutoff"),
    covariate.labels = c(
      "Female",
      "Bonus year",
      "Public",
      "ENEM Score",
      "Bonus year*Female",
      "Public*Female",
      "Bonus year*Public",
      "Bonus year*Public*Female"
    ),
    keep.stat = "n",
    out = str_c("Code/Results/reg/difference_around_cutoff", i, ".tex"),
    label = str_c('difference_around_cutoff', i),
    add.lines=list(c("Year Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes"), 
                   c("Personal Characteristics", "Yes", "Yes", "Yes", "Yes", "Yes"),
                   c("Parental Education", "Yes", "Yes", "Yes", "Yes", "Yes"),
                   c("Parental Occupation", "Yes", "Yes", "Yes", "Yes", "Yes"),
                   c("Career Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes"),
                   c("Municipality Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes")
    ),
    style = "AER",
    notes=note.latex,
    dep.var.labels.include = F
  )
  
}




#' # Others measures for competition
#' ## Students aprova ==1, passed stage 1
#' 

aprova.year<-df_first %>% 
  subset(aprova==1)  %>% count(year)

knitr::kable(aprova.year,
             col.names = c('Year', 'Number of students that passed at stage 1 ')) 

p<-ggplot(aprova.year, aes(year, n)) + 
  geom_line()+
  geom_point()+
  geom_vline(xintercept=2005)
p+ ggtitle("Number of students that passed stage 1") +
  xlab("Year") + ylab("Number")


reg<-lm(aprova ~ aa_year + factor(sex)+factor(sex):aa_year +
          aa_year:all_public+factor(sex):aa_year:all_public + factor(career_choice), data=df_first)
stargazer(reg, type="latex",  omit="career_choice",
               dep.var.labels=c("Probability of passing on stage 1"),
               covariate.labels=c("Bonus year","Female","Bonus year*Female","Bonus year*Public","Bonus year*Public*Female"),
               keep.stat="n",
               out=str_c("Code/Results/reg/number_students_passed_stage1",".tex"))

#' ## Cutoff Score of students passed stage 1 - Only medicine students

aprova.cutoff.year<-df_first %>% subset(aprova==1) %>% group_by(year, career_choice) %>% 
  subset(career_choice==75) %>% 
  summarise(across(NP_st1, min))
knitr::kable(aprova.cutoff.year,
  col.names = c('Year', 'Career Choice','Cutoff stage 1 '))

p<-ggplot(aprova.cutoff.year, aes(year, NP_st1)) + 
  geom_line()+
  geom_point()+
  geom_vline(xintercept=2005)
p+ ggtitle("Cutoff st1 score of students that passed stage 1") +
  xlab("Year") + ylab("Number")

#' ## Students by seats
slots.number.students$aa_year<-0
slots.number.students$aa_year[slots.number.students$year>2005]<-1
slots.number.students.2004.2006 <-
  slots.number.students %>%  subset(year=="2004" | year=="2006")

#' ### All period
reg<-lm(students_by_seat ~ aa_year + factor(career_choice) , data=slots.number.students)
stargazer(reg, type="latex",  omit="career_choice",
          dep.var.labels=c("Students by seat"),
          covariate.labels=c("Bonus year"),
          keep.stat="n",
          out=str_c("Code/Results/reg/students_by_seat_all",".tex"))

#' ### Only 2004 x 2006
reg<-lm(students_by_seat ~ aa_year + factor(career_choice), data=slots.number.students.2004.2006)
stargazer(reg, type="text",  omit="career_choice")
























#' ## Questionnaire - less competition female x male

df$qq_low_competition <- 0
df$qq_low_competition[df$reason_career==5]<-1
df$qq_low_competition[is.na(df$reason_career)] = NA

df.2004.2006$qq_low_competition <- 0
df.2004.2006$qq_low_competition[df.2004.2006$reason_career==5]<-1
df.2004.2006$qq_low_competition[is.na(df.2004.2006$reason_career)] = NA


#' ### All period
reg<-lm(qq_low_competition ~ aa_year , data=df)
stargazer(reg, type="text")

reg<-lm(qq_low_competition ~ female + aa_year + female:aa_year, data=df)
stargazer(reg, type="text")

#' with career_choice fixed effects
reg<-lm(qq_low_competition ~ female + aa_year + female:aa_year + enem + factor(career_choice), data=df)
stargazer(reg, type="text",  omit="career_choice")

#' ### Only 2004 x 2006
reg<-lm(qq_low_competition ~ aa_year , data=df.2004.2006)
stargazer(reg, type="text")

reg<-lm(qq_low_competition ~ female + aa_year + female:aa_year, data=df.2004.2006)
stargazer(reg, type="text")

reg<-lm(qq_low_competition ~ female + aa_year + female:aa_year + enem + factor(career_choice), data=df.2004.2006)
stargazer(reg, type="text",  omit="career_choice")


