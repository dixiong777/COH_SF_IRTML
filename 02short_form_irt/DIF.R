# CHL: Age group, 8-12, 13-17, gender (paper 6)
# PAT: Age group, >45, gender, education, home language (paper 7)

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(gdata)
library(MASS)
library(tidyverse)
library(readxl)
theta <- read_excel("thetaLF.xlsx", sheet = 1)
Ref <- colnames(theta)
Ref <- Ref[1:(length(Ref)-2)]
demo <- read.csv("Final_Data_demo.csv")
# subset children 8-17
demo <- demo[demo$P_age_g != "g1:2-7",]
demo <- demo[,c("P_age_g","P_AGE","P_GENDER","C_GENDER","P_EUC","P_HOMELAN","C_AGE")]
# df <- read.csv("data_CHL_IRT_head.csv")
df <- read.csv("data_PAR_IRT_head.csv")
df <- df[,-1]
df <- cbind(demo,df)
# x1 <- ifelse(df$C_AGE < 13,"<13",">=13")
# x2 <- df$C_GENDER
# x3 <- df$C_AGE
x1 <- ifelse(df$P_AGE < 45,"<45",">=45")
x2 <- df$P_GENDER
x3 <- df$P_AGE
nodiff_p_age<-c()
nodiff_p_gen<-c()
nodiff_p_age.0<-c()
nodiff_p_age.1<-c()
cut <- 45
#Ref-items
for (i in 1:(length(Ref))) {
  
  y <- as.factor(df[,Ref[i]])
  y.0 <- as.factor(df[,Ref[i]][x3 < cut])
  y.1 <- as.factor(df[,Ref[i]][x3 >= cut])
  
  #x1-agegroup, x2-gender
  if(nlevels(y) == 2) {
    nodiff_p_age[i] <- coef(summary(glm(y ~ x1 + theta$Theta,family=binomial(link='logit'),control = list(maxit = 50))))[2,4]
    nodiff_p_gen[i] <- coef(summary(glm(y ~ x2 + theta$Theta,family=binomial(link='logit'),control = list(maxit = 50))))[2,4]
  }
  else {
    ctable <- coef(summary(polr(y ~ x1+ theta$Theta, Hess=TRUE)))
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    nodiff_p_age[i] <- ctable[1,4]
    
    ctable <- coef(summary(polr(y ~ x2+ theta$Theta, Hess=TRUE)))
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    nodiff_p_gen[i] <- ctable[1,4]
  }
  
  ####
  if(nlevels(y.0) == 2) {
    nodiff_p_age.0[i] <- coef(summary(glm(y.0 ~ x3[x3 < cut] + theta[x3 < cut,]$Theta,family=binomial(link='logit'))))[2,4]
    
  }
  else {
    ctable <- coef(summary(polr(y.0 ~ x3[x3 < cut]+ theta[x3 < cut,]$Theta, Hess=TRUE)))
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    nodiff_p_age.0[i] <- ctable[1,4]
    
  }
  
  
  ###
  if(nlevels(y.1) == 2) {
    nodiff_p_age.1[i] <- coef(summary(glm(y.1 ~ x3[x3 >= cut] + theta[x3 >= cut,]$Theta,family=binomial(link='logit'))))[2,4]
  }
  else {
    ctable <- coef(summary(polr(y.1 ~ x3[x3 >= cut] + theta[x3 >= cut,]$Theta, Hess=TRUE)))
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    nodiff_p_age.1[i] <- ctable[1,4]
  }
}
nodiff_p_age_T <- (nodiff_p_age > 0.05)
nodiff_p_gen_T <- (nodiff_p_gen > 0.05)
nodiff_p_age.0_T <- (nodiff_p_age.0 > 0.05)
nodiff_p_age.1_T <- (nodiff_p_age.1 > 0.05)
cbind(nodiff_p_age,nodiff_p_gen,nodiff_p_age.0,nodiff_p_age.1)


x1 <- df$P_EUC
x2 <- df$P_HOMELAN
nodiff_p_age<-c()
nodiff_p_gen<-c()
for (i in 1:(length(Ref))) {
  
  y <- as.factor(df[,Ref[i]])
  
  #x1-education, x2-homelanguage
  if(nlevels(y) == 2) {
    nodiff_p_age[i] <- coef(summary(glm(y ~ x1 + theta$Theta,family=binomial(link='logit'),control = list(maxit = 50))))[2,4]
    nodiff_p_gen[i] <- coef(summary(glm(y ~ x2 + theta$Theta,family=binomial(link='logit'),control = list(maxit = 50))))[2,4]
  }
  else {
    ctable <- coef(summary(polr(y ~ x1+ theta$Theta, Hess=TRUE)))
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    nodiff_p_age[i] <- ctable[1,4]
    
    ctable <- coef(summary(polr(y ~ x2+ theta$Theta, Hess=TRUE)))
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    nodiff_p_gen[i] <- ctable[1,4]
  }
}
cbind(nodiff_p_age,nodiff_p_gen)

# Item Descriptive
df <- read.csv("data_CHL_IRT_head.csv")
df <- df[,-1]
r <- prop.table(table(df[,1]))
for (i in colnames(df)){
  #  print(i)
  r <- dplyr::bind_rows(r,prop.table(table(df[,i])))
}
View(r)
View(colnames(df))
df[,] <- df[,]+1
r <- c(mean(df[,1]),sd(df[,1]))
for (i in colnames(df)){
  #  print(i)
  r <- rbind(r,c(mean(df[,i]),sd(df[,i])))
}
View(r)
apply(df, 2, function(x) any(is.na(x)))
