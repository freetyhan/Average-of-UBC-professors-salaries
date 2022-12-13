# import profsSalary dataset
setwd("C:/Users/freet/Desktop/UBC_4th_year/STAT344/project")
library(readxl)
profsSalary <- read_excel("C:/Users/freet/Desktop/UBC_4th_year/STAT344/project/profsSalary.xlsx")
hist(profsSalary$Salary)

# profsSalary dataset should be imported
colnames(profsSalary)[1] ="Name"
colnames(profsSalary)[2] ="Salary"
colnames(profsSalary)[3] = "researchSupervisor"
profsSalary <- profsSalary[!is.na(profsSalary$Salary),]
profsSalary <- profsSalary[!is.na(profsSalary$researchSupervisor),]
# These are the proportions of the data split into the true and false categories 
N.h <- tapply(profsSalary$Salary, profsSalary$researchSupervisor, length)
# This is the population size 
N <- length(profsSalary$Salary)
# This is the sample size 
n <- 400
#SRS for continuous population variable 
SRS.indices <- sample.int(N,n,replace = F)
SRS.sample <- profsSalary[SRS.indices,]
ybar.continuous.srs <- mean(SRS.sample$Salary)
se.continuous.srs <- sqrt((1 - n/N) * var(SRS.sample$Salary) / n)
srs.continuous <- c(ybar.continuous.srs, se.continuous.srs)
set <- c(FALSE,TRUE)

#Stratifying where the samples sizes are optimal for continuous population variable
sd1 <- sd(profsSalary[profsSalary$researchSupervisor == FALSE,]$Salary)
sd2 <- sd(profsSalary[profsSalary$researchSupervisor == TRUE,]$Salary) 
w1 <- (sd1 * (N.h[1]/N)) / ((sd1 * (N.h[1]/N)) + (sd2 * (N.h[2]/N))) 
w2 <- (sd2 * (N.h[2]/N)) / ((sd1 * (N.h[1]/N)) + (sd2 * (N.h[2]/N)))
weights <- c(w1,w2)
n.h.optl <- round(weights * n)
STR.sample.optl <- NULL 
for (i in 1: 2) 
{  
  row.indices <- which(profsSalary$researchSupervisor == set[i]) 
  sample.indices <- sample(row.indices, n.h.optl[i], replace = F) 
  STR.sample.optl <- rbind(STR.sample.optl, profsSalary[sample.indices, ])    
} 
ybar.continuous.h.optl <- tapply(STR.sample.optl$Salary, STR.sample.optl$researchSupervisor, mean)  
var.continuous.h.optl <- tapply(STR.sample.optl$Salary, STR.sample.optl$researchSupervisor, var)
str.continuous.ybar.optl <- sum((N.h/N) * ybar.continuous.h.optl)  
str.continuous.se.optl <- sqrt(sum((N.h/N)^2 * (((1 - n.h.optl / N.h) * var.continuous.h.optl)/n.h.optl)))  
str.continuous.optl <- c(str.continuous.ybar.optl, str.continuous.se.optl)
#These outputs are the final result
# 95% confidence interval for SRS continuous population variable
srs.continuous.ci <- c(ybar.continuous.srs - 1.96 * se.continuous.srs, ybar.continuous.srs + 1.96 * se.continuous.srs) 
# 95% confidence interval for stratified sampling with optimal weights for continuous population variable
str.continuous.optl.ci <- c(str.continuous.ybar.optl - 1.96 * str.continuous.se.optl, str.continuous.ybar.optl + 1.96 * str.continuous.se.optl)
rbind(srs.continuous,srs.continuous.ci)
rbind(str.continuous.optl,str.continuous.optl.ci)

#Using a binary population variable 
srs.binary.ybar <- sum(SRS.sample$Salary > 135000) / n
# SRS sampling showed that 0.395 were above 135000
srs.binary.se <- sqrt((srs.binary.ybar)*(1 - srs.binary.ybar)/n)
# SE was 0.02444 for the SRS 
srs.binary.ci <- c(srs.binary.ybar - 1.96*srs.binary.se, srs.binary.ybar + 1.96*srs.binary.se)

#Using stratified sampling with optimal allocation for binary population variable
researchAssistant <- STR.sample.optl[STR.sample.optl$researchSupervisor == TRUE,]
nonResearchAssistant <- STR.sample.optl[STR.sample.optl$researchSupervisor == FALSE,]
rA.greaterThan <- sum(researchAssistant$Salary > 135000)
nRA.greaterThan <- sum(nonResearchAssistant$Salary > 135000)
beforeGreaterThan <- c(nRA.greaterThan, rA.greaterThan)
str.binary.ybar <- sum((N.h/N) * (beforeGreaterThan / n.h.optl))
str.binary.se.optl <- sqrt(sum((N.h/N)^2 * (1 - n.h.optl / N.h) * ((str.binary.ybar) * (1 - str.binary.ybar)/n.h.optl)))
str.binary.optl.ci <- c(str.binary.ybar - 1.96 * str.binary.se.optl, str.binary.ybar + 1.96 * str.binary.se.optl)
rbind(srs.binary.ybar,srs.binary.se)
srs.binary.ci
rbind(str.binary.ybar,str.binary.se.optl)
str.binary.optl.ci
