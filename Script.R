options(digits = 4)
library(readr)

dvis <- read_csv("dvis.csv")
str(dvis)
dvis <- unique(dvis)

######################################################
################# Data Exploration ###################
######################################################

### Define the colours
 
csteel <- "#B0C4DE"
ctan <- "#D2B48C"
corange <- "#FF8C00"
cgold <- "#FFD700"
ckhaki <- "#F0E68C"
clime<- "#32CD32"
ccyan <- "#00FFFF"
cblue <- "#00BFFF"
cviolet <- "#EE82EE"
cpink <- "#FFB6C1"
ccol <- c(csteel, ctan, corange, cgold, ckhaki, clime, 
          ccyan, cblue, cviolet, cpink)
palette(ccol)

dvis$fcol[dvis$female==0] <- ccol[1] # gender col
dvis$fcol[dvis$female==1] <- ccol[2]

dvis$kcol[dvis$hhkids==0] <- ccol[3] # kid col
dvis$kcol[dvis$hhkids==1] <- ccol[4]

dvis$mcol[dvis$married==0] <- ccol[5] # marriage col
dvis$mcol[dvis$married==1] <- ccol[6]

dvis$ecol[dvis$employed==0] <- ccol[7] # employment col
dvis$ecol[dvis$employed==1] <- ccol[8]

dvis$picol[dvis$privateins==0] <- ccol[9] # private insurance col
dvis$picol[dvis$privateins==1] <- ccol[10]

dvis$aicol[dvis$addins==0] <- ccol[1] # additional insurance col
dvis$aicol[dvis$addins==1] <- ccol[2]

dvis$etcol[dvis$eductype==0] <- ccol[3]
dvis$etcol[dvis$eductype==1] <- ccol[4]
dvis$etcol[dvis$eductype==2] <- ccol[5]
dvis$etcol[dvis$eductype==3] <- ccol[6]
dvis$etcol[dvis$eductype==4] <- ccol[7]
dvis$etcol[dvis$eductype==5] <- ccol[8]

attach(dvis)


par(mfrow=c(2,2))
plot(jitter(docvis) ~ jitter(educyrs,10),pch=16,cex.lab=1.5,
     xlab='Number of Years',ylab='Number of visits',
     main='Number of visits by schooling')


plot(jitter(docvis) ~ jitter(age,1),pch=16,cex.lab=1.5,
     xlab='Age',ylab='Number of visits',
     main='Number of visits by age')

plot(jitter(docvis) ~ jitter(hhninc,1),pch=16,cex.lab=1.5,
     xlab='Net Monthly Household Income',ylab='Number of visits',
     main='Number of visits by household income')

par(mfrow=c(1,1))


### Obtain the summary statistics

myfun <- function(x) c(mean(x),length(x))

female.stats <- tapply(docvis, female, myfun)
kid.stats <- tapply(docvis, hhkids, myfun)
marriage.stats <- tapply(docvis, married, myfun)
eductype.stats <- tapply(docvis, eductype, myfun)
employ.stats <- tapply(docvis, employed, myfun)
privins.stats <- tapply(docvis, privateins, myfun)
addins.stats <- tapply(docvis, addins, myfun)


### Barplots 

tab1 <- table(female, docvis)
barplot(tab1, beside = TRUE, xlab='Number of Visits', 
        ylab='Frequency', cex.lab=1.3, col=1:2, 
        main='Frequency of numbers of visits by gender')
legend('topright', c('male','female'), fill=1:2)

tab3 <- table(married, docvis)
barplot(tab3, beside=T, xlab='visits', ylab='Frequency',
        cex.lab=1.3, col=5:6, 
        main='Frequency of numbers of visits \n 
        by marriage condition')
legend('topright', c('unmarried','married'), fill=5:6)

tab5 <- table(employed,docvis)
barplot(tab5, beside=T, xlab='visits', ylab='Frequency',
        cex.lab=1.3, col=7:8, 
        main='Frequency of numbers of visits by employment')
legend('topright',c('unemployed','employed'), fill=7:8)

### Response against education type

plot(jitter(docvis) ~ jitter(eductype),pch=16,col=etcol,
     xlab='Education type',ylab='Number of visits',cex.lab=1.3,
     main='Number of visits by education type')


### Interaction 
## Visits against age, coloured and shaped by hhkids
plot(jitter(docvis) ~ age,pch=16+hhkids, col = kcol, 
     ylab='Number of visits', cex.lab=1.3,
     main='Number of visits by age coloured by \n 
     if children under 16 present in the household')
legend('topright', c('No','Yes'), pch=c(16,17), col=3:4)


## Visits against educyrs, coloured by private insurance
plot(jitter(docvis) ~ jitter(educyrs), pch=16+privateins,col=picol, 
     xlab='Education years', ylab='Number of visits',cex.lab=1.3,
     main='Number of visit by education year coloured by \n
     having private insurance or not')
legend('topleft', c('No','Yes'),col=9:10, pch=c(16,17))


tab.fk <- table(interaction(female,hhkids),docvis)
barplot(tab.fk,beside=T, xlab='Response', ylab='Frequency',
        cex.lab=1.3, col=3:6, 
        main='Frequency of numbers of visits by gender & children')
legend('topright',fill=3:6, 
       c('Male No Child','Female No Child',
         'Male with Child','Female with Child'))

tab.em <- table(interaction(employed,married),docvis)
barplot(tab.em,beside=T, xlab='Response', ylab='Frequency',
        cex.lab=1.3, col=7:10, 
        main='Frequency of numbers of visits by employment & marriage')
legend('topright',fill=7:10,
       c('Unemployed Unmarried','Employed Unmarried',
         'Unemployed Married','Employed Married'))



detach(dvis)

######################################################
######### Transform to Categorical Variables #########
######################################################

# Set indicators to factor (categorical)
dvis$female <- as.factor(dvis$female)
dvis$hhkids <- as.factor(dvis$hhkids)
dvis$married <- as.factor(dvis$married)
dvis$employed <- as.factor(dvis$employed)
dvis$privateins <- as.factor(dvis$privateins)
dvis$addins <- as.factor(dvis$addins)
dvis$eductype <- as.factor(dvis$eductype)

# Transform numerical variables for interpretation
mean.age <- mean(dvis$age)
dvis$age <- dvis$age - mean.age

mean.ninc <- mean(dvis$hhninc)
dvis$hhninc <-dvis$hhninc - mean.ninc

min.educyrs <- min(dvis$educyrs)
dvis$educyrs <- dvis$educyrs - min.educyrs

######################################################
################ Balance the data set ################
######################################################

library("dplyr") 

set.seed(42)
tab0 <- table(dvis$docvis)
n0 = tab0[1]

dvis.b <- dvis[dvis$docvis==0,]
for (i in 1:7){
    ni <- tab0[i+1]
    # How many times to replicate for data of i visits
    time.i <- n0 %/% ni 
    # Size for remaining random sampling
    remain.i <- n0 %% ni 
    dvis.i <- dvis[dvis$docvis==i,] 
    samp.i <- sample_n(dvis.i,remain.i,replace=FALSE)
    dvis.b <- rbind(dvis.b, # Balanced in the previous step
                    slice(dvis.i,rep(1:n(),each=time.i)),
                    samp.i)
}

# By now, the data set dvis.b contains equally number of samples 
# of each value of docvis.



######################################################
################## Model Selection ###################
######################################################

# In the report this is called M0 for simplicity
dv.glm0 <- glm(docvis ~ female + age + hhninc + hhkids + educyrs + 
               eductype + married + employed + privateins + addins,
               data = dvis.b, family=poisson())
summary(dv.glm0)

## The p-value of the Wald statistic of eductype 1 is not significant,
## so I want to test if the nested model putting educational types 0 
## and 1 together can be accepted by a LRT.

new.level <- c(1,1,2,3,4,5)
etype <- factor(new.level[dvis.b$eductype])
dvis.new <- cbind(select(dvis.b,-c('eductype')),etype)

## Add omitted variables in glm1 onto glm1a
# In the report this is called M1 for simplicity
dv.glm1a <- glm(docvis ~ female + age + hhninc + hhkids + etype + 
                married + educyrs + employed + privateins + addins, 
                family = poisson(), data = dvis.new)

## Deviance test on glm1a over glm0
1-pchisq(deviance(dv.glm1a)-deviance(dv.glm0),
         df=df.residual(dv.glm1a)-df.residual(dv.glm0)) # 0.1157 
# Insignificant, so we accept the nested model treating two of the 
# schooling types equally.

summary(dv.glm1a)

# Although removing one level of education type does not produce 
# a very significant LRT statistic, the Wald statistic of educyrs 
# has now become insignificant. Therefore I fit another model 
# excluding it.

# In the report this is called M2 for simplicity
dv.glm1b <- glm(docvis ~ female + age + hhninc + hhkids + etype + 
                married + employed + privateins + addins, 
                family = poisson(), data = dvis.new)

## Deviance test on glm1a over glm0
1-pchisq(deviance(dv.glm1b)-deviance(dv.glm1a),
         df=df.residual(dv.glm1b)-df.residual(dv.glm1a)) # 0.10

summary(dv.glm1b)

# The test statistic is again insignificant, so I decide to drop 
# educyrs. The remaining variables will all be included in the 
# model as main effects.

### Models with scripts 2 involve interactions
# In the report this is called P0 for simplicity
dv.glm2 <- glm(docvis ~(female + age + hhninc + hhkids + etype + 
               married + employed + privateins + addins)^2, 
               family = poisson(), data = dvis.new)
summary(dv.glm2)
# This can be considered as a full model since all two-way 
# interactions are involved in the model. However, in the model 
# summary, many Wald statistics are insignificant, including 
# those of the main effects which were once significant when 
# interactions were not present in the model.

step(dv.glm2) # gives dv.glm2a
# In the report this is called P1 for simplicity
dv.glm2a <- glm(docvis ~ female + age + hhninc + hhkids + etype + 
                married + employed + privateins + addins + female:age + 
                female:hhninc + female:hhkids + female:etype + 
                female:privateins + female:addins + age:hhkids + 
                age:etype + age:married + age:addins + hhninc:etype + 
                hhninc:employed + hhninc:addins + hhkids:etype + 
                hhkids:privateins + etype:married + etype:employed + 
                etype:privateins + etype:addins + married:employed + 
                married:addins + employed:privateins + employed:addins, 
                family = poisson(), data = dvis.new)


# Chi square test on omitting the interactions in dv.glm2
1-pchisq(deviance(dv.glm2a)-deviance(dv.glm2),
         df=df.residual(dv.glm2a)-df.residual(dv.glm2))
# gives 0.98 which suggests that the nested model glm2a is
# accepted by LRT

summary(dv.glm2a)
# From the summary of this nested model, some effects are 
# still doubtful, especially like etype:addins whose p values 
# of Wald statistics are fairly large. I decide to fit another 
# model excluding these effects and see whether the nested model 
# can be accepted by a LRT.

# In the report this is called P2 for simplicity
dv.glm2b <- glm(docvis ~ female + age + hhninc + hhkids + etype + 
                married + employed + privateins + addins + female:age + 
                female:hhninc + female:hhkids + female:etype + 
                female:privateins + female:addins + age:hhkids + 
                age:etype + age:married + age:addins + hhninc:etype + 
                hhninc:employed + hhninc:addins + hhkids:etype + 
                etype:married + etype:employed + etype:privateins + 
                etype:addins + married:employed + married:addins + 
                employed:privateins + employed:addins, 
                family = poisson(), data = dvis.new)

anova(dv.glm2b,dv.glm2a, test='Chisq') 
# 0.091, insignificant, so drop hhkids:privateins from model

summary(dv.glm2b)

# From the model summary, it can be observed that there no 
# longer exist insignificant interactions in the model, and 
# I shall stop my model selection here.

######################################################
############### Assess Goodness of Fit ###############
######################################################

require('rsq')
rsq.kl(dv.glm2b)

pchisq(deviance(dv.glm2b),df.residual(dv.glm2b),lower=FALSE)
# LRT against the saturated model fails

n.total <- nrow(dvis.new)
n <- nrow(dvis)
p <- length(dv.glm2b$coefficients)

index <- rownames(unique(dvis.new))

sum(abs(rstandard(dv.glm2b,type='pearson'))[index]>=2) /n
# Approximately 8% samples have standardised Pearson residual 
# greater than 2, which suggests that the goodness of fit is 
# not bad, but we would expect better performance.

#### Diagnostics
## Q-Q plot
qqnorm(rstandard(dv.glm2b,type='pearson')[index], 
       pch=19, cex.lab = 1.5,
       main="QQplot of Standardised Pearson Residuals")
qqline(rstandard(dv.glm2b,type='pearson')[index])

# Residuals vs Fitted
plot(predict(dv.glm2b,type='response')[index],
     rstandard(dv.glm2b,type='pearson')[index], 
     pch=1, cex.lab = 1.5, xlab='Fitted values',
     ylab='Standardised residuals', main='Residuals vs Fitted')

## Leverage
plot(influence(dv.glm2b)$hat[index], cex.lab=1.5, 
     main='Leverage for the samples',ylab='Leverage')
abline(h=2*p/n,lty=2,lwd=2,col='red')
abline(h=2*p/n.total,lty=2,lwd=2,col='blue')

# Cook's distances
plot(cooks.distance(dv.glm2b)[index],ylim=c(0,0.04), cex.lab=1.5,
     pch=1, ylab="Cook's Distance",
     main="Cook's Distances of the samples")
abline(h=8/(n-2*p),lty=2,lwd=2,col='red')
abline(h=8/(n.total-2*p),lty=2,lwd=2,col='blue')

# Table of potential outliers
out <- which(cooks.distance(dv.glm2b)[index]>8/(n-2*p))
out.table <- data.frame('Response' = dvis.new$docvis[out],
        'Fitted' = predict(dv.glm2b,type='response')[out],
        'standardised residual'=rstandard(dv.glm2b,type='pearson')[out],
        'leverage'=influence(dv.glm2b)$hat[out],
        'Cooks distance'=cooks.distance(dv.glm2b)[out])
View(out.table)

# Take a look at the summarising table of the samples whose Cook's
# Distances are higher than the rule of thumb. Notice that all those
# samples are of response value 0, and they all have large positive
# fitted values, resulting in their large residuals.
# Some of them show moderate levels of misfit, such as samples
# #54, 150 and 199. Their standardised residuals are not very extreme 
# (between pm2). However, other samples, especially #81, 181 and 173, 
# have very large fitted values and Cook's distances. 

View(dvis.new[out,])

######################################################
############# Estimating the Dispersion ##############
######################################################

sum(residuals(dv.glm2b,type='pearson')^2)/df.residual(dv.glm2b)
