options(digits = 5)
library(MASS) # for boxcox()
library(readr)

dvis <- read_csv("dvis.csv")
str(dvis)

######################################################
################# Data Exploration ###################
######################################################

csteel <- "#B0C4DE"
cmint <- "#F5FFFA"
corange <- "#FF8C00"
cgold <- "#FFD700"
ckhaki <- "#F0E68C"
clime<- "#32CD32"
ccyan <- "#00FFFF"
cblue <- "#00BFFF"
cviolet <- "#EE82EE"
cpink <- "#FFB6C1"
ccol <- c(csteel, cmint, corange, cgold, ckhaki, clime, 
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

plot(jitter(docvis) ~ age, data=dvis,pch=16)

dvis.fcount <- table(dvis$docvis[dvis$female==1])
dvis.mcount <- table(dvis$docvis[dvis$female==0])
barplot(rbind(dvis.mcount,dvis.fcount),beside=T,xlab='visits',ylab='Frequency',
        col=1:2,main='Number of visits by gender')
legend('topright',c('male','female'),fill=1:2)

dvis.k0count <- table(dvis$docvis[dvis$hhkids==0])
dvis.k1count <- table(dvis$docvis[dvis$hhkids==1])
barplot(rbind(dvis.k0count,dvis.k1count),beside=T,xlab='visits',ylab='Frequency',
        col=3:4,main='Number of visits by whether \n under the age of 16 children present')
legend('topright',c('Absent','Present'),fill=3:4)

dvis.m0count <- table(dvis$docvis[dvis$married==0])
dvis.m1count <- table(dvis$docvis[dvis$married==1])
barplot(rbind(dvis.m0count,dvis.m1count),beside=T,xlab='visits',ylab='Frequency',
        col=5:6,main='Number of visits by marriage condition')
legend('topright',c('unmarried','married'),fill=5:6)

plot(jitter(docvis) ~ jitter(hhninc), data=dvis,pch=16)

plot(jitter(docvis) ~ jitter(educyrs), data=dvis,pch=16,col=etcol)
legend('topleft',c('0','1','2','3','4','5'),col = 3:8, pch = 16,title='Educational Type')


######################################################
######### Transform to Categorical Variables #########
######################################################

dvis$female <- as.factor(dvis$female)
dvis$hhkids <- as.factor(dvis$hhkids)
dvis$married <- as.factor(dvis$married)
dvis$employed <- as.factor(dvis$employed)
dvis$privateins <- as.factor(dvis$privateins)
dvis$addins <- as.factor(dvis$addins)
dvis$eductype <- as.factor(dvis$eductype)

plot(dvis$docvis)


######################################################
################## Model Selection ###################
######################################################

dv.glm0 <- glm(docvis~.,data=dvis,family=poisson())
summary(dv.glm0)

dv.glmfull <- glm(docvis~.*.,data=dvis,family=poisson())
summary(dv.glmfull)

dv.glm1 <- glm(docvis~female+age+hhninc+hhkids+addins,data=dvis,family=poisson())
summary(dv.glm1)

## Deviance test on glm1 over glm0
1-pchisq(deviance(dv.glm1)-deviance(dv.glm0),
         df=df.residual(dv.glm1)-df.residual(dv.glm0)) # = 0.17617

dv.glm2 <- glm(docvis~(female+age+hhninc+hhkids+addins)^2,data=dvis,family=poisson())
summary(dv.glm2)
