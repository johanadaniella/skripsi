#GLMM Poisson CLAIM FREQ
library(lme4)
pois <- glmer(ClaimNb ~ (1|VehBody) + VehValue + VehAge + Gender + DrivAge 
              + offset(log(Exposure)), family = poisson(link = "log"), data = training, nAGQ = 25) 
summary(pois)
predps = predict(pois, newdata = testing, type = "link") 
muhatps = exp(predps)

## get random intercepts
int <- ranef(pois)$VehBody
## get prediction intervals for r.e.’s
str(rr1 <- ranef(pois, condVar = TRUE))
my.se.risk = sqrt(as.numeric(attributes(rr1$VehBody)$postVar))
# get prediction intervals for random intercepts (per riskclass)
lower.risk <- rr1$VehBody[[1]]-1.96*my.se.risk
upper.risk <- rr1$VehBody[[1]]+1.96*my.se.risk
int.risk <- cbind((lower.risk),(rr1$VehBody[[1]]),(upper.risk))
colnames(int.risk)  <- c("Lower","Estimate R.E.","Upper")
int.risk
#variance
VarCorr(pois)

#GLMM Negative Binomial CLAIM FREQ
library(MASS)
nb <- glmer.nb(ClaimNb ~ (1|VehBody) + Gender 
               + offset(log(Exposure)), data = training, nAGQ = 25)
summary(nb)
prednb = predict(nb, newdata = testing, type = "link") 
muhatnb = exp(prednb)

## get random intercepts
int <- ranef(nb)$VehBody
## get prediction intervals for r.e.’s
str(rr1 <- ranef(nb, condVar = TRUE))
my.se.risk = sqrt(as.numeric(attributes(rr1$VehBody)$postVar))
# get prediction intervals for random intercepts (per riskclass)
lower.risk <- rr1$VehBody[[1]]-1.96*my.se.risk
upper.risk <- rr1$VehBody[[1]]+1.96*my.se.risk
int.risk <- cbind((lower.risk),(rr1$VehBody[[1]]),(upper.risk))
colnames(int.risk)  <- c("Lower","Estimate R.E.","Upper")
int.risk
#variance
VarCorr(nb)

#GLMM Binomial CLAIM OCC
library(lme4)
bnm = glmer(ClaimOcc ~ (1|VehBody) + VehValue+VehAge+Gender+DrivAge, family = binomial(link = "logit"), data = training, offset = log(Exposure))
summary(bnm)
predbnm = predict(bnm, newdata = testing, type = "link") 
muhatbnm = (exp(predbnm)/(1+exp(predbnm)))

## get random intercepts
int <- ranef(bnm)$VehBody
## get prediction intervals for r.e.’s
str(rr1 <- ranef(bnm, condVar = TRUE))
my.se.risk = sqrt(as.numeric(attributes(rr1$VehBody)$postVar))
# get prediction intervals for random intercepts (per riskclass)
lower.risk <- rr1$VehBody[[1]]-1.96*my.se.risk
upper.risk <- rr1$VehBody[[1]]+1.96*my.se.risk
int.risk <- cbind((lower.risk),(rr1$VehBody[[1]]),(upper.risk))
colnames(int.risk)  <- c("Lower","Estimate R.E.","Upper")
int.risk
#variance
VarCorr(bnm)

#GLMM Inverse Gaussian CLAIM AMOUNT BARU 
library(lme4)
inversegaus = glmer(ClaimAmountBaru ~ (1|VehBody) +VehAge+Gender+DrivAge, family = inverse.gaussian(link = "identity"), data = training01, offset = log(Exposure), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))
summary(inversegaus)
predinversegauss = predict(inversegaus, newdata = testing, type = "link") 
muhatinversegaus = predinversegauss

## get random intercepts
int <- ranef(inversegaus)$VehBody
## get prediction intervals for r.e.’s
str(rr1 <- ranef(inversegaus, condVar = TRUE))
my.se.risk = sqrt(as.numeric(attributes(rr1$VehBody)$postVar))
# get prediction intervals for random intercepts (per riskclass)
lower.risk <- rr1$VehBody[[1]]-1.96*my.se.risk
upper.risk <- rr1$VehBody[[1]]+1.96*my.se.risk
int.risk <- cbind((lower.risk),(rr1$VehBody[[1]]),(upper.risk))
colnames(int.risk)  <- c("Lower","Estimate R.E.","Upper")
int.risk
#variance
VarCorr(inversegaus)

#GLMM Inverse Gaussian CLAIM AMOUNT 
library(lme4)
inversegaus1 = glmer(ClaimAmount ~ (1|VehBody) +VehValue+VehAge+Gender+DrivAge, family = inverse.gaussian(link = "identity"), data = training01, offset = log(Exposure), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))
summary(inversegaus1)
predinversegauss1 = predict(inversegaus1, newdata = testing, type = "link") 
muhatinversegaus1 = predinversegauss1

## get random intercepts
int <- ranef(inversegaus1)$VehBody
## get prediction intervals for r.e.’s
str(rr1 <- ranef(inversegaus1, condVar = TRUE))
my.se.risk = sqrt(as.numeric(attributes(rr1$VehBody)$postVar))
# get prediction intervals for random intercepts (per riskclass)
lower.risk <- rr1$VehBody[[1]]-1.96*my.se.risk
upper.risk <- rr1$VehBody[[1]]+1.96*my.se.risk
int.risk <- cbind((lower.risk),(rr1$VehBody[[1]]),(upper.risk))
colnames(int.risk)  <- c("Lower","Estimate R.E.","Upper")
int.risk
#variance
VarCorr(inversegaus1)

#GLMM Gamma CLAIM AMOUNT BARU
library(lme4)
gam1 <- glmer(ClaimAmountBaru ~ (1|VehBody)  + Gender + DrivAge
              , family = Gamma(link = "log"), data = training01, nAGQ = 25, offset = log(Exposure))
summary(gam1)
pdgamma1 = predict(gam1, newdata = testing, type = "link") 
mugamma1 = exp(pdgamma1)

## get random intercepts
int <- ranef(gam1)$VehBody
## get prediction intervals for r.e.’s
str(rr1 <- ranef(gam1, condVar = TRUE))
my.se.risk = sqrt(as.numeric(attributes(rr1$VehBody)$postVar))
# get prediction intervals for random intercepts (per riskclass)
lower.risk <- rr1$VehBody[[1]]-1.96*my.se.risk
upper.risk <- rr1$VehBody[[1]]+1.96*my.se.risk
int.risk <- cbind((lower.risk),(rr1$VehBody[[1]]),(upper.risk))
colnames(int.risk)  <- c("Lower","Estimate R.E.","Upper")
int.risk
#variance
VarCorr(gam1)

#GLMM Gamma CLAIM AMOUNT
gm <- glmer(ClaimAmount ~ (1|VehBody)  + Gender + DrivAge
            , family = Gamma(link = "log"), data = training01, nAGQ = 25, offset = log(Exposure))
summary(gm)
pdgm1 = predict(gm, newdata = testing, type = "link") 
mugm1 = exp(pdgm1)

## get random intercepts
int <- ranef(gm)$VehBody
## get prediction intervals for r.e.’s
str(rr1 <- ranef(gm, condVar = TRUE))
my.se.risk = sqrt(as.numeric(attributes(rr1$VehBody)$postVar))
# get prediction intervals for random intercepts (per riskclass)
lower.risk <- rr1$VehBody[[1]]-1.96*my.se.risk
upper.risk <- rr1$VehBody[[1]]+1.96*my.se.risk
int.risk <- cbind((lower.risk),(rr1$VehBody[[1]]),(upper.risk))
colnames(int.risk)  <- c("Lower","Estimate R.E.","Upper")
int.risk
#variance
VarCorr(gm)

