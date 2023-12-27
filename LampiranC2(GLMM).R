#GLMM Poisson CLAIM FREQ
pois <- glmer(ClaimNb ~ (1|VehBody) + DrivAge 
              + offset(log(Exposure)), family = poisson(link = "log"), data = training, nAGQ = 25) 
summary(pois)
predps = predict(pois, newdata = testing, type = "link") 
muhatps = exp(predps)

library(lme4)
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

#GLMM Negatif Binomial CLAIM FREQ
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

#GLMM Inverse Gaussian CLAIM AMOUNT BARU
library(lme4)
inversegaus = glmer(ClaimAmountBaru ~ (1|VehBody)+VehAge+Gender+DrivAge, family = inverse.gaussian(link = "identity"), data = training, offset = log(Exposure), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))
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

#GLMM Gamma CLAIM AMOUNT BARU
library(lme4)
gam1 <- glmer(ClaimAmountBaru ~ (1|VehBody)  + DrivAge
              , family = Gamma(link = "log"), data = training, nAGQ = 25, offset = log(Exposure), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))
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

