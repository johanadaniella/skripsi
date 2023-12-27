setwd("~/Desktop/Tugas Akhir 1/TA 1/CASdatasets/data")
load("~/Desktop/Tugas Akhir 1/TA 1/CASdatasets/data/ausprivauto0405.rda")
data = ausprivauto0405

ClaimNb <- ausprivauto0405$ClaimNb
VehValue <- ausprivauto0405$VehValue
VehAge <- ausprivauto0405$VehAge
VehBody <- ausprivauto0405$VehBody
Gender <- ausprivauto0405$Gender
DrivAge <- ausprivauto0405$DrivAge
ClaimOcc <- ausprivauto0405$ClaimOcc
ClaimAmount <- ausprivauto0405$ClaimAmount

#membuat variabel Claim Amount baru dengan rumus claim amount/claim nb 
ClaimAmountBaru = NULL 
for (i in 1:67856){
  if (ClaimAmount[i] > 0){
    ClaimAmountBaru[i] = ClaimAmount[i]/ClaimNb[i]
  } else {
    ClaimAmountBaru[i] = 0
  }
}

ausprivauto0405 = mutate(ausprivauto0405, ClaimAmountBaru)

set.seed(777)
training_index <- createDataPartition(ausprivauto0405$ClaimNb, p = 0.8, list = FALSE)
training <- ausprivauto0405[training_index, ]
testing <- ausprivauto0405[-training_index, ]
training01 <- subset(training, ClaimAmount != 0)

#GLM Poisson CLAIM FREQ
poisfreq = glm(ClaimNb~VehValue+VehAge+VehBody+Gender+DrivAge, family = poisson(link = "log"), data = training, offset = log(Exposure))
summary(poisfreq)
predpois = predict(poisfreq, newdata = testing, type = "link") 
mhatpois = exp(predpois)

#GLM Negative Binom CLAIM FREQ
library(MASS)
nbfreq = glm.nb(ClaimNb~VehValue+VehAge+VehBody+Gender+DrivAge, link = log, data = training, offset = log(Exposure)) 
summary(nbfreq)
prednegbin = predict(nbfreq, newdata = testing, type = "link") 
muhatnegbin = exp(prednegbin)

#GLM Binomial CLAIM OCC
binom = glm(ClaimOcc~VehValue+VehAge+VehBody+Gender+DrivAge, family = binomial(link = "logit"), data = training, offset = log(Exposure))
summary(binom)
predbinom = predict(binom, newdata = testing, type = "link") 
muhatbinom = (exp(predbinom)/(1+exp(predbinom)))

#GLM Inverse Gaussian CLAIM AMOUNT BARU
invgs = glm(ClaimAmountBaru~VehAge+VehBody+Gender+DrivAge, family = inverse.gaussian(link = "identity"), data = training01, offset = log(Exposure))
summary(invgs)
predgss = predict(invgs, newdata = testing, type = "link") 
muhatgs = predgss

#GLM Inverse Gaussian CLAIM AMOUNT 
invgs1 = glm(ClaimAmount~VehValue+VehAge+VehBody+Gender+DrivAge, family = inverse.gaussian(link = "identity"), data = training01, offset = log(Exposure),  control = glm.control(maxit = 10000)) 
summary(invgs1)
predgss1 = predict(invgs1, newdata = testing, type = "link") 
muhatgs1 = predgss1

#GLM Gamma CLAIM AMOUNT BARU
gamsev = glm(ClaimAmountBaru~VehValue+VehAge+VehBody+Gender+DrivAge, family = Gamma(link = "log"), data = training01)
summary(gamsev)
prdgam = predict(gamsev, newdata = testing, type = "link") 
mugam = exp(prdgam)

#GLM Gamma CLAIM AMOUNT  
gsev = glm(ClaimAmount~VehValue+VehAge+VehBody+Gender+DrivAge, family = Gamma(link = "log"), data = training01)
summary(gsev)
prdgam1 = predict(gsev, newdata = testing, type = "link") 
mugam1 = exp(prdgam1)

#pembuktian kemiripan mean testing training untuk menjawab setseed
mean(training$ClaimAmount)
mean(testing$ClaimAmount)
