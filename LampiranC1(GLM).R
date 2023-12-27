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
 
#subset data 
datapositive <- subset(ausprivauto0405, ClaimAmount != 0)
#Pembagian data 
set.seed(777)
train_index <- createDataPartition(datapositive$ClaimNb, p = 0.8, list = FALSE)
training <- datapositive[train_index, ]
testing <- datapositive[-train_index, ]

#GLM Poisson CLAIM FREQ
psfreq = glm(ClaimNb~VehValue+VehAge+VehBody+Gender+DrivAge, family = poisson(link = "log"), data = training, offset = log(Exposure))
summary(psfreq)
predpois = predict(psfreq, newdata = testing, type = "link") 
mhatpois = exp(predpois)

#GLM Negatif Binomial CLAIM FREQ
nbfreq = glm.nb(ClaimNb ~ VehValue + VehAge + DrivAge + VehBody + Gender, link = log, data = training)
summary(nbfreq)
prednegbin = predict(nbfreq, newdata = testing, type = "link") 
muhatnegbin = exp(prednegbin)

#GLM Inverse Gaussian CLAIM AMOUNT BARU
invgs = glm(ClaimAmountBaru~VehAge+VehBody+Gender+DrivAge, family = inverse.gaussian(link = "identity"), data = training, offset = log(Exposure), control = glm.control(maxit = 10000))
summary(invgs)
predgss = predict(invgs, newdata = testing, type = "link") 
muhatgs = predgss

#GLM Gamma CLAIM AMOUNT BARU
gamsev = glm(ClaimAmountBaru~VehValue+VehAge+VehBody+Gender+DrivAge, family = Gamma(link = "log"), data = training)
summary(gamsev)
prdgam = predict(gamsev, newdata = testing, type = "link") 
mugam = exp(prdgam)


