#GLM Pois - GLM Gamma
Pr1 = mhatpois*mugam
mean((testing$ClaimAmount-Pr1)^2)
sum(abs(testing$ClaimAmount-Pr1))/924

#GLM Pois - GLM Inv Gauss
Pr2 = mhatpois*muhatgs
mean((testing$ClaimAmount-Pr2)^2)
sum(abs(testing$ClaimAmount-Pr2))/924

#GLM Negbin - GLM Gamma
Pr3 = muhatnegbin*mugam
mean((testing$ClaimAmount-Pr3)^2)
sum(abs(testing$ClaimAmount-Pr3))/924

#GLM Negbin - GLM Inv Gauss
Pr4 = muhatnegbin*muhatgs
mean((testing$ClaimAmount-Pr4)^2)
sum(abs(testing$ClaimAmount-Pr4))/924

#---------------------------------------------
#GLMM Pois - GLMM Gamma
Pr7 = muhatps*mugamma1
mean((testing$ClaimAmount-Pr7)^2)
sum(abs(testing$ClaimAmount-Pr7))/924

#GLMM Pois - GLMM Inv Gauss
Pr8 = muhatps*muhatinversegaus
mean((testing$ClaimAmount-Pr8)^2)
sum(abs(testing$ClaimAmount-Pr8))/924

#GLMM Negbin - GLMM Gamma
Pr9 = muhatnb*mugamma1
mean((testing$ClaimAmount-Pr9)^2)
sum(abs(testing$ClaimAmount-Pr9))/924

#GLMM Negbin - GLMM Inv Gauss
Pr10 = muhatnb*muhatinversegaus
mean((testing$ClaimAmount-Pr10)^2)
sum(abs(testing$ClaimAmount-Pr10))/924

#---------------------------------------------
#GLMM Pois - GLM Gamma
Pr13 = muhatps*mugam
mean((testing$ClaimAmount-Pr13)^2)
sum(abs(testing$ClaimAmount-Pr13))/924

#GLMM Negbin - GLM Gamma
Pr14 = muhatnb*mugam
mean((testing$ClaimAmount-Pr14)^2)
sum(abs(testing$ClaimAmount-Pr14))/924

#GLMM Pois - GLM Inv Gaus
Pr16 = muhatps*muhatgs
mean((testing$ClaimAmount-Pr16)^2)
sum(abs(testing$ClaimAmount-Pr16))/924

#GlMM Negbin - GLM Inv gaus 
Pr17 = muhatnb*muhatgs
mean((testing$ClaimAmount-Pr17)^2)
sum(abs(testing$ClaimAmount-Pr17))/924

#---------------------------------------------
#GLM Pois - GLMM Gamma
Pr19 = mhatpois*mugamma1
mean((testing$ClaimAmount-Pr19)^2)
sum(abs(testing$ClaimAmount-Pr19))/924

#GLM Negbin - GLMM Gamma
Pr20 = muhatnegbin*mugamma1
mean((testing$ClaimAmount-Pr20)^2)
sum(abs(testing$ClaimAmount-Pr20))/924

#GLM Pois - GLMM Inv Gaus
Pr22 = mhatpois*muhatinversegaus
mean((testing$ClaimAmount-Pr22)^2)
sum(abs(testing$ClaimAmount-Pr22))/924

#GlM Negbin - GLMM Inv gaus 
Pr23 = muhatnegbin*muhatinversegaus
mean((testing$ClaimAmount-Pr23)^2)
sum(abs(testing$ClaimAmount-Pr23))/924

group1 = testing$ClaimAmount
group2 = Pr3
group3 = Pr4
group4 = Pr8
group5 = Pr13
group6 = Pr23
data <- list(group1, group2, group3, group4, group5, group6) 

library(dplyr)
boxplot(data,
        col = c("grey", "grey", "grey", "grey", "grey","grey"),  # Set box colors
        border = c("black", "black", "black", "black", "black", "black"),                # Set border colors
        names = c("Grup 1", "Grup 2", "Grup 3", "Grup 4", "Grup 5", "Grup 6"),       # Set group names
        main = "Perbandingan Data Asli dengan Hasil Model Terbaik bagi Seluruh Data",
        ylab = "ClaimAmount" ,ylim=c(0,10000))

means <- sapply(data, mean)
points(1:length(data), means, col = "red", pch = 18, cex = 2)
text(1:length(data), means + 20, round(means, 2), col = "red", pos = 3)

