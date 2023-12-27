#GLM Pois - GLM Gamma
Pr1 = mhatpois*mugam
mean((testing$ClaimAmount-Pr1)^2)
sum(abs(testing$ClaimAmount-Pr1))/13571

#GLM Pois - GLM Inv Gauss
Pr2 = mhatpois*muhatgs
mean((testing$ClaimAmount-Pr2)^2)
sum(abs(testing$ClaimAmount-Pr2))/13571

#GLM Negbin - GLM Gamma
Pr3 = muhatnegbin*mugam
mean((testing$ClaimAmount-Pr3)^2)
sum(abs(testing$ClaimAmount-Pr3))/13571

#GLM Negbin - GLM Inv Gauss
Pr4 = muhatnegbin*muhatgs
mean((testing$ClaimAmount-Pr4)^2)
sum(abs(testing$ClaimAmount-Pr4))/13571

#GLM Binom - GLM Gamma 
Pr5 = muhatbinom*mugam1
mean((testing$ClaimAmount-Pr5)^2)
sum(abs(testing$ClaimAmount-Pr5))/13571

#GLM Binom - GLM inverse gauss 
Pr6 = muhatbinom*muhatgs1
mean((testing$ClaimAmount-Pr6)^2)
sum(abs(testing$ClaimAmount-Pr6))/13571

#---------------------------------------------
#GLMM Pois - GLMM Gamma
Pr7 = muhatps*mugamma1
mean((testing$ClaimAmount-Pr7)^2)
sum(abs(testing$ClaimAmount-Pr7))/13571

#GLMM Pois - GLMM Inv Gauss
Pr8 = muhatps*muhatinversegaus
mean((testing$ClaimAmount-Pr8)^2)
sum(abs(testing$ClaimAmount-Pr8))/13571

#GLMM Negbin - GLMM Gamma
Pr9 = muhatnb*mugamma1
mean((testing$ClaimAmount-Pr9)^2)
sum(abs(testing$ClaimAmount-Pr9))/13571

#GLMM Negbin - GLMM Inv Gauss
Pr10 = muhatnb*muhatinversegaus
mean((testing$ClaimAmount-Pr10)^2)
sum(abs(testing$ClaimAmount-Pr10))/13571

#GlMM Binom - GLMM Gamma 
Pr11 = muhatbnm*mugm1
mean((testing$ClaimAmount-Pr11)^2)
sum(abs(testing$ClaimAmount-Pr11))/13571

#GLMM Binom - GLMM inverse gauss 
Pr12 = muhatbnm*muhatinversegaus1
mean((testing$ClaimAmount-Pr12)^2)
sum(abs(testing$ClaimAmount-Pr12))/13571

#---------------------------------------------
#GLMM Pois - GLM Gamma
Pr13 = muhatps*mugam
mean((testing$ClaimAmount-Pr13)^2)
sum(abs(testing$ClaimAmount-Pr13))/13571

#GLMM Negbin - GLM Gamma
Pr14 = muhatnb*mugam
mean((testing$ClaimAmount-Pr14)^2)
sum(abs(testing$ClaimAmount-Pr14))/13571

#GLMM binom - GLM Gamma
Pr15 = muhatbnm*mugam1
mean((testing$ClaimAmount-Pr15)^2)
sum(abs(testing$ClaimAmount-Pr15))/13571

#GLMM Pois - GLM Inv Gaus
Pr16 = muhatps*muhatgs
mean((testing$ClaimAmount-Pr16)^2)
sum(abs(testing$ClaimAmount-Pr16))/13571

#GlMM Negbin - GLM Inv gaus 
Pr17 = muhatnb*muhatgs
mean((testing$ClaimAmount-Pr17)^2)
sum(abs(testing$ClaimAmount-Pr17))/13571

#GLMM Binom - GLM inverse gauss 
Pr18 = muhatbnm*muhatgs1
mean((testing$ClaimAmount-Pr18)^2)
sum(abs(testing$ClaimAmount-Pr18))/13571

#---------------------------------------------
#GLM Pois - GLMM Gamma
Pr19 = mhatpois*mugamma1
mean((testing$ClaimAmount-Pr19)^2)
sum(abs(testing$ClaimAmount-Pr19))/13571

#GLM Negbin - GLMM Gamma
Pr20 = muhatnegbin*mugamma1
mean((testing$ClaimAmount-Pr20)^2)
sum(abs(testing$ClaimAmount-Pr20))/13571

#GLM binom - GLMM Gamma
Pr21 = muhatbinom*mugm1
mean((testing$ClaimAmount-Pr21)^2)
sum(abs(testing$ClaimAmount-Pr21))/13571

#GLM Pois - GLMM Inv Gaus
Pr22 = mhatpois*muhatinversegaus
mean((testing$ClaimAmount-Pr22)^2)
sum(abs(testing$ClaimAmount-Pr22))/13571

#GlM Negbin - GLMM Inv gaus 
Pr23 = muhatnegbin*muhatinversegaus
mean((testing$ClaimAmount-Pr23)^2)
sum(abs(testing$ClaimAmount-Pr23))/13571

#GLM Binom - GLMM inverse gauss 
Pr24 = muhatbinom*muhatinversegaus1
mean((testing$ClaimAmount-Pr24)^2)
sum(abs(testing$ClaimAmount-Pr24))/13571

library(dplyr)
group1 = testing$ClaimAmount
group2 = Pr5
group3 = Pr8
group4 = Pr13
group5 = Pr24
data <- list(group1, group2, group3, group4, group5)
boxplot(data,
        col = c("grey", "grey", "grey", "grey", "grey"),  # Set box colors
        border = c("black", "black", "black", "black", "black"),                # Set border colors
        names = c("Grup 1", "Grup 2", "Grup 3", "Grup 4", "Grup 5"),       # Set group names
        main = "Perbandingan Data Asli dengan Hasil Model Terbaik bagi Seluruh Data",
        ylab = "ClaimAmount" ,ylim=c(0,1000))

means <- sapply(data, mean)
points(1:length(data), means, col = "red", pch = 18, cex = 2)
text(1:length(data), means + 20, round(means, 2), col = "red", pos = 3)

