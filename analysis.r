###### Packages ######
library(psych)
library(vegan)
library(regclass)
library(lme4)
library(factoextra)
library(MASS)
library(car)
library(heatmaply)
library(fitdistrplus)
library(multcomp)
library(pscl)
library(dplyr)
library(r2glmm)
library(sjPlot)

###### Data ######
dados_TUDO <- read.table("Data.txt", h=T)
head(dados_TUDO)
summary(dados_TUDO)

##### Geral ####
levels(as.factor(dados_TUDO$Corte))
length(levels(as.factor(dados_TUDO$Corte)))
levels(as.factor(dados_TUDO$display))
length(levels(as.factor(dados_TUDO$display)))

dado_media <- dados_TUDO %>%
  group_by(Corte) %>%
  summarise(n = n(), N_machos = mean(N_machos)) 

mean(dado_media$n)
sd(dado_media$n)
mean(dado_media$N_machos)
sd(dado_media$N_machos)

mean(dados_freq_mod$mean)
sd(dados_freq_mod$mean)
max(dados_freq_mod$mean)
min(dados_freq_mod$mean)

##### Normalidade ####
colnames(dados_TUDO)

dados<-dados_TUDO[c(1:5, 8, 11, 18:20, 24:25)]
head(dados)
colnames(dados)

par(mfrow=c(4,2))

test_norm <- data.frame(Parametro = (NULL),
                        p.value = (NULL))

for (i in colnames(dados)[4:12]){
  j <- subset(dados, select = i)
  hist(j[,1], xlab = i, main = NA)
  print(i)
  test <- data.frame(p.value = round(shapiro.test(j[,1])$p.value, 5))
  test_1 <-  data.frame(Parametro = i)
  test_2 <- cbind(test_1, test) 
  test_norm <- rbind(test_norm, test_2)
}

test_norm

par(mfrow=c(1,1))

##### Correlacao ####
colnames(dados_TUDO)
dados<-dados_TUDO[c(1:5, 8, 11, 18:20, 24:25)]
head(dados)

colnames(dados) <- c("display", "Corte", "Poleiro", "RR", "DET", "DLEntr", "LAM","MCEntr", "r", "No_males", "Cop_rate", "Fem_rate")    

colnames(dados)

part_1 <- as.matrix(cbind(dados[c(4:12)])) 
head(part_1)
part_2 <- as.matrix(cbind(dados[-c(4:12)])) 
head(part_2)
part_3 <- as.matrix(cbind(dados[c(12:13)])) 
head(part_3)
part_1_1 <- as.matrix(cbind(dados[c(4:12)])) 
head(part_1_1)

para= data.frame(part_1)
cor(para, method = "spearman")

para= data.frame(part_3)
cor(para, method = "spearman")
cor.test(para$Copulation, para$Female, method = "spearman")

##### Colinearidade ####
colnames(dados)
model_test <- glm(No_males ~ RR+DET + DLEntr + LAM + SVector + EPSVector, 
                  data = dados,  family = "poisson")
VIF(model_test)

###### Testes ####
##### GLMM ####
#### Frequency (No significative)####
head(dados_freq_mod)
colnames(dados_freq_mod)

### Number of male ####
head(dados_freq_mod)
colnames(dados_freq_mod)

hist(dados_freq_mod$mean)
shapiro.test(dados_freq_mod$mean)

raster::cv(dados_freq_mod$mean)

modelo_freq <- lmer(mean ~ N_machos + (1|Corte), data = dados_freq_mod)
summary(modelo_freq)
Anova(modelo_freq)

### Visitation ####
modelo_freq_visit <- lmer(fem_hora ~ mean + (1|Corte), data = dados_freq_mod)
summary(modelo_freq_visit)
Anova(modelo_freq_visit)
shapiro.test(resid(modelo_freq_visit))

### Copulation ####
modelo_freq_cop_zero <- zeroinfl(as.numeric(as.factor(dados_freq_mod$cop_hora))-1 ~ mean, dist = "negbin", data = dados_freq_mod)
summary(modelo_freq_cop_zero)
Anova(modelo_freq_cop_zero)
shapiro.test(resid(modelo_freq_cop_zero))
plot(resid(modelo_freq_cop_zero))

#### Recurrence Parameters ~ Male Number ####
dados<-dados_TUDO[c(1:5, 8, 11, 18:20, 24:25)]
colnames(dados)

colnames(dados) <-  c("display", "Court", "Perch", "RR", "DET", "DLEntr", "LAM", "SVector", "EPSVector", "Males", "Copulation", "Female" )
colnames(dados)

summary(lme4::lmer(RR ~ Males + (1|Court), data = dados))[]$coefficients
summary(lme4::lmer(RR ~ Males + (1|Court), data = dados))[]$coefficients[1] # estimate intercept
summary(lme4::lmer(RR ~ Males + (1|Court), data = dados))[]$coefficients[2] # estimate parametro
summary(lme4::lmer(RR ~ Males + (1|Court), data = dados))[]$coefficients[3] # se intercept
summary(lme4::lmer(RR ~ Males + (1|Court), data = dados))[]$coefficients[4] # se parametro

summary(lme4::glmer(RR ~ Males + (1|Court), family = poisson, data = dados))[]$coefficients
summary(lme4::glmer(RR ~ Males + (1|Court), family = poisson, data = dados))[]$coefficients[1]
summary(lme4::glmer(RR ~ Males + (1|Court), family = poisson, data = dados))[]$coefficients[2]
summary(lme4::glmer(RR ~ Males + (1|Court), family = poisson, data = dados))[]$coefficients[3]
summary(lme4::glmer(RR ~ Males + (1|Court), family = poisson, data = dados))[]$coefficients[4]

Anova(lme4::lmer(RR ~ Males + (1|Court), data = dados))$Pr
Anova(lme4::lmer(RR ~ Males + (1|Court), data = dados))$Chisq
shapiro.test(resid(lme4::lmer(RR ~ Males + (1|Court), data = dados)))[]$p.value

shapiro.test(dados$DET)

dados_scale <- scale(dados[c(4:9)])

dados_mod <- cbind(dados_TUDO[c(1:3, 20, 24:25)], dados_scale)

dados<-dados_mod

colnames(dados)

colnames(dados) <-  c("display", "Court", "Perch", "Males", "Copulation", "Female", "RR", "DET", "DLEntr", "LAM", "SVector", "EPSVector" )
colnames(dados)

for (i in colnames(dados)[7:12]){
  j <- subset(dados, select = i)
  hist(log(j[,1]), xlab = i, main = NA)
  print(i)
  test_0 <- shapiro.test(j[,1])$p.value
  test_00 <- shapiro.test(log(j[,1]))$p.value
  print(test_0)
  print(test_00)
}

test_GLMM_machos <- data.frame(Parametro = (NULL),
                               Intercept = (NULL),
                               I_SE = (NULL),
                               estimate = (NULL),
                               Std.Error = (NULL),
                               Chisq = (NULL),
                               p.value = (NULL),
                               resid.p = (NULL))

for (i in colnames(dados)[7:12]){
  j <- subset(dados, select = i)
  hist(j[,1], xlab = i, main = NA)
  print(i)
  test_0 <- shapiro.test(j[,1])$p.value
  if (test_0 < 0.00005){
    print("log")
    test <- data.frame(p.value = car::Anova(lme4::glmer(log(j[,1]) ~ Males + (1|Court), data = dados))$Pr)
    test_1 <-  data.frame(Parametro = i)
    test_2 <- data.frame(Chisq = car::Anova(lme4::lmer(log(j[,1]) ~ Males + (1|Court), data = dados))$Chisq)
    test_3 <- data.frame(Estimate = summary(lme4::lmer(log(j[,1]) ~ Males + (1|Court), data = dados))[]$coefficients[2])
    test_4 <- data.frame(Std.Error = summary(lme4::lmer(log(j[,1]) ~ Males + (1|Court), data = dados))[]$coefficients[4])
    test_5 <- data.frame(Intercept = summary(lme4::lmer(log(j[,1]) ~ Males + (1|Court), data = dados))[]$coefficients[1]) # Estimate Intercept
    test_6 <- data.frame(I_SE = summary(lme4::lmer(log(j[,1]) ~ Males + (1|Court), data = dados))[]$coefficients[3]) # SE Intercept
    test_7 <- data.frame(resid.p = round(shapiro.test(resid(lme4::lmer(log(j[,1]) ~ Males + (1|Court), data = dados)))[]$p.value, 5))
    test_8 <- cbind(test_1, test_5, test_6, test_3, test_4, test_2, test, test_7)
  } else {
    test <- data.frame(p.value = car::Anova(lme4::lmer(j[,1] ~ Males + (1|Court), data = dados))$Pr)
    test_1 <-  data.frame(Parametro = i)
    test_2 <- data.frame(Chisq = car::Anova(lme4::lmer(j[,1] ~ Males + (1|Court), data = dados))$Chisq)
    test_3 <- data.frame(Estimate = summary(lme4::lmer(j[,1] ~ Males + (1|Court), data = dados))[]$coefficients[2])
    test_4 <- data.frame(Std.Error = summary(lme4::lmer(j[,1] ~ Males + (1|Court), data = dados))[]$coefficients[4])
    test_5 <- data.frame(Intercept = summary(lme4::lmer(j[,1] ~ Males + (1|Court), data = dados))[]$coefficients[1])
    test_6 <- data.frame(I_SE = summary(lme4::lmer(j[,1] ~ Males + (1|Court), data = dados))[]$coefficients[3])
    test_7 <- data.frame(resid.p = round(shapiro.test(resid(lme4::lmer(j[,1] ~ Males + (1|Court), data = dados)))[]$p.value, 5))
    test_8 <- cbind(test_1, test_5, test_6, test_3, test_4, test_2, test, test_7)
  }
  test_GLMM_machos <- rbind(test_GLMM_machos, test_8)
}  
test_GLMM_machos

#### Fêmea ~ Recorrencia ####
dados<-dados_TUDO[c(1:5, 8, 11, 18:20, 24:25)]
hist(dados$fem_hora)
shapiro.test(dados$fem_hora)

dados_scale <- scale(dados[c(4:9)])

dados_mod <- cbind(dados_TUDO[c(1:3, 20, 24:25)], dados_scale)

dados<-dados_mod

dcf_mod<-dados$fem_hora
fitdistrplus::descdist(dcf_mod, discrete = F)

n_dist <- fitdistrplus::fitdist(dcf_mod, "norm")
n_dist

plot(n_dist)

j_fem <- lme4::lmer(fem_hora ~ recurrence_rate + determinism + dl_entropy + laminarity + S_vector+ Eps_Vector + (1|Corte), data = dados_mod)
j_fem_2 <- lm(fem_hora ~ recurrence_rate + determinism + dl_entropy + laminarity + S_vector+ Eps_Vector, data = dados)

anova(j_fem, j_fem_2)

summary(j_fem)

m2<-lmer(fem_hora ~ (1|Corte), data = dados_mod)
anova(j_fem,m2)

car::Anova(j_fem)  

r2glmm::r2beta(j_fem, method = "kr")

shapiro.test(resid(j_fem))
hist(resid(j_fem))

plot(allEffects(j_fem))

#### Cópula ~ Recorrencia ####
dados<-dados_TUDO[c(1:5, 8, 11, 18:20, 24:25)]

hist(dados$cop_hora)
hist(sqrt(dados$cop_hora))
hist(round((dados$cop_hora)*10,0))
sort(dados$cop_hora)
sort(round((dados$cop_hora)*10,0))
shapiro.test(dados$cop_hora)

dados$cop<-round((dados$cop_hora)*10,0)

dados_scale <- scale(dados[c(4:9)])

dados_mod <- cbind(dados_TUDO[c(1:3, 20, 24:25)], dados_scale)

dados_mod$cop<-round((dados$cop_hora)*10,0)

summary(dados$cop)
hist(dados$cop)

dcf_mod <- dados$cop
fitdistrplus::descdist(dcf_mod, discrete = F)
n_dist <- fitdistrplus::fitdist(dcf_mod, "exp")
n_dist

plot(n_dist)

j_cop <- glmer(cop ~ recurrence_rate + determinism + dl_entropy + laminarity + S_vector + Eps_Vector +(1|Corte), family = poisson, data = dados_mod) # ESCOLHIDO

summary(j_cop)

shapiro.test(resid(j_cop))

Anova(j_cop)

r2glmm::r2beta(j_cop)

m2<-lmer(cop ~ (1|Corte), data = dados_mod)
anova(j_cop,m2)

