ex3_1 <- read.csv("C:/Users/Dell/Downloads/exercicio3_1.csv")
ex3_2 <- read.csv("C:/Users/Dell/Downloads/exercicio3_2.csv")

reghc1 <- lm(rendtrabprinc ~ anosest + idade, data = ex3_1)

reghc2 <- lm(logrendtrabprinc ~ anosest + idade, data = ex3_1)

reghc3 <- lm(logrendtrabprinc ~ anosest + idade + idade2, data = ex3_1)

reghc4 <- lm(logrendtrabprinc ~ anosest + idade + sexo + raca, data = ex3_1)

reghc5 <- lm(logrendtrabprinc ~ anosest + idade + sexo + raca + sexoraca, data = ex3_1)
 
reghc6 <- lm(logrendtrabprinc ~ anosest + idade + sexo + raca + sexoanosest, data = ex3_1)

ggplot(ex3_1, mapping = aes(x=anosest, y=logrendtrabprinc))+geom_point()+theme_bw()+geom_abline(slope = 0.1230423, intercept = 4.9999377)+ geom_abline(slope = 0.1020218, intercept = 5.608396)+ylim(0,13)+
  labs(title="Renda por Educação",x="Anos de Estudo",y="Log Renda")
labs(title="Renda por Educação",x="Anos de Estudo",y='Log da Renda")
  geom_abline(slope = 0.1096754, intercept = 5.1525137)+ geom_abline(slope = 0.1096754, intercept = 5.798319)+ylim(0,13)

ggplot(ex3_1, mapping = aes(x=idade, y=logrendtrabprinc))+geom_point(alpha = 0.1)+theme_bw()

ex3_1$granosest <- cut(ex3_1$anosest, breaks = c(0,3,8,11,15,16), labels = c('1','2','3','4','5'))

ex3_1$est1 <- ifelse(ex3_1$granosest == "1", 1, 0)
ex3_1$est2 <- ifelse(ex3_1$granosest == "2", 1, 0)
ex3_1$est3 <- ifelse(ex3_1$granosest == "3", 1, 0)
ex3_1$est4 <- ifelse(ex3_1$granosest == "4", 1, 0)
ex3_1$est5 <- ifelse(ex3_1$granosest == "5", 1, 0)

reghc7 <- lm(logrendtrabprinc ~ granosest + idade + sexo + raca, data = ex3_1)

reghc8 <- lm(logrendtrabprinc ~ est1 + est2 + est3 + est4 + est5 + idade + sexo + raca, data = ex3_1)

ex3_1$set1 <- ifelse(ex3_1$setorativ == "1", 1, 0)
ex3_1$set2 <- ifelse(ex3_1$setorativ == "2", 1, 0)
ex3_1$set3 <- ifelse(ex3_1$setorativ == "3", 1, 0)
ex3_1$set4 <- ifelse(ex3_1$setorativ == "4", 1, 0)
ex3_1$set5 <- ifelse(ex3_1$setorativ == "5", 1, 0)
ex3_1$set6 <- ifelse(ex3_1$setorativ == "6", 1, 0)
ex3_1$set7 <- ifelse(ex3_1$setorativ == "7", 1, 0)
ex3_1$set8 <- ifelse(ex3_1$setorativ == "8", 1, 0)
ex3_1$set9 <- ifelse(ex3_1$setorativ == "9", 1, 0)
ex3_1$set10 <- ifelse(ex3_1$setorativ == "10", 1, 0)
ex3_1$set11 <- ifelse(ex3_1$setorativ == "11", 1, 0)
ex3_1$set12 <- ifelse(ex3_1$setorativ == "12", 1, 0)

reghc9 <- lm(logrendtrabprinc ~ anosest + idade + set2 + set3 + set4 + set5 + set6 + set7 + set8 + set9 + set10 + set11 , data = ex3_1)

reghc10 <- lm(logrendtrabprinc ~ anosest + idade + sexo + set8 + set8*anosest, data = ex3_1)

ex3_1$Nordeste <- ifelse(ex3_1$regiao == '2', 1, 0)
ex3_1$Sudeste <- ifelse(ex3_1$regiao == '3', 1, 0)
ex3_1$Sul <- ifelse(ex3_1$regiao == '4', 1, 0)
ex3_1$Centrooeste <- ifelse(ex3_1$regiao == '5', 1, 0)

reghc11 <- lm(logrendtrabprinc ~ anosest + idade + set8 + anosest*set8, data = ex3_1) 
reghc12 <- lm(logrendtrabprinc ~ anosest + idade + set8 + anosest*set8 + set8*Nordeste + set8*Sudeste + set8*Sul + set8*Centrooeste,  data = ex3_1)

reggini1 <- lm(coefgini ~ logpibpc + urban + educ + crescpop + idhmedio + idhalto + idhmalto, data = ex3_2)

reghc13 <- lm(logrendtrabprinc ~ set8 + set9, data = ex3_1) 
reghc14 <- lm(logrendtrabprinc ~ set8 + set9 + anosest + idade, data = ex3_1)
reghc15 <- lm(logrendtrabprinc ~ set8 + set9 + anosest + idade + anosest*set8 + anosest*set9, data = ex3_1)
reghc16 <- lm(logrendtrabprinc ~ set8 + set9 + anosest + idade + anosest*set8 + anosest*set9 + idade*set8 + idade*set9, data = ex3_1)
reghc17 <- lm(logrendtrabprinc ~ set8 + anosest + idade + informal + sexo + raca, data = ex3_1)
reghc18 <- lm(logrendtrabprinc ~ set8 + anosest + idade + informal, data = ex3_1)

reghc19 <- lm(logrendtrabprinc ~ set8 + anosest + idade + informal + sexo + raca + Nordeste + Sudeste + Sul + Centrooeste, data = ex3_1)

reghc20 <- lm(logrendtrabprinc ~ set8 + anosest + idade + informal + sexo + raca + sexo*set8 + raca*set8, data = ex3_1)

reghc21 <- lm(logrendtrabprinc ~ set8 + anosest + idade + informal + sexo + raca + Nordeste + Sudeste + Sul + Centrooeste + set8*Nordeste + set8*Sudeste + set8*Sul + set8*Centrooeste + anosest*set8, data = ex3_1)


reggini4 <- lm(coefgini ~ pibpercapita + urban + educ + crescpop, data = ex3_2)
reggini5 <- lm(coefgini ~ pibpercapita + I(pibpercapita^2) + urban + educ + crescpop, data = ex3_2)
reggini6 <- lm(coefgini ~ logpibpc + urban + educ + crescpop, data = ex3_2)

reggini1 <- lm(coefgini ~ pibpercapita + urban + educ + crescpop + idhmedio + idhalto + idhmalto, data = ex3_2)
reggini2 <- lm(coefgini ~ pibpercapita + I(pibpercapita^2) + urban + educ + crescpop + idhmedio + idhalto + idhmalto, data = ex3_2)
reggini3 <- lm(coefgini ~ logpibpc + urban + educ + crescpop + idhmedio + idhalto + idhmalto, data = ex3_2)

reggini8 <- lm(coefgini ~ pibpercapita + I(pibpercapita^2) + urban + educ + crescpop + idhmedio + idhalto + idhmalto + impostosk + txhomicidio + txdesempr, data = ex3_2)
reggini7 <- lm(coefgini ~ pibpercapita + I(pibpercapita^2) + urban + educ + crescpop + idhmedio + idhalto + idhmalto + impostosk + txhomicidio + txdesempr, data = ex3_2)
reggini9 <- lm(coefgini ~ pibpercapita + I(pibpercapita^2) + urban + educ + crescpop + impostosk + txhomicidio + txdesempr + benefprev, data = ex3_2)