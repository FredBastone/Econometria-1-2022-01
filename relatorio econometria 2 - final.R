pnad <- read.csv(file = "C:/Users/Dell/Downloads/pnad042021.csv")

attach(pnad)

head(pnad)

hdr <- read.csv(file = "C:/Users/Dell/Downloads/hdr_2020.csv")

attach(hdr)

head(hdr)

#Exercicio 1 - Modelos Simples

pnadsp = pnad[pnad$UF == 35,]
pnaddf = pnad[pnad$UF == 53,]

mqoest <- lm(rendtrabprinc ~ anosest, pnad)
mqoage <- lm(rendtrabprinc ~ idade, pnad)
mqomult <- lm(rendtrabprinc ~ anosest + idade, pnad)
  
mqoestsp <- lm(rendtrabprinc ~ anosest, pnadsp)
mqoagesp <- lm(rendtrabprinc ~ idade, pnadsp)
mqomultsp <- lm(rendtrabprinc ~ anosest + idade, pnadsp)

mqoestdf <- lm(rendtrabprinc ~ anosest, pnaddf)
mqoagedf <- lm(rendtrabprinc ~ idade, pnaddf)
mqomultdf <- lm(rendtrabprinc ~ anosest + idade, pnaddf)

summary(mqoest)
summary(mqoage)
summary(mqomult)

summary(mqoestsp)
summary(mqoagesp)
summary(mqomultsp)

summary(mqoestdf)
summary(mqoagedf)
summary(mqomultdf)

pbr1 <- ggplot(pnad, mapping = aes(x=anosest, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Estudo Brasil",x="Anos de Estudo",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -1762.595, slope = 397.321)

pbr2 <- ggplot(pnad, mapping = aes(x=idade, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Idade Brasil",x="Idade",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = 1304.69, slope = 44.15)

psp1 <- ggplot(pnadsp, mapping = aes(x=anosest, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Estudo SP",x="Anos de Estudo",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -2837.60, slope = 537.34)

psp2 <- ggplot(pnadsp, mapping = aes(x=idade, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Idade SP",x="Idade",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = 2197.30, slope = 41.99)

pdf1 <- ggplot(pnaddf, mapping = aes(x=anosest, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Estudo DF",x="Anos de Estudo",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -2912.07, slope = 567.22)

pdf2 <- ggplot(pnaddf, mapping = aes(x=idade, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Idade DF",x="Idade",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = 1204.56, slope = 71.94)

ggarrange(pbr1,pbr2,psp1,psp2,pdf1,pdf2,ncol =2,nrow =3)

pbr1a <- ggplot(pnad, mapping = aes(x=anosest, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Estudo Brasil",x="Anos de Estudo",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -5260.858, slope = 432.430)

pbr2a <- ggplot(pnad, mapping = aes(x=idade, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Idade Brasil",x="Idade",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -5260.858, slope = 74.586)

psp1a <- ggplot(pnadsp, mapping = aes(x=anosest, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Estudo SP",x="Anos de Estudo",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -6835.22, slope = 581.03)

psp2a <- ggplot(pnadsp, mapping = aes(x=idade, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Idade SP",x="Idade",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -6835.22, slope = 83.61)

pdf1a <- ggplot(pnaddf, mapping = aes(x=anosest, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Estudo DF",x="Anos de Estudo",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -9345.18, slope = 646.41)

pdf2a <- ggplot(pnaddf, mapping = aes(x=idade, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Idade DF",x="Idade",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -9345.18, slope = 132.05)

ggarrange(pbr1a,pbr2a,psp1a,psp2a,pdf1a,pdf2a,ncol =2,nrow =3)

#Exercicio 1 - Modelo Simples Manualmente 

covxy = cov(anosest, rendtrabprinc)

varx <- var (anosest)

mediay <- mean (rendtrabprinc)

mediax <- mean (anosest)

b1hat <- (covxy/varx)

b0 <- mediay - b1hat*mediax

b.hatest <-coef (mqoest)

y.hatest <- fitted (mqoest)

u.hatest <-resid (mqoest)

summary (y.hatest)

summary (u.hatest)

cor (anosest, u.hatest)

b.hatage <-coef (mqoage)

y.hatage <- fitted (mqoage)

u.hatage <-resid (mqoage)

summary (y.hatage)

summary (u.hatage)

cor (idade, u.hatage)

r2age <- (var (y.hatage))/(var (rendtrabprinc))
r21age <- 1- (var (u.hatage))/(var (rendtrabprinc))

r2est <- (var (y.hatest))/(var (rendtrabprinc))
r21est <- 1- (var (u.hatest))/(var (rendtrabprinc))

r2age
r21age
r2est
r21est

#Exercicio 2 - Modelos Múltiplos

mqohdr1 <- lm(coefgini ~ pibpercapita, hdr)

mqohdr2 <- lm(coefgini ~ pibpercapita + urban, hdr)

mqohdr3 <- lm(coefgini ~ pibpercapita + urban + educ, hdr)

mqohdr4 <- lm(coefgini ~ pibpercapita + urban + educ + crescpop, hdr)

summary(mqohdr1)
summary(mqohdr2)
summary(mqohdr3)
summary(mqohdr4)

pbr1 <- ggplot(pnad, mapping = aes(x=anosest, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Estudo Brasil",x="Anos de Estudo",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -1762.595, slope = 397.321)

pbr2 <- ggplot(pnad, mapping = aes(x=idade, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Idade Brasil",x="Idade",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = 1304.69, slope = 44.15)

psp1 <- ggplot(pnadsp, mapping = aes(x=anosest, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Estudo SP",x="Anos de Estudo",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -2837.60, slope = 537.34)

psp2 <- ggplot(pnadsp, mapping = aes(x=idade, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Idade SP",x="Idade",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = 2197.30, slope = 41.99)

pdf1 <- ggplot(pnaddf, mapping = aes(x=anosest, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Estudo DF",x="Anos de Estudo",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -2912.07, slope = 567.22)

pdf2 <- ggplot(pnaddf, mapping = aes(x=idade, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Idade DF",x="Idade",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = 1204.56, slope = 71.94)

ggarrange(pbr1,pbr2,psp1,psp2,pdf1,pdf2,ncol =2,nrow =3)

pbr1a <- ggplot(pnad, mapping = aes(x=anosest, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Estudo Brasil",x="Anos de Estudo",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -5260.858, slope = 432.430)

pbr2a <- ggplot(pnad, mapping = aes(x=idade, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Idade Brasil",x="Idade",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -5260.858, slope = 74.586)

psp1a <- ggplot(pnadsp, mapping = aes(x=anosest, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Estudo SP",x="Anos de Estudo",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -6835.22, slope = 581.03)

psp2a <- ggplot(pnadsp, mapping = aes(x=idade, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Idade SP",x="Idade",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -6835.22, slope = 83.61)

pdf1a <- ggplot(pnaddf, mapping = aes(x=anosest, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Estudo DF",x="Anos de Estudo",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -9345.18, slope = 646.41)

pdf2a <- ggplot(pnaddf, mapping = aes(x=idade, y=rendtrabprinc))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Renda/Idade DF",x="Idade",y='Renda em R$')+ylim(0,60000)+
  geom_abline(intercept = -9345.18, slope = 132.05)

ggarrange(pbr1a,pbr2a,psp1a,psp2a,pdf1a,pdf2a,ncol =2,nrow =3)

phdr1 <- ggplot(hdr, mapping = aes(x=pibpercapita, y=coefgini))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Desigualdade por Renda \nper Capita",x="GDPpc em US$",y='Gini')+ylim(0,100)+xlim(0,80000)+
  geom_abline(intercept = 38.35, slope = -0.0001749)

phdr2 <-  ggplot(hdr, mapping = aes(x=educ, y=coefgini))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Desigualdade por Nível \nEducacional",x="Média de anos de Estudo",y='Gini')+ylim(0,100)+
  geom_abline(intercept = 38.35, slope = -0.2596)

phdr3 <-  ggplot(hdr, mapping = aes(x=crescpop, y=coefgini))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Desigualdade por Crescimento \nPopulacional",x="Crescimento Populacional %",y='Gini')+ylim(0,100)+
  geom_abline(intercept = 38.35, slope = 0.9134)

phdr4 <-  ggplot(hdr, mapping = aes(x=urban, y=coefgini))+geom_point(alpha = 0.5)+theme_bw()+
  labs(title="Desigualdade por \nUrbanização",x="População Urbana %",y='Gini')+ylim(0,100)+
  geom_abline(intercept = 38.35, slope = 0.07236)

ggarrange(phdr1,phdr2,phdr3,phdr4,ncol =2,nrow =2)

