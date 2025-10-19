rm(list=ls())
library(nlme)
library(dplyr)
library(ggplot2)

kajime<-read.csv("Kajime.csv",stringsAsFactors = TRUE)
kajime$Season <- factor(kajime$Season, levels = c("Flourish","Decline") )
names(kajime)

####### Central Pacific @ Kajime##########
kajime_CentralPacific<-subset(kajime,kajime$Region=="CentralPacific")
kajime_CentralPacific_Flourish<-subset(kajime,kajime$Region=="CentralPacific"&kajime$Season=="Flourish")
kajime_CentralPacific$Season <- 
  factor(kajime_CentralPacific$Season, levels = c("Flourish","Decline") )


####線形モデル#####
model1_cp<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = kajime_CentralPacific)
summary(model1_cp)
AIC(model1_cp)
BIC(model1_cp)

####累乗モデル#####
model2_cp<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=10,b = 1),data = kajime_CentralPacific)
summary(model2_cp)
AIC(model2_cp)
BIC(model2_cp)

####指数モデル#####
model3_cp<- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=100,b = 0.01), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data = kajime_CentralPacific,
              trace=TRUE)
summary(model3_cp)
AIC(model3_cp)
BIC(model3_cp)



####### Central Pacific @ Sagami##########
#Model1
dummy_CentralPacific<- expand.grid(Cover=seq(min(kajime_CentralPacific$Cover),
                                             max(kajime_CentralPacific$Cover),length=1000))

pred_CentralPacific<- predict(model1_cp,newdata=dummy_CentralPacific,se.fit=T)
dummy_CentralPacific$DW<-pred_CentralPacific



plot_CentralPacific1<-ggplot(data=kajime_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacific, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacific1

#Model2
pred_CentralPacific2<- predict(model2_cp,newdata=dummy_CentralPacific,se.fit=T)
dummy_CentralPacific$DW2<-pred_CentralPacific2

plot_CentralPacific2<-ggplot(data=kajime_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacific, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacific2

# save width 600 * Height 500 
#Model 3
pred_CentralPacific3<- predict(model3_cp,newdata=dummy_CentralPacific,se.fit=T)
dummy_CentralPacific$DW3<-pred_CentralPacific3

plot_CentralPacific3<-ggplot(data=kajime_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacific, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacific3


#width 600 * Height 500 save

####### NorthJapanSea @ Sado ##########
kajime_NorthJapanSea<-subset(kajime,kajime$Region=="NorthJapanSea")


####線形モデル#####
model1_NJ<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = kajime_NorthJapanSea)
summary(model1_NJ)
AIC(model1_NJ)
BIC(model1_NJ)

####累乗モデル#####
model2_NJ<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = kajime_NorthJapanSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_NJ)
AIC(model2_NJ)
BIC(model2_NJ)

####指数モデル#####
model3_NJ<- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=17.5,b =0.048), data = kajime_NorthJapanSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_NJ)
AIC(model3_NJ)
BIC(model3_NJ)

####二次関数モデル#####
model4_NJ<- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
              start = list(a=1, b=1,c=1), data =  kajime_NorthJapanSea)
summary(model4_NJ)
AIC(model4_NJ)
BIC(model4_NJ)


#######Plot line  NorthJapanSea @ Sado##########
#Model1
dummy_NorthJapanSea<- expand.grid(Cover=seq(min(kajime_NorthJapanSea$Cover),
                                            max(kajime_NorthJapanSea$Cover),length=1000))
pred_NorthJapanSea1<- predict(model1_NJ,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea$DW1<-pred_NorthJapanSea1

plot_NorthJapanSea1<-ggplot(data=kajime_NorthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1800,length=7),limits=c(0,1800))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_NorthJapanSea, aes(x=Cover, y=DW1),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_NorthJapanSea1


#Model2
dummy_NorthJapanSea<- expand.grid(Cover=seq(min(kajime_NorthJapanSea$Cover),
                                             max(kajime_NorthJapanSea$Cover),length=1000))
pred_NorthJapanSea2<- predict(model2_NJ,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea$DW2<-pred_NorthJapanSea2

plot_NorthJapanSea2<-ggplot(data=kajime_NorthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1800,length=7),limits=c(0,1800))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey50"))+
  labs(color='Season')+
  geom_line(data=dummy_NorthJapanSea, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_NorthJapanSea2

#Model3
dummy_NorthJapanSea<- expand.grid(Cover=seq(min(kajime_NorthJapanSea$Cover),
                                            max(kajime_NorthJapanSea$Cover),length=1000))
pred_NorthJapanSea3<- predict(model3_NJ,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea$DW3<-pred_NorthJapanSea3

plot_NorthJapanSea3<-ggplot(data=kajime_NorthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1800,length=7),limits=c(0,1800))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_NorthJapanSea, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_NorthJapanSea3

#width 600 * Height 500 save


####### Seto Island Sea  ##########
kajime_SetoIslandSea<-subset(kajime,kajime$Region=="SetoIslandSea")


####線形モデル#####
model1_ST<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = kajime_SetoIslandSea)
summary(model1_ST)
AIC(model1_ST)
BIC(model1_ST)

####累乗モデル#####
model2_ST<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = kajime_SetoIslandSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_ST)
AIC(model2_ST)
BIC(model2_ST)

####指数モデル#####
model3_ST<- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=17.5,b =0.048), data = kajime_SetoIslandSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_ST)
AIC(model3_ST)
BIC(model3_ST)


######Plot line  SetoIslandSea ##########
#Model1
dummy_SetoIslandSea<- expand.grid(Cover=seq(min(kajime_SetoIslandSea$Cover),
                                            max(kajime_SetoIslandSea$Cover),length=1000))
pred_SetoIslandSea1<- predict(model1_ST,newdata=dummy_SetoIslandSea,se.fit=T)
dummy_SetoIslandSea$DW1<-pred_SetoIslandSea1

plot_SetoIslandSea1<-ggplot(data=kajime_SetoIslandSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey50"))+
  labs(color='Season')+
  geom_line(data=dummy_SetoIslandSea, aes(x=Cover, y=DW1),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SetoIslandSea1


#Model2
dummy_SetoIslandSea<- expand.grid(Cover=seq(min(kajime_SetoIslandSea$Cover),
                                            max(kajime_SetoIslandSea$Cover),length=1000))
pred_SetoIslandSea2<- predict(model2_ST,newdata=dummy_SetoIslandSea,se.fit=T)
dummy_SetoIslandSea$DW2<-pred_SetoIslandSea2

plot_SetoIslandSea2<-ggplot(data=kajime_SetoIslandSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey50"))+
  labs(color='Season')+
  geom_line(data=dummy_SetoIslandSea, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SetoIslandSea2

#Model3
pred_SetoIslandSea3<- predict(model3_ST,newdata=dummy_SetoIslandSea,se.fit=T)
dummy_SetoIslandSea$DW3<-pred_SetoIslandSea3

plot_SetoIslandSea3<-ggplot(data=kajime_SetoIslandSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey50"))+
  labs(color='Season')+
  geom_line(data=dummy_SetoIslandSea, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SetoIslandSea3

#width 600 * Height 500 save


####### Southern Pacific @ Kajime##########
kajime_SouthernPacific<-subset(kajime,kajime$Region=="SouthernPacific")
kajime_SouthernPacific$Season <- 
  factor(kajime_SouthernPacific$Season, levels = c("Flourish","Decline") )


####線形モデル#####
model1_cp<- nls(DW_g_m2~ a*Cover,
                start = list(a=50),data = kajime_SouthernPacific)
summary(model1_cp)
AIC(model1_cp)
BIC(model1_cp)

####累乗モデル#####
model2_cp<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),data = kajime_SouthernPacific)
summary(model2_cp)
AIC(model2_cp)
BIC(model2_cp)

####指数モデル#####
model3_cp<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=50,b = 0.01), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = kajime_SouthernPacific,
                trace=TRUE)
summary(model3_cp)
AIC(model3_cp)
BIC(model3_cp)





####### Southern Pacific Shikoku##########
#Model1
dummy_SouthernPacific<- expand.grid(Cover=seq(min(kajime_SouthernPacific$Cover),
                                             max(kajime_SouthernPacific$Cover),length=1000))

pred_SouthernPacific<- predict(model1_cp,newdata=dummy_SouthernPacific,se.fit=T)
dummy_SouthernPacific$DW<-pred_SouthernPacific



plot_SouthernPacific1<-ggplot(data=kajime_SouthernPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthernPacific, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthernPacific1

#Model2
pred_SouthernPacific2<- predict(model2_cp,newdata=dummy_SouthernPacific,se.fit=T)
dummy_SouthernPacific$DW2<-pred_SouthernPacific2

plot_SouthernPacific2<-ggplot(data=kajime_SouthernPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthernPacific, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthernPacific2

# save width 600 * Height 500 
#Model 3
pred_SouthernPacific3<- predict(model3_cp,newdata=dummy_SouthernPacific,se.fit=T)
dummy_SouthernPacific$DW3<-pred_SouthernPacific3

plot_SouthernPacific3<-ggplot(data=kajime_SouthernPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1600))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthernPacific, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthernPacific3


#Model4
pred_SouthernPacific4<- predict(model4_cp,newdata=dummy_SouthernPacific,se.fit=T)
dummy_SouthernPacific$DW4<-pred_SouthernPacific4

plot_SouthernPacific4<-ggplot(data=kajime_SouthernPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthernPacific, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthernPacific4

#width 600 * Height 500 save


####### SouthJapanSea  ##########データ不足##
kajime_SouthJapanSea<-subset(kajime,kajime$Region=="SouthJapanSea")


####線形モデル#####
model1 <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = kajime_SouthJapanSea)
summary(model1)
AIC(model1)
BIC(model1)

####累乗モデル#####
model2 <- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = kajime_SouthJapanSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2)
AIC(model2)
BIC(model2)

####指数モデル#####
model3 <- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=17.5,b =0.048), data = kajime_SouthJapanSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3)
AIC(model3)
BIC(model3)

####二次関数モデル#####
model4 <- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
              start = list(a=-0.2, b=20,c=-50), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data =  kajime_SouthJapanSea)
summary(model4)
AIC(model4)
BIC(model4)

######Plot line  SouthJapanSea ##########
#Model1
dummy_SouthJapanSea<- expand.grid(Cover=seq(min(kajime_SouthJapanSea$Cover),
                                            max(kajime_SouthJapanSea$Cover),length=1000))
pred_SouthJapanSea1<- predict(model1,newdata=dummy_SouthJapanSea,se.fit=T)
dummy_SouthJapanSea$DW1<-pred_SouthJapanSea1

plot_SouthJapanSea1<-ggplot(data=kajime_SouthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 2000,length=5),limits=c(0,2000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthJapanSea, aes(x=Cover, y=DW1),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthJapanSea1


#Model2
dummy_SouthJapanSea<- expand.grid(Cover=seq(min(kajime_SouthJapanSea$Cover),
                                            max(kajime_SouthJapanSea$Cover),length=1000))
pred_SouthJapanSea2<- predict(model2,newdata=dummy_SouthJapanSea,se.fit=T)
dummy_SouthJapanSea$DW2<-pred_SouthJapanSea2

plot_SouthJapanSea2<-ggplot(data=kajime_SouthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthJapanSea, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthJapanSea2

#Model3
pred_SouthJapanSea3<- predict(model3,newdata=dummy_SouthJapanSea,se.fit=T)
dummy_SouthJapanSea$DW3<-pred_SouthJapanSea3

plot_SouthJapanSea3<-ggplot(data=kajime_SouthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthJapanSea, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthJapanSea3

#Model4
pred_SouthJapanSea4<- predict(model4,newdata=dummy_SouthJapanSea,se.fit=T)
dummy_SouthJapanSea$DW4<-pred_SouthJapanSea4

#width 600 * Height 500 save

####### EastChinaSea Tsuruarame  ##########
kajime_EastChinaSea<-subset(kajime,kajime$Region=="EastChinaSea"&kajime$Jap_name=="Tsuruarame")
kajime_EastChinaSea

####線形モデル#####
model1_EC <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = kajime_EastChinaSea)
summary(model1_EC)
AIC(model1_EC)
BIC(model1_EC)

####累乗モデル#####
model2_EC<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = kajime_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_EC)
AIC(model2_EC)
BIC(model2_EC)

####指数モデル#####
model3_EC<- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=17.5,b =0.048), data = kajime_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_EC)
AIC(model3_EC)
BIC(model3_EC)

model3_EC2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = kajime_EastChinaSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_EC2)
AIC(model3_EC2)
BIC(model3_EC2)


######Plot line  EastChinaSea ##########
#Model1
dummy_EastChinaSea<- expand.grid(Cover=seq(min(kajime_EastChinaSea$Cover),
                                            max(kajime_EastChinaSea$Cover),length=1000))
pred_EastChinaSea1<- predict(model1_EC,newdata=dummy_EastChinaSea,se.fit=T)
dummy_EastChinaSea$DW1<-pred_EastChinaSea1

plot_EastChinaSea1<-ggplot(data=kajime_EastChinaSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  ylim(0,500)+
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_EastChinaSea, aes(x=Cover, y=DW1),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSea1
#width 600 * Height 500 save

#Model2
dummy_EastChinaSea<- expand.grid(Cover=seq(min(kajime_EastChinaSea$Cover),
                                            max(kajime_EastChinaSea$Cover),length=1000))
pred_EastChinaSea2<- predict(model2_EC,newdata=dummy_EastChinaSea,se.fit=T)
dummy_EastChinaSea$DW2<-pred_EastChinaSea2

plot_EastChinaSea2<-ggplot(data=kajime_EastChinaSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  xlab("Coverage (%)")+ 
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  ylim(0,500)+
  geom_line(data=dummy_EastChinaSea, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSea2

#Model3
dummy_EastChinaSea<- expand.grid(Cover=seq(min(kajime_EastChinaSea$Cover),
                                           max(kajime_EastChinaSea$Cover),length=1000))
pred_EastChinaSea3<- predict(model3_EC2,newdata=dummy_EastChinaSea,se.fit=T)
dummy_EastChinaSea$DW3<-pred_EastChinaSea3

plot_EastChinaSea3<-ggplot(data=kajime_EastChinaSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  ylim(0,500)+
  geom_point(size=3.5)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey50"))+
  labs(color='Season')+
  geom_line(data=dummy_EastChinaSea, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSea3

#width 600 * Height 500 save

####### EastChinaSea @ Antokume##########

kajime_EastChinaSeaAN<-subset(kajime,kajime$Region=="EastChinaSea"&kajime$Jap_name=="Antokume")


####線形モデル#####
model1_ECAN <- nls(DW_g_m2~ a*Cover,
                 start = list(a=50),data = kajime_EastChinaSeaAN)
summary(model1_ECAN)
AIC(model1_ECAN)
BIC(model1_ECAN)

####累乗モデル#####
model2_ECAN<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=0.001,b = 2),data = kajime_EastChinaSeaAN,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)
summary(model2_ECAN)
AIC(model2_ECAN)
BIC(model2_ECAN)

####指数モデル#####
model3_ECAN<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=1,b =0.048), data = kajime_EastChinaSeaAN,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_ECAN)
AIC(model3_ECAN)
BIC(model3_ECAN)

model3_ECAN2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                  start = list(a=1,b =0.048), data = kajime_EastChinaSeaAN,
                  control = list(maxiter = 50000, warnOnly = TRUE),
                  trace=TRUE)

summary(model3_ECAN2)
AIC(model3_ECAN2)
BIC(model3_ECAN2)


######Plot line  EastChinaSeaAN ##########
#Model1
dummy_EastChinaSeaAN<- expand.grid(Cover=seq(min(kajime_EastChinaSeaAN$Cover),
                                           max(kajime_EastChinaSeaAN$Cover),length=1000))
pred_EastChinaSeaAN1<- predict(model1_ECAN,newdata=dummy_EastChinaSeaAN,se.fit=T)
dummy_EastChinaSeaAN$DW1<-pred_EastChinaSeaAN1

plot_EastChinaSeaAN1<-ggplot(data=kajime_EastChinaSeaAN, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  ylim(0,200)+
  geom_point(size=3.5)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_EastChinaSeaAN, aes(x=Cover, y=DW1),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSeaAN1


#Model2
dummy_EastChinaSeaAN<- expand.grid(Cover=seq(min(kajime_EastChinaSeaAN$Cover),
                                           max(kajime_EastChinaSeaAN$Cover),length=1000))
pred_EastChinaSeaAN2<- predict(model2_ECAN,newdata=dummy_EastChinaSeaAN,se.fit=T)
dummy_EastChinaSeaAN$DW2<-pred_EastChinaSeaAN2

plot_EastChinaSeaAN2<-ggplot(data=kajime_EastChinaSeaAN, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  xlab("Coverage (%)")+ 
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  ylim(0,200)+
  geom_line(data=dummy_EastChinaSeaAN, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSeaAN2

#Model3
dummy_EastChinaSeaAN<- expand.grid(Cover=seq(min(kajime_EastChinaSeaAN$Cover),
                                             max(kajime_EastChinaSeaAN$Cover),length=1000))
pred_EastChinaSeaAN3<- predict(model3_ECAN2,newdata=dummy_EastChinaSeaAN,se.fit=T)
dummy_EastChinaSeaAN$DW3<-pred_EastChinaSeaAN3

plot_EastChinaSeaAN3<-ggplot(data=kajime_EastChinaSeaAN, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  ylim(0,200)+
  geom_point(size=3.5)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey50"))+
  labs(color='Season')+
  geom_line(data=dummy_EastChinaSeaAN, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSeaAN3






#### Tsuruarame and Antokume combined ####
#Model3 Tsuruarame+Antokume
dummy_EastChinaSea<- expand.grid(Cover=seq(min(kajime_EastChinaSea$Cover),
                                           max(kajime_EastChinaSea$Cover),length=1000))
pred_EastChinaSea3<- predict(model3_EC2,newdata=dummy_EastChinaSea,se.fit=T)
dummy_EastChinaSea$DW3<-pred_EastChinaSea3

dummy_EastChinaSeaAN<- expand.grid(Cover=seq(min(kajime_EastChinaSeaAN$Cover),
                                             max(kajime_EastChinaSeaAN$Cover),length=1000))
pred_EastChinaSeaAN3<- predict(model3_ECAN2,newdata=dummy_EastChinaSeaAN,se.fit=T)
dummy_EastChinaSeaAN$DW3<-pred_EastChinaSeaAN3


Kajime_EC<-rbind(kajime_EastChinaSea,kajime_EastChinaSeaAN)
Kajime_EC$Jap_name<-factor(Kajime_EC$Jap_name, 
                           levels = c("Tsuruarame","Antokume") )

plot_EastChinaSea<-ggplot(data=Kajime_EC, 
                             aes(x=Cover, y=DW_g_m2,shape=Jap_name)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  ylim(0,300)+
  geom_point(size=8,color="black")+
  scale_shape_manual(labels = c("Tsuruarame", " Antokume"),values = c(1,2))+
  labs(color='Species')+
  geom_line(data=dummy_EastChinaSeaAN, aes(x=Cover, y=DW3),linetype = 2, linewidth=1,color="black",inherit.aes = FALSE)+
  geom_line(data=dummy_EastChinaSea,aes(x=Cover, y=DW3),linetype = 1, linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position.inside =c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSea



######Plot line  all species  ##########
dummy_CentralPacific<- expand.grid(Cover=seq(min(kajime_CentralPacific$Cover),
                                             max(kajime_CentralPacific$Cover),length=1000))
pred_CentralPacific<- predict(model2_cp,newdata=dummy_CentralPacific,se.fit=T)

dummy_CentralPacific$DW<-pred_CentralPacific
dummy_CentralPacific$RegionClass<-"CP Kajime"
dummy_CentralPacific
dummy_NorthJapanSea<- expand.grid(Cover=seq(min(kajime_NorthJapanSea$Cover),
                                            max(kajime_NorthJapanSea$Cover),length=1000))
pred_NorthJapanSea2<- predict(model2_NJ,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea$DW<-pred_NorthJapanSea2
dummy_NorthJapanSea$RegionClass<-"NJS Tsuruarame"

dummy_SetoIslandSea<- expand.grid(Cover=seq(min(kajime_SetoIslandSea$Cover),
                                            max(kajime_SetoIslandSea$Cover),length=1000))
pred_SetoIslandSea1<- predict(model1_ST,newdata=dummy_SetoIslandSea,se.fit=T)
dummy_SetoIslandSea$DW<-pred_SetoIslandSea1
dummy_SetoIslandSea$RegionClass<-"SIS Kajime/Kurome"

dummy_EastChinaSea<- expand.grid(Cover=seq(min(kajime_EastChinaSea$Cover),
                                           max(kajime_EastChinaSea$Cover),length=1000))
pred_EastChinaSea3<- predict(model3_EC2,newdata=dummy_EastChinaSea,se.fit=T)
dummy_EastChinaSea$DW<-pred_EastChinaSea3
dummy_EastChinaSea$RegionClass<-"ECS Tsuruarame"

dummy_EastChinaSeaAN<- expand.grid(Cover=seq(min(kajime_EastChinaSeaAN$Cover),
                                             max(kajime_EastChinaSeaAN$Cover),length=1000))
pred_EastChinaSeaAN3<- predict(model3_ECAN2,newdata=dummy_EastChinaSeaAN,se.fit=T)
dummy_EastChinaSeaAN$DW<-pred_EastChinaSeaAN3
dummy_EastChinaSeaAN$RegionClass<-"ECS Antokume"

dummy_kajime<-rbind(dummy_CentralPacific,dummy_NorthJapanSea,dummy_SetoIslandSea,
                    dummy_EastChinaSea,dummy_EastChinaSeaAN)
dummy_kajime$RegionClass<-factor(dummy_kajime$RegionClass, 
                                 levels = c("NJS Tsuruarame","CP Kajime","SIS Kajime/Kurome","ECS Tsuruarame","ECS Antokume") )

plot_Kajime_all<-ggplot(data=dummy_kajime, aes(x=Cover, y=DW,linetype = RegionClass,color= RegionClass)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+ 
  geom_line(linewidth=1.25)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.55), 
        legend.title=element_text(size=20),legend.text =  element_text(size = 20))
plot_Kajime_all

# save width 600 * Height 500
