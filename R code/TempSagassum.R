rm(list=ls())
library(nlme)
library(ggplot2)

Hondawara<-read.csv("Sargassum_temperate.csv",stringsAsFactors = TRUE)
Hondawara$Season <- factor(Hondawara$Season, levels = c("Flourish","Decline") )
names(Hondawara)

####### Hokkaido @ Uganomoku##########
Hondawara_Hokkaido<-subset(Hondawara,Hondawara$Region=="Hokkaido")
Hondawara_Hokkaido$DW_g_m2

###線形モデル#####
model1_HD<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Hondawara_Hokkaido)
summary(model1_HD)
AIC(model1_HD)
BIC(model1_HD)

####累乗モデル#####
model2_HD<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Hondawara_Hokkaido)
summary(model2_HD)
AIC(model2_HD)
BIC(model2_HD)

####指数モデル#####
model3_HD<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Hondawara_Hokkaido,
                trace=TRUE)
summary(model3_HD)
AIC(model3_HD)
BIC(model3_HD)

model3_HD2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Hondawara_Hokkaido,
                trace=TRUE)
summary(model3_HD2)
AIC(model3_HD2)
BIC(model3_HD2)


####二次関数モデル#####
model4_HD<- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
                start = list(a=1, b=1,c=1), data =  Hondawara_Hokkaido)
summary(model4_HD)
AIC(model4_HD)
BIC(model4_HD)



####### Hokkaido##########
library(ggplot2)

#Model1
dummy_Hokkaido<- expand.grid(Cover=seq(min(Hondawara_Hokkaido$Cover),
                                       max(Hondawara_Hokkaido$Cover),length=1000))

pred_Hokkaido<- predict(model1_HD,newdata=dummy_Hokkaido,se.fit=T)
dummy_Hokkaido$DW<-pred_Hokkaido

plot_Hokkaido1<-ggplot(data=Hondawara_Hokkaido, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=5),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido1

#Model2
pred_Hokkaido2<- predict(model2_HD,newdata=dummy_Hokkaido,se.fit=T)
dummy_Hokkaido$DW2<-pred_Hokkaido2

plot_Hokkaido2<-ggplot(data=Hondawara_Hokkaido, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=5),limits=c(0,1500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido2
 
# save width 600 * Height 500 

#Model 3
pred_Hokkaido3<- predict(model3_HD2,newdata=dummy_Hokkaido,se.fit=T)
dummy_Hokkaido$DW3<-pred_Hokkaido3

plot_Hokkaido3<-ggplot(data=Hondawara_Hokkaido, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 12000,length=7),limits=c(0,12000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido3


#Model4
pred_Hokkaido4<- predict(model4_HD,newdata=dummy_Hokkaido,se.fit=T)
dummy_Hokkaido$DW4<-pred_Hokkaido4

plot_Hokkaido4<-ggplot(data=Hondawara_Hokkaido, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=5),limits=c(0,1500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido4

#width 600 * Height 500 save


####### NorthJapanSea @ Sado ##########
Hondawara_NorthJapanSea<-subset(Hondawara,Hondawara$Region=="NorthJapanSea",na.rm = TRUE)


####線形モデル#####
model1_NJ<- nls(DW_g_m2~ a*Cover,
                start = list(a=50),data = Hondawara_NorthJapanSea)
summary(model1_NJ)
AIC(model1_NJ)
BIC(model1_NJ)

####累乗モデル#####
model2_NJ<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=1.444,b = 1.3181),data = Hondawara_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)
summary(model2_NJ)
AIC(model2_NJ)
BIC(model2_NJ)

####指数モデル#####
model3_NJ<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=17.5,b =0.048), data = Hondawara_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_NJ)
AIC(model3_NJ)
BIC(model3_NJ)

model3_NJ2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = Hondawara_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_NJ2)
AIC(model3_NJ2)
BIC(model3_NJ2)

####二次関数モデル#####
model4_NJ<- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
                start = list(a=1, b=1,c=1), data =  Hondawara_NorthJapanSea)
summary(model4_NJ)
AIC(model4_NJ)
BIC(model4_NJ)


#######Plot line  NorthJapanSea @ Sado##########
#Model1
dummy_NorthJapanSea<- expand.grid(Cover=seq(min(Hondawara_NorthJapanSea$Cover,na.rm=T),
                                            max(Hondawara_NorthJapanSea$Cover,na.rm=T),length=1000,na.rm = TRUE))
pred_NorthJapanSea1<- predict(model1_NJ,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea$DW1<-pred_NorthJapanSea1

plot_NorthJapanSea1<-ggplot(data=Hondawara_NorthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
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
dummy_NorthJapanSea<- expand.grid(Cover=seq(min(Hondawara_NorthJapanSea$Cover),
                                            max(Hondawara_NorthJapanSea$Cover),length=1000))
pred_NorthJapanSea2<- predict(model2_NJ,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea$DW2<-pred_NorthJapanSea2

plot_NorthJapanSea2<-ggplot(data=Hondawara_NorthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1800,length=7),limits=c(0,1800))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_NorthJapanSea, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_NorthJapanSea2

#Model3
dummy_NorthJapanSea<- expand.grid(Cover=seq(min(Hondawara_NorthJapanSea$Cover,na.rm=T),
                                            max(Hondawara_NorthJapanSea$Cover,na.rm=T),length=1000))
pred_NorthJapanSea3<- predict(model3_NJ2,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea$DW3<-pred_NorthJapanSea3

plot_NorthJapanSea3<-ggplot(data=Hondawara_NorthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1800,length=7),limits=c(0,1800))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_NorthJapanSea, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_NorthJapanSea3

#Model4
pred_NorthJapanSea4<- predict(model4_NJ,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea$DW4<-pred_NorthJapanSea4

plot_NorthJapanSea4<-ggplot(data=Hondawara_NorthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1800,length=7),limits=c(0,1800))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_NorthJapanSea, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_NorthJapanSea4

#width 600 * Height 500 save

####### Central Pacific @ Hondawara##########
Hondawara_CentralPacific<-subset(Hondawara,Hondawara$Region=="CentralPacific")

####線形モデル#####
model1_cp<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Hondawara_CentralPacific)
summary(model1_cp)
AIC(model1_cp)
BIC(model1_cp)

####累乗モデル#####
model2_cp<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=10,b = 1),data = Hondawara_CentralPacific)
summary(model2_cp)
AIC(model2_cp)
BIC(model2_cp)

####指数モデル#####
model3_cp<- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=50,b = 0.01), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data = Hondawara_CentralPacific,
              trace=TRUE)
summary(model3_cp)
AIC(model3_cp)
BIC(model3_cp)

model3_cp2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=50,b = 0.01), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Hondawara_CentralPacific,
                trace=TRUE)
summary(model3_cp2)
AIC(model3_cp2)
BIC(model3_cp2)

####二次関数モデル#####
model4_cp<- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
              start = list(a=1, b=1,c=1), data =  Hondawara_CentralPacific)
summary(model4_cp)
AIC(model4_cp)
BIC(model4_cp)



####### Central Pacific @ Hondawara##########
#Model1
dummy_CentralPacific<- expand.grid(Cover=seq(min(Hondawara_CentralPacific$Cover),
                                             max(Hondawara_CentralPacific$Cover),length=1000))

pred_CentralPacific<- predict(model1_cp,newdata=dummy_CentralPacific,se.fit=T)
dummy_CentralPacific$DW<-pred_CentralPacific
  
plot_CentralPacific1<-ggplot(data=Hondawara_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
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

plot_CentralPacific2<-ggplot(data=Hondawara_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
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

plot_CentralPacific3<-ggplot(data=Hondawara_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
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


#Model4
pred_CentralPacific4<- predict(model4_cp,newdata=dummy_CentralPacific,se.fit=T)
dummy_CentralPacific$DW4<-pred_CentralPacific4

plot_CentralPacific4<-ggplot(data=Hondawara_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacific, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacific4

#width 600 * Height 500 save




####### SetoIslandSea  ##########
Hondawara_SetoIslandSea<-subset(Hondawara,Hondawara$Region=="SetoIslandSea")


####線形モデル#####
model1_ST<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Hondawara_SetoIslandSea)
summary(model1_ST)
AIC(model1_ST)
BIC(model1_ST)

####累乗モデル#####
model2_ST<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = Hondawara_SetoIslandSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_ST)
AIC(model2_ST)
BIC(model2_ST)

####指数モデル#####
model3_ST<- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=17.5,b =0.048), data = Hondawara_SetoIslandSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_ST)
AIC(model3_ST)
BIC(model3_ST)

model3_ST2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = Hondawara_SetoIslandSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_ST2)
AIC(model3_ST2)
BIC(model3_ST2)

####二次関数モデル#####
model4_ST<- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
              start = list(a=1, b=1,c=1), data =  Hondawara_SetoIslandSea)
summary(model4_ST)
AIC(model4_ST)
BIC(model4_ST)

######Plot line  SetoIslandSea @ Sado##########
#Model1
dummy_SetoIslandSea<- expand.grid(Cover=seq(min(Hondawara_SetoIslandSea$Cover),
                                            max(Hondawara_SetoIslandSea$Cover),length=1000))
pred_SetoIslandSea1<- predict(model1_ST,newdata=dummy_SetoIslandSea,se.fit=T)
dummy_SetoIslandSea$DW1<-pred_SetoIslandSea1

plot_SetoIslandSea1<-ggplot(data=Hondawara_SetoIslandSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_SetoIslandSea, aes(x=Cover, y=DW1),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SetoIslandSea1


#Model2
pred_SetoIslandSea2<- predict(model2_ST,newdata=dummy_SetoIslandSea,se.fit=T)
dummy_SetoIslandSea$DW2<-pred_SetoIslandSea2

plot_SetoIslandSea2<-ggplot(data=Hondawara_SetoIslandSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
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

plot_SetoIslandSea3<-ggplot(data=Hondawara_SetoIslandSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SetoIslandSea, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SetoIslandSea3

#Model4
pred_SetoIslandSea4<- predict(model4_ST,newdata=dummy_SetoIslandSea,se.fit=T)
dummy_SetoIslandSea$DW4<-pred_SetoIslandSea4

plot_SetoIslandSea4<-ggplot(data=Hondawara_SetoIslandSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SetoIslandSea, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SetoIslandSea4

#width 600 * Height 500 save

####### SouthJapanSea  ##########
Hondawara_SouthJapanSea<-subset(Hondawara,Hondawara$Region=="SouthJapanSea")


####線形モデル#####
model1 <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Hondawara_SouthJapanSea)
summary(model1)
AIC(model1)
BIC(model1)

####累乗モデル#####
model2 <- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = Hondawara_SouthJapanSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2)
AIC(model2)
BIC(model2)

####指数モデル#####
model3 <- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=17.5,b =0.048), data = Hondawara_SouthJapanSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3)
AIC(model3)
BIC(model3)

model3_2 <- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=17.5,b =0.048), data = Hondawara_SouthJapanSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_2)
AIC(model3_2)
BIC(model3_2)

####二次関数モデル#####
model4 <- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
              start = list(a=-0.2, b=20,c=-50), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data =  Hondawara_SouthJapanSea)
summary(model4)
AIC(model4)
BIC(model4)

######Plot line  SouthJapanSea ##########
#Model1
dummy_SouthJapanSea<- expand.grid(Cover=seq(min(Hondawara_SouthJapanSea$Cover),
                                            max(Hondawara_SouthJapanSea$Cover),length=1000))
pred_SouthJapanSea1<- predict(model1,newdata=dummy_SouthJapanSea,se.fit=T)
dummy_SouthJapanSea$DW1<-pred_SouthJapanSea1

plot_SouthJapanSea1<-ggplot(data=Hondawara_SouthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthJapanSea, aes(x=Cover, y=DW1),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthJapanSea1


#Model2
dummy_SouthJapanSea<- expand.grid(Cover=seq(min(Hondawara_SouthJapanSea$Cover),
                                            max(Hondawara_SouthJapanSea$Cover),length=1000))
pred_SouthJapanSea2<- predict(model2,newdata=dummy_SouthJapanSea,se.fit=T)
dummy_SouthJapanSea$DW2<-pred_SouthJapanSea2

plot_SouthJapanSea2<-ggplot(data=Hondawara_SouthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthJapanSea, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthJapanSea2

#Model3
pred_SouthJapanSea3<- predict(model3_2,newdata=dummy_SouthJapanSea,se.fit=T)
dummy_SouthJapanSea$DW3<-pred_SouthJapanSea3

plot_SouthJapanSea3<-ggplot(data=Hondawara_SouthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
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

plot_SouthJapanSea4<-ggplot(data=Hondawara_SouthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthJapanSea, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthJapanSea4

#width 600 * Height 500 save

####### EastChinaSea  ##########
Hondawara_EastChinaSea<-subset(Hondawara,Hondawara$Region=="EastChinaSea")


####線形モデル#####
model1_EC <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Hondawara_EastChinaSea)
summary(model1_EC)
AIC(model1_EC)
BIC(model1_EC)

####累乗モデル#####
model2_EC<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = Hondawara_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_EC)
AIC(model2_EC)
BIC(model2_EC)

####指数モデル#####
model3_EC<- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=17.5,b =0.048), data = Hondawara_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_EC)
AIC(model3_EC)
BIC(model3_EC)


model3_EC2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = Hondawara_EastChinaSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_EC2)
AIC(model3_EC2)
BIC(model3_EC2)

####二次関数モデル#####
model4_EC<- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
              start = list(a=-0.2, b=20,c=-50), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data =  Hondawara_EastChinaSea)
summary(model4_EC)
AIC(model4_EC)
BIC(model4_EC)

######Plot line  EastChinaSea ##########
#Model1
dummy_EastChinaSea<- expand.grid(Cover=seq(min(Hondawara_EastChinaSea$Cover),
                                            max(Hondawara_EastChinaSea$Cover),length=1000))
pred_EastChinaSea1<- predict(model1_EC,newdata=dummy_EastChinaSea,se.fit=T)
dummy_EastChinaSea$DW1<-pred_EastChinaSea1

plot_EastChinaSea1<-ggplot(data=Hondawara_EastChinaSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=6),limits=c(0,1000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_EastChinaSea, aes(x=Cover, y=DW1),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSea1


#Model2
dummy_EastChinaSea<- expand.grid(Cover=seq(min(Hondawara_EastChinaSea$Cover),
                                           max(Hondawara_EastChinaSea$Cover),length=1000))
pred_EastChinaSea2<- predict(model2_EC,newdata=dummy_EastChinaSea,se.fit=T)
dummy_EastChinaSea$DW2<-pred_EastChinaSea2

plot_EastChinaSea2<-ggplot(data=Hondawara_EastChinaSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  xlab("Coverage (%)")+ 
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=6),limits=c(0,1000))+ 
  geom_line(data=dummy_EastChinaSea, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSea2

#Model3
pred_EastChinaSea3<- predict(model3_EC,newdata=dummy_EastChinaSea,se.fit=T)
dummy_EastChinaSea$DW3<-pred_EastChinaSea3

plot_EastChinaSea3<-ggplot(data=Hondawara_EastChinaSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  ylim(0,500)+
  geom_point(size=3.5)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_EastChinaSea, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSea3

#Model4
pred_EastChinaSea4<- predict(model4_EC,newdata=dummy_EastChinaSea,se.fit=T)
dummy_EastChinaSea$DW4<-pred_EastChinaSea4

plot_EastChinaSea4<-ggplot(data=Hondawara_EastChinaSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  ylim(0,500)+
  geom_point(size=3.5)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_EastChinaSea, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSea4

#width 600 * Height 500 save

