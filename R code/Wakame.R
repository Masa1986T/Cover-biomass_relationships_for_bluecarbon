rm(list=ls())
library(nlme)
library(ggplot2)

Wakame<-read.csv("Wakame.csv",stringsAsFactors = TRUE)
Wakame$Season <- factor(Wakame$Season, 
                       levels = c("Flourish","Decline") )
names(Wakame)

####### TohokuPacific @ Wakame##########
Wakame_TohokuPacific<-subset(Wakame,Wakame$Region=="TohokuPacific")
Wakame_TohokuPacific$DW_g_m2

###線形モデル#####
model1_TP<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Wakame_TohokuPacific)
summary(model1_TP)
AIC(model1_TP)
BIC(model1_TP)

####累乗モデル#####
model2_TP<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Wakame_TohokuPacific)
summary(model2_TP)
AIC(model2_TP)
BIC(model2_TP)

####指数モデル#####
model3_TP<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Wakame_TohokuPacific,
                trace=TRUE)
summary(model3_TP)
AIC(model3_TP)
BIC(model3_TP)


model3_TP2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Wakame_TohokuPacific,
                trace=TRUE)
summary(model3_TP2)
AIC(model3_TP2)
BIC(model3_TP2)



####### TohokuPacific##########
#Model1
dummy_TohokuPacific<- expand.grid(Cover=seq(min(Wakame_TohokuPacific$Cover),
                                             max(Wakame_TohokuPacific$Cover),length=1000))

pred_TohokuPacific<- predict(model1_TP,newdata=dummy_TohokuPacific,se.fit=T)
dummy_TohokuPacific$DW<-pred_TohokuPacific

plot_TohokuPacific1<-ggplot(data=Wakame_TohokuPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 12000,length=7),limits=c(0,12000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_TohokuPacific, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_TohokuPacific1

#Model2
pred_TohokuPacific2<- predict(model2_TP,newdata=dummy_TohokuPacific,se.fit=T)
dummy_TohokuPacific$DW2<-pred_TohokuPacific2

plot_TohokuPacific2<-ggplot(data=Wakame_TohokuPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 12000,length=7),limits=c(0,12000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_TohokuPacific, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_TohokuPacific2

# save width 600 * Height 500 
#Model 3
dummy_TohokuPacific<- expand.grid(Cover=seq(min(Wakame_TohokuPacific$Cover),
                                            max(Wakame_TohokuPacific$Cover),length=1000))
pred_TohokuPacific3<- predict(model3_TP2,newdata=dummy_TohokuPacific,se.fit=T)
dummy_TohokuPacific$DW3<-pred_TohokuPacific3

plot_TohokuPacific3<-ggplot(data=Wakame_TohokuPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 2500,length=6),limits=c(0,2500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_TohokuPacific, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_TohokuPacific3


#Model4
pred_TohokuPacific4<- predict(model4_TP,newdata=dummy_TohokuPacific,se.fit=T)
dummy_TohokuPacific$DW4<-pred_TohokuPacific4

plot_TohokuPacific4<-ggplot(data=Wakame_TohokuPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 12000,length=7),limits=c(0,12000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_TohokuPacific, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_TohokuPacific4

#width 600 * Height 500 save

####### NorthJapanSea @ Sado ##########
Wakame_NorthJapanSea<-subset(Wakame,Wakame$Region=="NorthJapanSea")


####線形モデル#####
model1_NJ<- nls(DW_g_m2~ a*Cover,
                start = list(a=50),data = Wakame_NorthJapanSea)
summary(model1_NJ)
AIC(model1_NJ)
BIC(model1_NJ)

####累乗モデル#####
model2_NJ<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=1.444,b = 1.3181),data = Wakame_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)
summary(model2_NJ)
AIC(model2_NJ)
BIC(model2_NJ)

####指数モデル#####
model3_NJ<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=17.5,b =0.048), data = Wakame_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_NJ)
AIC(model3_NJ)
BIC(model3_NJ)

model3_NJ2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = Wakame_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_NJ2)
AIC(model3_NJ2)
BIC(model3_NJ2)


####二次関数モデル#####
model4_NJ<- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
                start = list(a=1, b=1,c=1), data =  Wakame_NorthJapanSea)
summary(model4_NJ)
AIC(model4_NJ)
BIC(model4_NJ)


#######Plot line  NorthJapanSea @ Sado##########
#Model1
dummy_NorthJapanSea<- expand.grid(Cover=seq(min(Wakame_NorthJapanSea$Cover),
                                            max(Wakame_NorthJapanSea$Cover),length=1000))
pred_NorthJapanSea1<- predict(model1_NJ,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea$DW1<-pred_NorthJapanSea1

plot_NorthJapanSea1<-ggplot(data=Wakame_NorthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 2500,length=6),limits=c(0,2500))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_NorthJapanSea, aes(x=Cover, y=DW1),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_NorthJapanSea1


#Model2
dummy_NorthJapanSea<- expand.grid(Cover=seq(min(Wakame_NorthJapanSea$Cover),
                                            max(Wakame_NorthJapanSea$Cover),length=1000))
pred_NorthJapanSea2<- predict(model2_NJ,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea$DW2<-pred_NorthJapanSea2

plot_NorthJapanSea2<-ggplot(data=Wakame_NorthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
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
dummy_NorthJapanSea<- expand.grid(Cover=seq(min(Wakame_NorthJapanSea$Cover),
                                            max(Wakame_NorthJapanSea$Cover),length=1000))
pred_NorthJapanSea3<- predict(model3_NJ2,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea$DW3<-pred_NorthJapanSea3

plot_NorthJapanSea3<-ggplot(data=Wakame_NorthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
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

#Model4
pred_NorthJapanSea4<- predict(model4_NJ,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea$DW4<-pred_NorthJapanSea4

plot_NorthJapanSea4<-ggplot(data=Wakame_NorthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
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


####### Central Pacific Sagami @ Wakame##########
Wakame_CentralPacific<-subset(Wakame,Wakame$Region=="CentralPacific")

Wakame_CentralPacific$DW_g_m2

####線形モデル#####
model1_CP<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Wakame_CentralPacific)
summary(model1_CP)
AIC(model1_CP)
BIC(model1_CP)

####累乗モデル#####
model2_CP<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=10,b = 1),data = Wakame_CentralPacific)
summary(model2_CP)
AIC(model2_CP)
BIC(model2_CP)

####指数モデル#####
model3_CP<- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=50,b = 0.01), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data = Wakame_CentralPacific,
              trace=TRUE)
summary(model3_CP)
AIC(model3_CP)
BIC(model3_CP)

model3_CP2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=50,b = 0.01), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Wakame_CentralPacific,
                trace=TRUE)
summary(model3_CP2)
AIC(model3_CP2)
BIC(model3_CP2)

####二次関数モデル#####
model4_CP<- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
              start = list(a=1, b=1,c=1), data =  Wakame_CentralPacific)
summary(model4_CP)
AIC(model4_CP)
BIC(model4_CP)



####### Central Pacific@Sagami##########
#Model1
dummy_CentralPacific<- expand.grid(Cover=seq(min(Wakame_CentralPacific$Cover),
                                             max(Wakame_CentralPacific$Cover),length=1000))
pred_CentralPacific<- predict(model1_CP,newdata=dummy_CentralPacific,se.fit=T)
dummy_CentralPacific$DW<-pred_CentralPacific
  
plot_CentralPacific<-ggplot(data=Wakame_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacific, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacific

#Model2
dummy_CentralPacific<- expand.grid(Cover=seq(min(Wakame_CentralPacific$Cover),
                                             max(Wakame_CentralPacific$Cover),length=1000))
pred_CentralPacific2<- predict(model2_CP,newdata=dummy_CentralPacific,se.fit=T)
dummy_CentralPacific$DW2<-pred_CentralPacific2

plot_CentralPacific2<-ggplot(data=Wakame_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 2500,length=6),limits=c(0,2500))+
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
pred_CentralPacific3<- predict(model3_CP2,newdata=dummy_CentralPacific,se.fit=T)
dummy_CentralPacific$DW3<-pred_CentralPacific3

plot_CentralPacific3<-ggplot(data=Wakame_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 2500,length=6),limits=c(0,2500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacific, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacific3


#Model4
pred_CentralPacific4<- predict(model4_CP,newdata=dummy_CentralPacific,se.fit=T)
dummy_CentralPacific$DW4<-pred_CentralPacific4

plot_CentralPacific4<-ggplot(data=Wakame_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacific, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacific4

#width 600 * Height 500 save
dummy_TohokuPacific1<- expand.grid(Cover=seq(min(Wakame_TohokuPacific$Cover),
                                            max(Wakame_TohokuPacific$Cover),length=1000))
pred_TohokuPacific3<- predict(model3_TP2,newdata=dummy_TohokuPacific1,se.fit=T)
dummy_TohokuPacific1$DW<-pred_TohokuPacific3
dummy_TohokuPacific1$RegionClass<-"TP Wakame"

dummy_NorthJapanSea1<- expand.grid(Cover=seq(min(Wakame_NorthJapanSea$Cover),
                                            max(Wakame_NorthJapanSea$Cover),length=1000))
pred_NorthJapanSea1<- predict(model1_NJ,newdata=dummy_NorthJapanSea1,se.fit=T)
dummy_NorthJapanSea1$DW<-pred_NorthJapanSea1
dummy_NorthJapanSea1$RegionClass<-"NJS Wakame"

dummy_CentralPacific1<- expand.grid(Cover=seq(min(Wakame_CentralPacific$Cover),
                                             max(Wakame_CentralPacific$Cover),length=1000))
pred_CentralPacific2<- predict(model3_CP2,newdata=dummy_CentralPacific1,se.fit=T)
dummy_CentralPacific1$DW<-pred_CentralPacific2
dummy_CentralPacific1$RegionClass<-"CP Wakame"


dummy_Wakame<-rbind(dummy_TohokuPacific1,dummy_NorthJapanSea1,dummy_CentralPacific1)
dummy_Wakame$RegionClass<-factor(dummy_Wakame$RegionClass, 
                                levels = c("TP Wakame","NJS Wakame","CP Wakame") )

plot_Wakame_all<-ggplot(data=dummy_Wakame, aes(x=Cover, y=DW,linetype = RegionClass,color= RegionClass)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+ 
  geom_line(linewidth=1.25)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.55), 
        legend.title=element_text(size=20),legend.text =  element_text(size = 20))
plot_Wakame_all
