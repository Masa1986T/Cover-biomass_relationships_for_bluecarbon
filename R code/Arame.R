rm(list=ls())
library(ggplot2)
library(propagate)
library(dplyr)

Arame<-read.csv("Arame.csv",stringsAsFactors = TRUE)
Arame$Season <- factor(Arame$Season, 
                       levels = c("Flourish","Decline") )
names(Arame)

####### TohokuPacific@Arame##########
Arame_TohokuPacific<-subset(Arame,Arame$Region=="TohokuPacific")
Arame_TohokuPacific$DW_g_m2

###Linear model#####
model1_TP<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Arame_TohokuPacific)
summary(model1_TP)
AIC(model1_TP)
BIC(model1_TP)

####Power model#####
model2_TP<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Arame_TohokuPacific)
summary(model2_TP)
AIC(model2_TP)
BIC(model2_TP)

####Exponential model#####
model3_TP<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Arame_TohokuPacific,
                trace=TRUE)
summary(model3_TP)
AIC(model3_cp)
BIC(model3_cp)


model3_TP2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Arame_TohokuPacific,
                trace=TRUE)
summary(model3_TP2)
AIC(model3_TP2)
BIC(model3_TP2)

####### Plot for  TohokuPacific##########
library(ggplot2)
library(propagate)
#Model1
dummy_TohokuPacific<- expand.grid(Cover=seq(min(Arame_TohokuPacific$Cover),
                                             max(Arame_TohokuPacific$Cover),length=1000))
pred_TohokuPacific<- predict(model1_TP,newdata=dummy_TohokuPacific,se.fit=T)
dummy_TohokuPacific$DW<-pred_TohokuPacific

plot_TohokuPacific1<-ggplot(data=Arame_TohokuPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
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
plot_pred_TohokuPacific1

#Model2
dummy_TohokuPacific2<- expand.grid(Cover=seq(min(Arame_TohokuPacific$Cover),
                                            max(Arame_TohokuPacific$Cover),length=1000))
pred_TohokuPacific2<- predict(model2_TP,newdata=dummy_TohokuPacific2,se.fit=T)
dummy_TohokuPacific2$DW<-pred_TohokuPacific2

plot_TohokuPacific2<-ggplot(data=Arame_TohokuPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 12000,length=7),limits=c(0,12000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_TohokuPacific2, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_TohokuPacific2

#Model3
dummy_TohokuPacific3<- expand.grid(Cover=seq(min(Arame_TohokuPacific$Cover),
                                             max(Arame_TohokuPacific$Cover),length=1000))
pred_TohokuPacific3<- predict(model3_TP2,newdata=dummy_TohokuPacific3,se.fit=T)
dummy_TohokuPacific3$DW<-pred_TohokuPacific3

plot_TohokuPacific3<-ggplot(data=Arame_TohokuPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 12000,length=7),limits=c(0,12000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_TohokuPacific3, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_TohokuPacific3


####### Central Pacific Choshi @ Arame##########
Arame_CentralPacificCH<-subset(Arame,Arame$Region=="CentralPacific"&Arame$Site.1=="Choshi")


####Linear model#####
model1_CH<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Arame_CentralPacificCH)
summary(model1_CH)
AIC(model1_CH)
BIC(model1_CH)

####Power model#####
model2_CH<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=10,b = 1),data = Arame_CentralPacificCH)
summary(model2_CH)
AIC(model2_CH)
BIC(model2_CH)

####Exponential model#####
model3_CH<- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=50,b = 0.01), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data = Arame_CentralPacificCH,
              trace=TRUE)
summary(model3_CH)
AIC(model3_CH)
BIC(model3_CH)

model3_CH2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=50,b = 0.01), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Arame_CentralPacificCH,
                trace=TRUE)
summary(model3_CH2)
AIC(model3_CH2)
BIC(model3_CH2)

####### Central Pacific Choshi##########
#Model1
dummy_CentralPacificCH1<- expand.grid(Cover=seq(min(Arame_CentralPacificCH$Cover),
                                            max(Arame_CentralPacificCH$Cover),length=1000))
pred_CentralPacificCH<- predict(model1_CH,newdata=dummy_CentralPacificCH1,se.fit=T)
dummy_CentralPacificCH1$DW<-pred_CentralPacificCH

plot_CentralPacificCH1<-ggplot(data=Arame_CentralPacificCH, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacificCH1, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacificCH1

#Model2
dummy_CentralPacificCH2<- expand.grid(Cover=seq(min(Arame_CentralPacificCH$Cover),
                                               max(Arame_CentralPacificCH$Cover),length=1000))
pred_CentralPacificCH2<- predict(model2_CH,newdata=dummy_TohokuPacific,se.fit=T)
dummy_CentralPacificCH2$DW<-pred_CentralPacificCH2

plot_CentralPacificCH2<-ggplot(data=Arame_CentralPacificCH, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacificCH2, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacificCH2

# save width 600 * Height 500 
#Model 3
dummy_CentralPacificCH3<- expand.grid(Cover=seq(min(Arame_CentralPacificCH$Cover),
                                                max(Arame_CentralPacificCH$Cover),length=1000))
pred_CentralPacificCH3<- predict(model3_CH2,newdata=dummy_CentralPacificCH3,se.fit=T)
dummy_CentralPacificCH3$DW<-pred_CentralPacificCH3

plot_CentralPacificCH3<-ggplot(data=Arame_CentralPacificCH, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacificCH3, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacificCH3

#width 600 * Height 500 save


####### Central Pacific Sagami @ Arame##########
Arame_CentralPacific<-subset(Arame,Arame$Region=="CentralPacific"&Arame$Site=="神奈川県")
####Linear model#####
model1_cp<- nls(DW_g_m2~ a*Cover,
                start = list(a=50),data = Arame_CentralPacific)
summary(model1_cp)
AIC(model1_cp)
BIC(model1_cp)

####Power model#####
model2_cp<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),data = Arame_CentralPacific)
summary(model2_cp)
AIC(model2_cp)
BIC(model2_cp)

####Exponential model#####
model3_cp<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=50,b = 0.01), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Arame_CentralPacific,
                trace=TRUE)
summary(model3_cp)
AIC(model3_cp)
BIC(model3_cp)

model3_cp2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=50,b = 0.01), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Arame_CentralPacific,
                trace=TRUE)
summary(model3_cp2)
AIC(model3_cp2)
BIC(model3_cp2)


####### Central Pacific @ Sagami##########
#Model1
dummy_CentralPacific1<- expand.grid(Cover=seq(min(Arame_CentralPacific$Cover,na.rm = TRUE),
                                               max(Arame_CentralPacific$Cover,na.rm = TRUE),length=1000))
pred_CentralPacific1<- predict(model1_cp,newdata=dummy_CentralPacific1,se.fit=T)
dummy_CentralPacific1$DW<-pred_CentralPacific1

plot_CentralPacific1<-ggplot(data=Arame_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacific1, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacific1

#Model2
dummy_CentralPacific2<- expand.grid(Cover=seq(min(Arame_CentralPacific$Cover),
                                             max(Arame_CentralPacific$Cover),length=1000))
pred_CentralPacific2<- predict(model2_cp,newdata=dummy_CentralPacific2,se.fit=T)
dummy_CentralPacific2$DW<-pred_CentralPacific2

plot_CentralPacific2<-ggplot(data=Arame_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacific2, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacific2

#Model3
dummy_CentralPacific3<- expand.grid(Cover=seq(min(Arame_CentralPacific$Cover,na.rm = TRUE),
                                              max(Arame_CentralPacific$Cover,na.rm = TRUE),length=1000))
pred_CentralPacific3<- predict(model3_cp2,newdata=dummy_CentralPacific3,se.fit=T)
dummy_CentralPacific3$DW<-pred_CentralPacific3

plot_CentralPacific3<-ggplot(data=Arame_CentralPacific, aes(x=Cover, y=DW_g_m2)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=6),limits=c(0,1000))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacific3, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacific3
#width 600 * Height 500 save

####### SouthJapanSea  ##########
Arame_SouthJapanSea<-subset(Arame,Arame$Region=="SouthJapanSea")


####Linear model#####
model1_SJS<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Arame_SouthJapanSea)
summary(model1_SJS)
AIC(model1_SJS)
BIC(model1_SJS)

####Power model#####
model2_SJS<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = Arame_SouthJapanSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_SJS)
AIC(model2_SJS)
BIC(model2_SJS)

####Exponential model#####
model3_SJS<- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=17.5,b =0.048), data = Arame_SouthJapanSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_SJS)
AIC(model3_SJS)
BIC(model3_SJS)

model3_SJS2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                 start = list(a=17.5,b =0.048), data = Arame_SouthJapanSea,
                 control = list(maxiter = 50000, warnOnly = TRUE),
                 trace=TRUE)

summary(model3_SJS2)
AIC(model3_SJS2)
BIC(model3_SJS2)

######Plot line  SouthJapanSea ##########
#Model1
dummy_SouthJapanSea1<- expand.grid(Cover=seq(min(Arame_SouthJapanSea$Cover),
                                              max(Arame_SouthJapanSea$Cover),length=1000))
pred_SouthJapanSea1<- predict(model1_SJS,newdata=dummy_SouthJapanSea1,se.fit=T)
dummy_SouthJapanSea1$DW<-pred_SouthJapanSea1

plot_SouthJapanSea1<-ggplot(data=Arame_SouthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthJapanSea1, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthJapanSea1

#Model2
dummy_SouthJapanSea2<- expand.grid(Cover=seq(min(Arame_SouthJapanSea$Cover),
                                             max(Arame_SouthJapanSea$Cover),length=1000))
pred_SouthJapanSea2<- predict(model2_SJS,newdata=dummy_SouthJapanSea2,se.fit=T)
dummy_SouthJapanSea2$DW<-pred_SouthJapanSea2

plot_SouthJapanSea2<-ggplot(data=Arame_SouthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthJapanSea2, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthJapanSea2

#Model3
dummy_SouthJapanSea3<- expand.grid(Cover=seq(min(Arame_SouthJapanSea$Cover),
                                             max(Arame_SouthJapanSea$Cover),length=1000))
pred_SouthJapanSea3<- predict(model3_SJS,newdata=dummy_SouthJapanSea3,se.fit=T)
dummy_SouthJapanSea3$DW<-pred_SouthJapanSea3

plot_SouthJapanSea3<-ggplot(data=Arame_SouthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthJapanSea3, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthJapanSea3

#width 600 * Height 500 save

####### Sagarame @ SoutherPacific  ##########
Sagarame<-subset(Arame,Arame$Jap_name=="Sagarame")


####Linear model#####
model1_Saga<- nls(DW_g_m2~ a*Cover,
                  start = list(a=50),data = Sagarame)
summary(model1_Saga)
AIC(model1_Saga)
BIC(model1_Saga)

####Power model#####
model2_Saga<- nls(DW_g_m2~ a*Cover^b,
                  start = list(a=1.444,b = 1.3181),data = Sagarame,
                  control = list(maxiter = 50000, warnOnly = TRUE),
                  trace=TRUE)
summary(model2_Saga)
AIC(model2_Saga)
BIC(model2_Saga)

####Exponential model#####
model3_Saga<- nls(DW_g_m2~ a*exp(b*Cover),
                  start = list(a=17.5,b =0.048), data = Sagarame,
                  control = list(maxiter = 50000, warnOnly = TRUE),
                  trace=TRUE)

summary(model3_Saga)
AIC(model3_Saga)
BIC(model3_Saga)

model3_Saga2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                  start = list(a=17.5,b =0.048), data = Sagarame,
                  control = list(maxiter = 50000, warnOnly = TRUE),
                  trace=TRUE)

summary(model3_Saga2)
AIC(model3_Saga2)
BIC(model3_Saga2)


######Plot line  Sagarame @ Tokushima##########
#Model1
dummy_Sagarame1<- expand.grid(Cover=seq(min(Sagarame$Cover),
                                             max(Sagarame$Cover),length=1000))
pred_Sagarame1<- predict(model1_Saga,newdata=dummy_Sagarame1,se.fit=T)
dummy_Sagarame1$DW<-pred_Sagarame1

plot_Sagarame1<-ggplot(data=Sagarame, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_Sagarame1, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Sagarame1


#Model2
dummy_Sagarame2<- expand.grid(Cover=seq(min(Sagarame$Cover),
                                        max(Sagarame$Cover),length=1000))
pred_Sagarame2<- predict(model2_Saga,newdata=dummy_Sagarame2,se.fit=T)
dummy_Sagarame2$DW<-pred_Sagarame2

plot_Sagarame2<-ggplot(data=Sagarame, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Sagarame2, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Sagarame2

#Model3
dummy_Sagarame3<- expand.grid(Cover=seq(min(Sagarame$Cover),
                                        max(Sagarame$Cover),length=1000))
pred_Sagarame3<- predict(model2_Saga,newdata=dummy_Sagarame3,se.fit=T)
dummy_Sagarame3$DW<-pred_Sagarame3

plot_Sagarame3<-ggplot(data=Sagarame, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Sagarame3, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Sagarame3

#width 600 * Height 500 save

####### EastChinaSea  ##########
Arame_EastChinaSea<-subset(Arame,Arame$Region=="EastChinaSea")

####Linear model#####
model1_EC <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Arame_EastChinaSea)
summary(model1_EC)
AIC(model1_EC)
BIC(model1_EC)

####Power model#####
model2_EC<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = Arame_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_EC)
AIC(model2_EC)
BIC(model2_EC)

####Exponential model#####
model3_EC<- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=17.5,b =0.048), data = Arame_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_EC)
AIC(model3_EC)
BIC(model3_EC)

model3_EC2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = Arame_EastChinaSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_EC2)
AIC(model3_EC2)
BIC(model3_EC2)


######Plot line  EastChinaSea ##########
#Model1
dummy_EastChinaSea1<- expand.grid(Cover=seq(min(Arame_EastChinaSea$Cover),
                                             max(Arame_EastChinaSea$Cover),length=1000))
pred_EastChinaSea1<- predict(model1_EC,newdata=dummy_EastChinaSea1,se.fit=T)
dummy_EastChinaSea1$DW<-pred_EastChinaSea1

plot_EastChinaSea1<-ggplot(data=Arame_EastChinaSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_EastChinaSea1, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSea1

#Model2
dummy_EastChinaSea2<- expand.grid(Cover=seq(min(Arame_EastChinaSea$Cover),
                                            max(Arame_EastChinaSea$Cover),length=1000))
pred_EastChinaSea2<- predict(model2_EC,newdata=dummy_EastChinaSea2,se.fit=T)
dummy_EastChinaSea2$DW<-pred_EastChinaSea2

plot_EastChinaSea2<-ggplot(data=Arame_EastChinaSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_EastChinaSea2, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSea2

#Model3
dummy_EastChinaSea3<- expand.grid(Cover=seq(min(Arame_EastChinaSea$Cover),
                                            max(Arame_EastChinaSea$Cover),length=1000))
pred_EastChinaSea3<- predict(model3_EC,newdata=dummy_EastChinaSea3,se.fit=T)
dummy_EastChinaSea3$DW<-pred_EastChinaSea3

plot_EastChinaSea3<-ggplot(data=Arame_EastChinaSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_EastChinaSea3, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSea3
######Plot line  all species  ##########
dummy_TohokuPacific3$RegionClass<-"TP Arame"
dummy_CentralPacificCH3$RegionClass<-"CP Arame"
dummy_SouthJapanSea1$RegionClass<-"SJS Arame"
dummy_Sagarame1$RegionClass<-"CP&SP Sagarame"
dummy_EastChinaSea1$RegionClass<-"ECS Arame"


dummy_Arame<-rbind(dummy_TohokuPacific3,dummy_CentralPacificCH3,dummy_SouthJapanSea1,
             dummy_Sagarame1,dummy_EastChinaSea1)

dummy_Arame$RegionClass<-factor(dummy_Arame$RegionClass, 
                                 levels = c("TP Arame","CP Arame","SJS Arame","CP&SP Sagarame","ECS Arame") )



library(ggplot2)

plot_Arame_all<-ggplot(data=dummy_Arame, aes(x=Cover, y=DW,linetype = RegionClass,color= RegionClass)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 10000,length=6),limits=c(0,10000))+ 
  geom_line(linewidth=1.25)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.55), 
        legend.title=element_text(size=20),legend.text =  element_text(size = 20))
plot_Arame_all

#save as 1400 * 400

