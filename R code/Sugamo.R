#######################################################
### R codes for Sugamo (Phyllospadix iwatensis) #########
######################################################

rm(list=ls())
library(nlme)
library(ggplot2)

Sugamo<-read.csv("Sugamo.csv",stringsAsFactors = TRUE)
Sugamo$Season <- factor(Sugamo$Season, 
                       levels = c("Flourish","Decline") )
names(Sugamo)

####### Hokkaido @ Sugamo##########
Sugamo_Hokkaido<-subset(Sugamo,Sugamo$Region=="Hokkaido")
Sugamo_Hokkaido$DW_g_m2

###Linear model#####
model1_HD<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Sugamo_Hokkaido)
summary(model1_HD)
AIC(model1_HD)
BIC(model1_HD)

####Power model#####
model2_HD<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Sugamo_Hokkaido)
summary(model2_HD)
AIC(model2_HD)
BIC(model2_HD)

####Exponential model#####
model3_HD<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Sugamo_Hokkaido,
                trace=TRUE)
summary(model3_HD)
AIC(model3_HD)
BIC(model3_HD)

model3_HD2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Sugamo_Hokkaido,
                trace=TRUE)
summary(model3_HD2)
AIC(model3_HD2)
BIC(model3_HD2)




####### Hokkaido##########
library(ggplot2)

#Model1
dummy_Hokkaido<- expand.grid(Cover=seq(min(Sugamo_Hokkaido$Cover),
                                            max(Sugamo_Hokkaido$Cover),length=1000))

pred_Hokkaido<- predict(model1_HD,newdata=dummy_Hokkaido,se.fit=T)
dummy_Hokkaido$DW<-pred_Hokkaido

plot_Hokkaido1<-ggplot(data=Sugamo_Hokkaido, aes(x=Cover, y=DW_g_m2,color = Season)) + 
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
dummy_Hokkaido<- expand.grid(Cover=seq(min(Sugamo_Hokkaido$Cover),
                                       max(Sugamo_Hokkaido$Cover),length=1000))
pred_Hokkaido2<- predict(model2_HD,newdata=dummy_Hokkaido,se.fit=T)
dummy_Hokkaido$DW2<-pred_Hokkaido2

plot_Hokkaido2<-ggplot(data=Sugamo_Hokkaido, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=5),limits=c(0,1500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
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

plot_Hokkaido3<-ggplot(data=Sugamo_Hokkaido, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido3



#width 600 * Height 500 save

####### CentralPacific @ Sugamo##########
Sugamo_CentralPacific<-subset(Sugamo,Sugamo$Region=="CentralPacific")
Sugamo_CentralPacific$DW_g_m2

###Linear model#####
model1_CP<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Sugamo_CentralPacific)
summary(model1_CP)
AIC(model1_CP)
BIC(model1_CP)

####Power model#####
model2_CP<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Sugamo_CentralPacific)
summary(model2_CP)
AIC(model2_CP)
BIC(model2_CP)

####Exponential model#####
model3_CP<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Sugamo_CentralPacific,
                trace=TRUE)
summary(model3_CP)
AIC(model3_CP)
BIC(model3_CP)

model3_CP2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Sugamo_CentralPacific,
                trace=TRUE)
summary(model3_CP2)
AIC(model3_CP2)
BIC(model3_CP2)



####### CentralPacific##########
library(ggplot2)

#Model1
dummy_CentralPacific<- expand.grid(Cover=seq(min(Sugamo_CentralPacific$Cover),
                                             max(Sugamo_CentralPacific$Cover),length=1000))

pred_CentralPacific<- predict(model1_CP,newdata=dummy_CentralPacific,se.fit=T)
dummy_CentralPacific$DW<-pred_CentralPacific

plot_CentralPacific1<-ggplot(data=Sugamo_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacific, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacific1

#Model2
pred_CentralPacific2<- predict(model2_CP,newdata=dummy_CentralPacific,se.fit=T)
dummy_CentralPacific$DW2<-pred_CentralPacific2

plot_CentralPacific2<-ggplot(data=Sugamo_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=5),limits=c(0,1000))+
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
pred_CentralPacific3<- predict(model3_CP,newdata=dummy_CentralPacific,se.fit=T)
dummy_CentralPacific$DW3<-pred_CentralPacific3

plot_CentralPacific3<-ggplot(data=Sugamo_CentralPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=5),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_CentralPacific, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_CentralPacific3




#width 600 * Height 500 save

dummy_Hokkaido1<- expand.grid(Cover=seq(min(Sugamo_Hokkaido$Cover),
                                        max(Sugamo_Hokkaido$Cover),length=1000))
pred_Hokkaido2<- predict(model3_HD2,newdata=dummy_Hokkaido1,se.fit=T)
dummy_Hokkaido1$DW<-pred_Hokkaido2
dummy_Hokkaido1$RegionClass<-"HD Sugamo"

dummy_CentralPacific1<- expand.grid(Cover=seq(min(Sugamo_CentralPacific$Cover),
                                              max(Sugamo_CentralPacific$Cover),length=1000))
pred_CentralPacific<- predict(model1_TP,newdata=dummy_CentralPacific1,se.fit=T)
dummy_CentralPacific1$DW<-pred_CentralPacific
dummy_CentralPacific1$RegionClass<-"CP Sugamo"


########################################################################
####### Comparison of the relationships for Sugamo among regions#########
########################################################################

dummy_Sugamo<-rbind(dummy_Hokkaido1,dummy_CentralPacific1)

dummy_Sugamo$RegionClass<-factor(dummy_Sugamo$RegionClass, 
                                 levels = c("HD Sugamo","CP Sugamo") )
library(ggplot2)

plot_Sugamo_all<-ggplot(data=dummy_Sugamo, aes(x=Cover, y=DW,linetype = RegionClass,color= RegionClass)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+ 
  geom_line(linewidth=1.25)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.55), 
        legend.title=element_text(size=20),legend.text =  element_text(size = 20))
plot_Sugamo_all



