rm(list=ls())
library(ggplot2)

Smallalgae<-read.csv("Smallalgae.csv",stringsAsFactors = TRUE)
Smallalgae$Season <- factor(Smallalgae$Season, levels = c("Flourish","Decline") )
names(Smallalgae)

####### Hokkaido @ Uganomoku##########
Smallalgae_Hokkaido<-subset(Smallalgae,Smallalgae$Region=="Hokkaido")
Smallalgae_Hokkaido$DW_g_m2

###Linear model#####
model1_HD<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Smallalgae_Hokkaido)
summary(model1_HD)
AIC(model1_HD)
BIC(model1_HD)

####Power model#####
model2_HD<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Smallalgae_Hokkaido)
summary(model2_HD)
AIC(model2_HD)
BIC(model2_HD)

####Exponential model#####
model3_HD<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=1,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Smallalgae_Hokkaido,
                trace=TRUE)
summary(model3_HD)
AIC(model3_HD)
BIC(model3_HD)

model3_HD2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=1,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Smallalgae_Hokkaido,
                trace=TRUE)
summary(model3_HD2)
AIC(model3_HD2)
BIC(model3_HD2)






####### Hokkaido##########
library(ggplot2)

#Model1
dummy_Hokkaido<- expand.grid(Cover=seq(min(Smallalgae_Hokkaido$Cover),
                                       max(Smallalgae_Hokkaido$Cover),length=1000))

pred_Hokkaido<- predict(model1_HD,newdata=dummy_Hokkaido,se.fit=T)
dummy_Hokkaido$DW<-pred_Hokkaido

plot_Hokkaido1<-ggplot(data=Smallalgae_Hokkaido, aes(x=Cover, y=DW_g_m2,color = Season)) + 
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

plot_Hokkaido2<-ggplot(data=Smallalgae_Hokkaido, aes(x=Cover, y=DW_g_m2,color = Season)) + 
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
dummy_Hokkaido3<- expand.grid(Cover=seq(min(Smallalgae_Hokkaido$Cover),
                                       max(Smallalgae_Hokkaido$Cover),length=1000))
pred_Hokkaido3<- predict(model3_HD2,newdata=dummy_Hokkaido,se.fit=T)
dummy_Hokkaido3$DW3<-pred_Hokkaido3

plot_Hokkaido3<-ggplot(data=Smallalgae_Hokkaido, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=6),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido3, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido3





#width 600 * Height 500 save


####### NorthJapanSea @ Sado ##########
Smallalgae_NorthJapanSea<-subset(Smallalgae,Smallalgae$Region=="NorthJapanSea",na.rm = TRUE)
Smallalgae_NorthJapanSea<-subset(Smallalgae_NorthJapanSea,Smallalgae_NorthJapanSea$Jap_name=="Keurujigusa; Habamodoki",na.rm = TRUE)


####Linear model#####
model1_NJ<- nls(DW_g_m2~ a*Cover,
                start = list(a=50),data = Smallalgae_NorthJapanSea)
summary(model1_NJ)
AIC(model1_NJ)
BIC(model1_NJ)

####Power model#####
model2_NJ<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=1.444,b = 1.3181),data = Smallalgae_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)
summary(model2_NJ)
AIC(model2_NJ)
BIC(model2_NJ)

####Exponential model#####
model3_NJ<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=17.5,b =0.048), data = Smallalgae_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_NJ)
AIC(model3_NJ)
BIC(model3_NJ)

model3_NJ2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = Smallalgae_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_NJ2)
AIC(model3_NJ2)
BIC(model3_NJ2)



#######Plot line  NorthJapanSea @ Sado##########
#Model1
dummy_NorthJapanSea1<- expand.grid(Cover=seq(min(Smallalgae_NorthJapanSea$Cover,na.rm=T),
                                            max(Smallalgae_NorthJapanSea$Cover,na.rm=T),length=1000))
pred_NorthJapanSea1<- predict(model1_NJ,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea1$DW1<-pred_NorthJapanSea1

plot_NorthJapanSea1<-ggplot(data=Smallalgae_NorthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=6),limits=c(0,1000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_NorthJapanSea, aes(x=Cover, y=DW1),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_NorthJapanSea1


#Model2
dummy_NorthJapanSea<- expand.grid(Cover=seq(min(Smallalgae_NorthJapanSea$Cover),
                                            max(Smallalgae_NorthJapanSea$Cover),length=1000))
pred_NorthJapanSea2<- predict(model2_NJ,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea$DW2<-pred_NorthJapanSea2

plot_NorthJapanSea2<-ggplot(data=Smallalgae_NorthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
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
pred_NorthJapanSea3<- predict(model3_NJ,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea$DW3<-pred_NorthJapanSea3

plot_NorthJapanSea3<-ggplot(data=Smallalgae_NorthJapanSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
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

####### Central Pacific Kominato @ Umiuchiwa##########
Smallalgae_Kominato<-subset(Smallalgae,Smallalgae$Point=="Kominato")

####Linear model#####
model1_cp<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Smallalgae_Kominato)
summary(model1_cp)
AIC(model1_cp)
BIC(model1_cp)

####Power model#####
model2_cp<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=10,b = 1),data = Smallalgae_Kominato)
summary(model2_cp)
AIC(model2_cp)
BIC(model2_cp)

####Exponential model#####
model3_cp<- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=50,b = 0.01), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data = Smallalgae_Kominato,
              trace=TRUE)
summary(model3_cp)
AIC(model3_cp)
BIC(model3_cp)

model3_cp2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=50,b = 0.01), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Smallalgae_Kominato,
                trace=TRUE)
summary(model3_cp2)
AIC(model3_cp2)
BIC(model3_cp2)



####### Central Pacific @ Kominato##########
#Model1
dummy_Kominato<- expand.grid(Cover=seq(min(Smallalgae_Kominato$Cover),
                                             max(Smallalgae_Kominato$Cover),length=1000))

pred_Kominato<- predict(model1_cp,newdata=dummy_Kominato,se.fit=T)
dummy_Kominato$DW<-pred_Kominato
  
plot_Kominato1<-ggplot(data=Smallalgae_Kominato, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Kominato, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Kominato1

#Model2
pred_Kominato2<- predict(model2_cp,newdata=dummy_Kominato,se.fit=T)
dummy_Kominato$DW2<-pred_Kominato2

plot_Kominato2<-ggplot(data=Smallalgae_Kominato, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Kominato, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Kominato2

# save width 600 * Height 500 
#Model 3
dummy_Kominato3<- expand.grid(Cover=seq(min(Smallalgae_Kominato$Cover),
                                       max(Smallalgae_Kominato$Cover),length=1000))
pred_Kominato3<- predict(model3_cp2,newdata=dummy_Kominato,se.fit=T)
dummy_Kominato3$DW3<-pred_Kominato3

plot_Kominato3<-ggplot(data=Smallalgae_Kominato, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=6),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_Kominato3, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Kominato3


#Model4
pred_Kominato4<- predict(model4_cp,newdata=dummy_Kominato,se.fit=T)
dummy_Kominato$DW4<-pred_Kominato4

plot_Kominato4<-ggplot(data=Smallalgae_Kominato, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Kominato, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Kominato4

#width 600 * Height 500 save


###### Central Pacific Shimoda @ Amijigusa##########
Smallalgae_Shimoda<-subset(Smallalgae,Smallalgae$Point=="Shimoda")
Smallalgae_Shimoda_Amizi<-subset(Smallalgae,Smallalgae$Jap_name=="Amizigusa")



####Linear model#####
model1_SA<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Smallalgae_Shimoda_Amizi)
summary(model1_SA)
AIC(model1_SA)
BIC(model1_SA)

####Power model#####
model2_SA<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=10,b = 1),data = Smallalgae_Shimoda_Amizi)
summary(model2_SA)
AIC(model2_SA)
BIC(model2_SA)

####Exponential model#####
model3_SA<- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=50,b = 0.01), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data = Smallalgae_Shimoda_Amizi,
              trace=TRUE)
summary(model3_SA)
AIC(model3_SA)
BIC(model3_SA)

model3_SA2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=50,b = 0.01), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Smallalgae_Shimoda_Amizi,
                trace=TRUE)
summary(model3_SA2)
AIC(model3_SA2)
BIC(model3_SA2)



####### Central Pacific @ Shimoda_Amizi##########
#Model1
dummy_Shimoda_Amizi<- expand.grid(Cover=seq(min(Smallalgae_Shimoda_Amizi$Cover),
                                             max(Smallalgae_Shimoda_Amizi$Cover),length=1000))

pred_Shimoda_Amizi<- predict(model1_SA,newdata=dummy_Shimoda_Amizi,se.fit=T)
dummy_Shimoda_Amizi$DW<-pred_Shimoda_Amizi
  
plot_Shimoda_Amizi1<-ggplot(data=Smallalgae_Shimoda_Amizi, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Shimoda_Amizi, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Shimoda_Amizi1

#Model2
pred_Shimoda_Amizi2<- predict(model2_SA,newdata=dummy_Shimoda_Amizi,se.fit=T)
dummy_Shimoda_Amizi$DW2<-pred_Shimoda_Amizi2

plot_Shimoda_Amizi2<-ggplot(data=Smallalgae_Shimoda_Amizi, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Shimoda_Amizi, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Shimoda_Amizi2

# save width 600 * Height 500 
#Model 3
dummy_Shimoda_Amizi3<- expand.grid(Cover=seq(min(Smallalgae_Shimoda_Amizi$Cover),
                                            max(Smallalgae_Shimoda_Amizi$Cover),length=1000))
pred_Shimoda_Amizi3<- predict(model3_SA2,newdata=dummy_Shimoda_Amizi,se.fit=T)
dummy_Shimoda_Amizi3$DW3<-pred_Shimoda_Amizi3

plot_Shimoda_Amizi3<-ggplot(data=Smallalgae_Shimoda_Amizi, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=6),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_Shimoda_Amizi3, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Shimoda_Amizi3



#width 600 * Height 500 save

###### Central Pacific Shimoda @ Koso##########
Smallalgae_Shimoda<-subset(Smallalgae,Smallalgae$Site=="Shimoda")
Smallalgae_Shimoda_koso<-subset(Smallalgae_Shimoda,Smallalgae_Shimoda$Class=="Rhodophyta")



####Linear model#####
model1_SK<- nls(DW_g_m2~ a*Cover,
                start = list(a=50),data = Smallalgae_Shimoda_koso)
summary(model1_SK)
AIC(model1_SK)
BIC(model1_SK)

####Power model#####
model2_SK<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),data = Smallalgae_Shimoda_koso)
summary(model2_SK)
AIC(model2_SK)
BIC(model2_SK)

####Exponential model#####
model3_SK<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=50,b = 0.01), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Smallalgae_Shimoda_koso,
                trace=TRUE)
summary(model3_SK)
AIC(model3_SK)
BIC(model3_SK)

model3_SK2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=50,b = 0.01), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Smallalgae_Shimoda_koso,
                trace=TRUE)
summary(model3_SK2)
AIC(model3_SK2)
BIC(model3_SK2)





####### Central Pacific @ Shimoda_koso##########
#Model1
dummy_Shimoda_koso<- expand.grid(Cover=seq(min(Smallalgae_Shimoda_koso$Cover),
                                            max(Smallalgae_Shimoda_koso$Cover),length=1000))

pred_Shimoda_koso<- predict(model1_SK,newdata=dummy_Shimoda_koso,se.fit=T)
dummy_Shimoda_koso$DW<-pred_Shimoda_koso

plot_Shimoda_koso1<-ggplot(data=Smallalgae_Shimoda_koso, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Shimoda_koso, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Shimoda_koso1

#Model2
pred_Shimoda_koso2<- predict(model2_SK,newdata=dummy_Shimoda_koso,se.fit=T)
dummy_Shimoda_koso$DW2<-pred_Shimoda_koso2

plot_Shimoda_koso2<-ggplot(data=Smallalgae_Shimoda_koso, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Shimoda_koso, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Shimoda_koso2

# save width 600 * Height 500 
#Model 3
dummy_Shimoda_koso3<- expand.grid(Cover=seq(min(Smallalgae_Shimoda_koso$Cover),
                                           max(Smallalgae_Shimoda_koso$Cover),length=1000))
pred_Shimoda_koso3<- predict(model3_SK2,newdata=dummy_Shimoda_koso,se.fit=T)
dummy_Shimoda_koso3$DW3<-pred_Shimoda_koso3

plot_Shimoda_koso3<-ggplot(data=Smallalgae_Shimoda_koso, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=6),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_Shimoda_koso3, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Shimoda_koso3

#Model 3 Umiuchiwa+ Amijigusa +Koso
Smallalgae_CentralPacific<-subset(Smallalgae,Smallalgae$Region=="CentralPacific")


plot_Shimoda_mix3<-ggplot(data=Smallalgae_CentralPacific, aes(x=Cover, y=DW_g_m2,shape = Jap_name)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=6),limits=c(0,1000))+
  scale_shape_manual(labels = c("Amijigusa", "Tengusa","Umiuchiwa"),values = c(19,2,18))+
  labs(color='Season')+
  geom_line(data=dummy_Shimoda_Amizi, aes(x=Cover, y=DW3),linetype = 1,linewidth=1,color="black",inherit.aes = FALSE)+
  geom_line(data=dummy_Shimoda_koso, aes(x=Cover, y=DW3),linetype = 2,linewidth=1,color="black",inherit.aes = FALSE)+
  geom_line(data=dummy_Kominato, aes(x=Cover, y=DW3),linetype = 3,linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Shimoda_mix3

####### Tsukasaami @ SoutherPacific  ##########
Tsukasaami<-subset(Smallalgae,Smallalgae$Jap_name=="Tsukasaami")


####Linear model#####
model1_Tsuka<- nls(DW_g_m2~ a*Cover,
                  start = list(a=50),data = Tsukasaami)
summary(model1_Tsuka)
AIC(model1_Tsuka)
BIC(model1_Tsuka)

####Power model#####
model2_Tsuka<- nls(DW_g_m2~ a*Cover^b,
                  start = list(a=1.444,b = 1.3181),data = Tsukasaami,
                  control = list(maxiter = 50000, warnOnly = TRUE),
                  trace=TRUE)
summary(model2_Tsuka)
AIC(model2_Tsuka)
BIC(model2_Tsuka)

####Exponential model#####
model3_Tsuka<- nls(DW_g_m2~ a*exp(b*Cover),
                  start = list(a=17.5,b =0.048), data = Tsukasaami,
                  control = list(maxiter = 50000, warnOnly = TRUE),
                  trace=TRUE)

summary(model3_Tsuka)
AIC(model3_Tsuka)
BIC(model3_Tsuka)

model3_Tsuka2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                   start = list(a=17.5,b =0.048), data = Tsukasaami,
                   control = list(maxiter = 50000, warnOnly = TRUE),
                   trace=TRUE)

summary(model3_Tsuka2)
AIC(model3_Tsuka2)
BIC(model3_Tsuka2)


######Plot line  Tsukasaami @ Tokushima##########
#Model1
dummy_Tsukasaami<- expand.grid(Cover=seq(min(Tsukasaami$Cover),
                                       max(Tsukasaami$Cover),length=1000))
pred_Tsukasaami1<- predict(model1_Tsuka,newdata=dummy_Tsukasaami,se.fit=T)
dummy_Tsukasaami$DW1<-pred_Tsukasaami1

plot_Tsukasaami1<-ggplot(data=Tsukasaami, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Tsukasaami, aes(x=Cover, y=DW1),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Tsukasaami1


#Model2
dummy_Tsukasaami<- expand.grid(Cover=seq(min(Tsukasaami$Cover),
                                       max(Tsukasaami$Cover),length=1000))
pred_Tsukasaami2<- predict(model2_Tsuka,newdata=dummy_Tsukasaami,se.fit=T)
dummy_Tsukasaami$DW2<-pred_Tsukasaami2

plot_Tsukasaami2<-ggplot(data=Tsukasaami, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Tsukasaami, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Tsukasaami2

#Model3
dummy_Tsukasaami3<- expand.grid(Cover=seq(min(Tsukasaami$Cover),
                                         max(Tsukasaami$Cover),length=1000))
pred_Tsukasaami3<- predict(model3_Tsuka2,newdata=dummy_Tsukasaami,se.fit=T)
dummy_Tsukasaami3$DW3<-pred_Tsukasaami3

plot_Tsukasaami3<-ggplot(data=Tsukasaami, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=6),limits=c(0,1000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_Tsukasaami, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Tsukasaami3





#width 600 * Height 500 save

#################################################################################
#######Comparison of the relationships for small algae among regions##########
##############################################################################

dummy_Hokkaido1<- expand.grid(Cover=seq(min(Smallalgae_Hokkaido$Cover),
                                        max(Smallalgae_Hokkaido$Cover),length=1000))
pred_Hokkaido3<- predict(model3_HD2,newdata=dummy_Hokkaido1,se.fit=T)
dummy_Hokkaido1$DW<-pred_Hokkaido3
dummy_Hokkaido1$RegionClass<-"HD Anaaosa"

dummy_NorthJapanSea1<- expand.grid(Cover=seq(min(Smallalgae_NorthJapanSea$Cover,na.rm=T),
                                             max(Smallalgae_NorthJapanSea$Cover,na.rm=T),length=1000))
pred_NorthJapanSea1<- predict(model1_NJ,newdata=dummy_NorthJapanSea1,se.fit=T)
dummy_NorthJapanSea1$DW<-pred_NorthJapanSea1
dummy_NorthJapanSea1$RegionClass<-"NJS Kogatakasso"

dummy_Kominato1<- expand.grid(Cover=seq(min(Smallalgae_Kominato$Cover),
                                        max(Smallalgae_Kominato$Cover),length=1000))
pred_Kominato3<- predict(model3_cp2,newdata=dummy_Kominato1,se.fit=T)
dummy_Kominato1$DW<-pred_Kominato3
dummy_Kominato1$RegionClass<-"CP Umiuchiwa"


dummy_Shimoda_Amizi1<- expand.grid(Cover=seq(min(Smallalgae_Shimoda_Amizi$Cover),
                                             max(Smallalgae_Shimoda_Amizi$Cover),length=1000))
pred_Shimoda_Amizi3<- predict(model3_SA2,newdata=dummy_Shimoda_Amizi1,se.fit=T)
dummy_Shimoda_Amizi1$DW<-pred_Shimoda_Amizi3
dummy_Shimoda_Amizi1$RegionClass<-"CP Amijigusa"

dummy_Shimoda_koso1<- expand.grid(Cover=seq(min(Smallalgae_Shimoda_koso$Cover),
                                            max(Smallalgae_Shimoda_koso$Cover),length=1000))
pred_Shimoda_koso3<- predict(model3_SK2,newdata=dummy_Shimoda_koso1,se.fit=T)
dummy_Shimoda_koso1$DW<-pred_Shimoda_koso3
dummy_Shimoda_koso1$RegionClass<-"CP Tengusa"

dummy_Tsukasaami1<- expand.grid(Cover=seq(min(Tsukasaami$Cover),
                                          max(Tsukasaami$Cover),length=1000))
pred_Tsukasaami3<- predict(model3_Tsuka2,newdata=dummy_Tsukasaami1,se.fit=T)
dummy_Tsukasaami1$DW<-pred_Tsukasaami3
dummy_Tsukasaami1$RegionClass<-"SP Tsukasaami"

dummy_kogata<-rbind(dummy_Hokkaido1,dummy_NorthJapanSea1,dummy_Kominato1,
                    dummy_Shimoda_Amizi1,dummy_Shimoda_koso1,dummy_Tsukasaami1)

dummy_kogata$RegionClass<-factor(dummy_kogata$RegionClass, 
                                 levels = c("HD Anaaosa","NJS Kogatakasso","CP Umiuchiwa",
                                            "CP Amijigusa","CP Tengusa","SP Tsukasaami") )
library(ggplot2)

plot_Kogata_all<-ggplot(data=dummy_kogata, aes(x=Cover, y=DW,linetype = RegionClass,color= RegionClass)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=6),limits=c(0,1000))+ 
  geom_line(linewidth=1)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.55), 
        legend.title=element_text(size=16),legend.text =  element_text(size = 16))
plot_Kogata_all


