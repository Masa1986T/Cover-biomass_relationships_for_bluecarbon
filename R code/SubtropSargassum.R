#################################################
### R codes for subtropical Sargassecea #########
#################################################

rm(list=ls())

library(ggplot2)

SubtropHond<-read.csv("Surgassum_subtropical.csv",stringsAsFactors = TRUE)
SubtropHond$Season <- factor(SubtropHond$Season, levels = c("Flourish","Decline") )
names(SubtropHond)



####### SouthPacific  ##########
SubtropHond_SouthPacific<-subset(SubtropHond,SubtropHond$Region=="SouthPacific")


####Linear model#####
model1 <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = SubtropHond_SouthPacific)
summary(model1)
AIC(model1)
BIC(model1)

####Power model#####
model2 <- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = SubtropHond_SouthPacific,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2)
AIC(model2)
BIC(model2)

####Exponential model#####
model3 <- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=17.5,b =0.048), data = SubtropHond_SouthPacific,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3)
AIC(model3)
BIC(model3)

model3_2 <- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=17.5,b =0.048), data = SubtropHond_SouthPacific,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_2)
AIC(model3_2)
BIC(model3_2)


######Plot line  SouthPacific ##########
#Model1
dummy_SouthPacific<- expand.grid(Cover=seq(min(SubtropHond_SouthPacific$Cover),
                                            max(SubtropHond_SouthPacific$Cover),length=1000))
pred_SouthPacific1<- predict(model1,newdata=dummy_SouthPacific,se.fit=T)
dummy_SouthPacific$DW1<-pred_SouthPacific1

plot_SouthPacific1<-ggplot(data=SubtropHond_SouthPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthPacific, aes(x=Cover, y=DW1),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthPacific1


#Model2
dummy_SouthPacific<- expand.grid(Cover=seq(min(SubtropHond_SouthPacific$Cover),
                                            max(SubtropHond_SouthPacific$Cover),length=1000))
pred_SouthPacific2<- predict(model2,newdata=dummy_SouthPacific,se.fit=T)
dummy_SouthPacific$DW2<-pred_SouthPacific2

plot_SouthPacific2<-ggplot(data=SubtropHond_SouthPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthPacific, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthPacific2

#Model3
dummy_SouthPacific<- expand.grid(Cover=seq(min(SubtropHond_SouthPacific$Cover),
                                           max(SubtropHond_SouthPacific$Cover),length=1000))
pred_SouthPacific3<- predict(model3,newdata=dummy_SouthPacific,se.fit=T)
dummy_SouthPacific$DW3<-pred_SouthPacific3

plot_SouthPacific3<-ggplot(data=SubtropHond_SouthPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthPacific, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthPacific3



#width 600 * Height 500 save

####### EastChinaSea  ##########
SubtropHond_EastChinaSea<-subset(SubtropHond,SubtropHond$Region=="EastChinaSea")


####Linear model#####
model1_EC <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = SubtropHond_EastChinaSea)
summary(model1_EC)
AIC(model1_EC)
BIC(model1_EC)

####Power model#####
model2_EC<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = SubtropHond_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_EC)
AIC(model2_EC)
BIC(model2_EC)

####Exponential model#####
model3_EC<- nls(DW_g_m2~ a*exp(b*Cover),
              start = list(a=17.5,b =0.048), data = SubtropHond_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_EC)
AIC(model3_EC)
BIC(model3_EC)

model3_EC2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = SubtropHond_EastChinaSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_EC2)
AIC(model3_EC2)
BIC(model3_EC2)



######Plot line  EastChinaSea ##########
#Model1
dummy_EastChinaSea<- expand.grid(Cover=seq(min(SubtropHond_EastChinaSea$Cover),
                                            max(SubtropHond_EastChinaSea$Cover),length=1000))
pred_EastChinaSea1<- predict(model1_EC,newdata=dummy_EastChinaSea,se.fit=T)
dummy_EastChinaSea$DW1<-pred_EastChinaSea1

plot_EastChinaSea1<-ggplot(data=SubtropHond_EastChinaSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
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

pred_EastChinaSea2<- predict(model2_EC,newdata=dummy_EastChinaSea,se.fit=T)
dummy_EastChinaSea$DW2<-pred_EastChinaSea2

plot_EastChinaSea2<-ggplot(data=SubtropHond_EastChinaSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
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
dummy_EastChinaSea<- expand.grid(Cover=seq(min(SubtropHond_EastChinaSea$Cover),
                                           max(SubtropHond_EastChinaSea$Cover),length=1000))
pred_EastChinaSea3<- predict(model3_EC2,newdata=dummy_EastChinaSea,se.fit=T)
dummy_EastChinaSea$DW3<-pred_EastChinaSea3

plot_EastChinaSea3<-ggplot(data=SubtropHond_EastChinaSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+ 
  geom_point(size=8)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_EastChinaSea, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_EastChinaSea3



#width 600 * Height 500 save

####### Ryukyu  ##########
SubtropHond_Ryukyu<-subset(SubtropHond,SubtropHond$Region=="Ryukyu")


####Linear model#####
model1_RK <- nls(DW_g_m2~ a*Cover,
                 start = list(a=50),data = SubtropHond_Ryukyu)
summary(model1_RK)
AIC(model1_RK)
BIC(model1_RK)

####Power model#####
model2_RK<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=1.444,b = 1.3181),data = SubtropHond_Ryukyu,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)
summary(model2_RK)
AIC(model2_RK)
BIC(model2_RK)

####Exponential model#####
model3_RK<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=17.5,b =0.048), data = SubtropHond_Ryukyu,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)
summary(model3_RK)
AIC(model3_RK)
BIC(model3_RK)

model3_RK2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = SubtropHond_Ryukyu,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)
summary(model3_RK2)
AIC(model3_RK2)
BIC(model3_RK2)



######Plot line  Ryukyu ##########
#Model1
dummy_Ryukyu<- expand.grid(Cover=seq(min(SubtropHond_Ryukyu$Cover),
                                           max(SubtropHond_Ryukyu$Cover),length=1000))
pred_Ryukyu1<- predict(model1_RK,newdata=dummy_Ryukyu,se.fit=T)
dummy_Ryukyu$DW1<-pred_Ryukyu1

plot_Ryukyu1<-ggplot(data=SubtropHond_Ryukyu, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=8)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("black","grey"))+
  labs(color='Season')+
  geom_line(data=dummy_Ryukyu, aes(x=Cover, y=DW1),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Ryukyu1


#Model2

pred_Ryukyu2<- predict(model2_RK,newdata=dummy_Ryukyu,se.fit=T)
dummy_Ryukyu$DW2<-pred_Ryukyu2

plot_Ryukyu2<-ggplot(data=SubtropHond_Ryukyu, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  xlab("Coverage (%)")+ 
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+ 
  geom_line(data=dummy_Ryukyu, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Ryukyu2

#Model3
pred_Ryukyu3<- predict(model3_RK,newdata=dummy_Ryukyu,se.fit=T)
dummy_Ryukyu$DW3<-pred_Ryukyu3

plot_Ryukyu3<-ggplot(data=SubtropHond_Ryukyu, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  ylim(0,500)+
  geom_point(size=3.5)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Ryukyu, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Ryukyu3



#width 600 * Height 500 save

######################################################################################
####Comparison of the relationships for subtropical Sargassum among regions #####
#######################################################################################

dummy_SouthPacific1<- expand.grid(Cover=seq(min(SubtropHond_SouthPacific$Cover),
                                            max(SubtropHond_SouthPacific$Cover),length=1000))
pred_SouthPacific3<- predict(model3_2,newdata=dummy_SouthPacific1,se.fit=T)
dummy_SouthPacific1$DW<-pred_SouthPacific3
dummy_SouthPacific1$RegionClass<-"SP Hiragi/Kireba"


dummy_EastChinaSea1<- expand.grid(Cover=seq(min(SubtropHond_EastChinaSea$Cover),
                                            max(SubtropHond_EastChinaSea$Cover),length=1000))
pred_EastChinaSea3<- predict(model3_EC2,newdata=dummy_EastChinaSea1,se.fit=T)
dummy_EastChinaSea1$DW<-pred_EastChinaSea3
dummy_EastChinaSea1$RegionClass<-"ECS Hodawara mix"

dummy_Ryukyu1<- expand.grid(Cover=seq(min(SubtropHond_Ryukyu$Cover),
                                      max(SubtropHond_Ryukyu$Cover),length=1000))
pred_Ryukyu1<- predict(model1_RK,newdata=dummy_Ryukyu1,se.fit=T)
dummy_Ryukyu1$DW<-pred_Ryukyu1
dummy_Ryukyu1$RegionClass<-"RK Hodawara mix"

dummy_STHondawara<-rbind(dummy_SouthPacific1,dummy_EastChinaSea1,dummy_Ryukyu1)

dummy_STHondawara$RegionClass<-factor(dummy_STHondawara$RegionClass, 
                                      levels = c("SP Hiragi/Kireba","ECS Hodawara mix","RK Hodawara mix") )
library(ggplot2)

plot_STHondawara_all<-ggplot(data=dummy_STHondawara, aes(x=Cover, y=DW,linetype = RegionClass,color= RegionClass)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+ 
  geom_line(linewidth=1.25)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.55), 
        legend.title=element_text(size=20),legend.text =  element_text(size = 20))
plot_STHondawara_all


