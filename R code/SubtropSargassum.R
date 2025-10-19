rm(list=ls())
library(nlme)
library(ggplot2)

SubtropHond<-read.csv("Surgassum_subtropical.csv",stringsAsFactors = TRUE)
SubtropHond$Season <- factor(SubtropHond$Season, levels = c("Flourish","Decline") )
names(SubtropHond)



####### SouthPacific  ##########
SubtropHond_SouthPacific<-subset(SubtropHond,SubtropHond$Region=="SouthPacific")


####線形モデル#####
model1 <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = SubtropHond_SouthPacific)
summary(model1)
AIC(model1)
BIC(model1)

####累乗モデル#####
model2 <- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = SubtropHond_SouthPacific,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2)
AIC(model2)
BIC(model2)

####指数モデル#####
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

####二次関数モデル#####
model4 <- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
              start = list(a=-0.2, b=20,c=-50), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data =  SubtropHond_SouthPacific)
summary(model4)
AIC(model4)
BIC(model4)

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

#Model4
pred_SouthPacific4<- predict(model4,newdata=dummy_SouthPacific,se.fit=T)
dummy_SouthPacific$DW4<-pred_SouthPacific4

plot_SouthPacific4<-ggplot(data=SubtropHond_SouthPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 4000,length=5),limits=c(0,4000))+ 
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_SouthPacific, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_SouthPacific4

#width 600 * Height 500 save

####### EastChinaSea  ##########
SubtropHond_EastChinaSea<-subset(SubtropHond,SubtropHond$Region=="EastChinaSea")


####線形モデル#####
model1_EC <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = SubtropHond_EastChinaSea)
summary(model1_EC)
AIC(model1_EC)
BIC(model1_EC)

####累乗モデル#####
model2_EC<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = SubtropHond_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_EC)
AIC(model2_EC)
BIC(model2_EC)

####指数モデル#####
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

####二次関数モデル#####
model4_EC<- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
              start = list(a=-0.2, b=20,c=-50), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data =  SubtropHond_EastChinaSea)
summary(model4_EC)
AIC(model4_EC)
BIC(model4_EC)

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

#Model4
pred_EastChinaSea4<- predict(model4_EC,newdata=dummy_EastChinaSea,se.fit=T)
dummy_EastChinaSea$DW4<-pred_EastChinaSea4

plot_EastChinaSea4<-ggplot(data=SubtropHond_EastChinaSea, aes(x=Cover, y=DW_g_m2,color = Season)) + 
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

####### Ryukyu  ##########
SubtropHond_Ryukyu<-subset(SubtropHond,SubtropHond$Region=="Ryukyu")


####線形モデル#####
model1_RK <- nls(DW_g_m2~ a*Cover,
                 start = list(a=50),data = SubtropHond_Ryukyu)
summary(model1_RK)
AIC(model1_RK)
BIC(model1_RK)

####累乗モデル#####
model2_RK<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=1.444,b = 1.3181),data = SubtropHond_Ryukyu,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)
summary(model2_RK)
AIC(model2_RK)
BIC(model2_RK)

####指数モデル#####
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

####二次関数モデル#####
model4_RK<- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
                start = list(a=-0.2, b=20,c=-50), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data =  SubtropHond_Ryukyu)
summary(model4_RK)
AIC(model4_RK)
BIC(model4_RK)

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

#Model4
pred_Ryukyu4<- predict(model4_RK,newdata=dummy_Ryukyu,se.fit=T)
dummy_Ryukyu$DW4<-pred_Ryukyu4

plot_Ryukyu4<-ggplot(data=SubtropHond_Ryukyu, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  ylim(0,500)+
  geom_point(size=3.5)+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Ryukyu, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Ryukyu4

#width 600 * Height 500 save
