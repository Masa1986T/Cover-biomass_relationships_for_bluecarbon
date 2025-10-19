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
