
dummy_Hokkaido1<- expand.grid(Cover=seq(min(Hondawara_Hokkaido$Cover),
                                       max(Hondawara_Hokkaido$Cover),length=1000))
pred_Hokkaido2<- predict(model2_HD,newdata=dummy_Hokkaido1,se.fit=T)
dummy_Hokkaido1$DW<-pred_Hokkaido2
dummy_Hokkaido1$RegionClass<-"HD Uganomoku"

dummy_NorthJapanSea1<- expand.grid(Cover=seq(min(Hondawara_NorthJapanSea$Cover,na.rm=T),
                                            max(Hondawara_NorthJapanSea$Cover,na.rm=T),length=1000))
pred_NorthJapanSea3<- predict(model3_NJ2,newdata=dummy_NorthJapanSea,se.fit=T)
dummy_NorthJapanSea1$DW<-pred_NorthJapanSea3
dummy_NorthJapanSea1$RegionClass<-"NJS Hodawara mix"

dummy_CentralPacific1<- expand.grid(Cover=seq(min(Hondawara_CentralPacific$Cover),
                                             max(Hondawara_CentralPacific$Cover),length=1000))
pred_CentralPacific<- predict(model1_cp,newdata=dummy_CentralPacific,se.fit=T)
dummy_CentralPacific1$DW<-pred_CentralPacific
dummy_CentralPacific1$RegionClass<-"CP Ohbamoku"

dummy_SetoIslandSea1<- expand.grid(Cover=seq(min(Hondawara_SetoIslandSea$Cover),
                                            max(Hondawara_SetoIslandSea$Cover),length=1000))
pred_SetoIslandSea1<- predict(model1_ST,newdata=dummy_SetoIslandSea1,se.fit=T)
dummy_SetoIslandSea1$DW<-pred_SetoIslandSea1
dummy_SetoIslandSea1$RegionClass<-"SIS Hodawara mix"

dummy_SouthJapanSea1<- expand.grid(Cover=seq(min(Hondawara_SouthJapanSea$Cover),
                                            max(Hondawara_SouthJapanSea$Cover),length=1000))
pred_SouthJapanSea1<- predict(model1,newdata=dummy_SouthJapanSea1,se.fit=T)
dummy_SouthJapanSea1$DW<-pred_SouthJapanSea1
dummy_SouthJapanSea1$RegionClass<-"SJS Hodawara mix"

dummy_EastChinaSea1<- expand.grid(Cover=seq(min(Hondawara_EastChinaSea$Cover),
                                           max(Hondawara_EastChinaSea$Cover),length=1000))
pred_EastChinaSea2<- predict(model2_EC,newdata=dummy_EastChinaSea1,se.fit=T)
dummy_EastChinaSea1$DW<-pred_EastChinaSea2
dummy_EastChinaSea1$RegionClass<-"ECS Hodawara mix"

dummy_Hodawara<-rbind(dummy_Hokkaido1,dummy_NorthJapanSea1,dummy_CentralPacific1,
                      dummy_SetoIslandSea1,dummy_SouthJapanSea1,dummy_EastChinaSea1)
dummy_Hodawara$RegionClass<-factor(dummy_Hodawara$RegionClass, 
                                 levels = c("HD Uganomoku","NJS Hodawara mix","CP Ohbamoku","SIS Hodawara mix","SJS Hodawara mix","ECS Hodawara mix") )
library(ggplot2)

plot_Hodawara_all<-ggplot(data=dummy_Hodawara, aes(x=Cover, y=DW,linetype = RegionClass,color= RegionClass)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 2500,length=6),limits=c(0,2500))+ 
  geom_line(linewidth=1.25)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.55), 
        legend.title=element_text(size=16),legend.text =  element_text(size = 16))
plot_Hodawara_all
