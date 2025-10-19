################################################################################
### Estimated biomass of Sugamo (Phyllospadix iwatensis) at 100% coverage ######
################################################################################

library(ggplot2)

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

dummy_Sugamo<-rbind(dummy_Hokkaido1,dummy_CentralPacific1)

dummy_Sugamo$RegionClass<-factor(dummy_Sugamo$RegionClass, 
                                      levels = c("HD Sugamo","CP Sugamo") )


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
