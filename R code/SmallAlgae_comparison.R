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

