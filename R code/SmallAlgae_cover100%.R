rm(list=ls())
library(ggplot2)

Smallalgae<-read.csv("Smallalgae.csv",stringsAsFactors = TRUE)
Smallalgae$Season <- factor(Smallalgae$Season, levels = c("Flourish","Decline") )
names(Smallalgae)

####### Hokkaido @ Aosa##########
Smallalgae_Hokkaido<-subset(Smallalgae,Smallalgae$Region=="Hokkaido")
Smallalgae_Hokkaido$DW_g_m2

###線形モデル#####
model1_HD<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Smallalgae_Hokkaido)
summary(model1_HD)
AIC(model1_HD)
BIC(model1_HD)

####累乗モデル#####
model2_HD<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Smallalgae_Hokkaido)
summary(model2_HD)
AIC(model2_HD)
BIC(model2_HD)

####指数モデル#####
model3_HD<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=1,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Smallalgae_Hokkaido,
                trace=TRUE)
summary(model3_HD)
AIC(model3_HD)
BIC(model3_HD)




####### Hokkaido##########
library(ggplot2)
library(propagate)

dummy_100<- expand.grid(Cover=100)
pred1_HD100_Aosa<- predictNLS(model1_HD,newdata=dummy_100,interval = c("confidence"))
pred1_HD100_Aosa
pred1_HD100_Aosa_vector<-pred1_HD100_Aosa[[1]]
pred1_HD100_Aosa_vector<-data.frame(ID="pred1_HD100_Aosa",Region="Hokkaido",
                                         Region="HD",
                                         Type="Aosa",
                                         Equation="Linear",
                                         pred1_HD100_Aosa_vector)
pred1_HD100_Aosa_vector


#Model2
pred2_HD100_Aosa<- predictNLS(model2_HD,newdata=dummy_100,interval = c("confidence"))
pred2_HD100_Aosa
pred2_HD100_Aosa_vector<-pred2_HD100_Aosa[[1]]
pred2_HD100_Aosa_vector<-data.frame(ID="pred2_HD100_Aosa",Region="Hokkaido",
                                         Region="HD",
                                         Type="Aosa",
                                         Equation="Power",
                                         pred2_HD100_Aosa_vector)
pred2_HD100_Aosa_vector

# save width 600 * Height 500 

#Model 3
pred3_HD100_Aosa<- predictNLS(model3_HD,newdata=dummy_100,interval = c("confidence"))
pred3_HD100_Aosa
pred3_HD100_Aosa_vector<-pred3_HD100_Aosa[[1]]
pred3_HD100_Aosa_vector<-data.frame(ID="pred3_HD100_Aosa",Region="Hokkaido",
                                         Region="HD",
                                         Type="Aosa",
                                         Equation="Exponent",
                                         pred3_HD100_Aosa_vector)
pred3_HD100_Aosa_vector

Aosa_HD100<-rbind(pred1_HD100_Aosa_vector,
                       pred2_HD100_Aosa_vector,
                       pred3_HD100_Aosa_vector)
Aosa_HD100


#width 600 * Height 500 save


####### NorthJapanSea @ Sado ##########
Smallalgae_NorthJapanSea<-subset(Smallalgae,Smallalgae$Region=="NorthJapanSea",na.rm = TRUE)
Smallalgae_NorthJapanSea<-subset(Smallalgae_NorthJapanSea,Smallalgae_NorthJapanSea$Jap_name=="Keurujigusa; Habamodoki",na.rm = TRUE)


####線形モデル#####
model1_NJ<- nls(DW_g_m2~ a*Cover,
                start = list(a=50),data = Smallalgae_NorthJapanSea)
summary(model1_NJ)
AIC(model1_NJ)
BIC(model1_NJ)

####累乗モデル#####
model2_NJ<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=1.444,b = 1.3181),data = Smallalgae_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)
summary(model2_NJ)
AIC(model2_NJ)
BIC(model2_NJ)

####指数モデル#####
model3_NJ<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = Smallalgae_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_NJ)
AIC(model3_NJ)
BIC(model3_NJ)


#######Plot line  NorthJapanSea @ Sado##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_NJS100_Kogataso<- predictNLS(model1_NJ,newdata=dummy_100,interval = c("confidence"))
pred1_NJS100_Kogataso
rm(pred1_NJS100_Kogataso_vector)
pred1_NJS100_Kogataso_vector<-pred1_NJS100_Kogataso[[1]]
pred1_NJS100_Kogataso_vector<-data.frame(ID="pred1_NJS100_Kogataso",Region="North Japan Sea",
                                          Region="NJS",
                                          Type="Kogataso",
                                          Equation="Linear",
                                          pred1_NJS100_Kogataso_vector)
pred1_NJS100_Kogataso_vector


#Model2
pred2_NJS100_Kogataso<- predictNLS(model2_NJ,newdata=dummy_100,interval = c("confidence"))
rm(pred2_NJS100_Kogataso_vector)
pred2_NJS100_Kogataso_vector<-pred2_NJS100_Kogataso[[1]]
pred2_NJS100_Kogataso_vector<-data.frame(ID="pred2_NJS100_Kogataso",Region="North Japan Sea",
                                          Region="NJS",
                                          Type="Kogataso",
                                          Equation="Power",
                                          pred2_NJS100_Kogataso_vector)
pred2_NJS100_Kogataso_vector

# save width 600 * Height 500 

#Model 3
pred3_NJS100_Kogataso<- predictNLS(model3_NJ,newdata=dummy_100,interval = c("confidence"))
rm(pred3_NJS100_Kogataso_vector)
pred3_NJS100_Kogataso_vector<-pred3_NJS100_Kogataso[[1]]
pred3_NJS100_Kogataso_vector<-data.frame(ID="pred3_NJS100_Kogataso",Region="North Japan Sea",
                                          Region="NJS",
                                          Type="Kogataso",
                                          Equation="Exponent",
                                          pred3_NJS100_Kogataso_vector)
pred3_NJS100_Kogataso_vector

Kogataso_NJS100<-rbind(pred1_NJS100_Kogataso_vector,
                        pred2_NJS100_Kogataso_vector,
                        pred3_NJS100_Kogataso_vector)
Kogataso_NJS100

####### Central Pacific Kominato @ Smallalgae##########
Smallalgae_Kominato<-subset(Smallalgae,Smallalgae$Point=="Kominato")

####線形モデル#####
model1_cp<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Smallalgae_Kominato)
summary(model1_cp)
AIC(model1_cp)
BIC(model1_cp)

####累乗モデル#####
model2_cp<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=10,b = 1),data = Smallalgae_Kominato)
summary(model2_cp)
AIC(model2_cp)
BIC(model2_cp)

####指数モデル#####
model3_cp<- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=50,b = 0.01), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data = Smallalgae_Kominato,
              trace=TRUE)
summary(model3_cp)
AIC(model3_cp)
BIC(model3_cp)




####### Central Pacific @ Kominato##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_CP100_Umiuchiwa<- predictNLS(model1_cp,newdata=dummy_100,interval = c("confidence"))
pred1_CP100_Umiuchiwa
pred1_CP100_Umiuchiwa_vector<-pred1_CP100_Umiuchiwa[[1]]
pred1_CP100_Umiuchiwa_vector<-data.frame(ID="pred1_CP100_Umiuchiwa",Region="Central Pacific",
                                         Region="CP",
                                         Type="Umiuchiwa",
                                         Equation="Linear",
                                         pred1_CP100_Umiuchiwa_vector)
pred1_CP100_Umiuchiwa_vector


#Model2
pred2_CP100_Umiuchiwa<- predictNLS(model2_cp,newdata=dummy_100,interval = c("confidence"))
pred2_CP100_Umiuchiwa
rm(pred2_CP100_Umiuchiwa_vector)
pred2_CP100_Umiuchiwa_vector<-pred2_CP100_Umiuchiwa[[1]]
pred2_CP100_Umiuchiwa_vector<-data.frame(ID="pred2_CP100_Umiuchiwa",Region="Central Pacific",
                                         Region="CP",
                                         Type="Umiuchiwa",
                                         Equation="Power",
                                         pred2_CP100_Umiuchiwa_vector)
pred2_CP100_Umiuchiwa_vector

# save width 600 * Height 500 

#Model 3
pred3_CP100_Umiuchiwa<- predictNLS(model3_cp,newdata=dummy_100,interval = c("confidence"))
pred3_CP100_Umiuchiwa
rm(pred3_CP100_Umiuchiwa_vector)
pred3_CP100_Umiuchiwa_vector<-pred3_CP100_Umiuchiwa[[1]]
pred3_CP100_Umiuchiwa_vector<-data.frame(ID="pred3_CP100_Umiuchiwa",Region="Central Pacific",
                                         Region="CP",
                                         Type="Umiuchiwa",
                                         Equation="Exponent",
                                         pred3_CP100_Umiuchiwa_vector)
pred3_CP100_Umiuchiwa_vector

Umiuchiwa_CP100<-rbind(pred1_CP100_Umiuchiwa_vector,
                       pred2_CP100_Umiuchiwa_vector,
                       pred3_CP100_Umiuchiwa_vector)
Umiuchiwa_CP100


#width 600 * Height 500 save


###### Central Pacific Shimoda @ Amijigusa##########
Smallalgae_Shimoda<-subset(Smallalgae,Smallalgae$Point=="Shimoda")
Smallalgae_Shimoda_Amizi<-subset(Smallalgae,Smallalgae$Jap_name=="Amizigusa")



####線形モデル#####
model1_SA<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Smallalgae_Shimoda_Amizi)
summary(model1_SA)
AIC(model1_SA)
BIC(model1_SA)

####累乗モデル#####
model2_SA<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=10,b = 1),data = Smallalgae_Shimoda_Amizi)
summary(model2_SA)
AIC(model2_SA)
BIC(model2_SA)

####指数モデル#####
model3_SA<- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=50,b = 0.01), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data = Smallalgae_Shimoda_Amizi,
              trace=TRUE)
summary(model3_SA)
AIC(model3_SA)
BIC(model3_SA)


####### Central Pacific @ Shimoda_Amizi##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_SA100_Amizigusa<- predictNLS(model1_SA,newdata=dummy_100,interval = c("confidence"))
pred1_SA100_Amizigusa
rm(pred1_SA100_Amizigusa_vector)
pred1_SA100_Amizigusa_vector<-pred1_SA100_Amizigusa[[1]]
pred1_SA100_Amizigusa_vector<-data.frame(ID="pred1_SA100_Amizigusa",Region="Central Pacific",
                                         Region="CP",
                                         Type="Amizigusa",
                                         Equation="Linear",
                                         pred1_SA100_Amizigusa_vector)
pred1_SA100_Amizigusa_vector


#Model2
pred2_SA100_Amizigusa<- predictNLS(model2_SA,newdata=dummy_100,interval = c("confidence"))
pred2_SA100_Amizigusa
rm(pred2_SA100_Amizigusa_vector)
pred2_SA100_Amizigusa_vector<-pred2_SA100_Amizigusa[[1]]
pred2_SA100_Amizigusa_vector<-data.frame(ID="pred2_SA100_Amizigusa",Region="Central Pacific",
                                         Region="CP",
                                         Type="Amizigusa",
                                         Equation="Power",
                                         pred2_SA100_Amizigusa_vector)
pred2_SA100_Amizigusa_vector

# save width 600 * Height 500 

#Model 3
pred3_SA100_Amizigusa<- predictNLS(model3_SA,newdata=dummy_100,interval = c("confidence"))
pred3_SA100_Amizigusa
rm(pred3_SA100_Amizigusa_vector)
pred3_SA100_Amizigusa_vector<-pred3_SA100_Amizigusa[[1]]
pred3_SA100_Amizigusa_vector<-data.frame(ID="pred3_SA100_Amizigusa",Region="Central Pacific",
                                         Region="CP",
                                         Type="Amizigusa",
                                         Equation="Exponent",
                                         pred3_SA100_Amizigusa_vector)
pred3_SA100_Amizigusa_vector

Amizigusa_SA100<-rbind(pred1_SA100_Amizigusa_vector,
                       pred2_SA100_Amizigusa_vector,
                       pred3_SA100_Amizigusa_vector)
Amizigusa_SA100


#width 600 * Height 500 save

###### Central Pacific Shimoda @ Koso##########
Smallalgae_Shimoda<-subset(Smallalgae,Smallalgae$Site=="Shimoda")
Smallalgae_Shimoda_koso<-subset(Smallalgae_Shimoda,Smallalgae_Shimoda$Class=="Rhodophyta")



####線形モデル#####
model1_SK<- nls(DW_g_m2~ a*Cover,
                start = list(a=50),data = Smallalgae_Shimoda_koso)
summary(model1_SK)
AIC(model1_SK)
BIC(model1_SK)

####累乗モデル#####
model2_SK<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),data = Smallalgae_Shimoda_koso)
summary(model2_SK)
AIC(model2_SK)
BIC(model2_SK)

####指数モデル#####
model3_SK<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=50,b = 0.01), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Smallalgae_Shimoda_koso,
                trace=TRUE)
summary(model3_SK)
AIC(model3_SK)
BIC(model3_SK)




####### Central Pacific @ Shimoda_koso##########
library(propagate)
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_SK100_Koso<- predictNLS(model1_SK,newdata=dummy_100,interval = c("confidence"))
pred1_SK100_Koso
rm(pred1_SK100_Koso_vector)
pred1_SK100_Koso_vector<-pred1_SK100_Koso[[1]]
pred1_SK100_Koso_vector<-data.frame(ID="pred1_SK100_Koso",Region="Central Pacific",
                                         Region="CP",
                                         Type="Koso",
                                         Equation="Linear",
                                         pred1_SK100_Koso_vector)
pred1_SK100_Koso_vector

#Model2
pred2_SK100_Koso<- predictNLS(model2_SK,newdata=dummy_100,interval = c("confidence"))
pred2_SK100_Koso
rm(pred2_SK100_Koso_vector)
pred2_SK100_Koso_vector<-pred2_SK100_Koso[[1]]
pred2_SK100_Koso_vector<-data.frame(ID="pred2_SK100_Koso",Region="Central Pacific",
                                         Region="CP",
                                         Type="Koso",
                                         Equation="Power",
                                         pred2_SK100_Koso_vector)
pred2_SK100_Koso_vector

# save width 600 * Height 500 

#Model 3
pred3_SK100_Koso<- predictNLS(model3_SK,newdata=dummy_100,interval = c("confidence"))
pred3_SK100_Koso
rm(pred3_SK100_Koso_vector)
pred3_SK100_Koso_vector<-pred3_SK100_Koso[[1]]
pred3_SK100_Koso_vector<-data.frame(ID="pred3_SK100_Koso",Region="Central Pacific",
                                         Region="CP",
                                         Type="Koso",
                                         Equation="Exponent",
                                         pred3_SK100_Koso_vector)
pred3_SK100_Koso_vector

Koso_SK100<-rbind(pred1_SK100_Koso_vector,
                       pred2_SK100_Koso_vector,
                       pred3_SK100_Koso_vector)
Koso_SK100


####### Tsukasaami @ SoutherPacific  ##########
Tsukasaami<-subset(Smallalgae,Smallalgae$Jap_name=="Tsukasaami")


####線形モデル#####
model1_Tsuka<- nls(DW_g_m2~ a*Cover,
                  start = list(a=50),data = Tsukasaami)
summary(model1_Tsuka)
AIC(model1_Tsuka)
BIC(model1_Tsuka)

####累乗モデル#####
model2_Tsuka<- nls(DW_g_m2~ a*Cover^b,
                  start = list(a=1.444,b = 1.3181),data = Tsukasaami,
                  control = list(maxiter = 50000, warnOnly = TRUE),
                  trace=TRUE)
summary(model2_Tsuka)
AIC(model2_Tsuka)
BIC(model2_Tsuka)

####指数モデル#####
model3_Tsuka<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                  start = list(a=17.5,b =0.048), data = Tsukasaami,
                  control = list(maxiter = 50000, warnOnly = TRUE),
                  trace=TRUE)

summary(model3_Tsuka)
AIC(model3_Tsuka)
BIC(model3_Tsuka)


######Plot line  Tsukasaami @ Tokushima##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_SP100_Tsuka<- predictNLS(model1_Tsuka,newdata=dummy_100,interval = c("confidence"))
pred1_SP100_Tsuka
rm(pred1_SP100_Tsuka_vector)
pred1_SP100_Tsuka_vector<-pred1_SP100_Tsuka[[1]]
pred1_SP100_Tsuka_vector<-data.frame(ID="pred1_SP100_Tsuka",Region="Southern Pacific",
                                    Region="SP",
                                    Type="Tsukasaami",
                                    Equation="Linear",
                                    pred1_SP100_Tsuka_vector)
pred1_SP100_Tsuka_vector

#Model2
pred2_SP100_Tsuka<- predictNLS(model2_Tsuka,newdata=dummy_100,interval = c("confidence"))
pred2_SP100_Tsuka
rm(pred2_SP100_Tsuka_vector)
pred2_SP100_Tsuka_vector<-pred2_SP100_Tsuka[[1]]
pred2_SP100_Tsuka_vector<-data.frame(ID="pred2_SP100_Tsuka",Region="Southern Pacific",
                                    Region="SP",
                                    Type="Tsukasaami",
                                    Equation="Power",
                                    pred2_SP100_Tsuka_vector)
pred2_SP100_Tsuka_vector

# save width 600 * Height 500 

#Model 3
pred3_SP100_Tsuka<- predictNLS(model3_Tsuka,newdata=dummy_100,interval = c("confidence"))
pred3_SP100_Tsuka
rm(pred3_SP100_Tsuka_vector)
pred3_SP100_Tsuka_vector<-pred3_SP100_Tsuka[[1]]
pred3_SP100_Tsuka_vector<-data.frame(ID="pred3_SP100_Tsuka",Region="Southern Pacific",
                                    Region="SP",
                                    Type="Tsukasaami",
                                    Equation="Exponent",
                                    pred3_SP100_Tsuka_vector)
pred3_SP100_Tsuka_vector

Tsukasaami_SP100<-rbind(pred1_SP100_Tsuka_vector,
                  pred2_SP100_Tsuka_vector,
                  pred3_SP100_Tsuka_vector)
Tsukasaami_SP100

#width 600 * Height 500 save

######Plot line  all species  ##########
Smallalgae100<-rbind(Aosa_HD100,Kogataso_NJS100,Umiuchiwa_CP100,
                     Amizigusa_SA100,Koso_SK100,Tsukasaami_SP100)
Smallalgae100
Smallalgae100$Equation <- factor(Smallalgae100$Equation, 
                                  levels = c("Linear","Power","Exponent") )
library(dplyr)
Smallalgae100 <- Smallalgae100 %>%
  mutate(Smallalgae100,RegionType = interaction(Region.1, Type))
Smallalgae100$RegionType
Smallalgae100$RegionType <- factor(Smallalgae100$RegionType, 
                                    levels = c("HD.Aosa","NJS.Kogataso","CP.Umiuchiwa",
                                               "CP.Amizigusa","CP.Koso","SP.Tsukasaami") )
Smallalgae100


library(ggplot2)
plot_all_Smallalgae100<-ggplot(data=Smallalgae100, 
                               aes(x=RegionType, y=Prop.Mean.1,color= Equation,shape = Equation)) + 
  xlab("Region Type")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_y_continuous(breaks=seq(0, 1000,length=6),limits=c(0,1000))+ 
  geom_point(stat = "identity", position = position_dodge(width = 0.9),size=5) +
  geom_errorbar(aes(ymin = Prop.Mean.1 - Prop.sd.1, ymax = Prop.Mean.1 + Prop.sd.1),
                width = 0.5, position = position_dodge(width = 0.9))+
  theme(axis.title =element_text(size=20),axis.text = element_text(color="black",size=20))+
  theme(legend.title=element_text(size=20),legend.text =  element_text(size = 20))
plot_all_Smallalgae100
#save as 1400 * 400
#save as 1600 * 400
