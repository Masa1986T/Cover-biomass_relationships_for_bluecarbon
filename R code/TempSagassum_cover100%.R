rm(list=ls())
library(nlme)
library(dplyr)
library(ggplot2)
library(propagate)

Hondawara<-read.csv("Sargassum_temperate.csv",stringsAsFactors = TRUE)
Hondawara$Season <- factor(Hondawara$Season, levels = c("Flourish","Decline") )
names(Hondawara)

####### Hokkaido @ Uganomoku##########
Hondawara_Hokkaido<-subset(Hondawara,Hondawara$Region=="Hokkaido")
Hondawara_Hokkaido$DW_g_m2

###線形モデル#####
model1_HD<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Hondawara_Hokkaido)
summary(model1_HD)
AIC(model1_HD)
BIC(model1_HD)

####累乗モデル#####
model2_HD<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Hondawara_Hokkaido)
summary(model2_HD)
AIC(model2_HD)
BIC(model2_HD)

####指数モデル#####
model3_HD<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Hondawara_Hokkaido,
                trace=TRUE)
summary(model3_HD)
AIC(model3_HD)
BIC(model3_HD)



####### Hokkaido##########
library(ggplot2)

#Model1
dummy_100<- expand.grid(Cover=100)
pred1_HD100_Uganomoku<- predictNLS(model1_HD,newdata=dummy_100,interval = c("confidence"))
pred1_HD100_Uganomoku
pred1_HD100_Uganomoku_vector<-pred1_HD100_Uganomoku[[1]]
pred1_HD100_Uganomoku_vector<-data.frame(ID="pred1_HD100_Uganomoku",Region="Hokkaido",
                                      Region="HD",
                                      Type="Uganomoku",
                                      Equation="Linear",
                                      pred1_HD100_Uganomoku_vector)
pred1_HD100_Uganomoku_vector


#Model2
pred2_HD100_Uganomoku<- predictNLS(model2_HD,newdata=dummy_100,interval = c("confidence"))
pred2_HD100_Uganomoku
pred2_HD100_Uganomoku_vector<-pred2_HD100_Uganomoku[[1]]
pred2_HD100_Uganomoku_vector<-data.frame(ID="pred2_HD100_Uganomoku",Region="Hokkaido",
                                      Region="HD",
                                      Type="Uganomoku",
                                      Equation="Power",
                                      pred2_HD100_Uganomoku_vector)
pred2_HD100_Uganomoku_vector

# save width 600 * Height 500 

#Model 3
pred3_HD100_Uganomoku<- predictNLS(model3_HD,newdata=dummy_100,interval = c("confidence"))
pred3_HD100_Uganomoku
pred3_HD100_Uganomoku_vector<-pred3_HD100_Uganomoku[[1]]
pred3_HD100_Uganomoku_vector<-data.frame(ID="pred3_HD100_Uganomoku",Region="Hokkaido",
                                      Region="HD",
                                      Type="Uganomoku",
                                      Equation="Exponent",
                                      pred3_HD100_Uganomoku_vector)
pred3_HD100_Uganomoku_vector

Uganomoku_HD100<-rbind(pred1_HD100_Uganomoku_vector,
                    pred2_HD100_Uganomoku_vector,
                    pred3_HD100_Uganomoku_vector)
Uganomoku_HD100


#width 600 * Height 500 save


####### NorthJapanSea @ Sado ##########
Hondawara_NorthJapanSea<-subset(Hondawara,Hondawara$Region=="NorthJapanSea",na.rm = TRUE)


####線形モデル#####
model1_NJ<- nls(DW_g_m2~ a*Cover,
                start = list(a=50),data = Hondawara_NorthJapanSea)
summary(model1_NJ)
AIC(model1_NJ)
BIC(model1_NJ)

####累乗モデル#####
model2_NJ<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=1.444,b = 1.3181),data = Hondawara_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)
summary(model2_NJ)
AIC(model2_NJ)
BIC(model2_NJ)

####指数モデル#####
model3_NJ<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = Hondawara_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_NJ)
AIC(model3_NJ)
BIC(model3_NJ)



#######Plot line  NorthJapanSea @ Sado##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_NJS100_Hondawara<- predictNLS(model1_NJ,newdata=dummy_100,interval = c("confidence"))
pred1_NJS100_Hondawara
rm(pred1_NJS100_Hondawara_vector)
pred1_NJS100_Hondawara_vector<-pred1_NJS100_Hondawara[[1]]
pred1_NJS100_Hondawara_vector<-data.frame(ID="pred1_NJS100_Hondawara",Region="North Japan Sea",
                                         Region="NJS",
                                         Type="Hondawara",
                                         Equation="Linear",
                                         pred1_NJS100_Hondawara_vector)
pred1_NJS100_Hondawara_vector


#Model2
pred2_NJS100_Hondawara<- predictNLS(model2_NJ,newdata=dummy_100,interval = c("confidence"))
rm(pred2_NJS100_Hondawara_vector)
pred2_NJS100_Hondawara_vector<-pred2_NJS100_Hondawara[[1]]
pred2_NJS100_Hondawara_vector<-data.frame(ID="pred2_NJS100_Hondawara",Region="North Japan Sea",
                                         Region="NJS",
                                         Type="Hondawara",
                                         Equation="Power",
                                         pred2_NJS100_Hondawara_vector)
pred2_NJS100_Hondawara_vector

# save width 600 * Height 500 

#Model 3
pred3_NJS100_Hondawara<- predictNLS(model3_NJ,newdata=dummy_100,interval = c("confidence"))
rm(pred3_NJS100_Hondawara_vector)
pred3_NJS100_Hondawara_vector<-pred3_NJS100_Hondawara[[1]]
pred3_NJS100_Hondawara_vector<-data.frame(ID="pred3_NJS100_Hondawara",Region="North Japan Sea",
                                         Region="NJS",
                                         Type="Hondawara",
                                         Equation="Exponent",
                                         pred3_NJS100_Hondawara_vector)
pred3_NJS100_Hondawara_vector

Hondawara_NJS100<-rbind(pred1_NJS100_Hondawara_vector,
                       pred2_NJS100_Hondawara_vector,
                       pred3_NJS100_Hondawara_vector)
Hondawara_NJS100


#width 600 * Height 500 save

####### Central Pacific @ Hondawara##########
Hondawara_CentralPacific<-subset(Hondawara,Hondawara$Region=="CentralPacific")

####線形モデル#####
model1_cp<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Hondawara_CentralPacific)
summary(model1_cp)
AIC(model1_cp)
BIC(model1_cp)

####累乗モデル#####
model2_cp<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=10,b = 1),data = Hondawara_CentralPacific)
summary(model2_cp)
AIC(model2_cp)
BIC(model2_cp)

####指数モデル#####
model3_cp<- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=50,b = 0.01), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data = Hondawara_CentralPacific,
              trace=TRUE)
summary(model3_cp)
AIC(model3_cp)
BIC(model3_cp)


####### Central Pacific @ Kominato##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_CP100_Hondawara<- predictNLS(model1_cp,newdata=dummy_100,interval = c("confidence"))
pred1_CP100_Hondawara
pred1_CP100_Hondawara_vector<-pred1_CP100_Hondawara[[1]]
pred1_CP100_Hondawara_vector<-data.frame(ID="pred1_CP100_Hondawara",Region="Central Pacific",
                                          Region="CP",
                                          Type="Hondawara",
                                          Equation="Linear",
                                          pred1_CP100_Hondawara_vector)
pred1_CP100_Hondawara_vector


#Model2
pred2_CP100_Hondawara<- predictNLS(model2_cp,newdata=dummy_100,interval = c("confidence"))
pred2_CP100_Hondawara
rm(pred2_CP100_Hondawara_vector)
pred2_CP100_Hondawara_vector<-pred2_CP100_Hondawara[[1]]
pred2_CP100_Hondawara_vector<-data.frame(ID="pred2_CP100_Hondawara",Region="Central Pacific",
                                         Region="CP",
                                         Type="Hondawara",
                                         Equation="Power",
                                         pred2_CP100_Hondawara_vector)
pred2_CP100_Hondawara_vector

# save width 600 * Height 500 

#Model 3
pred3_CP100_Hondawara<- predictNLS(model3_cp,newdata=dummy_100,interval = c("confidence"))
pred3_CP100_Hondawara
rm(pred3_CP100_Hondawara_vector)
pred3_CP100_Hondawara_vector<-pred3_CP100_Hondawara[[1]]
pred3_CP100_Hondawara_vector<-data.frame(ID="pred3_CP100_Hondawara",Region="Central Pacific",
                                         Region="CP",
                                         Type="Hondawara",
                                         Equation="Exponent",
                                         pred3_CP100_Hondawara_vector)
pred3_CP100_Hondawara_vector

Hondawara_CP100<-rbind(pred1_CP100_Hondawara_vector,
                       pred2_CP100_Hondawara_vector,
                       pred3_CP100_Hondawara_vector)
Hondawara_CP100





####### SetoIslandSea  ##########
Hondawara_SetoIslandSea<-subset(Hondawara,Hondawara$Region=="SetoIslandSea")


####線形モデル#####
model1_ST<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Hondawara_SetoIslandSea)
summary(model1_ST)
AIC(model1_ST)
BIC(model1_ST)

####累乗モデル#####
model2_ST<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = Hondawara_SetoIslandSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_ST)
AIC(model2_ST)
BIC(model2_ST)

####指数モデル#####
model3_ST<- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=17.5,b =0.048), data = Hondawara_SetoIslandSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_ST)
AIC(model3_ST)
BIC(model3_ST)


######Plot line  SetoIslandSea @ Sado##########
library(propagate)
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_ST100_Hondawara<- predictNLS(model1_ST,newdata=dummy_100,interval = c("confidence"))
pred1_ST100_Hondawara
pred1_ST100_Hondawara_vector<-pred1_ST100_Hondawara[[1]]
pred1_ST100_Hondawara_vector<-data.frame(ID="pred1_ST100_Hondawara",Region="Seto Island Sea",
                                         Region="ST",
                                         Type="Hondawara",
                                         Equation="Linear",
                                         pred1_ST100_Hondawara_vector)
pred1_ST100_Hondawara_vector

#Model2
pred2_ST100_Hondawara<- predictNLS(model2_ST,newdata=dummy_100,interval = c("confidence"))
pred2_ST100_Hondawara
pred2_ST100_Hondawara_vector<-pred2_ST100_Hondawara[[1]]
pred2_ST100_Hondawara_vector<-data.frame(ID="pred2_ST100_Hondawara",Region="Seto Island Sea",
                                         Region="ST",
                                         Type="Hondawara",
                                         Equation="Power",
                                         pred2_ST100_Hondawara_vector)
pred2_ST100_Hondawara_vector

#Model3
pred3_ST100_Hondawara<- predictNLS(model3_ST,newdata=dummy_100,interval = c("confidence"))
pred3_ST100_Hondawara
pred3_ST100_Hondawara_vector<-pred3_ST100_Hondawara[[1]]
pred3_ST100_Hondawara_vector<-data.frame(ID="pred3_ST100_Hondawara",Region="Seto Island Sea",
                                         Region="ST",
                                         Type="Hondawara",
                                         Equation="Exponent",
                                         pred3_ST100_Hondawara_vector)
pred3_ST100_Hondawara_vector

Hondawara_ST100<-rbind(pred1_ST100_Hondawara_vector,
                       pred2_ST100_Hondawara_vector,
                       pred3_ST100_Hondawara_vector)
Hondawara_ST100

#width 600 * Height 500 save

####### SouthJapanSea  ##########
Hondawara_SouthJapanSea<-subset(Hondawara,Hondawara$Region=="SouthJapanSea")


####線形モデル#####
model1_SJ <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Hondawara_SouthJapanSea)
summary(model1_SJ)
AIC(model1_SJ)
BIC(model1_SJ)

####累乗モデル#####
model2_SJ <- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = Hondawara_SouthJapanSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_SJ)
AIC(model2_SJ)
BIC(model2_SJ)

####指数モデル#####
model3_SJ <- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=17.5,b =0.048), data = Hondawara_SouthJapanSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_SJ)
AIC(model3_SJ)
BIC(model3_SJ)

######Plot line  SouthJapanSea ##########
#Model1
pred1_SJ100_Hondawara<- predictNLS(model1_SJ,newdata=dummy_100,interval = c("confidence"))
pred1_SJ100_Hondawara
rm(pred1_SJ100_Hondawara_vector)
pred1_SJ100_Hondawara_vector<-pred1_SJ100_Hondawara[[1]]
pred1_SJ100_Hondawara_vector<-data.frame(ID="pred1_SJ100_Hondawara",Region="South Japan Sea",
                                         Region="SJS",
                                         Type="Hondawara",
                                         Equation="Linear",
                                         pred1_SJ100_Hondawara_vector)
pred1_SJ100_Hondawara_vector


#Model2
pred2_SJ100_Hondawara<- predictNLS(model2_SJ,newdata=dummy_100,interval = c("confidence"))
pred2_SJ100_Hondawara
rm(pred2_SJ100_Hondawara_vector)
pred2_SJ100_Hondawara_vector<-pred2_SJ100_Hondawara[[1]]
pred2_SJ100_Hondawara_vector<-data.frame(ID="pred2_SJ100_Hondawara",Region="South Japan Sea",
                                         Region="SJS",
                                         Type="Hondawara",
                                         Equation="Power",
                                         pred2_SJ100_Hondawara_vector)
pred2_SJ100_Hondawara_vector

#Model3
pred3_SJ100_Hondawara<- predictNLS(model3_SJ,newdata=dummy_100,interval = c("confidence"))
pred3_SJ100_Hondawara
rm(pred3_SJ100_Hondawara_vector)
pred3_SJ100_Hondawara_vector<-pred3_SJ100_Hondawara[[1]]
pred3_SJ100_Hondawara_vector<-data.frame(ID="pred3_SJ100_Hondawara",Region="South Japan Sea",
                                         Region="SJS",
                                         Type="Hondawara",
                                         Equation="Exponent",
                                         pred3_SJ100_Hondawara_vector)
pred3_SJ100_Hondawara_vector

Hondawara_SJ100<-rbind(pred1_SJ100_Hondawara_vector,
                       pred2_SJ100_Hondawara_vector,
                       pred3_SJ100_Hondawara_vector)
Hondawara_SJ100

#width 600 * Height 500 save

####### EastChinaSea  ##########
Hondawara_EastChinaSea<-subset(Hondawara,Hondawara$Region=="EastChinaSea")


####線形モデル#####
model1_EC <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Hondawara_EastChinaSea)
summary(model1_EC)
AIC(model1_EC)
BIC(model1_EC)

####累乗モデル#####
model2_EC<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = Hondawara_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_EC)
AIC(model2_EC)
BIC(model2_EC)

####指数モデル#####
model3_EC<- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=17.5,b =0.048), data = Hondawara_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_EC)
AIC(model3_EC)
BIC(model3_EC)

######Plot line  EastChinaSea ##########
#Model1
pred1_EC100_Hondawara<- predictNLS(model1_EC,newdata=dummy_100,interval = c("confidence"))
pred1_EC100_Hondawara
rm(pred1_EC100_Hondawara_vector)
pred1_EC100_Hondawara_vector<-pred1_EC100_Hondawara[[1]]
pred1_EC100_Hondawara_vector<-data.frame(ID="pred1_EC100_Hondawara",Region="East China Sea",
                                         Region="ECS",
                                         Type="Hondawara",
                                         Equation="Linear",
                                         pred1_EC100_Hondawara_vector)
pred1_EC100_Hondawara_vector


#Model2
pred2_EC100_Hondawara<- predictNLS(model2_EC,newdata=dummy_100,interval = c("confidence"))
pred2_EC100_Hondawara
rm(pred2_EC100_Hondawara_vector)
pred2_EC100_Hondawara_vector<-pred2_EC100_Hondawara[[1]]
pred2_EC100_Hondawara_vector<-data.frame(ID="pred2_EC100_Hondawara",Region="East China Sea",
                                         Region="ECS",
                                         Type="Hondawara",
                                         Equation="Power",
                                         pred2_EC100_Hondawara_vector)
pred2_EC100_Hondawara_vector

#Model3
pred3_EC100_Hondawara<- predictNLS(model3_EC,newdata=dummy_100,interval = c("confidence"))
pred3_EC100_Hondawara
rm(pred3_EC100_Hondawara_vector)
pred3_EC100_Hondawara_vector<-pred3_EC100_Hondawara[[1]]
pred3_EC100_Hondawara_vector<-data.frame(ID="pred3_EC100_Hondawara",Region="East China Sea",
                                         Region="ECS",
                                         Type="Hondawara",
                                         Equation="Exponent",
                                         pred3_EC100_Hondawara_vector)
pred3_EC100_Hondawara_vector

Hondawara_EC100<-rbind(pred1_EC100_Hondawara_vector,
                       pred2_EC100_Hondawara_vector,
                       pred3_EC100_Hondawara_vector)
Hondawara_EC100

#width 600 * Height 500 save
######Plot line  all species  ##########
rm(Hondawara100)
Hondawara100<-rbind(Uganomoku_HD100,Hondawara_NJS100,Hondawara_CP100,
                    Hondawara_ST100,Hondawara_SJ100,Hondawara_EC100)
Hondawara100
Hondawara100$Equation <- factor(Hondawara100$Equation, 
                             levels = c("Linear","Power","Exponent") )
library(dplyr)

Hondawara100 <- Hondawara100 %>%
  mutate(Hondawara100,RegionType = interaction(Region.1, Type))
Hondawara100$RegionType
Hondawara100$RegionType <- factor(Hondawara100$RegionType, 
                               levels = c("HD.Uganomoku","NJS.Hondawara","CP.Hondawara",
                                          "ST.Hondawara","SJS.Hondawara","ECS.Hondawara") )
Hondawara100

plot_all_Hondawara100<-ggplot(data=Hondawara100, 
                              aes(x=RegionType, y=Prop.Mean.1,color= Equation,shape= Equation)) + 
  xlab("Region Type")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_y_continuous(breaks=seq(0, 3500,length=8),limits=c(-50,3500))+ 
  geom_point(stat = "identity", position = position_dodge(width = 0.9),size=5) +
  geom_errorbar(aes(ymin = Prop.Mean.1 - Prop.sd.1, ymax = Prop.Mean.1 + Prop.sd.1),
                width = 0.5, position = position_dodge(width = 0.9))+
  theme(axis.title =element_text(size=20),axis.text = element_text(color="black",size=20))+
  theme(legend.title=element_text(size=20),legend.text =  element_text(size = 20))
plot_all_Hondawara100
#save as 1400 * 400
