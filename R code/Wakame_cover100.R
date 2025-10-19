rm(list=ls())
library(nlme)
library(ggplot2)
library(propagate)
library(dplyr)

Wakame<-read.csv("Wakame.csv",stringsAsFactors = TRUE)
Wakame$Season <- factor(Wakame$Season, 
                       levels = c("Flourish","Decline") )
names(Wakame)

####### TohokuPacific @ Wakame##########
Wakame_TohokuPacific<-subset(Wakame,Wakame$Region=="TohokuPacific")
Wakame_TohokuPacific$DW_g_m2

###線形モデル#####
model1_TP<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Wakame_TohokuPacific)
summary(model1_TP)
AIC(model1_TP)
BIC(model1_TP)

####累乗モデル#####
model2_TP<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Wakame_TohokuPacific)
summary(model2_TP)
AIC(model2_TP)
BIC(model2_TP)

####指数モデル#####
model3_TP<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Wakame_TohokuPacific,
                trace=TRUE)
summary(model3_TP)
AIC(model3_TP)
BIC(model3_TP)



####### TohokuPacific##########
#Model1

library(ggplot2)
library(propagate)

dummy_100<- expand.grid(Cover=100)
pred1_TP100_Wakame<- predictNLS(model1_TP,newdata=dummy_100,interval = c("confidence"))
pred1_TP100_Wakame
pred1_TP100_Wakame_vector<-pred1_TP100_Wakame[[1]]
pred1_TP100_Wakame_vector<-data.frame(ID="pred1_TP100_Wakame",Region="Tohoku Pacific",
                                     Region="TP",
                                     Type="Wakame",
                                     Equation="Linear",
                                     pred1_TP100_Wakame_vector)
pred1_TP100_Wakame_vector

#Model2
pred2_TP100_Wakame<- predictNLS(model2_TP,newdata=dummy_100,interval = c("confidence"))
pred2_TP100_Wakame
pred2_TP100_Wakame_vector<-pred2_TP100_Wakame[[1]]
pred2_TP100_Wakame_vector<-data.frame(ID="pred2_TP100_Wakame",Region="Tohoku Pacific",
                                     Region="TP",
                                     Type="Wakame",
                                     Equation="Power",
                                     pred2_TP100_Wakame_vector)
pred2_TP100_Wakame_vector


# save width 600 * Height 500 

#Model3
pred3_TP100_Wakame<- predictNLS(model3_TP,newdata=dummy_100,interval = c("confidence"))
pred3_TP100_Wakame
pred3_TP100_Wakame_vector<-pred3_TP100_Wakame[[1]]
pred3_TP100_Wakame_vector<-data.frame(ID="pred3_TP100_Wakame",Region="Tohoku Pacific",
                                     Region="TP",
                                     Type="Wakame",
                                     Equation="Exponent",
                                     pred3_TP100_Wakame_vector)
pred3_TP100_Wakame_vector

Wakame_TP100<-rbind(pred1_TP100_Wakame_vector,
                   pred2_TP100_Wakame_vector,
                   pred3_TP100_Wakame_vector)
Wakame_TP100


#width 600 * Height 500 save

####### NorthJapanSea @ Sado ##########
Wakame_NorthJapanSea<-subset(Wakame,Wakame$Region=="NorthJapanSea")


####線形モデル#####
model1_NJ<- nls(DW_g_m2~ a*Cover,
                start = list(a=50),data = Wakame_NorthJapanSea)
summary(model1_NJ)
AIC(model1_NJ)
BIC(model1_NJ)

####累乗モデル#####
model2_NJ<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=1.444,b = 1.3181),data = Wakame_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)
summary(model2_NJ)
AIC(model2_NJ)
BIC(model2_NJ)

####指数モデル#####
model3_NJ<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = Wakame_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_NJ)
AIC(model3_NJ)
BIC(model3_NJ)


#######Plot line  NorthJapanSea @ Sado##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_NJS100_Wakame<- predictNLS(model1_NJ,newdata=dummy_100,interval = c("confidence"))
pred1_NJS100_Wakame
rm(pred1_NJS100_Wakame_vector)
pred1_NJS100_Wakame_vector<-pred1_NJS100_Wakame[[1]]
pred1_NJS100_Wakame_vector<-data.frame(ID="pred1_NJS100_Wakame",Region="North Japan Sea",
                                      Region="NJS",
                                      Type="Wakame",
                                      Equation="Linear",
                                      pred1_NJS100_Wakame_vector)
pred1_NJS100_Wakame_vector

#Model2
pred2_NJS100_Wakame<- predictNLS(model2_NJ,newdata=dummy_100,interval = c("confidence"))
pred2_NJS100_Wakame
rm(pred2_NJS100_Wakame_vector)
pred2_NJS100_Wakame_vector<-pred2_NJS100_Wakame[[1]]
pred2_NJS100_Wakame_vector<-data.frame(ID="pred2_NJS100_Wakame",Region="North Japan Sea",
                                       Region="NJS",
                                       Type="Wakame",
                                       Equation="Power",
                                       pred2_NJS100_Wakame_vector)
pred2_NJS100_Wakame_vector

#Model3
pred3_NJS100_Wakame<- predictNLS(model3_NJ,newdata=dummy_100,interval = c("confidence"))
pred3_NJS100_Wakame
rm(pred3_NJS100_Wakame_vector)
pred3_NJS100_Wakame_vector<-pred3_NJS100_Wakame[[1]]
pred3_NJS100_Wakame_vector<-data.frame(ID="pred3_NJS100_Wakame",Region="North Japan Sea",
                                       Region="NJS",
                                       Type="Wakame",
                                       Equation="Exponent",
                                       pred3_NJS100_Wakame_vector)
pred3_NJS100_Wakame_vector

Wakame_NJS100<-rbind(pred1_NJS100_Wakame_vector,
                    pred2_NJS100_Wakame_vector,
                    pred3_NJS100_Wakame_vector)
Wakame_NJS100
#width 600 * Height 500 save


####### Central Pacific Sagami @ Wakame##########
Wakame_CentralPacific<-subset(Wakame,Wakame$Region=="CentralPacific")

Wakame_CentralPacific$DW_g_m2

####線形モデル#####
model1_CH<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Wakame_CentralPacific)
summary(model1_CH)
AIC(model1_CH)
BIC(model1_CH)

####累乗モデル#####
model2_CH<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=10,b = 1),data = Wakame_CentralPacific)
summary(model2_CH)
AIC(model2_CH)
BIC(model2_CH)

####指数モデル#####
model3_CH<- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=50,b = 0.01), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data = Wakame_CentralPacific,
              trace=TRUE)
summary(model3_CH)
AIC(model3_CH)
BIC(model3_CH)

####### Central Pacific@Sagami##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_CP100_Wakame<- predictNLS(model1_CH,newdata=dummy_100,interval = c("confidence"))
pred1_CP100_Wakame
rm(pred1_CP100_Wakame_vector)
pred1_CP100_Wakame_vector<-pred1_CP100_Wakame[[1]]
pred1_CP100_Wakame_vector<-data.frame(ID="pred1_CP100_Wakame",Region="Central Pacific",
                                       Region="CP",
                                       Type="Wakame",
                                       Equation="Linear",
                                       pred1_CP100_Wakame_vector)
pred1_CP100_Wakame_vector

#Model2
pred2_CP100_Wakame<- predictNLS(model2_CH,newdata=dummy_100,interval = c("confidence"))
pred2_CP100_Wakame
rm(pred2_CP100_Wakame_vector)
pred2_CP100_Wakame_vector<-pred2_CP100_Wakame[[1]]
pred2_CP100_Wakame_vector<-data.frame(ID="pred2_CP100_Wakame",Region="Central Pacific",
                                       Region="CP",
                                       Type="Wakame",
                                       Equation="Power",
                                       pred2_CP100_Wakame_vector)
pred2_CP100_Wakame_vector

#Model3
pred3_CP100_Wakame<- predictNLS(model3_CH,newdata=dummy_100,interval = c("confidence"))
pred3_CP100_Wakame
pred3_CP100_Wakame_vector<-pred3_CP100_Wakame[[1]]
pred3_CP100_Wakame_vector<-data.frame(ID="pred3_CP100_Wakame",Region="Central Pacific",
                                       Region="CP",
                                       Type="Wakame",
                                       Equation="Exponent",
                                       pred3_CP100_Wakame_vector)
pred3_CP100_Wakame_vector

Wakame_CP100<-rbind(pred1_CP100_Wakame_vector,
                     pred2_CP100_Wakame_vector,
                     pred3_CP100_Wakame_vector)
Wakame_CP100

######Plot line  all species  ##########

Wakame100<-rbind(Wakame_TP100,Wakame_NJS100,Wakame_CP100)
Wakame100
Wakame100$Equation <- factor(Wakame100$Equation, 
                            levels = c("Linear","Power","Exponent") )
library(dplyr)

Wakame100 <- Wakame100 %>%
  mutate(Wakame100,RegionType = interaction(Region.1, Type))
Wakame100$RegionType
Wakame100$RegionType <- factor(Wakame100$RegionType, 
                              levels = c("TP.Wakame","NJS.Wakame","CP.Wakame") )
Wakame100



library(ggplot2)
plot_all_wakame100<-ggplot(data=Wakame100, 
                           aes(x=RegionType, y=Prop.Mean.1,color= Equation,shape=Equation)) + 
  xlab("Region Type")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1600))+ 
  geom_point(stat = "identity", position = position_dodge(width = 0.9),size=5) +
  geom_errorbar(aes(ymin = Prop.Mean.1 - Prop.sd.1, ymax = Prop.Mean.1 + Prop.sd.1),
                width = 0.5, position = position_dodge(width = 0.9))+
  theme(axis.title =element_text(size=20),axis.text = element_text(color="black",size=20))+
  theme(legend.title=element_text(size=20),legend.text =  element_text(size = 20))
plot_all_wakame100

#save as 1400 * 400

