rm(list=ls())
library(ggplot2)
library(propagate)
library(dplyr)

Arame<-read.csv("Arame.csv",stringsAsFactors = TRUE)
Arame$Season <- factor(Arame$Season, 
                       levels = c("Flourish","Decline") )
names(Arame)

####### TohokuPacific @ Arame##########
Arame_TohokuPacific<-subset(Arame,Arame$Region=="TohokuPacific")
Arame_TohokuPacific$DW_g_m2

###Linear model#####
model1_TP<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Arame_TohokuPacific)
summary(model1_TP)
AIC(model1_TP)
BIC(model1_TP)

####Power model#####
model2_TP<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Arame_TohokuPacific)
summary(model2_TP)
AIC(model2_TP)
BIC(model2_TP)

####Exponential model#####
model3_TP<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Arame_TohokuPacific,
                trace=TRUE)
summary(model3_TP)
AIC(model3_cp)
BIC(model3_cp)



####### TohokuPacific##########
library(ggplot2)
library(propagate)
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_TP100_Arame<- predictNLS(model1_TP,newdata=dummy_100,interval = c("confidence"))
pred1_TP100_Arame
pred1_TP100_Arame_vector<-pred1_TP100_Arame[[1]]
pred1_TP100_Arame_vector<-data.frame(ID="pred1_TP100_Arame",Region="Tohoku Pacific",
                                      Region="TP",
                                      Type="Arame",
                                      Equation="Linear",
                                     pred1_TP100_Arame_vector)
pred1_TP100_Arame_vector

#Model2
pred2_TP100_Arame<- predictNLS(model2_TP,newdata=dummy_100,interval = c("confidence"))
pred2_TP100_Arame
pred2_TP100_Arame_vector<-pred2_TP100_Arame[[1]]
pred2_TP100_Arame_vector<-data.frame(ID="pred2_TP100_Arame",Region="Tohoku Pacific",
                                     Region="TP",
                                     Type="Arame",
                                     Equation="Power",
                                     pred2_TP100_Arame_vector)
pred2_TP100_Arame_vector

#Model3
pred3_TP100_Arame<- predictNLS(model3_TP,newdata=dummy_100,interval = c("confidence"))
pred3_TP100_Arame
pred3_TP100_Arame_vector<-pred3_TP100_Arame[[1]]
pred3_TP100_Arame_vector<-data.frame(ID="pred3_TP100_Arame",Region="Tohoku Pacific",
                                     Region="TP",
                                     Type="Arame",
                                     Equation="Exponent",
                                     pred3_TP100_Arame_vector)
pred3_TP100_Arame_vector

Arame_TP100<-rbind(pred1_TP100_Arame_vector,
                    pred2_TP100_Arame_vector,
                    pred3_TP100_Arame_vector)
Arame_TP100


####### Central Pacific Choshi @ Arame##########
Arame_CentralPacificCH<-subset(Arame,Arame$Region=="CentralPacific"&Arame$Site.1=="Choshi")


####Linear model#####
model1_CH<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Arame_CentralPacificCH)
summary(model1_CH)
AIC(model1_CH)
BIC(model1_CH)

####Power model#####
model2_CH<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=10,b = 1),data = Arame_CentralPacificCH)
summary(model2_CH)
AIC(model2_CH)
BIC(model2_CH)

####Exponential model#####
model3_CH<- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=50,b = 0.01), 
              control = list(maxiter = 50000, warnOnly = TRUE),
              data = Arame_CentralPacificCH,
              trace=TRUE)
summary(model3_CH)
AIC(model3_CH)
BIC(model3_CH)


####### Central Pacific Choshi##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_CH100_Arame<- predictNLS(model1_CH,newdata=dummy_100,interval = c("confidence"))
pred1_CH100_Arame
rm(pred1_CH100_Arame_vector)
pred1_CH100_Arame_vector<-pred1_CH100_Arame[[1]]
pred1_CH100_Arame_vector<-data.frame(ID="pred1_CH100_Arame",Region="Central Pacific",
                                     Region="CP",
                                     Type="Arame",
                                     Equation="Linear",
                                     pred1_CH100_Arame_vector)
pred1_CH100_Arame_vector

#Model2
pred2_CH100_Arame<- predictNLS(model2_CH,newdata=dummy_100,interval = c("confidence"))
pred2_CH100_Arame

rm(pred2_CH100_Arame_vector)
pred2_CH100_Arame_vector<-pred2_CH100_Arame[[1]]

pred2_CH100_Arame_vector<-data.frame(ID="pred2_CH100_Arame",Region="Central Pacific",
                                     Region="CP",
                                     Type="Arame",
                                     Equation="Power",
                                     pred2_CH100_Arame_vector)
pred2_CH100_Arame_vector

# save width 600 * Height 500 
#Model 3
pred3_CH100_Arame<- predictNLS(model3_CH,newdata=dummy_100,interval = c("confidence"))
pred3_CH100_Arame
rm(pred3_CH100_Arame_vector)
pred3_CH100_Arame_vector<-pred3_CH100_Arame[[1]]
pred3_CH100_Arame_vector<-data.frame(ID="pred3_CH100_Arame",Region="Central Pacific",
                                     Region="CP",
                                     Type="Arame",
                                     Equation="Exponent",
                                     pred3_CH100_Arame_vector)
pred3_CH100_Arame_vector

Arame_CP100<-rbind(pred1_CH100_Arame_vector,
                   pred2_CH100_Arame_vector,
                   pred3_CH100_Arame_vector)
Arame_CP100

#width 600 * Height 500 save


####### Central Pacific Sagami @ Arame##########
Arame_CentralPacific<-subset(Arame,Arame$Region=="CentralPacific"&Arame$Site=="神奈川県")

####Linear model#####
model1_cp<- nls(DW_g_m2~ a*Cover,
                start = list(a=50),data = Arame_CentralPacific)
summary(model1_cp)
AIC(model1_cp)
BIC(model1_cp)

####Power model#####
model2_cp<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),data = Arame_CentralPacific)
summary(model2_cp)
AIC(model2_cp)
BIC(model2_cp)

####Exponential model#####
model3_cp<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=50,b = 0.01), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Arame_CentralPacific,
                trace=TRUE)
summary(model3_cp)
AIC(model3_cp)
BIC(model3_cp)



####### Central Pacific @ Sagami##########
#Model1


#Model2
 
#Model 3

#width 600 * Height 500 save

####### SouthJapanSea  ##########
Arame_SouthJapanSea<-subset(Arame,Arame$Region=="SouthJapanSea")


####Linear model#####
model1_SJS<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Arame_SouthJapanSea)
summary(model1_SJS)
AIC(model1_SJS)
BIC(model1_SJS)

####Power model#####
model2_SJS<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = Arame_SouthJapanSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_SJS)
AIC(model2_SJS)
BIC(model2_SJS)

####Exponential model#####
model3_SJS<- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=17.5,b =0.048), data = Arame_SouthJapanSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_SJS)
AIC(model3_SJS)
BIC(model3_SJS)


######Plot line  SouthJapanSea ##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_SJS100_Arame<- predictNLS(model1_SJS,newdata=dummy_100,interval = c("confidence"))
pred1_SJS100_Arame
rm(pred1_SJS100_Arame_vector)
pred1_SJS100_Arame_vector<-pred1_SJS100_Arame[[1]]
pred1_SJS100_Arame_vector<-data.frame(ID="pred1_SJS100_Arame",Region="South Japan Sea",
                                     Region="SJS",
                                     Type="Arame",
                                     Equation="Linear",
                                     pred1_SJS100_Arame_vector)
pred1_SJS100_Arame_vector
#Model2
pred2_SJS100_Arame<- predictNLS(model2_SJS,newdata=dummy_100,interval = c("confidence"))
pred2_SJS100_Arame
rm(pred2_SJS100_Arame_vector)
pred2_SJS100_Arame_vector<-pred2_SJS100_Arame[[1]]
pred2_SJS100_Arame_vector<-data.frame(ID="pred2_SJS100_Arame",Region="South Japan Sea",
                                      Region="SJS",
                                      Type="Arame",
                                      Equation="Power",
                                      pred2_SJS100_Arame_vector)
pred2_SJS100_Arame_vector

#Model3
pred3_SJS100_Arame<- predictNLS(model3_SJS,newdata=dummy_100,interval = c("confidence"))
pred3_SJS100_Arame
rm(pred3_SJS100_Arame_vector)
pred3_SJS100_Arame_vector<-pred3_SJS100_Arame[[1]]
pred3_SJS100_Arame_vector<-data.frame(ID="pred3_SJS100_Arame",Region="South Japan Sea",
                                      Region="SJS",
                                      Type="Arame",
                                      Equation="Exponent",
                                      pred3_SJS100_Arame_vector)
pred3_SJS100_Arame_vector

Arame_SJS100<-rbind(pred1_SJS100_Arame_vector,
                   pred2_SJS100_Arame_vector,
                   pred3_SJS100_Arame_vector)
Arame_SJS100

#width 600 * Height 500 save

####### Sagarame @ SoutherPacific  ##########
Sagarame<-subset(Arame,Arame$Jap_name=="Sagarame")


####Linear model#####
model1_Saga<- nls(DW_g_m2~ a*Cover,
                  start = list(a=50),data = Sagarame)
summary(model1_Saga)
AIC(model1_Saga)
BIC(model1_Saga)

####Power model#####
model2_Saga<- nls(DW_g_m2~ a*Cover^b,
                  start = list(a=1.444,b = 1.3181),data = Sagarame,
                  control = list(maxiter = 50000, warnOnly = TRUE),
                  trace=TRUE)
summary(model2_Saga)
AIC(model2_Saga)
BIC(model2_Saga)

####Exponential model#####
model3_Saga<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                  start = list(a=17.5,b =0.048), data = Sagarame,
                  control = list(maxiter = 50000, warnOnly = TRUE),
                  trace=TRUE)

summary(model3_Saga)
AIC(model3_Saga)
BIC(model3_Saga)


######Plot line  Sagarame @ Tokushima##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_TS100_Sagarame<- predictNLS(model1_Saga,newdata=dummy_100,interval = c("confidence"))
pred1_TS100_Sagarame

pred1_TS100_Sagarame_vector<-pred1_TS100_Sagarame[[1]]
pred1_TS100_Sagarame_vector<-data.frame(ID="pred1_TS100_Sagarame",Region="Central Pacific",
                                     Region="CP&SP",
                                     Type="Sagarame",
                                     Equation="Linear",
                                     pred1_TS100_Sagarame_vector)
pred1_TS100_Sagarame_vector


#Model2
pred2_TS100_Sagarame<- predictNLS(model2_Saga,newdata=dummy_100,interval = c("confidence"))
pred2_TS100_Sagarame
rm(pred2_TS100_Sagarame_vector)
pred2_TS100_Sagarame_vector<-pred2_TS100_Sagarame[[1]]
pred2_TS100_Sagarame_vector<-data.frame(ID="pred2_TS100_Sagarame",Region="Central Pacific",
                                        Region="CP&SP",
                                        Type="Sagarame",
                                        Equation="Power",
                                        pred2_TS100_Sagarame_vector)
pred2_TS100_Sagarame_vector

#Model3
pred3_TS100_Sagarame<- predictNLS(model3_Saga,newdata=dummy_100,interval = c("confidence"))
pred3_TS100_Sagarame
rm(pred3_TS100_Sagarame_vector)
pred3_TS100_Sagarame_vector<-pred3_TS100_Sagarame[[1]]
pred3_TS100_Sagarame_vector<-data.frame(ID="pred3_TS100_Sagarame",Region="Central Pacific",
                                        Region="CP&SP",
                                        Type="Sagarame",
                                        Equation="Exponent",
                                        pred3_TS100_Sagarame_vector)
pred3_TS100_Sagarame_vector

Sagarame_TS100<-rbind(pred1_TS100_Sagarame_vector,
                    pred2_TS100_Sagarame_vector,
                    pred3_TS100_Sagarame_vector)
Sagarame_TS100

#width 600 * Height 500 save

####### EastChinaSea  ##########
Arame_EastChinaSea<-subset(Arame,Arame$Region=="EastChinaSea")

####Linear model#####
model1_EC <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = Arame_EastChinaSea)
summary(model1_EC)
AIC(model1_EC)
BIC(model1_EC)

####Power model#####
model2_EC<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = Arame_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_EC)
AIC(model2_EC)
BIC(model2_EC)

####Exponential model#####
model3_EC<- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=17.5,b =0.048), data = Arame_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_EC)
AIC(model3_EC)
BIC(model3_EC)


######Plot line  EastChinaSea ##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_EC100_Arame<- predictNLS(model1_EC,newdata=dummy_100,interval = c("confidence"))
pred1_EC100_Arame
rm(pred1_EC100_Arame_vector)
pred1_EC100_Arame_vector<-pred1_EC100_Arame[[1]]
pred1_EC100_Arame_vector<-data.frame(ID="pred1_EC100_Arame",Region="East China Sea",
                                      Region="ECS",
                                      Type="Arame",
                                      Equation="Linear",
                                      pred1_EC100_Arame_vector)
pred1_EC100_Arame_vector


#Model2
pred2_EC100_Arame<- predictNLS(model2_EC,newdata=dummy_100,interval = c("confidence"))
pred2_EC100_Arame
rm(pred2_EC100_Arame_vector)
pred2_EC100_Arame_vector<-pred2_EC100_Arame[[1]]
pred2_EC100_Arame_vector<-data.frame(ID="pred2_EC100_Arame",Region="East China Sea",
                                     Region="ECS",
                                     Type="Arame",
                                     Equation="Power",
                                     pred2_EC100_Arame_vector)
pred2_EC100_Arame_vector

#Model3
pred3_EC100_Arame<- predictNLS(model3_EC,newdata=dummy_100,interval = c("confidence"))
pred3_EC100_Arame
rm(pred3_EC100_Arame_vector)
pred3_EC100_Arame_vector<-pred3_EC100_Arame[[1]]
pred3_EC100_Arame_vector<-data.frame(ID="pred3_EC100_Arame",Region="East China Sea",
                                     Region="ECS",
                                     Type="Arame",
                                     Equation="Exponent",
                                     pred3_EC100_Arame_vector)
pred3_EC100_Arame_vector

Arame_EC100<-rbind(pred1_EC100_Arame_vector,
                   pred2_EC100_Arame_vector,
                   pred3_EC100_Arame_vector)
Arame_EC100

######Plot line  all species  ##########

Arame100<-rbind(Arame_TP100,Arame_CP100,Arame_SJS100,
                 Sagarame_TS100,Arame_EC100)
Arame100
Arame100$Equation <- factor(Arame100$Equation, 
                             levels = c("Linear","Power","Exponent") )
library(dplyr)

Arame100 <- Arame100 %>%
  mutate(Arame100,RegionType = interaction(Region.1, Type))
Arame100$RegionType
Arame100$RegionType <- factor(Arame100$RegionType, 
                               levels = c("TP.Arame","CP.Arame","SJS.Arame",
                                          "CP&SP.Sagarame","ECS.Arame") )
Arame100



plot_all_Arame100<-ggplot(data=Arame100, 
                          aes(x=RegionType, y=Prop.Mean.1,color= Equation,shape=Equation)) + 
  xlab("Region Type")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_y_continuous(breaks=seq(0, 11000,length=12),limits=c(0,11000))+ 
  geom_point(stat = "identity", position = position_dodge(width = 0.9),size=5) +
  geom_errorbar(aes(ymin = Prop.Mean.1 - Prop.sd.1, ymax = Prop.Mean.1 + Prop.sd.1),
                width = 0.5, position = position_dodge(width = 0.9))+
  theme(axis.title =element_text(size=20),axis.text = element_text(color="black",size=20))+
  theme(legend.title=element_text(size=20),legend.text =  element_text(size = 20))
plot_all_Arame100

#save as 1400 * 400

