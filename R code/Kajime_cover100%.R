##################################################################
### Estimated biomass of Kajime (Ecklonia) at 100% coverage ######
##################################################################


rm(list = ls())
library(dplyr)
library(ggplot2)
library(propagate)

kajime<-read.csv("Kajime.csv",stringsAsFactors = TRUE)
kajime$Season <- factor(kajime$Season, levels = c("Flourish","Decline") )
names(kajime)

####### Central Pacific @ Kajime##########
kajime_CentralPacific<-subset(kajime,kajime$Region=="CentralPacific")
kajime_CentralPacific_Flourish<-subset(kajime,kajime$Region=="CentralPacific"&kajime$Season=="Flourish")
kajime_CentralPacific$Season <- 
  factor(kajime_CentralPacific$Season, levels = c("Flourish","Decline") )


####Linear model#####
model1_cp<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = kajime_CentralPacific)
summary(model1_cp)
AIC(model1_cp)
BIC(model1_cp)

####Power model#####
model2_cp<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=10,b = 1),data = kajime_CentralPacific)
summary(model2_cp)
AIC(model2_cp)
BIC(model2_cp)

####Exponential model#####
model3_cp<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=50,b = 0.01), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = kajime_CentralPacific,
                trace=TRUE)
summary(model3_cp)
AIC(model3_cp)
BIC(model3_cp)


####### Plot #######
library(propagate)
#Model1

dummy_100<- expand.grid(Cover=100)
pred1_cp100_Kajime<- predictNLS(model1_cp,newdata=dummy_100,interval = c("confidence"))
pred1_cp100_Kajime
pred1_cp100_Kajime_vector<-pred1_cp100_Kajime[[1]]
pred1_cp100_Kajime_vector<-data.frame(ID="pred1_cp100_Kajime",Region="Central Pacific",
                                       Region="CP",
                                       Type="Kajime",
                                       Equation="Linear",
                                      pred1_cp100_Kajime_vector)
pred1_cp100_Kajime_vector

#Model2
pred2_cp100_Kajime<- predictNLS(model2_cp,newdata=dummy_100,interval = c("confidence"))
pred2_cp100_Kajime
pred2_cp100_Kajime_vector<-pred2_cp100_Kajime[[1]]
pred2_cp100_Kajime_vector<-data.frame(ID="pred2_cp100_Kajime",Region="Central Pacific",
                                      Region="CP",
                                      Type="Kajime",
                                      Equation="Power",
                                      pred2_cp100_Kajime_vector)
pred2_cp100_Kajime_vector

# save width 600 * Height 500 
#Model 3
pred3_cp100_Kajime<- predictNLS(model3_cp,newdata=dummy_100,interval = c("confidence"))
pred3_cp100_Kajime
pred3_cp100_Kajime_vector<-pred3_cp100_Kajime[[1]]
pred3_cp100_Kajime_vector<-data.frame(ID="pred3_cp100_Kajime",Region="Central Pacific",
                                      Region="CP",
                                      Type="Kajime",
                                      Equation="Exponent",
                                      pred3_cp100_Kajime_vector)
pred3_cp100_Kajime_vector


Kajime_cp100<-rbind(pred1_cp100_Kajime_vector,
                    pred2_cp100_Kajime_vector,
                    pred3_cp100_Kajime_vector)
Kajime_cp100
#width 600 * Height 500 save

####### NorthJapanSea @ Sado ##########
kajime_NorthJapanSea<-subset(kajime,kajime$Region=="NorthJapanSea")


####Linear model#####
model1_NJ<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = kajime_NorthJapanSea)
summary(model1_NJ)
AIC(model1_NJ)
BIC(model1_NJ)

####Power model#####
model2_NJ<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = kajime_NorthJapanSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_NJ)
AIC(model2_NJ)
BIC(model2_NJ)

####Exponential model#####
model3_NJ<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = kajime_NorthJapanSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_NJ)
AIC(model3_NJ)
BIC(model3_NJ)

#######Plot line  NorthJapanSea @ Sado ##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_NJ100_Tsuru<- predictNLS(model1_NJ,newdata=dummy_100,interval = c("confidence"))
pred1_NJ100_Tsuru
rm(pred1_NJ100_Tsuru_vector)
pred1_NJ100_Tsuru_vector<-pred1_NJ100_Tsuru[[1]]
pred1_NJ100_Tsuru_vector<-data.frame(ID="pred1_NJ100_Tsuru",Region="North Japan Sea",
                                      Region="NJS",
                                      Type="Tsuruarame",
                                      Equation="Linear",
                                      pred1_NJ100_Tsuru_vector)
pred1_NJ100_Tsuru_vector

#Model2
pred2_NJ100_Tsuru<- predictNLS(model2_NJ,newdata=dummy_100,interval = c("confidence"))
pred2_NJ100_Tsuru
rm(pred2_NJ100_Tsuru_vector)
pred2_NJ100_Tsuru_vector<-pred2_NJ100_Tsuru[[1]]
pred2_NJ100_Tsuru_vector<-data.frame(ID="pred2_NJ100_Tsuru",Region="North Japan Sea",
                                     Region="NJS",
                                     Type="Tsuruarame",
                                     Equation="Power",
                                     pred2_NJ100_Tsuru_vector)
pred2_NJ100_Tsuru_vector
# save width 600 * Height 500 
#Model 3
pred3_NJ100_Tsuru<- predictNLS(model3_NJ,newdata=dummy_100,interval = c("confidence"))
pred3_NJ100_Tsuru
rm(pred3_NJ100_Tsuru_vector)
pred3_NJ100_Tsuru_vector<-pred3_NJ100_Tsuru[[1]]
pred3_NJ100_Tsuru_vector<-data.frame(ID="pred3_NJ100_Tsuru",Region="North Japan Sea",
                                     Region="NJS",
                                     Type="Tsuruarame",
                                     Equation="Exponent",
                                     pred3_NJ100_Tsuru_vector)
pred3_NJ100_Tsuru_vector
Tsuruarame_NJ100<-rbind(pred1_NJ100_Tsuru_vector,
                        pred2_NJ100_Tsuru_vector,
                        pred3_NJ100_Tsuru_vector)
Tsuruarame_NJ100              
#width 600 * Height 500 save


####### SetoIslandSea Kajime/Kurome  ##########
kajime_SetoIslandSea<-subset(kajime,kajime$Region=="SetoIslandSea")


####Linear model#####
model1_ST<- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = kajime_SetoIslandSea)
summary(model1_ST)
AIC(model1_ST)
BIC(model1_ST)

####Power model#####
model2_ST<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = kajime_SetoIslandSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_ST)
AIC(model2_ST)
BIC(model2_ST)

####Exponential model#####
model3_ST2<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = kajime_SetoIslandSea,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_ST2)
AIC(model3_ST2)
BIC(model3_ST2)


######Biomass with 100% cover ##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_ST100_Kajime<- predictNLS(model1_ST,newdata=dummy_100,interval = c("confidence"))
pred1_ST100_Kajime
pred1_ST100_Kajime_vector<-pred1_ST100_Kajime[[1]]
pred1_ST100_Kajime_vector<-data.frame(ID="pred1_ST100_Kajime",Region="Seto Island Sea",
                                     Region="SIS",
                                     Type="Kajime/Kurome",
                                     Equation="Linear",
                                     pred1_ST100_Kajime_vector)
pred1_ST100_Kajime_vector

#Model2
pred2_ST100_Kajime<- predictNLS(model2_ST,newdata=dummy_100,interval = c("confidence"))
pred2_ST100_Kajime
rm(pred2_ST100_Kajime_vector)
pred2_ST100_Kajime_vector<-pred2_ST100_Kajime[[1]]
pred2_ST100_Kajime_vector<-data.frame(ID="pred2_ST100_Kajime",Region="Seto Island Sea",
                                      Region="SIS",
                                      Type="Kajime/Kurome",
                                      Equation="Power",
                                      pred2_ST100_Kajime_vector)
pred2_ST100_Kajime_vector

#Model3
pred3_ST100_Kajime<- predictNLS(model3_ST2,newdata=dummy_100,interval = c("confidence"))
pred3_ST100_Kajime
rm(pred3_ST100_Kajime_vector)
pred3_ST100_Kajime_vector<-pred3_ST100_Kajime[[1]]

pred3_ST100_Kajime_vector<-data.frame(ID="pred3_ST100_Kajime",Region="Seto Island Sea",
                                      Region="SIS",
                                      Type="Kajime/Kurome",
                                      Equation="Exponent",
                                      pred3_ST100_Kajime_vector)
pred3_ST100_Kajime_vector


Kajime_ST100<-rbind(pred1_ST100_Kajime_vector,
                        pred2_ST100_Kajime_vector,
                        pred3_ST100_Kajime_vector)
Kajime_ST100


####### Southern Pacific @ Kajime ##########
### Not used and Just for calculation due to data shortage for recent years###
kajime_SouthernPacific<-subset(kajime,kajime$Region=="SouthernPacific")
kajime_SouthernPacific$Season <- 
  factor(kajime_SouthernPacific$Season, levels = c("Flourish","Decline") )

plot(DW_g_m2~Cover, data = kajime_SouthernPacific)

####Linear model#####
model1_sp<- nls(DW_g_m2~ a*Cover,
                start = list(a=50),data = kajime_SouthernPacific)
summary(model1_sp)
AIC(model1_sp)
BIC(model1_sp)

####Power model#####
model2_sp<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),data = kajime_SouthernPacific)
summary(model2_sp)
AIC(model2_sp)
BIC(model2_sp)

####Exponential model#####
model3_sp<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=50,b = 0.01), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = kajime_SouthernPacific,
                trace=TRUE)
summary(model3_sp)
AIC(model3_sp)
BIC(model3_sp)


####### Southern Pacific Shikoku Kajime/Kurome##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_SP100_Kajime<- predictNLS(model1_sp,newdata=dummy_100,interval = c("confidence"))
pred1_SP100_Kajime
pred1_SP100_Kajime_vector<-pred1_SP100_Kajime[[1]]
pred1_SP100_Kajime_vector<-data.frame(ID="pred1_SP100_Kajime",Region="Southern Pacific",
                                      Region="SP",
                                      Type="Kajime/Kurome",
                                      Equation="Linear",
                                      pred1_SP100_Kajime_vector)
pred1_SP100_Kajime_vector

#Model2
pred2_SP100_Kajime<- predictNLS(model2_sp,newdata=dummy_100,interval = c("confidence"))
pred2_SP100_Kajime
pred2_SP100_Kajime_vector<-pred2_SP100_Kajime[[1]]
pred2_SP100_Kajime_vector<-data.frame(ID="pred2_SP100_Kajime",Region="Southern Pacific",
                                      Region="SP",
                                      Type="Kajime/Kurome",
                                      Equation="Power",
                                      pred2_SP100_Kajime_vector)
pred2_SP100_Kajime_vector

# save width 600 * Height 500 
#Model 3
pred3_SP100_Kajime<- predictNLS(model3_sp,newdata=dummy_100,interval = c("confidence"))
pred3_SP100_Kajime
pred3_SP100_Kajime_vector<-pred3_SP100_Kajime[[1]]
pred3_SP100_Kajime_vector<-data.frame(ID="pred3_SP100_Kajime",Region="Southern Pacific",
                                      Region="SP",
                                      Type="Kajime/Kurome",
                                      Equation="Exponent",
                                      pred3_SP100_Kajime_vector)
pred3_SP100_Kajime_vector
#width 600 * Height 500 save

Kajime_SP100<-rbind(pred1_SP100_Kajime_vector,
                    pred2_SP100_Kajime_vector,
                    pred3_SP100_Kajime_vector)

####### EastChinaSea Tsuruarame  ##########
kajime_EastChinaSea<-subset(kajime,kajime$Region=="EastChinaSea"&kajime$Jap_name=="Tsuruarame")
kajime_EastChinaSea
####Linear model#####
model1_EC <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = kajime_EastChinaSea)
summary(model1_EC)
AIC(model1_EC)
BIC(model1_EC)

####Power model#####
model2_EC<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = kajime_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_EC)
AIC(model2_EC)
BIC(model2_EC)

####Exponential model#####
model3_EC<- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=17.5,b =0.048), data = kajime_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_EC)
AIC(model3_EC)
BIC(model3_EC)


######Plot line  EastChinaSea TsuruArame ##########
#Model1
pred1_EC100_Kajime<- predictNLS(model1_EC,newdata=dummy_100,interval = c("confidence"))
pred1_EC100_Kajime
pred1_EC100_Kajime_vector<-pred1_EC100_Kajime[[1]]
pred1_EC100_Kajime_vector<-data.frame(ID="pred1_EC100_Kajime",Region="EastChinaSea",
                                      Region="ECS",
                                      Type="Tsuruarame",
                                      Equation="Linear",
                                      pred1_EC100_Kajime_vector)
pred1_EC100_Kajime_vector
#width 600 * Height 500 save



#Model2
pred2_EC100_Kajime<- predictNLS(model2_EC,newdata=dummy_100,interval = c("confidence"))
pred2_EC100_Kajime
pred2_EC100_Kajime_vector<-pred2_EC100_Kajime[[1]]
pred2_EC100_Kajime_vector<-data.frame(ID="pred2_EC100_Kajime",Region="EastChinaSea",
                                      Region="ECS",
                                      Type="Tsuruarame",
                                      Equation="Power",
                                      pred2_EC100_Kajime_vector)
pred2_EC100_Kajime_vector


#Model3
pred3_EC100_Kajime<- predictNLS(model3_EC,newdata=dummy_100,interval = c("confidence"))
pred3_EC100_Kajime
pred3_EC100_Kajime_vector<-pred3_EC100_Kajime[[1]]
pred3_EC100_Kajime_vector<-data.frame(ID="pred3_EC100_Kajime",Region="EastChinaSea",
                                      Region="ECS",
                                      Type="Tsuruarame",
                                      Equation="Exponent",
                                      pred3_EC100_Kajime_vector)
pred3_EC100_Kajime_vector


Tsuruarame_EC100<-rbind(pred1_EC100_Kajime_vector,
                    pred2_EC100_Kajime_vector,
                    pred3_EC100_Kajime_vector)
Tsuruarame_EC100
#width 600 * Height 500 save

####### EastChinaSea @ Antokume##########

kajime_EastChinaSeaAN<-subset(kajime,kajime$Region=="EastChinaSea"&kajime$Jap_name=="Antokume")


####Linear model#####
model1_ECAN <- nls(DW_g_m2~ a*Cover,
                 start = list(a=50),data = kajime_EastChinaSeaAN)
summary(model1_ECAN)
AIC(model1_ECAN)
BIC(model1_ECAN)

####Power model#####
model2_ECAN<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=0.001,b = 2),data = kajime_EastChinaSeaAN,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)
summary(model2_ECAN)
AIC(model2_ECAN)
BIC(model2_ECAN)

####Exponential model#####
model3_ECAN<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=1,b =0.048), data = kajime_EastChinaSeaAN,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_ECAN)
AIC(model3_ECAN)
BIC(model3_ECAN)


######Plot line  EastChinaSeaAN ##########
#Model1
library(propagate)
pred1_EC100_Antokume<- predictNLS(model1_ECAN,newdata=dummy_100,interval = c("confidence"))
pred1_EC100_Antokume
pred1_EC100_Antokume_vector<-pred1_EC100_Antokume[[1]]
pred1_EC100_Antokume_vector<-data.frame(ID="pred1_EC100_Antokume",Region="EastChinaSea",
                                      Region="ECS",
                                      Type="Antokume",
                                      Equation="Linear",
                                      pred1_EC100_Antokume_vector)
pred1_EC100_Antokume_vector


#Model2
pred2_EC100_Antokume<- predictNLS(model2_ECAN,newdata=dummy_100,interval = c("confidence"))
pred2_EC100_Antokume
rm(pred2_EC100_Kajime_vector)
pred2_EC100_Antokume_vector<-pred2_EC100_Antokume[[1]]
pred2_EC100_Antokume_vector<-data.frame(ID="pred2_EC100_Antokume",Region="EastChinaSea",
                                        Region="ECS",
                                        Type="Antokume",
                                        Equation="Power",
                                        pred2_EC100_Antokume_vector)
pred2_EC100_Antokume_vector


#Model3
pred3_EC100_Antokume<- predictNLS(model3_ECAN,newdata=dummy_100,interval = c("confidence"))
pred3_EC100_Antokume

pred3_EC100_Antokume_vector<-pred3_EC100_Antokume[[1]]
pred3_EC100_Antokume_vector<-data.frame(ID="pred3_EC100_Antokume",Region="EastChinaSea",
                                        Region="ECS",
                                        Type="Antokume",
                                        Equation="Exponent",
                                        pred3_EC100_Antokume_vector)
pred3_EC100_Antokume_vector


Antokume_EC100<-rbind(pred1_EC100_Antokume_vector,
                    pred2_EC100_Antokume_vector,
                    pred3_EC100_Antokume_vector)

Antokume_EC100

######Plot line  all species  ##########
rm(Kajime100)

Kajime100<-rbind(Kajime_cp100,Tsuruarame_NJ100,Kajime_ST100,
                  Tsuruarame_EC100,Antokume_EC100)
Kajime100
Kajime100$Equation <- factor(Kajime100$Equation, 
                            levels = c("Linear","Power","Exponent") )
library(dplyr)

Kajime100 <- Kajime100 %>%
  mutate(Kajime100,RegionType = interaction(Region.1, Type))
Kajime100$RegionType
Kajime100$RegionType <- factor(Kajime100$RegionType, 
                              levels = c("NJS.Tsuruarame","CP.Kajime","SIS.Kajime/Kurome",
                                         "ECS.Tsuruarame","ECS.Antokume") )
Kajime100



library(ggplot2)
plot_all_Kajime100<-ggplot(data=Kajime100, 
                           aes(x=RegionType, y=Prop.Mean.1,color= Equation,shape=Equation)) + 
  xlab("Region Type")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_y_continuous(breaks=seq(-500, 4000,length=10),limits=c(-750,4000))+ 
  geom_point(stat = "identity", position = position_dodge(width = 0.9),size=5) +
  geom_errorbar(aes(ymin = Prop.Mean.1 - Prop.sd.1, ymax = Prop.Mean.1 + Prop.sd.1),
                width = 0.5, position = position_dodge(width = 0.9))+
  theme(axis.title =element_text(size=20),axis.text = element_text(color="black",size=20))+
  theme(legend.title=element_text(size=20),legend.text =  element_text(size = 20))
plot_all_Kajime100
#save as 1400 * 400
