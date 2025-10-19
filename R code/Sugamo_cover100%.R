####################################################################
### Estimated biomass of Sugamo (Phyllospadix iwatensis) at 100% coverage ######
####################################################################

rm(list=ls())
library(ggplot2)
library(propagate)

Sugamo<-read.csv("Sugamo.csv",stringsAsFactors = TRUE)
Sugamo$Season <- factor(Sugamo$Season, 
                       levels = c("Flourish","Decline") )
names(Sugamo)

####### Hokkaido @ Sugamo##########
Sugamo_Hokkaido<-subset(Sugamo,Sugamo$Region=="Hokkaido")
Sugamo_Hokkaido$DW_g_m2

#### Linear model #####
model1_HD<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Sugamo_Hokkaido)
summary(model1_HD)
AIC(model1_HD)
BIC(model1_HD)

####Powe model #####
model2_HD<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Sugamo_Hokkaido)
summary(model2_HD)
AIC(model2_HD)
BIC(model2_HD)

####Exponential model #####
model3_HD<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Sugamo_Hokkaido,
                trace=TRUE)
summary(model3_HD)
AIC(model3_HD)
BIC(model3_HD)



####### Hokkaido##########
library(ggplot2)
library(propagate)

#Model1


dummy_100<- expand.grid(Cover=100)
pred1_HD100_Sugamo<- predictNLS(model1_HD,newdata=dummy_100,interval = c("confidence"))
pred1_HD100_Sugamo
pred1_HD100_Sugamo_vector<-pred1_HD100_Sugamo[[1]]
pred1_HD100_Sugamo_vector<-data.frame(ID="pred1_HD100_Sugamo",Region="Hokkaido",
                                      Region="HD",
                                      Type="Sugamo",
                                      Equation="Linear",
                                      pred1_HD100_Sugamo_vector)
pred1_HD100_Sugamo_vector

#Model2
dummy_100<- expand.grid(Cover=100)
pred2_HD100_Sugamo<- predictNLS(model2_HD,newdata=dummy_100,interval = c("confidence"))
pred2_HD100_Sugamo
pred2_HD100_Sugamo_vector<-pred2_HD100_Sugamo[[1]]
pred2_HD100_Sugamo_vector<-data.frame(ID="pred2_HD100_Sugamo",Region="Hokkaido",
                                      Region="HD",
                                      Type="Sugamo",
                                      Equation="Power",
                                      pred2_HD100_Sugamo_vector)
pred2_HD100_Sugamo_vector

# save width 600 * Height 500 
#Model 3
dummy_100<- expand.grid(Cover=100)
pred3_HD100_Sugamo<- predictNLS(model3_HD,newdata=dummy_100,interval = c("confidence"))
pred3_HD100_Sugamo
pred3_HD100_Sugamo_vector<-pred3_HD100_Sugamo[[1]]
pred3_HD100_Sugamo_vector<-data.frame(ID="pred3_HD100_Sugamo",Region="Hokkaido",
                                      Region="HD",
                                      Type="Sugamo",
                                      Equation="Exponent",
                                      pred3_HD100_Sugamo_vector)
pred3_HD100_Sugamo_vector

Sugamo_HD100<-rbind(pred1_HD100_Sugamo_vector,
                    pred2_HD100_Sugamo_vector,
                    pred3_HD100_Sugamo_vector)
Sugamo_HD100
#width 600 * Height 500 save

####### Central Pacific @ Sugamo##########
Sugamo_CentralPacific<-subset(Sugamo,Sugamo$Region=="CentralPacific")
Sugamo_CentralPacific$DW_g_m2

###Linear model#####
model1_CP<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Sugamo_CentralPacific)
summary(model1_CP)
AIC(model1_CP)
BIC(model1_CP)

####Power model#####
model2_CP<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Sugamo_CentralPacific)
summary(model2_CP)
AIC(model2_CP)
BIC(model2_CP)

####Exponential model#####
model3_CP<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Sugamo_CentralPacific,
                trace=TRUE)
summary(model3_CP)
AIC(model3_CP)
BIC(model3_CP)


####### Central Pacific##########
library(ggplot2)
library(propagate)

#Model1
dummy_100<- expand.grid(Cover=100)
pred1_CP100_Sugamo<- predictNLS(model1_CP,newdata=dummy_100,interval = c("confidence"))
pred1_CP100_Sugamo
rm(pred1_CP100_Sugamo_vector)
pred1_CP100_Sugamo_vector<-pred1_CP100_Sugamo[[1]]
pred1_CP100_Sugamo_vector<-data.frame(ID="pred1_CP100_Sugamo",Region="Central Pacific",
                                      Region="CP",
                                      Type="Sugamo",
                                      Equation="Linear",
                                      pred1_CP100_Sugamo_vector)
pred1_CP100_Sugamo_vector

#Model2
pred2_CP100_Sugamo<- predictNLS(model2_CP,newdata=dummy_100,interval = c("confidence"))
pred2_CP100_Sugamo
pred2_CP100_Sugamo_vector<-pred2_CP100_Sugamo[[1]]
pred2_CP100_Sugamo_vector<-data.frame(ID="pred2_CP100_Sugamo",Region="Central Pacific",
                                      Region="CP",
                                      Type="Sugamo",
                                      Equation="Power",
                                      pred2_CP100_Sugamo_vector)
pred2_CP100_Sugamo_vector

# save width 600 * Height 500 
#Model 3
pred3_CP100_Sugamo<- predictNLS(model3_CP,newdata=dummy_100,interval = c("confidence"))
pred3_CP100_Sugamo
pred3_CP100_Sugamo_vector<-pred3_CP100_Sugamo[[1]]
pred3_CP100_Sugamo_vector<-data.frame(ID="pred3_CP100_Sugamo",Region="Central Pacific",
                                      Region="CP",
                                      Type="Sugamo",
                                      Equation="Exponent",
                                      pred3_CP100_Sugamo_vector)
pred3_CP100_Sugamo_vector

Sugamo_CP100<-rbind(pred1_CP100_Sugamo_vector,
                    pred2_CP100_Sugamo_vector,
                    pred3_CP100_Sugamo_vector)
Sugamo_CP100


#width 600 * Height 500 save

######Plot line  all  ##########
Sugamo100<-rbind(Sugamo_HD100,Sugamo_CP100)
Sugamo100
Sugamo100$Equation <- factor(Sugamo100$Equation, 
                             levels = c("Linear","Power","Exponent") )
library(dplyr)

Sugamo100 <- Sugamo100 %>%
  mutate(Sugamo100,RegionType = interaction(Region.1, Type))
Sugamo100$RegionType
Sugamo100$RegionType <- factor(Sugamo100$RegionType, 
                               levels = c("HD.Sugamo","CP.Sugamo") )
Sugamo100



library(ggplot2)
plot_all_Sugamo100<-ggplot(data=Sugamo100, 
                           aes(x=RegionType, y=Prop.Mean.1,color= Equation,shape=Equation)) + 
  xlab("Region Type")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1600))+ 
  geom_point(stat = "identity", position = position_dodge(width = 0.9),size=3) +
  geom_errorbar(aes(ymin = Prop.Mean.1 - Prop.sd.1, ymax = Prop.Mean.1 + Prop.sd.1),
                width = 0.5, position = position_dodge(width = 0.9))+
  theme(axis.title =element_text(size=20),axis.text = element_text(color="black",size=20))+
  theme(legend.title=element_text(size=20),legend.text =  element_text(size = 20))
plot_all_Sugamo100

#700* 400




