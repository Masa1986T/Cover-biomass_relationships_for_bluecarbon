rm(list=ls())
library(nlme)
library(dplyr)
library(ggplot2)
library(propagate)

SubtropHond<-read.csv("Surgassum_subtropical.csv",stringsAsFactors = TRUE)
SubtropHond$Season <- factor(SubtropHond$Season, levels = c("Flourish","Decline") )
names(SubtropHond)



####### SouthPacific  ##########
SubtropHond_SouthPacific<-subset(SubtropHond,SubtropHond$Region=="SouthPacific")


####線形モデル#####
model1_SP <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = SubtropHond_SouthPacific)
summary(model1_SP)
AIC(model1_SP)
BIC(model1_SP)

####累乗モデル#####
model2_SP <- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = SubtropHond_SouthPacific,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_SP)
AIC(model2_SP)
BIC(model2_SP)

####指数モデル#####
model3_SP <- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=17.5,b =0.048), data = SubtropHond_SouthPacific,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_SP)
AIC(model3_SP)
BIC(model3_SP)


######Plot line  SouthPacific ##########
#Model1
library(ggplot2)
library(propagate)

dummy_100<- expand.grid(Cover=100)
pred1_SP100_SubtropHond<- predictNLS(model1_SP,newdata=dummy_100,interval = c("confidence"))
pred1_SP100_SubtropHond
rm(pred1_SP100_SubtropHond_vector)
pred1_SP100_SubtropHond_vector<-pred1_SP100_SubtropHond[[1]]
pred1_SP100_SubtropHond_vector<-data.frame(ID="pred1_SP100_SubtropHond",Region="Southern Pacific",
                                         Region="SP",
                                         Type="Hondawara",
                                         Equation="Linear",
                                         pred1_SP100_SubtropHond_vector)
pred1_SP100_SubtropHond_vector

#Model2
pred2_SP100_SubtropHond<- predictNLS(model2_SP,newdata=dummy_100,interval = c("confidence"))
pred2_SP100_SubtropHond
pred2_SP100_SubtropHond_vector<-pred2_SP100_SubtropHond[[1]]
pred2_SP100_SubtropHond_vector<-data.frame(ID="pred2_SP100_SubtropHond",Region="Southern Pacific",
                                           Region="SP",
                                           Type="Hondawara",
                                           Equation="Power",
                                           pred2_SP100_SubtropHond_vector)
pred2_SP100_SubtropHond_vector

#Model3
pred3_SP100_SubtropHond<- predictNLS(model3_SP,newdata=dummy_100,interval = c("confidence"))
pred3_SP100_SubtropHond
rm(pred3_SP100_SubtropHond_vector)
pred3_SP100_SubtropHond_vector<-pred3_SP100_SubtropHond[[1]]
pred3_SP100_SubtropHond_vector<-data.frame(ID="pred3_SP100_SubtropHond",Region="Southern Pacific",
                                           Region="SP",
                                           Type="Hondawara",
                                           Equation="Exponent",
                                           pred3_SP100_SubtropHond_vector)
pred3_SP100_SubtropHond_vector

SubtropHond_SP100<-rbind(pred1_SP100_SubtropHond_vector,
                         pred2_SP100_SubtropHond_vector,
                       pred3_SP100_SubtropHond_vector)
SubtropHond_SP100

#width 600 * Height 500 save

####### EastChinaSea  ##########
SubtropHond_EastChinaSea<-subset(SubtropHond,SubtropHond$Region=="EastChinaSea")


####線形モデル#####
model1_EC <- nls(DW_g_m2~ a*Cover,
              start = list(a=50),data = SubtropHond_EastChinaSea)
summary(model1_EC)
AIC(model1_EC)
BIC(model1_EC)

####累乗モデル#####
model2_EC<- nls(DW_g_m2~ a*Cover^b,
              start = list(a=1.444,b = 1.3181),data = SubtropHond_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)
summary(model2_EC)
AIC(model2_EC)
BIC(model2_EC)

####指数モデル#####
model3_EC<- nls(DW_g_m2~ a*exp(b*Cover)-a,
              start = list(a=17.5,b =0.048), data = SubtropHond_EastChinaSea,
              control = list(maxiter = 50000, warnOnly = TRUE),
              trace=TRUE)

summary(model3_EC)
AIC(model3_EC)
BIC(model3_EC)


######Plot line  EastChinaSea ##########
dummy_100<- expand.grid(Cover=100)
pred1_ECS100_SubtropHond<- predictNLS(model1_EC,newdata=dummy_100,interval = c("confidence"))
pred1_ECS100_SubtropHond
rm(pred1_ECS100_SubtropHond_vector)
pred1_ECS100_SubtropHond_vector<-pred1_ECS100_SubtropHond[[1]]
pred1_ECS100_SubtropHond_vector<-data.frame(ID="pred1_ECS100_SubtropHond",Region="East China Sea",
                                           Region="ECS",
                                           Type="Hondawara",
                                           Equation="Linear",
                                           pred1_ECS100_SubtropHond_vector)
pred1_ECS100_SubtropHond_vector

#Model2
pred2_ECS100_SubtropHond<- predictNLS(model2_EC,newdata=dummy_100,interval = c("confidence"))
pred2_ECS100_SubtropHond
pred2_ECS100_SubtropHond_vector<-pred2_ECS100_SubtropHond[[1]]
pred2_ECS100_SubtropHond_vector<-data.frame(ID="pred2_ECS100_SubtropHond",Region="East China Sea",
                                           Region="ECS",
                                           Type="Hondawara",
                                           Equation="Power",
                                           pred2_ECS100_SubtropHond_vector)
pred2_ECS100_SubtropHond_vector

#Model3
pred3_ECS100_SubtropHond<- predictNLS(model3_EC,newdata=dummy_100,interval = c("confidence"))
pred3_ECS100_SubtropHond
rm(pred3_ECS100_SubtropHond_vector)
pred3_ECS100_SubtropHond_vector<-pred3_ECS100_SubtropHond[[1]]
pred3_ECS100_SubtropHond_vector<-data.frame(ID="pred3_ECS100_SubtropHond",Region="East China Sea",
                                           Region="ECS",
                                           Type="Hondawara",
                                           Equation="Exponent",
                                           pred3_ECS100_SubtropHond_vector)
pred3_ECS100_SubtropHond_vector

SubtropHond_EC100<-rbind(pred1_ECS100_SubtropHond_vector,
                         pred2_ECS100_SubtropHond_vector,
                         pred3_ECS100_SubtropHond_vector)
SubtropHond_EC100

#width 600 * Height 500 save

####### Ryukyu  ##########
SubtropHond_Ryukyu<-subset(SubtropHond,SubtropHond$Region=="Ryukyu")


####線形モデル#####
model1_RK <- nls(DW_g_m2~ a*Cover,
                 start = list(a=50),data = SubtropHond_Ryukyu)
summary(model1_RK)
AIC(model1_RK)
BIC(model1_RK)

####累乗モデル#####
model2_RK<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=1.444,b = 1.3181),data = SubtropHond_Ryukyu,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)
summary(model2_RK)
AIC(model2_RK)
BIC(model2_RK)

####指数モデル#####
model3_RK<- nls(DW_g_m2~ a*exp(b*Cover)-a,
                start = list(a=17.5,b =0.048), data = SubtropHond_Ryukyu,
                control = list(maxiter = 50000, warnOnly = TRUE),
                trace=TRUE)

summary(model3_EC)
AIC(model3_RK)
BIC(model3_RK)



######Plot line  Ryukyu ##########
#Model1
dummy_100<- expand.grid(Cover=100)
pred1_RK100_SubtropHond<- predictNLS(model1_RK,newdata=dummy_100,interval = c("confidence"))
pred1_RK100_SubtropHond
rm(pred1_RK100_SubtropHond_vector)
pred1_RK100_SubtropHond_vector<-pred1_RK100_SubtropHond[[1]]
pred1_RK100_SubtropHond_vector<-data.frame(ID="pred1_RK100_SubtropHond",Region="Ryukyu",
                                            Region="RK",
                                            Type="Hondawara",
                                            Equation="Linear",
                                            pred1_RK100_SubtropHond_vector)
pred1_RK100_SubtropHond_vector

#Model2
pred2_RK100_SubtropHond<- predictNLS(model2_RK,newdata=dummy_100,interval = c("confidence"))
pred2_RK100_SubtropHond
pred2_RK100_SubtropHond_vector<-pred2_RK100_SubtropHond[[1]]
pred2_RK100_SubtropHond_vector<-data.frame(ID="pred2_RK100_SubtropHond",Region="Ryukyu",
                                            Region="RK",
                                            Type="Hondawara",
                                            Equation="Power",
                                            pred2_RK100_SubtropHond_vector)
pred2_RK100_SubtropHond_vector

#Model3
pred3_RK100_SubtropHond<- predictNLS(model3_RK,newdata=dummy_100,interval = c("confidence"))
pred3_RK100_SubtropHond
rm(pred3_RK100_SubtropHond_vector)
pred3_RK100_SubtropHond_vector<-pred3_RK100_SubtropHond[[1]]
pred3_RK100_SubtropHond_vector<-data.frame(ID="pred3_RK100_SubtropHond",Region="Ryukyu",
                                            Region="RK",
                                            Type="Hondawara",
                                            Equation="Exponent",
                                            pred3_RK100_SubtropHond_vector)
pred3_RK100_SubtropHond_vector

SubtropHond_RK100<-rbind(pred1_RK100_SubtropHond_vector,
                         pred2_RK100_SubtropHond_vector,
                         pred3_RK100_SubtropHond_vector)
SubtropHond_RK100


#width 600 * Height 500 save


######Plot line  all species  ##########
SubtropHond100<-rbind(SubtropHond_SP100,SubtropHond_EC100,SubtropHond_RK100)
SubtropHond100
SubtropHond100$Equation <- factor(SubtropHond100$Equation, 
                                levels = c("Linear","Power","Exponent") )
library(dplyr)

SubtropHond100 <- SubtropHond100 %>%
  mutate(SubtropHond100,RegionType = interaction(Region.1, Type))
SubtropHond100$RegionType
SubtropHond100$RegionType <- factor(SubtropHond100$RegionType, 
                                  levels = c("SP.Hondawara","ECS.Hondawara","RK.Hondawara") )
SubtropHond100


library(ggplot2)
plot_all_SubtropHond100<-ggplot(data=SubtropHond100, 
                                aes(x=RegionType, y=Prop.Mean.1,color= Equation,shape= Equation)) + 
  xlab("Region Type")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_y_continuous(breaks=seq(0, 1500,length=7),limits=c(0,1500))+ 
  geom_point(stat = "identity", position = position_dodge(width = 0.9),size=5) +
  geom_errorbar(aes(ymin = Prop.Mean.1 - Prop.sd.1, ymax = Prop.Mean.1 + Prop.sd.1),
                width = 0.5, position = position_dodge(width = 0.9))+
  theme(axis.title =element_text(size=20),axis.text = element_text(color="black",size=20))+
  theme(legend.title=element_text(size=20),legend.text =  element_text(size = 20))
plot_all_SubtropHond100
#save as 900 * 400