install.packages('WDI')
library(WDI)
WDIsearch('gdp')
WDIsearch('gdp')[1:10,]

ex <- NE.CON.TOTL.KD.ZG
ex
WDIsearch("NE.CON.TOTL.KD.ZG")
WDI(indicator="NE.CON.TOTL.KD.ZG")
data<- WDI(indicator=c('NY.GDP.PCAP.CD', 'SP.DYN.IMRT.IN'), country='all', start=1990, end=2020)
WDI(indicator=c('NY.GDP.PCAP.CD', 'SP.DYN.IMRT.IN'))
WDIsearch('gdp')
WDIsearch('health')
install.packages("writexl")
library("writexl")
df<- WDI(indicator=c('SH.STA.BRTC.ZS', 'SH.MED.CMHW.P3','SH.XPD.CHEX.PC.CD','SH.XPD.CHEX.GD.ZS','SH.XPD.GHED.GD.ZS','SH.XPD.GHED.GD.ZS','SH.XPD.OOPC.CH.ZS','SH.DYN.NCOM.FE.ZS','SH.STA.WASH.P5','SH.STA.ODFC.ZS','SH.H2O.BASW.ZS','SH.STA.BASS.ZS','SH.H2O.SMDW.ZS','SH.STA.SMSS.ZS','SH.STA.HYGN.ZS','SH.DTH.COMM.ZS','SH.MED.BEDS.ZS','SH.STA.AIRP.P5'), country='all', start=1990, end=2020)
df
write_xlsx(WORLD,"C:/Users/ASUS/Downloads/R Practical/WORLD.xlsx")
df1<- WDI(indicator=c('NY.GDP.PCAP.CD'), country='all', start=2015, end=2016)

WORLD<- WDI(indicator=c('NY.ADJ.NNTY.PC.KD','NY.GDP.PCAP.KD','SH.XPD.CHEX.PC.CD','SH.XPD.CHEX.GD.ZS','SH.DTH.COMM.ZS','SH.MED.BEDS.ZS','SH.IMM.IDPT','SH.IMM.HEPB','SP.POP.65UP.TO.ZS','SH.STA.HYGN.ZS','SH.STA.SMSS.ZS','SH.H2O.SMDW.ZS','SH.STA.BASS.ZS','SH.STA.ODFC.ZS','SP.DYN.IMRT.IN','SH.STA.WASH.P5','SH.DYN.NCOM.ZS','SH.MMR.RISK.ZS','SH.DTH.COMM.ZS','SP.POP.DPND'), country='all', start=1990, end=2020)

library(Metrics)
library(MASS)
library(tidyverse)
library(sf)

excel = read.csv("C:/Users/ASUS/Downloads/R Practical/case4.csv")
head(excel)
model1 = glm("NY.GDP.PCAP.KD ~ SP.POP.65UP.TO.ZS + SH.STA.SMSS.ZS + SH.STA.BASS.ZS + SH.STA.ODFC.ZS + SH.DYN.NCOM.ZS", data = excel)
summary(model1)
str(excel)
model2 = glm("SH.XPD.CHEX.PC.CD ~ SP.POP.65UP.TO.ZS + SH.STA.SMSS.ZS + SH.STA.BASS.ZS + SH.STA.ODFC.ZS + SH.DYN.NCOM.ZS", data = excel)
summary(model2)

model3 = glm("SP.POP.65UP.TO.ZS ~ NY.GDP.PCAP.KD + SH.XPD.CHEX.PC.CD + SH.XPD.CHEX.GD.ZS", data = excel)
summary(model3)
model4 = glm("SH.STA.SMSS.ZS ~ NY.GDP.PCAP.KD + SH.XPD.CHEX.PC.CD + SH.XPD.CHEX.GD.ZS", data = excel)
summary(model4)
model5 = glm("SH.STA.BASS.ZS ~ NY.GDP.PCAP.KD + SH.XPD.CHEX.PC.CD + SH.XPD.CHEX.GD.ZS", data = excel)
summary(model5)
model6 = glm("SH.STA.ODFC.ZS ~ NY.GDP.PCAP.KD + SH.XPD.CHEX.PC.CD + SH.XPD.CHEX.GD.ZS", data = excel)
summary(model6)
model7 = glm("SH.DYN.NCOM.ZS ~ NY.GDP.PCAP.KD + SH.XPD.CHEX.PC.CD + SH.XPD.CHEX.GD.ZS", data = excel)
summary(model7)



plot(model3)





ggplot(data=model1, aes(y=SP.POP.65UP.TO.ZS, x=NY.GDP.PCAP.KD)) +
  geom_point() + geom_abline(slope = 5, intercept = 0, col = 'blue')+
  ggtitle("Percentage of disadvantaged boys VS Avg attainment 8 score per boy")+
  ylab("Average attainment 8 score per boy")+
  xlab("Percentage of disavantaged pupils")
