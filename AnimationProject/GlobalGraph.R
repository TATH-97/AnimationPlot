#pkg install
install.packages("xlsx")

#library loading
library(ggplot2)
library(ggthemes)
library(gganimate)
library(gapminder)
library(magick)
library(purrr)
library(xlsx)

#working directory
setwd("C:/WorkingDirectoryCapstone/HDI v TFR")

#global dataset
HDR=read.csv("HDRcomplete.csv")
FertilityRate=read.csv("FertilityRate.csv")
Population=read.csv("totalpopulationtrim.csv")
combo2=merge(combo,Population,by="iso3")
write.csv(combo2,file= "combo2.csv")


combo2=read.csv(file.choose(), header=T)
attach(combo2)

#Create new vectors for the new data frame for animation
hdiIn<-c()
tfrIn<-c()
nationIn<-c()
yearIn<-c()
popIn<-c()
regionIn<-c()

#Loop through the rows
for(n in 1:nrow(combo2)) {
  #grab nation name
  Nat=combo2[n,2]
  #grab region code --- replace "a" with column that region code is in [unhashtag below when ready to run]
  #Reg=combo2[n,a]
  #repeate nation + region name same number of times as data collected to keep all/
  #vectors even
  nationIn<-append(nationIn, rep.int(Nat, 31))
  #repeate nation + region name same number of times as data collected to keep all/
  #vectors even [unhashtag below line when ready to run]
  #RegIn<-Append(regIn, rep.int(Reg, 196))
  #Loop through tfr vals, add them to the tfrIn vecotor
  for(i in 3:33) {
    tfrIn<-append(tfrIn, combo2[n, i])
  }
  #ditto for hdi vals
  for(i in 34:64) {
    hdiIn<-append(hdiIn, combo2[n, i])
  }
  #ditto for pop vals
  for(i in 65:95) {
    popIn<-append(popIn, combo2[n, i])
  }
  #ditto for regions
  reg=combo2[n,1]
  regionIn=append(regionIn, rep.int(reg, 30))
}

yearIn<-rep.int(1990:2020, 196)

Nation=nationIn
Region=RegIn
Year=yearIn
HDI=hdiIn
TFR=tfrIn
Pop=popIn

animationData<-data.frame(Nation, Reg, Year, HDI, TFR, Pop)

