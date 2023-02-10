install.packages("magick")
install.packages("purrr")

library(ggplot2)
library(gganimate)
library(gapminder)
library(magick)
library(purrr)



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
  #repeate nation name same number of times as data collected to keep all/
  #vectors even
  nationIn<-append(nationIn, rep.int(Nat, 31))
  #Same idea for regions
  reg=combo2[n,1]
  regionIn<-append(regionIn, rep.int(reg, 31))
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
}
yearIn<-rep.int(1990:2020, 195)

Nation=nationIn
Year=yearIn
HDI=hdiIn
TFR=tfrIn
Pop=popIn
Region=regionIn

animationData<-data.frame(Nation, Region, Year, HDI, TFR, Pop)
attach(animationData)

#The Good One 
p <- ggplot(
  animationData, 
  aes(x = HDI, y=TFR, size=Pop, color=Region, shape=Region)
) +
  geom_point(show.legend = T, alpha=2) +
  #scale_color_viridis_d() +
  scale_size(range = c(1, 12)) +
  scale_x_log10() +
  labs(x = "HDI", y = "TFR")
p

p + transition_time(Year) +
  labs(title = "Year: {frame_time}")+
  shadow_wake(wake_length = 0.1, alpha = F)

