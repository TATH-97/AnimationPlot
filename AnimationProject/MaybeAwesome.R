library(ggplot2)
library(gganimate)
library(gapminder)
library(magick)
library(purrr)

#UsedData1_CSV datasheet, will not work otherwise!
combo2=read.csv(file.choose(), header=T)
attach(combo2)

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

#Function that generates dataframes by any given col name
FrameMaker<-function(region) {
  hdi<-c()
  tfr<-c()
  nat<-c()
  yr<-c()
  pop<-c()
  nat=Nation[Region==region]
  tfr=TFR[Region==region]
  hdi=HDI[Region==region]
  yr=Year[Region==region]
  pop=Pop[Region==region]
  df=data.frame(nat, yr, hdi, tfr, pop)
  return(df)
}

#Generates animation by given region. Uses FrameMaker to generate dataframe
Ani<-function(region) {
  df=FrameMaker(region)
  attach(df)
  p <- ggplot(
    df, 
    aes(x = hdi, y=tfr, size=pop, color=nat)
  ) +
    geom_point(show.legend = F, alpha=.5) +
    scale_size(range = c(1, 12)) +
    scale_x_log10() +
    labs(x = "HDI", y = "TFR")
  p
  
  p + transition_time(yr) +
    labs(title = "{region} Year: {frame_time}")+
    shadow_wake(wake_length = 0.1, alpha = F)
}

LegendMaker<-function(region) {
  df=FrameMaker(region)
  attach(df)
  p <- ggplot(
    df, 
    aes(x = hdi, y=tfr, size=pop, color=nat)
  ) +
    geom_point(show.legend = T, alpha=.5) +
    scale_size(range = c(1, 12)) +
    scale_x_log10() +
    labs(x = "HDI", y = "TFR")
  p
}

table(Region)

Ani("EaPac")
LegendMaker("EaPac")
Ani("EuroEura")
LegendMaker("EuroEura")
Ani("NEast")
LegendMaker("NEast")
Ani("SCenAsia")
LegendMaker("SCenAsia")
Ani("SSAfrica")
LegendMaker("SSAfrica")
Ani("WEST")
LegendMaker("WEST")

Reg<-c("EaPac", "EuroEura", "NEast", "SCenAsia", "SSAfrica", "WEST")
corAns<-c()
for(i in Reg) {
  EA=FrameMaker(i)
  corr=cor(EA$hdi[EA$yr==1990], EA$tfr[EA$yr==1990], use = "na.or.complete", method = "pearson")
  corAns=append(corAns,corr)
  print(corr)
}
print(corAns)



