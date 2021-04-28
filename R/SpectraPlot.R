library(tidyverse)

hytes=seq(7500,1200,len=256)
avirisng=seq(380,2510,by=5)

#https://directory.eoportal.org/web/eoportal/airborne-sensors/prism
prism=c(seq(349.9, 1053.5,by=2.83),1240, 1640) #

data=rbind.data.frame(
  data.frame(sensor="HYTes",l=hytes),
  data.frame(sensor="AVIRIS-ng",l=avirisng),
  data.frame(sensor="PRISM",l=prism)
)

ggplot(data,aes(x=l,y=sensor,col=l))+
  geom_point()
