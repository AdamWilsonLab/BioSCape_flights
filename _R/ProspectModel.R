#Prospect


#library(devtools)
#install_github("serbinsh/R-PROSPECT")
library(Rprospect)


#N:	leaf structure parameter. Number of elementary layers
#Cab: leaf chlorophyll a+b content in ug/cm2
#Car:  leaf carotenoid content ug/cm2
#Cbrown: brown pigments content in arbitrary units
#Cw:  leaf equivalent water thickness (EWT) in g/cm2 or cm-1
#Cm: leaf dry matter content in g/cm2 (alias leaf mass per area [LMA])

inv=prospect5B(N=1.78, Cab=45.3, Car=6.82, Cbrown=1.29, Cw=0.012, Cm=0.008)

plot(Reflectance~Wavelength,data=inv,type="l")


