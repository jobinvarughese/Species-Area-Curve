# Species-Area-Curve
Making species Area curve with species matrix

#####Spec ar curve for Shola####
data<-read.csv(file ="~/Desktop/iiser/8 sem/Vegetation paper/2017_only_nilgiri_anamalai_palani/shola regeneration munnar-valparai-berijam2 for SACurve.csv", head = T, row.names = 1)


data1<-data[,c(4:38)]

curveall <- specaccum(data1, "random", permutations = 1000)

plot(curveall , ci.type="bar", col="blue", lwd=.6, ci.length = .05, ci.col="lightblue", main = "Shola Spp Accum Curve in All plots" , xlab="Number of Sites", 
     ylab = "Number of Species")



#segregate data only for Eucalyptus 
commEUcalyptus<-data[data$Type1.1=="Eucalyptus",]
#remove columns conatining 'PLOTNO' and 'FORTYPE'
commEUcalyptus1<-commEUcalyptus[,c(4:38)]
commEUcalyptus1
curveeucal <- specaccum(commEUcalyptus1, "random", permutations = 1000)

plot(curveeucal, ci.type="bar", col="blue", lwd=.6, ci.length = .05, main = "Shola Spp Accum Curve in Eucalyptus" , xlab="Number of Sites", 
     ylab = "Number of Species")



commAcacia_Eucalyptus<-data[data$Type1.1=="Acacia-Eucalyptus",]
#remove columns conatining 'PLOTNO' and 'FORTYPE'
commAcacia_Eucalyptus1<-commAcacia_Eucalyptus[,c(4:38)]
commAcacia_Eucalyptus1
curveAca_Euc <- specaccum(commAcacia_Eucalyptus1, "random", permutations = 1000)

plot(curveAca-Euc, ci.type="bar", col="blue", lwd=.6, ci.length = .05, main = "Shola Spp Accum Curve in Acacia_Eucalyptus" , xlab="Number of Sites", 
     ylab = "Number of Species")

commAcacia<-data[data$Type1.1=="Acacia",]
#remove columns conatining 'PLOTNO' and 'FORTYPE'
commAcacia1<-commAcacia[,c(4:38)]
commAcacia1
curveAca <- specaccum(commAcacia1, "random", permutations = 1000)

plot(curveAca, ci.type="bar", col="blue", lwd=.6, ci.length = .05, main = "Shola Spp Accum Curve in Acacia" , xlab="Number of Sites", 
     ylab = "Number of Species")

commPine<-data[data$Type1.1=="Pine",]
#remove columns conatining 'PLOTNO' and 'FORTYPE'
commPine1<-commPine[,c(4:38)]
curvepin <- specaccum(commPine1, "random", permutations = 1000)


plot(curveall , ci.type="bar", col="#BEBADA", lwd=.6, ci.length = .05, ci.col="#BEBADA", main = "Shola Spp Accum Curve in All plots" , xlab="Number of Sites", 
     ylab = "Number of Species")

plot(curveeucal, ci.type="bar", col="#0073C2FF", lwd=.6, ci.length = .05, ci.col="#0073C2FF", add =TRUE)
plot(curveAca_Euc, ci.type="bar", col="#EFC000FF", lwd=.6, ci.col="#EFC000FF",ci.length = .05,add =TRUE)
plot(curveAca, ci.type="bar", col="#868686FF", lwd=.6, ci.col="#868686FF", ci.length = .05, add =TRUE)
plot(curvepin, ci.type="bar", col="#FB8072", lwd=.6, ci.col="#FB8072", ci.length = .05, add =TRUE)

#"#0073C2FF", "#EFC000FF", "#868686FF" "#8DD3C7" "#FFFFB3" "#BEBADA" "#FB8072“
my_colors<-c("#BEBADA",  "#0073C2FF", "#EFC000FF", "#868686FF", "#FB8072")

legend(105,10,c("All_plots", "Eucalyptus","Acacia-Eucalyptus", "Acacia", "Pine"), cex = 1, pch= 19,col = my_colors)





#####Spec ar curve for invasives####
data<-read.csv(file ="~/Desktop/iiser/8 sem/Vegetation paper/invasive_noshola_noothers_grtr_thnzerobasalar.csv", head = T, row.names = 1)
unique(data$Site)

data1<-data[,c(4:16,18:19, 21:28, 40)]

curveall <- specaccum(data1, "random", permutations = 1000)

plot(curveall , ci.type="bar", col="blue", lwd=.6, ci.length = .05, ci.col="lightblue", main = "Invasive Spp Accum Curve in All plots" , xlab="Number of Sites", 
     ylab = "Number of Species")



#segregate data only for Eucalyptus 
commEUcalyptus<-data[data$Type2=="Eucalyptus",]
#remove columns conatining 'PLOTNO' and 'FORTYPE'
commEUcalyptus1<-commEUcalyptus[,c(4:16,18:19, 21:28, 40)]
commEUcalyptus1
curveeucal <- specaccum(commEUcalyptus1, "random", permutations = 1000)

plot(curveeucal, ci.type="bar", col="blue", lwd=.6, ci.length = .05, main = "Invasive Spp Accum Curve in Eucalyptus" , xlab="Number of Sites", 
     ylab = "Number of Species")



commAcacia_Eucalyptus<-data[data$Type2=="Acacia-Eucalyptus",]
#remove columns conatining 'PLOTNO' and 'FORTYPE'
commAcacia_Eucalyptus1<-commAcacia_Eucalyptus[,c(4:16,18:19, 21:28, 40)]
commAcacia_Eucalyptus1
curveAca_Euc <- specaccum(commAcacia_Eucalyptus1, "random", permutations = 1000)

plot(curveAca_Euc, ci.type="bar", col="blue", lwd=.6, ci.length = .05, main = "Invasive Spp Accum Curve in Acacia_Eucalyptus" , xlab="Number of Sites", 
     ylab = "Number of Species")

commAcacia<-data[data$Type2=="Acacia",]
#remove columns conatining 'PLOTNO' and 'FORTYPE'
commAcacia1<-commAcacia[,c(4:16,18:19, 21:28, 40)]
commAcacia1
curveAca <- specaccum(commAcacia1, "random", permutations = 1000)

plot(curveAca, ci.type="bar", col="blue", lwd=.6, ci.length = .05, main = "Invasive Spp Accum Curve in Acacia" , xlab="Number of Sites", 
     ylab = "Number of Species")

commPine<-data[data$Type2=="Pinus",]
#remove columns conatining 'PLOTNO' and 'FORTYPE'
commPine1<-commPine[,c(4:16,18:19, 21:28, 40)]
curvepin <- specaccum(commPine1, "random", permutations = 1000)


plot(curveall , ci.type="bar", col="#BEBADA", lwd=.6, ci.length = .05, ci.col="#BEBADA", main = "Invasive Species Accumulation Curve" , xlab="Number of Sites", 
     ylab = "Number of Species")

plot(curveeucal, ci.type="bar", col="#0073C2FF", lwd=.6, ci.length = .05, ci.col="#0073C2FF", add =TRUE)
plot(curveAca_Euc, ci.type="bar", col="#EFC000FF", lwd=.6, ci.col="#EFC000FF",ci.length = .05,add =TRUE)
plot(curveAca, ci.type="bar", col="#868686FF", lwd=.6, ci.col="#868686FF", ci.length = .05, add =TRUE)
plot(curvepin, ci.type="bar", col="#FB8072", lwd=.6, ci.col="#FB8072", ci.length = .05, add =TRUE)

#"#0073C2FF", "#EFC000FF", "#868686FF" "#8DD3C7" "#FFFFB3" "#BEBADA" "#FB8072“
my_colors<-c("#BEBADA",  "#0073C2FF", "#EFC000FF", "#868686FF", "#FB8072")

legend(105,10,c("All_plots", expression(italic("Eucalyptus")), expression(italic("Acacia-Eucalyptus")), expression(italic("Acacia")), expression(italic("Pinus"))), cex = 1, pch= 19,col = my_colors)


