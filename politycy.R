library(corrgram)
library(dplyr) 
library(ggplot2)
library(gplots)
library(fitdistrplus)

options(max.print=800000)

global_path <- "E:/Analizy i sprawozdania/Politycy" #�cie�ka do folderu z danymi
setwd(global_path) #ustawianie �cie�ki dost�pu do folderu z projektem
my_palette <- colorRampPalette(c( "seagreen4","deeppink4"))(n = 299)
my_palette2 <- colorRampPalette(c( "seagreen4","white","deeppink4"))(n = 600)

dane = read.csv("politycy.csv", header = TRUE, encoding = "UTF-8",sep = ";",stringsAsFactors = F) #wczytywanie danych

for(i in 3:17 )
{
  dane[,i] <- as.numeric(gsub(",", ".", dane[,i]))
}

jm_corrgram <- function(data, font.labels = 1, cex = 1, mar = 8)
{
  cors <- cor(data, method = "pearson") 
  corsSp <- cor(data, method = "spearman")
  for(i in 1:nrow(cors))
  {
    for(j in i:ncol(cors))
    {
      cors[i,j] <- corsSp[i,j]
    }
  }
  labs = colnames(data)
  corrgram(cors, type = "corr",lower.panel=panel.pie, upper.panel=panel.pie, cex= cex, 
           outer.labels = list(bottom=list(labels=labs,cex=font.labels,srt=60),
                               left=list(labels=labs,cex=font.labels,srt=30)),
           oma=c(mar,mar, 2, 2), col.regions = colorRampPalette(c("deeppink4" ,"white","springgreen4")))
  
  return(cors)
}

dane <- as.data.frame(dane)
dane[is.na(dane)] <- 0 
data.cor <- dane %>% dplyr::select(g�osy, wiek, pln, obce, nieruchomo�ci, akcje, kredyty, p�e�, 
                                   wykszta�cenie, PZPR, pozycja_lista, emerytura)
data.cor$aktywa <- data.cor$pln + data.cor$obce + data.cor$akcje
data.cor$maj�tek <- data.cor$aktywa + data.cor$nieruchomo�ci
jm_corrgram(data.cor)

dane$aktywa <- data.cor$aktywa
dane$maj�tek <- data.cor$maj�tek

library(PerformanceAnalytics)
chart.Correlation(dane[,c(3,4,5,6,8,9,11,13,15,16)], bg=as.factor(dane$partia), pch=21)


library(GGally)
library(ggplot2)
png("pairs.png", height = 1400, width = 1400)
ggpairs(dane[,c(2,3,4,5,6,9,11,13,15,18,19)], 
        mapping=ggplot2::aes(colour = as.factor(dane$partia)), alpha=0.7)
dev.off()

sum(dane$ruchomo�ci>0)

kobiety <- c(1:7)
pis <- dane[dane$partia == "PiS",]
po <- dane[dane$partia == "PO",]
sld <- dane[dane$partia == "SLD",]
psl <- dane[dane$partia == "PSL",]
partie <- c("PiS","PO","SLD","PSL","Wiosna", "KO", "all")
wiosna  <- dane[dane$partia == "Wiosna",]
ko <- dane[dane$partia == "PO" | dane$partia == "SLD" | dane$partia == "PSL",]

wilcox.fem <- matrix(0,4,3)
for(i in 1:5)
{
  kobiety[i] <- sum(dane$p�e�>1 & dane$partia==partie[i])/sum(dane$partia==partie[i])
}
kobiety[6] <- sum(ko$p�e� >1)/length(ko)
kobiety[7] <- sum(dane$p�e�>1)/51
xx <- barplot(kobiety, col = "springgreen4", names.arg = partie, ylim = c(0,0.48))
text(x = xx, y = kobiety, label = round(kobiety,2), pos = 3, cex = 0.8, col = "deeppink4")

for( i in c(1,2))
{
  wilcox.fem[i,1] <- wilcox.test(x = dane[dane$partia == partie[i] & dane$p�e� == 1 & dane$Nazwisko!="Sikorski Radek" & dane$Nazwisko!="Karski Adam" ,"maj�tek"], 
            y = dane[dane$partia == partie[i] & dane$p�e� == 2 & dane$Nazwisko != "Wi�niewska Jad�ka","maj�tek"], paired = FALSE, 
            alternative = "less")$p.value
  wilcox.fem[i,2] <- wilcox.test(x = dane[dane$partia == partie[i] & dane$p�e� == 1 & dane$Nazwisko!="Sikorski Radek" & dane$Nazwisko!="Karski Adam" ,"maj�tek"], 
                              y = dane[dane$partia == partie[i] & dane$p�e� == 2 & dane$Nazwisko != "Wi�niewska Jad�ka","maj�tek"], 
                              paired = FALSE, alternative = "two.sided")$p.value
  wilcox.fem[i,3] <- wilcox.test(x = dane[dane$partia == partie[i] & dane$p�e� == 1 & dane$Nazwisko!="Sikorski Radek" & dane$Nazwisko!="Karski Adam" ,
                                          "maj�tek"], 
                              y = dane[dane$partia == partie[i] & dane$p�e� == 2 & dane$Nazwisko != "Wi�niewska Jad�ka","maj�tek"], 
                              paired = FALSE, alternative = "greater")$p.value
}
wilcox.fem[3,1] <- wilcox.test(x = ko[ ko$p�e� == 1 & ko$Nazwisko!="Sikorski Radek" & ko$Nazwisko!="Karski Adam" ,"maj�tek"], 
                               y = ko[ ko$p�e� == 2,"maj�tek"], paired = FALSE, 
                               alternative = "less")$p.value
wilcox.fem[3,2] <- wilcox.test(x = ko[ ko$p�e� == 1 & ko$Nazwisko!="Sikorski Radek" & ko$Nazwisko!="Karski Adam" ,"maj�tek"], 
                               y = ko[ ko$p�e� == 2,"maj�tek"], paired = FALSE, 
                               alternative = "two.sided")$p.value
wilcox.fem[3,3] <- wilcox.test(x = ko[ ko$p�e� == 1 & ko$Nazwisko!="Sikorski Radek" & ko$Nazwisko!="Karski Adam" ,"maj�tek"], 
                               y = ko[ ko$p�e� == 2,"maj�tek"], paired = FALSE, 
                               alternative = "greater")$p.value
wilcox.fem[4,1] <- wilcox.test(x = dane[ dane$p�e� == 1 & dane$Nazwisko!="Sikorski Radek" & dane$Nazwisko!="Karski Adam" ,"maj�tek"], 
                               y = dane[ dane$p�e� == 2 & dane$Nazwisko != "Wi�niewska Jad�ka","maj�tek"], paired = FALSE, 
                               alternative = "less")$p.value
wilcox.fem[4,2] <- wilcox.test(x = dane[ dane$p�e� == 1 & dane$Nazwisko!="Sikorski Radek" & dane$Nazwisko!="Karski Adam" ,"maj�tek"], 
                               y = dane[ dane$p�e� == 2 & dane$Nazwisko != "Wi�niewska Jad�ka","maj�tek"], 
                               paired = FALSE, alternative = "two.sided")$p.value
wilcox.fem[4,3] <- wilcox.test(x = dane[ dane$p�e� == 1 & dane$Nazwisko!="Sikorski Radek" & dane$Nazwisko!="Karski Adam" ,"maj�tek"], 
                               y = dane[dane$p�e� == 2 & dane$Nazwisko != "Wi�niewska Jad�ka","maj�tek"], 
                               paired = FALSE, alternative = "greater")$p.value


heatmap.2(wilcox.fem,dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none', labCol = c("less","two-sided","greater"),
          col = my_palette, notecex = 2.0, notecol = "burlywood2", cellnote =round(wilcox.fem,2),
          labRow = c("PiS","PO","KO","all"))

wilcox.test(rnorm(1000,245), rnorm(1000,102),paired = FALSE, alternative = "less")
wilcox.test(rnorm(1000,245), rnorm(1000,102),paired = FALSE, alternative = "two.sided")
wilcox.test(rnorm(1000,245), rnorm(1000,102),paired = FALSE, alternative = "greater")



par(mfrow=c(3,2))
hist(pis[pis$p�e�==1,"maj�tek"], breaks = 8, col = "seagreen4", main="PiS - m�czy�ni", xlab = "maj�tek", 
     xlim = c(0,max(dane$maj�tek)))
hist(pis[pis$p�e�==2,"maj�tek"], breaks = 8, col = "deeppink4", main="PiS - kobiety", xlab = "maj�tek",
     xlim = c(0,max(dane$maj�tek)))
hist(ko[ko$p�e�==1,"maj�tek"], breaks = 8, col = "seagreen4", main="KO - m�czy�ni", xlab = "maj�tek",
     xlim = c(0,max(dane$maj�tek)))
hist(ko[ko$p�e�==2,"maj�tek"], breaks = 8, col = "deeppink4", main="KO - kobiety", xlab = "maj�tek",
     xlim = c(0,max(dane$maj�tek)))
hist(dane[dane$p�e�==1,"maj�tek"], breaks = 8, col = "seagreen4", main="M�czy�ni", xlab = "maj�tek",
     xlim = c(0,max(dane$maj�tek)))
hist(dane[dane$p�e�==2,"maj�tek"], breaks = 8, col = "deeppink4", main="Kobiety", xlab = "maj�tek",
     xlim = c(0,max(dane$maj�tek)))
par(mfrow=c(1,1))
dane[dane$maj�tek ==max(dane$maj�tek),]
dane[dane$maj�tek ==min(dane$maj�tek),]
dane[order(dane$maj�tek),]

summary(pis[pis$p�e�==1,"maj�tek"])
summary(pis[pis$p�e�==2,"maj�tek"])
summary(ko[ko$p�e�==1,"maj�tek"])
summary(ko[ko$p�e�==2,"maj�tek"])
bp1 <-boxplot(pis[pis$p�e�==1,"maj�tek"], pis[pis$p�e�==2,"maj�tek"], ko[ko$p�e�==1,"maj�tek"], ko[ko$p�e�==2,"maj�tek"], 
        notch = TRUE, col = c("seagreen4", "deeppink4"), horizontal = TRUE, names = c("PiS M", "PiS K", "KO M", "KO K"),
        ylim = c(0,12000000))
text(                                              # the x locations 
     bp1$out,
     c(1,2,3),
     dane[which(dane$maj�tek %in%  bp1$out, arr.ind=TRUE)[c(1,3,2)],"Nazwisko"],  # the labels
     pos = 1) 

dane[which(dane$maj�tek %in% bp1$out),"Nazwisko"]


boxplot(dane$maj�tek)


dane$patriotyzm <- dane$pln - dane$obce
hist(dane$patriotyzm)
boxplot(dane$patriotyzm)

bp2 <- boxplot(dane$patriotyzm, dane[dane$partia== "PiS","patriotyzm"], dane[dane$partia== "PO","patriotyzm"],
        horizontal = TRUE, names = c("all","PiS", "PO"), notch = TRUE, col = c("gray","blue","orange"))

kolejno�� <- 1:length(bp2$out)
for(i in 1:length(kolejno��))
{
  kolejno��[i] <- which(dane$patriotyzm ==  bp2$out[i])
}
#kolejno�� <- dane[,"Nazwisko"]
text(                                              # the x locations 
  bp2$out,
  bp2$group,
  dane[kolejno��,"Nazwisko"],  # the labels
  pos = 1, srt = 38, cex = 0.9) 
abline(v=0, col = "red",lty = 2)
#todo
niezdrajcy <- c(1:5)
patrioci <- c(1:5)
for(i in 1:5)
{
  niezdrajcy[i] <- sum(dane$patriotyzm > 0 & dane$partia == partie[i])/sum(dane$partia == partie[i])
  patrioci[i] <- sum(dane$obce == 0 & dane$partia == partie[i])/sum(dane$partia == partie[i])
  
}
barplot(rbind(patrioci,niezdrajcy-patrioci), col = c( "seagreen4","bisque3"), las = 2, names = partie[1:5])
#czy PiS g�osowa� ch�tniej na patriot�w?
library(dplyr) 
library(fitdistrplus) #do dopasowywania rozk�ad�w
library(ggcorrplot)
library(car)
library(devtools)
install_github("vqv/ggbiplot")
library(gclus)
library(MASS)
scatterplot(dane[dane$partia == "PiS" & dane$p�e� == 1,"patriotyzm"],dane[dane$partia == "PiS" & dane$p�e� == 1,"g�osy"])
scatterplot(dane[dane$partia == "PiS" & dane$p�e� == 1,"obce"],dane[dane$partia == "PiS" & dane$p�e� == 1,"g�osy"])
scatterplot(dane[dane$partia == "PiS" & dane$p�e� == 2,"patriotyzm"],dane[dane$partia == "PiS" & dane$p�e� == 2,"g�osy"])
scatterplot(dane[dane$partia == "PiS" & dane$p�e� == 2,"obce"],dane[dane$partia == "PiS" & dane$p�e� == 2,"g�osy"])
scatterplot(dane[dane$partia == "PiS" ,"patriotyzm"],dane[dane$partia == "PiS" ,"g�osy"])
scatterplot(dane[dane$partia == "PiS" ,"patriotyzm"],dane[dane$partia == "PiS" ,"g�osy"])
scatterplot(dane[dane$partia %in% c("PO","SLD","PSL")  ,"patriotyzm"],dane[dane$partia %in% c("PO","SLD","PSL") ,"g�osy"])
scatterplot(dane[dane$partia %in% c("PO","SLD","PSL")  ,"obce"],dane[dane$partia %in% c("PO","SLD","PSL") ,"g�osy"])


wilcox.test(dane[dane$partia == "PSL" & dane$obce == 0 & dane$p�e� == 1,"g�osy"], dane[dane$partia == "PSL" & dane$obce >0 ,"g�osy"],
            paired = FALSE, alternative = "less")

install.packages('BBmisc')
library(dendextend)
library(BBmisc)

dfdendro <- dane %>% dplyr::select( g�osy, PZPR, pln, obce, nieruchomo�ci, akcje, wykszta�cenie, kredyty, pozycja_lista, wiek, p�e�)
rownames(dfdendro) <- paste(dane$partia, dane$Nazwisko)
rownames(dfdendro) <- gsub("PO", "POb" ,rownames(dfdendro))
dfdendro[, c("g�osy",  "pln", "obce", "nieruchomo�ci", "akcje", "kredyty",  "wiek")] <- normalize(dfdendro[, c("g�osy",  "pln", "obce", "nieruchomo�ci", "akcje", "kredyty",  "wiek")])

dists <- dist(dfdendro)
hc <- hclust(dists)
colorCodes <- c(PiS="blue", PSL="green", SLD="red", Wio="purple", POb = "orange")
labelCol <- function(x) {
  if (is.leaf(x)) {
    ## fetch label
    label <- attr(x, "label")
    code <- substr(label, 1, 3)
    ## use the following line to reset the label to one letter code
    # attr(x, "label") <- code
    attr(x, "nodePar") <- list(lab.col=colorCodes[code])
  }
  return(x)
}
d <- dendrapply(as.dendrogram(hc), labelCol)


par(mar = c(13, 2, 2, 2))
plot(d)
library('dendextend')
jm_normalize <-function(x)
{
  m <- mean(dane$maj�tek)
  sdx <- sd(dane$maj�tek)
  return((x-m)/sdx)
}

colbranches <- function(n, col)
{
  a <- attributes(n) # Find the attributes of current node
  # Color edges with requested color
  attr(n, "edgePar") <- c(a$edgePar, list(col=col, lwd=2))
  n # Don't forget to return the node!
}

dfdendro2 <- dane %>% dplyr::select( g�osy, PZPR, pln, obce, nieruchomo�ci, akcje, wykszta�cenie, kredyty, wiek)
rownames(dfdendro2) <- paste(dane$partia, dane$Nazwisko)
rownames(dfdendro2) <- gsub("PO", "POb" ,rownames(dfdendro2))
dfdendro2[, c("g�osy",    "wiek")] <- normalize(dfdendro2[, c("g�osy",  "wiek")])
dfdendro2[, c( "pln", "obce", "nieruchomo�ci", "akcje", "kredyty")] <- jm_normalize(dfdendro2[, c( "pln", "obce", "nieruchomo�ci", "akcje", "kredyty")])
dists2 <- dist(dfdendro2)
hc2 <- hclust(dists2)


d2 <- dendrapply(as.dendrogram(hc2), labelCol)
#d2[[1]] = dendrapply(d2[[1]], colbranches, "seagreen4")
#d2[[2]] = dendrapply(d2[[2]], colbranches, "deappink4")
d2<-color_branches(d2,k=4, col = c("seagreen4","deeppink4", "orange", "blue"))
plot(d2)

dfdendro2[c(8,46,49),]
dane[c(8,46,49),]

d2[[1]]
d2[[2]][[1]]
d2$members
d3 <- as.dendrogram(hc2)
d3$x.member
dendextend:::cutree.dendrogram(d3,h=4.6)
dane$clust <- dendextend:::cutree.dendrogram(d3,h=4.6)
dfdendro2 

dane[17,"emerytura"] <- 7000
emeryci <- dane[(dane$wiek>=65 & dane$p�e� ==1) | (dane$wiek>=60 & dane$p�e� ==2), ]

em2<-emeryci[order(emeryci$emerytura, decreasing = TRUE),c("Nazwisko","emerytura")]
rownames(em2) <-em2[,1]
par(mar = c(10, 5, 2, 2))
barplot(em2[em2$emerytura>0,2], col = "springgreen4", names.arg = em2[em2$emerytura>0,1], las = 2)
em2[em2$emerytura==0,]


library(ParetoPosStable)
pareto.fit(dane$maj�tek)
par(mfrow=c(2,2))
hist(log(dane$maj�tek), col = "springgreen4")
hist(dane$maj�tek^0.2675, col = "springgreen4")
qqnorm(log(dane$maj�tek), col = "springgreen4")
qqline(log(dane$maj�tek), col = "red")
qqnorm(dane$maj�tek^0.2675, col = "springgreen4")
qqline(dane$maj�tek^0.2675, col = "red")
par(mfrow=c(1,1))
params <- as.list(fitdistr(log(dane$maj�tek), "exponential")$estimate)
qplot(sample = log(dane$maj�tek), geom = 'blank') +
  stat_qq(distribution = qexp, dparams = params)
