#########Projet : Elections présidentielles de 2012
#######Sarah Chitarra et Rémi Labory

library(gdata)
library(MASS)
library(FactoMineR)
library(ROCR)
library(sp)
library(rgeos)
library(maptools)
library(ggplot2)
library(nnet)

set.seed(42)
setwd("C:/Users/Sarah/Documents/Cours_M2_S1/Recueil de données/Projet/Données/")

# Age moyen 
Ages<-read.csv("Ages.csv",header=T,sep=";")
Ages<-cbind(Ages[,1],Ages[,1],Ages[,3:(100+3)]+Ages[,(100+4):(100+4+100)])
Age_moyen<-cbind(Ages[,1],Ages[,2],as.matrix(Ages[,-c(1,2)])%*%0:100/rowSums(Ages[,-c(1,2)]))
colnames(Age_moyen)<-c("CODGEO","LIBGEO","AGE_MOYEN")

# Resultat Tour 1
Resultats1<-read.csv("selection_Tour1.csv",header=T,sep=";")
#nombre de 0 à rajouter
zeros<-3-(floor(log(Resultats1[,3],10))+1)


CodeVille<-as.numeric(gsub(" ","",unlist(apply(cbind(Resultats1,zeros),1,function(x){
  return(
    ifelse(x[length(x)]==2,
           paste(as.character(x[1]),"0","0",as.character(x[3]),sep=""),
           paste(as.character(x[1]),as.character(rep(0,x[length(x)])),as.character(x[3]),sep="")
    ))
}))))


CodeVille[1:20]<-75101:75120

which(sort(Age_moyen[,1])!=sort(CodeVille))
#Tous les numeros sont identiques, on a pas oublié de ville

# Resultats Tour 2
Resultats2<-read.csv("selection_Tour2.csv",row.names=NULL,header=T,sep=";")
#nombre de 0 à rajouter
zeros2<-3-(floor(log(Resultats2[,3],10))+1)


CodeVille2<-as.numeric(gsub(" ","",unlist(apply(cbind(Resultats2,zeros),1,function(x){
  return(
    ifelse(x[length(x)]==2,
           paste0(as.character(x[1]),"0","0",as.character(x[3]),sep=""),
           paste0(as.character(x[1]),as.character(rep(0,x[length(x)])),as.character(x[3]),sep="")
    ))
}))))

sum(CodeVille!=CodeVille2)
#toutes les villes sont identiques

#On va garder les variables qui nous interessent :

Resultats1<-data.frame(CodeVille=CodeVille,
                       Gauche=as.numeric(gsub(",",".",Resultats1[,21]))+as.numeric(gsub(",",".",Resultats1[,75])),
                       Droite=as.numeric(gsub(",",".",Resultats1[,69]))+as.numeric(gsub(",",".",Resultats1[,33])),
                       Centre=as.numeric(gsub(",",".",Resultats1[,63])),
                       ExtDroite=as.numeric(gsub(",",".",Resultats1[,27])),
                       ExtGauche=as.numeric(gsub(",",".",Resultats1[,39]))+as.numeric(gsub(",",".",Resultats1[,45]))+as.numeric(gsub(",",".",Resultats1[,51])),
                       Abst=as.numeric(gsub(",",".",Resultats1[,7]))
)
Resultats1$Resultat=apply(Resultats1[2:6],1,which.max)
Resultats1$Resultat=colnames(Resultats1[2:6])[Resultats1$Resultat]

table(Resultats1$Resultat)

Resultats2<-data.frame(CodeVille=CodeVille,
                       Hollande=as.numeric(gsub(",",".",Resultats2[,21])),
                       Sarkozy=as.numeric(gsub(",",".",Resultats2[,27])),
                       Abst=as.numeric(gsub(",",".",Resultats2[,7]))
)
Resultats2$Resultat=apply(Resultats2[2:3],1,which.max)
Resultats2$Resultat=colnames(Resultats2[2:3])[Resultats2$Resultat]

table(Resultats2$Resultat)





# Diplomés
#Recueil des données socio-démographiques
Diplomes<-read.csv("Diplomes.csv",header=T,sep=";",stringsAsFactors=F)
Diplomes<-data.frame(CODGEO=Diplomes$CODGEO,
                     DEP=Diplomes$DEP,
                     population=Diplomes$"P11_POP0205"+Diplomes$"P11_POP0610"+Diplomes$"P11_POP1114"+Diplomes$"P11_POP1517"+Diplomes$"P11_POP1824"+Diplomes$"P11_POP2529"+Diplomes$"P11_POP30P"
                     ,dip=Diplomes$"P11_NSCOL15P_BACP2"+Diplomes$"P11_NSCOL15P_SUP")
Diplomes$dip=Diplomes$dip/Diplomes$population

# Emploi
Emploi<-read.csv("Emploi.csv",header=T,sep=";",stringsAsFactors=F)
Emploi<-data.frame(CODGEO=Emploi$CODGEO,
                   DEP=Emploi$DEP,
                   Chom=Emploi$"P11_CHOM1564",
                   Etud=Emploi$"P11_ETUD1564",
                   Actifs=Emploi$"C11_ACT1564",
                   Agr=Emploi$"C11_ACT1564_CS1",
                   Art=Emploi$"C11_ACT1564_CS2",
                   Cadr=Emploi$"C11_ACT1564_CS3",
                   ProfInt=Emploi$"C11_ACT1564_CS4",
                   Empl=Emploi$"C11_ACT1564_CS5",
                   Ouvr=Emploi$"C11_ACT1564_CS6")
Emploi$Etud=Emploi$Etud/Diplomes$population
Emploi[,-c(1,2,4,5)]=Emploi[,-c(1,2,4,5)]/Emploi$Actifs

# Familles
Famille<-read.csv("Famille.csv",header=T,sep=";",stringsAsFactors=F)
Famille<-data.frame(CODGEO=Famille$CODGEO,
                    DEP=Famille$DEP,
                    Familles=Famille$"C11_FAM",
                    Mono=Famille$"C11_FAMMONO"/Famille$"C11_FAM",
                    Enfants=(Famille$"C11_NE24F1"+2*Famille$"C11_NE24F2"+3*Famille$"C11_NE24F3"+4*Famille$"C11_NE24F4P")/Famille$"C11_FAM")

# revenus median
Revenus<-read.csv("Revenu.csv",header=T,sep=";",stringsAsFactors=F)
Revenus<-data.frame(CODGEO=Revenus$CODGEO,
                    DEP=Revenus$DEP,
                    Mediane=Revenus$DEC5UC10)
Revenus$Mediane=as.numeric(gsub(",",".",Revenus$Mediane))

# Creation des données
data=merge(merge(merge(merge(Revenus,Emploi),Famille),Diplomes),Age_moyen)
selec=which(!is.na(data$Mediane)) 
data=data[selec,]
data=data[,-c(6,13,16,18)]
Resultats1=Resultats1[selec,]
Resultats2=Resultats2[selec,]
cor<-cor(cbind(data[,-c(1,2)],Resultats1[,2:7]))

## GRAPHIQUES ##
ggplot(data=datarep)+geom_point(aes(x=Mediane,y=ExtGauche,colour=Resultat))+scale_colour_manual(name="Resultats",values=c("ExtDroite"="darkblue","Droite"="blue1","Gauche"="red3"))+ylab("Chômage")
ggplot(data=datarep)+
  geom_point(aes(x=Mediane,y=ExtGauche,col="Extreme Gauche"),size=1,alpha=0.4)+
  stat_smooth(method=lm,aes(x=Mediane,y=ExtGauche),col="darkred",alpha=0.1)+
  geom_point(aes(x=Mediane,y=Centre,col="Centre"),size=1,alpha=0.4)+
  stat_smooth(method=lm,aes(x=Mediane,y=Centre),col="orange",alpha=0.4)+
  geom_point(aes(x=Mediane,y=Gauche,col="Gauche"),size=1,alpha=0.4)+
  stat_smooth(method=lm,aes(x=Mediane,y=Gauche),col="red",alpha=0.4)+
  geom_point(aes(x=Mediane,y=Droite,col="Droite"),size=1,alpha=0.4)+
  stat_smooth(method=lm,aes(x=Mediane,y=Droite),col="blue",alpha=0.4)+
  geom_point(aes(x=Mediane,y=ExtDroite,col="Extreme Droite"),size=1,alpha=0.4)+
  stat_smooth(method=lm,aes(x=Mediane,y=ExtDroite),col="black",alpha=0.4)+
scale_color_manual(name="Courants politiques",values=c("Extreme Gauche"="darkred","Centre"="orange","Gauche"="red","Droite"="blue","Extreme Droite"="black"))+
  guides(colour=guide_legend(override.aes=list(size=2)))+ylab("Score au premier tour (en %)")+ggtitle("Score par commune des différents courants en fonction de la médiane des revenus")+ 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
  
  
#ACP
acp<-PCA(cbind(data[,-c(1,2)],Resultats1[,2:8]),quali.sup=20) # Quali.sup = Resultats
plot(acp$eig[,1])
acp$eig[4,3]
#est-ce que 4 axes suffisent?
# Var
which(rowSums(acp$var$cos2[,1:4])<0.5)
rowSums(acp$var$cos2[,1:4])[which(rowSums(acp$var$cos2[,1:4])<0.5)]
# Ind
sum(rowSums(acp$ind$cos2[,1:4])<0.4)
Var<-cbind(round(acp$var$cos2[,1:4],2),round(acp$var$contrib[,1:4],2),round(acp$var$cor[,1:4],2))
Var
1/19
plot(acp,shadowtext=T,label="quali",habillage=20,col.hab=c("blue","black","red"))
plot(acp,col.ind=(2*(Resultats1[,8]=="Gauche")+4*(Resultats1[,8]=="Droite")+1*(Resultats1[,8]=="ExtDroite")))
legend(x=-14,y=-2,legend=c("Gauche","Droite","ExtDroite"),col=c("red","blue","black"),pch=16,cex=1,bty="n")
plot(acp,invisible=c("ind","var")) # Barycentre des classes


# Echantillon test
n<-length(data[,1])
selec.modele<-sample(1:n,80/100*n,replace=F) # 80% des individus 
colnames(data)

variables<-3:15


### CARTES ###
mytheme= theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), panel.background=element_blank(), panel.border=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), plot.background=element_blank())

communes=readShapeSpatial("./communes-plus-20140630-100m.shp",
                          proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

## Tour 1 ##
RepResultats1<-Resultats1

communesIDF=subset(communes,communes@data$code_reg==11)
communesIDF@data$insee[which(!(communesIDF@data$insee %in% RepResultats1$CodeVille))]

gg = fortify(communesIDF,region="insee")
ggdata=merge(gg,RepResultats1,by.x="id",by.y="CodeVille")
pe=order(ggdata$group,ggdata$order)
ggdata=ggdata[pe,]

ggplot(ggdata,aes(x=long,y=lat,group=group,fill=Gauche))+geom_polygon()+coord_map()+scale_fill_gradient(low="white", high="red")+mytheme+labs(title="Score Tour 1 pour la Gauche")
ggplot(ggdata,aes(x=long,y=lat,group=group,fill=Droite))+geom_polygon()+coord_map()+scale_fill_gradient(low="white", high="blue")+mytheme+labs(title="Score Tour 1 pour la Droite")
ggplot(ggdata,aes(x=long,y=lat,group=group,fill=ExtDroite))+geom_polygon()+coord_map()+scale_fill_gradient(low="white", high="black")+mytheme+labs(title="Score Tour 1 pour l'Extrême Droite")
ggplot(ggdata,aes(x=long,y=lat,group=group,fill=Centre))+geom_polygon()+coord_map()+scale_fill_gradient(low="white", high="orange")+mytheme+labs(title="Score Tour 1 pour le Centre")
ggplot(ggdata,aes(x=long,y=lat,group=group,fill=ExtGauche))+geom_polygon()+coord_map()+scale_fill_gradient(low="white", high="darkred")+mytheme+labs(title="Score Tour 1 pour l'Extrême Gauche")
ggplot(ggdata,aes(x=long,y=lat,group=group,fill=Resultat))+geom_polygon()+coord_map()+scale_fill_manual(name="Resultats",values=c("ExtDroite"="darkblue","Droite"="blue","Gauche"="red"))+mytheme+labs(title="Résultats Tour 1")



## Tour 2 ##
RepResultats2<-Resultats2

gg2 = fortify(communesIDF,region="insee")
ggdata2=merge(gg2,RepResultats2,by.x="id",by.y="CodeVille")
pe2=order(ggdata2$group,ggdata2$order)
ggdata2=ggdata2[pe,]

ggplot(ggdata2,aes(x=long,y=lat,group=group,fill=Hollande))+geom_polygon()+coord_map()+scale_fill_gradient(low="white", high="red")+mytheme+labs(title="Score Tour 2 pour la Gauche")
ggplot(ggdata2,aes(x=long,y=lat,group=group,fill=Sarkozy))+geom_polygon()+coord_map()+scale_fill_gradient(low="white", high="blue")+mytheme+labs(title="Score Tour 2 pour la Droite")
ggplot(ggdata2,aes(x=long,y=lat,group=group,fill=Resultat))+geom_polygon()+coord_map()+scale_fill_manual(name="Resultats",values=c("Sarkozy"="blue1","Hollande"="red3"))+mytheme+labs(title="Résultats Tour 2")


###################
### RESULTATS 1 ###
###################

### LDA ###

# Echantillon test
# Modele analyse linéaire canonique tour 1
modele1<-lda(Resultats1[selec.modele,8]~.,data[selec.modele,variables])
modele1
pred1<-predict(modele1,data[-selec.modele,variables])
pred1
table(pred1$class,Resultats1[-selec.modele,8]) 
errlda1<-sum(pred1$class!=Resultats1[-selec.modele,8])/length(Resultats1[-selec.modele,1])
errlda1

# analyser les axes par rapport aux groupes 
groupes1<-predict(modele1,as.data.frame(modele1$means))
groupes1$x

#représentation : 
x11()
plot(modele1,col=(2*(Resultats1[selec.modele,8]=="Gauche")+4*(Resultats1[selec.modele,8]=="Droite")+1*(Resultats1[selec.modele,8]=="ExtDroite")))
legend(x=-5,y=-2,legend=c("Gauche","Droite","ExtDroite"),col=c("red","blue","black"),pch=16,cex=1,bty="n")
title("Représentation canonique")
#les barycentres
x11()
plot(groupes1$x,col=c("blue","black","red"),cex=1,pch=16,xlim=c(min(groupes1$x[,1])-0.5,max(groupes1$x[,1]+0.5)),ylim=c(min(groupes1$x[,2])-0.5,max(groupes1$x[,2]+0.5)))
text(groupes1$x,c("Droite","ExtDroite","Gauche"),col=c("blue","black","red"),cex=0.8,pos=2)
abline(h=0,col="grey")
abline(v=0,col="grey")
title("Représentation des barycentres")

# Modele total lda 1
modeletot1<-lda(Resultats1[,8]~.,data[,variables])
modeletot1
predtot1<-predict(modeletot1,data[,variables])
table(predtot1$class,Resultats1[,8]) 
errldatot1<-sum(predtot1$class!=Resultats1[,8])/length(Resultats1[,1])
errldatot1

# analyser les axes par rapport aux groupes 
groupes1tot<-predict(modeletot1,as.data.frame(modeletot1$means))
groupes1tot$x

#représentation :  
x11()
plot(modeletot1,col=(2*(Resultats1[,8]=="Gauche")+4*(Resultats1[,8]=="Droite")+1*(Resultats1[,8]=="ExtDroite")))
legend(x=-5,y=-2,legend=c("Gauche","Droite","ExtDroite"),col=c("red","blue","black"),pch=16,cex=1,bty="n")
title("Représentation canonique")
#les barycentres
x11()
plot(groupes1tot$x,col=c("blue","black","red"),cex=1,pch=16,xlim=c(min(groupes1tot$x[,1])-0.5,max(groupes1tot$x[,1]+0.5)),ylim=c(min(groupes1tot$x[,2])-0.5,max(groupes1tot$x[,2]+0.5)))
text(groupes1tot$x,c("Droite","ExtDroite","Gauche"),col=c("blue","black","red"),cex=0.8,pos=2)
abline(h=0,col="grey")
abline(v=0,col="grey")
title("Représentation des barycentres")


#Carte
x11()
resultatlda1<-cbind(data[,1:2],predtot1)
colnames(resultatlda1)[3]<-"Resultats"
communes=readShapeSpatial("./communes-plus-20140630-100m.shp",
                          proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
communesIDF=subset(communes,communes@data$code_reg==11)
gg = fortify(communesIDF,region="insee")
ggdata=merge(gg,resultatlda1,by.x="id",by.y="CODGEO")
pe=order(ggdata$group,ggdata$order)
ggdata=ggdata[pe,]
ggplot(ggdata,aes(x=long,y=lat,group=group,fill=Resultats))+geom_polygon()+coord_map()+scale_fill_manual(name="Resultats",values=c("Droite"="blue","Gauche"="red","ExtDroite"="Darkblue"))+mytheme+labs(title="Analyse linéaire canonique Résultats Tour 1")


### MULTINOMIALE ###

# Echantillon test
# Regression multinomiale pour le tour 1
modeleinitial1<-multinom(Resultats1[selec.modele,8] ~ 1, data = data[selec.modele,variables])
multinom1<- multinom(Resultats1[selec.modele,8] ~ ., data = data[selec.modele,variables],Hess=T)
varcov1<-solve(multinom1$Hessian)
# test de wald 
z1 <- summary(multinom1)$coefficients/matrix(sqrt(diag(varcov1)),nrow=2,byrow=T)
p1<-2*(1-pnorm(abs(z1),0,1))
p1
#ensuite selection step
stepmulti1<-step(multinom1)
summary(stepmulti1)
varcov1b<-solve(stepmulti1$Hessian)
# test de wald 
z1b <- summary(stepmulti1)$coefficients/matrix(sqrt(diag(varcov1b)),nrow=2,byrow=T)
p1b<-2*(1-pnorm(abs(z1bb),0,1))
p1b
GauExt1<-summary(stepmulti1)$coefficients[2,]-summary(stepmulti1)$coefficients[1,]
GauExt1
# pv valeurs 3eme equation
P<-length(coef(stepmulti1)[1,])
a<-as.vector(t(coef(stepmulti1)))
M<-cbind(diag(rep(1,P)),diag(rep(-1,P)))
stat<-c()
for( j in 1:P){
  stat<-c(stat,t(a)%*%as.matrix(M[j,])%*%(t(as.matrix(M[j,]))%*%varcov1b%*%as.matrix(M[j,]))^(-1)%*%t(as.matrix(M[j,]))%*%a)
}
ps<-1-pchisq(stat,1)
ps
#puis comparaison aic avec modele vide : 
anova(modeleinitial1,stepmulti1)
#Une fois qu'on est sur que step est meilleur : coef significatifs sur une equation au moins, AIC meilleur, on fait test sur 70% des données
predmulti1<-predict(multinom1,data[-selec.modele,variables])
table(predmulti1,Resultats1[-selec.modele,8])
err1<-sum(as.data.frame(predmulti1)!=Resultats1[-selec.modele,8])/length(Resultats1[-selec.modele,1])
err1


# modele Total
modeleinitial<-multinom(Resultats1[,8] ~ 1, data = data[,variables])
multinom<- multinom(Resultats1[,8] ~ ., data = data[,variables],Hess=T)
varcov<-solve(multinom$Hessian)
z <- summary(multinom)$coefficients^2/matrix(diag(varcov),nrow=2,byrow=T)
p<-1-pchisq(z,1)
p
#ensuite selection step
stepmulti<-step(multinom)
summary(stepmulti)
varcovb<-solve(stepmulti$Hessian)
zb<- summary(stepmulti)$coefficients^2/matrix(diag(varcovb),nrow=2,byrow=T)
# pvaleurs
pb<-1-pchisq(zb,1)
pb
GauExt<-summary(stepmulti)$coefficients[2,]-summary(stepmulti)$coefficients[1,]
GauExt
round(GauExt,2)
# pv valeurs 3eme equation
P<-length(coef(stepmulti)[1,])
a<-as.vector(t(coef(stepmulti)))
M<-cbind(diag(rep(1,P)),diag(rep(-1,P)))
stat<-c()
for( j in 1:P){
  stat<-c(stat,t(a)%*%as.matrix(M[j,])%*%(t(as.matrix(M[j,]))%*%varcovb%*%as.matrix(M[j,]))^(-1)%*%t(as.matrix(M[j,]))%*%a)
}
ps<-1-pchisq(stat,1)
ps
#puis comparaison aic avec modele vide : 
anova(modeleinitial,stepmulti)
predmulti<-predict(multinom,data[,variables])
table(predmulti,Resultats1[,8])
err2<-sum(as.data.frame(predmulti)!=Resultats1[,8])/length(Resultats1[,1])
err2


# Donne la carte des résultats prévus
resultatprev1<-cbind(data[,1:2],predmulti)
colnames(resultatprev1)[3]<-"Resultats"
gg = fortify(communesIDF,region="insee")
ggdata=merge(gg,resultatprev1,by.x="id",by.y="CODGEO")
pe=order(ggdata$group,ggdata$order)
ggdata=ggdata[pe,]
ggplot(ggdata,aes(x=long,y=lat,group=group,fill=Resultats))+geom_polygon()+coord_map()+scale_fill_manual(name="Resultats",values=c("Droite"="blue","Gauche"="red","ExtDroite"="Darkblue"))+mytheme+labs(title="Régression multinomiale Résultats Tour 1")


###################
### RESULTATS 2 ###
###################

### LDA ###

# Echantillon test
#modele analyse linéaire canonique tour 2
modele2<-lda(Resultats2[selec.modele,5]~.,data[selec.modele,variables])
modele2
pred2<-predict(modele2,data[-selec.modele,variables])
table(pred2$class,Resultats2[-selec.modele,5])
errlda2<-sum(pred2$class!=Resultats2[-selec.modele,5])/length(Resultats2[-selec.modele,1])
errlda2
# groupes
groupes2<-predict(modele2,as.data.frame(modele2$means))
groupes2$x

# modele total lda 2
modeletot2<-lda(Resultats2[,5]~.,data[,variables])
modeletot2
predtot2<-predict(modeletot2,data[,variables])
table(predtot2$class,Resultats2[,5])
errldatot2<-sum(predtot2$class!=Resultats2[,5])/length(Resultats2[,1])
errldatot2
# groupes
groupes2tot<-predict(modeletot2,as.data.frame(modeletot2$means))
groupes2tot$x

# Donne la carte des résultats prévus
resultatlda2<-cbind(data[,1:2],predtot2)
colnames(resultatlda2)[3]<-"Resultats"
gg2 = fortify(communesIDF,region="insee")
ggdata2=merge(gg2,resultatlda2,by.x="id",by.y="CODGEO")
pe2=order(ggdata2$group,ggdata2$order)
ggdata2=ggdata2[pe2,]
ggplot(ggdata2,aes(x=long,y=lat,group=group,fill=Resultats))+geom_polygon()+coord_map()+scale_fill_manual(name="Resultats",values=c("Sarkozy"="blue1","Hollande"="red3"))+mytheme+labs(title="Analyse linéaire canonique Résultats Tour 2")

### LOGIT ###

# Echantillon test
#modele logit tour 2
modele2b<-glm(Resultats2[selec.modele,5]=="Hollande"~.,family=binomial,data=data[selec.modele,variables])
summary(modele2b)
stepmodele2<-step(modele2b,direction="backward")
summary(stepmodele2)
plot(stepmodele2)
proba2<-predict(modele2b,data[-selec.modele,variables],type="response")
table(proba2>0.5,Resultats2[-selec.modele,5]) 
# Predict>0.5 c'est pour savoir s'il vote Hollande ou pas
Errlog1<-(sum(proba2>0.5&Resultats2[-selec.modele,5]=="Sarkozy")+sum(proba2<0.5&Resultats2[-selec.modele,5]=="Hollande"))/length(Resultats2[-selec.modele,5])
Errlog1

# modele logit total tour 2
modeletotal2<-glm(Resultats2[,5]=="Hollande"~.,family=binomial,data=data[,variables])
summary(modeletotal2)
stepmodeletot2<-step(modeletotal2,direction="backward")
summary(stepmodeletot2)
plot(stepmodeletot2)
probatot2<-predict(modeletotal2,data[,variables],type="response")
table(probatot2>0.5,Resultats2[,5])
Errlog2<-(sum(probatot2>0.5&Resultats2[,5]=="Sarkozy")+sum(probatot2<0.5&Resultats2[,5]=="Hollande"))/length(Resultats2[,5])
Errlog2


modeleNULL<-glm(Resultats2[,5]=="Hollande"~1,family=binomial,data=data[,variables])
summary(modeleNULL)
anova(stepmodeletot2,modeleNULL,test="Chisq")

# Donne la carte des résultats prévus
resultatprev2<-cbind(data[,1:2],ifelse(probatot2>0.5,"Hollande","Sarkozy"))
colnames(resultatprev2)[3]<-"Resultats"
gg2 = fortify(communesIDF,region="insee")
ggdata2=merge(gg2,resultatprev2,by.x="id",by.y="CODGEO")
pe2=order(ggdata2$group,ggdata2$order)
ggdata2=ggdata2[pe2,]
ggplot(ggdata2,aes(x=long,y=lat,group=group,fill=Resultats))+geom_polygon()+coord_map()+scale_fill_manual(name="Resultats",values=c("Sarkozy"="blue1","Hollande"="red3"))+mytheme+labs(title="Régression logistique Résultats Tour 2")
