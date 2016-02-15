MDN = read.csv("MDN.csv")
## descriptive statistics##
summary(MDN)  
dim(MDN)
names(MDN)

#the main company and the branch
# note:The ID number that uniquely identifies umbrella of supplier working for same Supplier Company "Parent/Child - relationship"
#table(MDN$Company, MDN$Department)
length(table(MDN$Supplier.No))
length(table(MDN$Linkage.Id))

#table(table(MDN$Linkage.Id))
#table(MDN$Supplier.No,MDN$Linkage.Id)
#link each branches to the main company
linka = sort(unique(MDN$Linkage.Id))
linkano= list()
for(i in linka) linkano[[i]]= sort(unique(
  MDN$Supplier.No[MDN$Linkage.Id==i]))

##################################
#part 1:suppliers and departments#
#################################
                                      ###clustering###
#cluster the suppliers with the department to find the most frequency suppliers for each departments
table(MDN$Linkage.Id,MDN$Department) -tt
#quantile(tt)-quantile(x) using the same idea of the qq-plot
c1 = cor(tt)
#shrink the correlation 
c1[c1 < 0.013 ] = 0#multivariate media=0.013

dim(c1)
c2 = c1
dimnames(c2) = NULL
pairs(c2,pch=".",col=2)
round(c2,2)

###part of bayesian network-precision matrix###
library(glasso)
glasso(s=c2[c(1:11),c(1:11)], 0.05)# get the 0 in precision matrix

                              ###cluster using c3###
c3 = sqrt(1 -c2^2)#similarity, square(1-corr^2)=distance, produce the correlation distance to cluster
round(c3,2)
hc = hclust(as.dist(c3), method="ward")
The "ward" method has been renamed to "ward.D"; note new "ward.D2"
plot(hc)

ff = function(x,rango= c(2:12)) {
  library(cluster)
  
  sl = rango
  i0 = rango[1]-1
  for(i in rango) 
    sl[i-i0]=mean(silhouette(pam(x, k=i))[,3])
  names(sl) = rango
  uu = rango[which.max(sl)]
  attr(uu,"a")=sl
  uu
}
ff(tt)
attr(,"a")


## Adding date
dd=unlist(strsplit(as.character(MDN$PO.Create)," "))
dd = dd[2*(1:(length(dd)/2))-1]
dd = as.Date(dd,"%m/%d/%Y")

d0 =as.numeric(strsplit(as.character(min(dd)),"-")[[1]][1:2])
d1 = matrix(as.numeric(unlist(strsplit(as.character(dd),"-"))),ncol=3,byrow=T)
month = d1[,2] # month of the year
dmonth = (d1[,1]-d0[1])*12+d1[,2]-d0[2]+1 # Months since the begining of the data
                                          ###analysis data of each
 ttn = names(tt) [tt >20]
ttni = match (MDN$Requester.Supervisor.WWID,ttn)
i = MDN$Requester.Supervisor.WWID == ttn[1]
i[is.na(i)]=F
plot(dd[i],pch=".",ylab="time",xlab="purchase request",main="Each supervisor's purchase records") ##check each supervisor's purchase records

nm = names(MDN)

##analysis the type of the suppliers
y = MDN[,21] # "Credo.Spend.Ind" or women minority owned 
#0-n 1-y change logistic to number
  Y=(y=="N")*1
y = (MDN[,21] =="Y")*1
yy = (MDN[,20] =="S")*1 # "Business.Size.Code"  Small or  nor small
table(y,yy)/length(yy)
yy

categ = MDN[,14]
typ= MDN[,24]
tot = MDN[,16]
site= MDN[,3]
##plot each type by month
tt = table(y,yy,dmonth)
for(i in 1:36) tt[,,i] = tt[,,i]/sum(tt[,,i]) #  Over time
plot(tt[1,1,],type="l",ylim=range(tt),xlab="time(month)",ylab="proportion",main="the proportion of different suppliers over time")
lines(tt[1,2,],col=2); lines(tt[2,1,],col=3); lines(tt[2,2,],col=4)
legend("topright",col=c(1:4),lty=1,cex=.7,legend=c("large & No M/WBE","small &No M/WBE","large M/WBE","small M/WBE"))

par(mfrow=c(1,1))
plot(tt[2,1,]+tt[2,2,],ylim=c(0,.7))
plot(tt[2,2,]+tt[1,2,],ylim=c(0,.4))
plot(tt[2,2,],ylim=c(0,.2))
plot(tt[2,1,])
par(mfrow=c(2,3))
plot(tt[2,1,]+tt[2,2,],xlab="Index(month)", ylab="M/WBE suppliers")
plot(tt[1,1,],xlab="Index(month)", ylab="Not M/WBE & large suppliers")
title(main="Size & Type of suppliers")
plot(tt[2,1,],xlab="Index(month)", ylab=" M/WBE & large suppliers")
plot(tt[2,2,]+tt[1,2,],xlab="Index(month)", ylab="small suppliers")
plot(tt[1,2,],xlab="Index(month)", ylab="NotM/WBE & small suppliers")
plot(tt[2,2,],xlab="Index(month)", ylab="M/WBE & small suppliers")
#w/m suppliers and the type of purchase over time
  
tt = table(y,typ,dmonth)
for(i in 1:36) tt[,,i] = tt[,,i]/sum(tt[,,i]) #  Over time
par(mfrow=c(2,5),mar=c(1,1,3,3))
for(i in 1:2) for(j in 1:5) plot(tt[i,j,])

### BICLUSTERS ###
library(biclust)

tt =as.matrix(table(site,categ))
tt1 = array(unlist(sqrt(tt)),dim=dim(tt))
tt2 = array(unlist(tt),dim=dim(tt))
biclust(tt1,method=BCCC(), delta=1.5,  alpha=1, number=10)-bb
tt[bb@RowxNumber[,1], bb@NumberxCol[1,]]
#cluster 2,3,4…
tt[bb@RowxNumber[,2], bb@NumberxCol[2,]]
tt[bb@RowxNumber[,3], bb@NumberxCol[3,]]
tt[bb@RowxNumber[,4], bb@NumberxCol[4,]]
heatmapBC(x = tt1, bicResult = bb)
image(tt[bb@RowxNumber[,1], bb@NumberxCol[1,]],main="Partition of bicluster 1")
image(tt[bb@RowxNumber[,2], bb@NumberxCol[2,]],main="Partition of bicluster 2")
image(tt[bb@RowxNumber[,3], bb@NumberxCol[3,]],main="Partition of bicluster 3")
image(tt[bb@RowxNumber[,4], bb@NumberxCol[4,]],main="Partition of bicluster 4")
categ1 = substring( as.character(categ),1,5) 
datt = data.frame(y,yy,categ=categ1,typ,tot,site)
 
library(rpart)
for(j in 1:36) print(rpart( y~categ+typ+tot+ site, data=datt[dmonth==(j-1),]))
for(j in 1:36) print(rpart( yy~categ+typ+tot+ site, data=datt[dmonth==(j-1),]))
####glm##
table(categ)
c1 = as.character(datt$categ)
c2 = as.numeric(factor(datt$categ))
table(c2)[10:20]
c1[c2 <=3] = "Other"
c1[c2==6 | c2 ==5] = "Other"
c1[c2>=9 & c2 <=11] = "Other"
c1[c2>=15 & c2 <=20] = "Other"
table(c2)
for( i in 1:21) if (table(c2)[i]<500) c1[c2==i]="Other"
dattt = datt
names(dattt)
hist(log(1+datt$tot),200)
dattt$tot = cut(log(1+datt$tot),c(-Inf,4,7,9,14,Inf))
dattt$categ= factor(c1)
table(dattt$categ, dattt$tot)
summary( glm(y~yy+categ*typ+categ*site + tot,data=dattt[dmonth==26,],family=binomial))
summary( glm(y~.,data=dattt,family=binomial))
    ####bayesian network####
library(bnlearn)
datt = data.frame(y,yy,categ,typ,tot,site)
dattt=datt
dattt$categ<-as.numeric(factor(datt$categ))
dattt$y<-as.numeric(factor(datt$y))
dattt$y<-as.numeric(factor(datt$yy))
dattt$typ<-as.numeric(factor(datt$typ))
dattt$tot<-as.numeric(factor(datt$tot))
dattt$site<-as.numeric(factor(datt$site))
for(i in 1:36)plot(mmpc(dattt[dmonth==i,])) 
pdf("bayesn2.pdf",width=9,height=9) 
par(mfrow=c(6,6),mar=c(0,0,0,0),cex=0.5)
for(i in 1:36)
  plot(mmpc(dattt[dmonth==i,]))
dev.off ()






