ncluster<-50
clust<-rep(1:ncluster,each=25)
x<-runif(length(clust))
clust<-clust[which(x<.95)]
ns<-table(clust)

mw<-rnorm(ncluster)
inter<-rnorm(ncluster,25,6)
b1<-rnorm(ncluster,5,5)
b2<-.5
b4<-2
dat<-NULL
for (i in 1:ncluster) {
  x<-rnorm(ns[i])
  w<-mw[i]
  y<-inter[i]+b1[i]*x+b2*w+b4*x*w+rnorm(ns[i],0,9)
  dat<-rbind(dat,cbind(y,x,w,i))
}

dat<-as.data.frame(dat)
head(dat)

library(lmerTest)
dat$cluster<-factor(dat$i)

dat$y<-round(100*(dat$y-min(dat$y))/(max(dat$y)-min(dat$y)))
dat$x<-round(10*(dat$x-min(dat$x))/(max(dat$x)-min(dat$x)))
dat$w<-round(10*(dat$w-min(dat$w))/(max(dat$w)-min(dat$w)))


dat$w<-dat$w-mean(dat$w)
dat$x<-dat$x-mean(dat$x)

model<-lmer(y~(1+x|cluster)+x*w,data=dat)
summary(model)

plot(predict(model)~dat$x)


names(dat)<-c("efficacy","read","teacheffic","schoolclass")
write.csv(dat,"efficacy_schools.csv")
