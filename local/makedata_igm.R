### crossed stimuli ###
nsubj<-80
nstimuli<-70
asub<-rnorm(nsubj,10,5) # subjects intercepts
fac<-c(-1,1)
#bx<-c(rnorm(nsubj/2,35,5),rnorm(nsubj/2,-35,5)) # linear effect for participant
#bx2<--sign(bx)*(abs(bx)/20)*abs(rnorm(nsubj,.5,1)) # quadratic effect for participant
bx<-list()
bx[[1]]<-rnorm(nsubj,-45,5) # linear effect for participant
bx[[2]]<-rnorm(nsubj,-15,5) # linear effect for participant
bx2<-list()
bx2[[1]]<-rnorm(nsubj,3,.5) # linear effect for participant
bx2[[2]]<-rnorm(nsubj,1,.5) # linear effect for participant

dat<-NULL
a<-20

for (i in 1:nsubj) {
     ai<-asub[i]
     x<-(1:nstimuli)/10
     cond<-rbinom(1,1,.5)+1
     bxi<-bx[[cond]][i]
     bx2i<-bx2[[cond]][i]
     y<-a+ai+bxi*x+bx2i*x^2+rnorm(length(x),0,50)
     x<-x+max(x)
     dat<-rbind(dat,cbind(y,x,i,cond))
}
dat<-as.data.frame(dat)

library(lmerTest)
dat$y<-((dat$y-mean(dat$y))/(3*sd(dat$y)))+36
dat$cx<-(dat$x-mean(dat$x))/10
dat$subj<-factor(dat$i)
summary(dat$timebin)
dat$timebin<-dat$x-7
cols<-dat$cond+2
dat$cond<-factor(dat$cond)
contrasts(dat$cond)<-contr.sum(2)

model<-NULL
model<-lmer(y~(1+cx+I(cx^2)|subj)+cx+I(cx^2)+cx:cond+I(cx^2):cond,data=dat)
summary(model)
plot(predict(model)~dat$x,col=cols)

#plot(dat$y~dat$x,col=cols)
names(dat)
sdat<-dat[,c("subj","y","cond","timebin")]
write.csv(sdat,"data/temptime.csv",row.names = F)
#tapply(dat$cx, dat$subj, function(a) cor(a,a^2))
# x<-1:70
# y<-10-32*x+.2*x^2
# plot(y~x)
# 
# y<-10+32*x-.2*x^2
# plot(y~x)

