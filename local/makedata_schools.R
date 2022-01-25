nsubj<-50
nstimuli<-30
asub<-rnorm(nsubj,0,2) # subjects intercepts
asti<-rnorm(nstimuli,0,2) # stimuli intercepts
fac<-c(-1,1)
bfac<-rnorm(nsubj,0,1)
dat<-NULL
a<-20
i<-i
j<-1
for (i in 1:nsubj) {
  for (j in 1:nstimuli) {
     x<-fac
     aj<-asti[j]
     ai<-asub[i]
     bi<-bfac[i]
     y<-a+ai+aj+bi*x+rnorm(length(x),0,3)
     dat<-rbind(dat,cbind(y,x,i,j))
}
}
dat<-as.data.frame(dat)

library(lmerTest)
dat$subj<-factor(dat$i)
dat$stimulus<-factor(dat$j)
dat$cond<-factor(dat$x)
model<-lmer(y~(1+cond|subj)+(1|stimulus)+cond,data=dat)
summary(model)
contrasts(dat$cond)<-contr.sum(2)
#write.csv(dat,"data/subjects_by_stimuli.csv")

model<-lmer(y~(1+cond|subj)+(1|stimulus)+cond,data=dat)
summary(model)
