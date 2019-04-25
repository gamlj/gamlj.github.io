### crossed stimuli ###
nsubj<-50
nstimuli<-30
asub<-rnorm(nsubj,0,2) # subjects intercepts
asti<-rnorm(nstimuli,0,2) # stimuli intercepts
fac<-c(-1,1)
bfac<-rnorm(nsubj,.4,1)
dat<-NULL
a<-20
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
dat<-dat[,c("y","cond","subj","stimulus")]
#write.csv(dat,"data/subjects_by_stimuli.csv")

model<-lmer(y~(1+cond|subj)+(1|stimulus)+cond,data=dat)
summary(model)

### nested stimuli ###

nsubj<-50
nstimuli<-36
asub<-rnorm(nsubj,0,2) # subjects intercepts
asti<-rnorm(nstimuli,0,2) # stimuli intercepts
fac<-c(0,1)
b<-1.9
bfac<-rnorm(nsubj,b,1)
dat<-NULL
a<-20
j<-1

for (i in 1:nsubj) {
  for (k in 0:(length(fac)-1)) {
  start<-(nstimuli*k/2)+1
  end<-(nstimuli*(1+k))/2
  x<-k-.5
  for (j in start:end) {
    aj<-asti[j]
    ai<-asub[i]
    bi<-bfac[i]
    y<-a+ai+aj+bi*x+rnorm(length(x),0,3)
    dat<-rbind(dat,cbind(y,k,i,j))
  }
}
}
dat<-as.data.frame(dat)
table(dat$i)

library(lmerTest)
dat$subj<-factor(dat$i)
dat$stimulus<-factor(dat$j)
dat$cond<-factor(dat$k)
model<-lmer(y~(1+cond|subj)+(1|stimulus)+cond,data=dat)
summary(model)
contrasts(dat$cond)<-contr.sum(2)
names(dat)
dat<-dat[,c("y","cond","subj","stimulus")]
#write.csv(dat,"data/subjects_on_stimuli.csv")

model<-lmer(y~(1+cond|subj)+(1|stimulus)+cond,data=dat)
summary(model)
table(dat$cond,dat$stimulus)
