# Mobile cellular subscriptions (per 100 people)
# https://data.worldbank.org/indicator/IT.CEL.SETS.P2

odata<-read.csv("data/mobiles.csv",stringsAsFactors = F)
mdata<-read.csv("data/mobilesmeta.csv",stringsAsFactors = F)
odata<-merge(odata,mdata,by="Country.Code")
odata<-odata[!odata$Region=="",]
names(odata)
long<-list()
cods<-unique(odata$Country.Code)
co<-as.character(cods[[1]])
year<-2000:2019
vars<-paste0("X",year)
odata$check<-(!is.na(odata$X2000) & odata$X2000>5)
odata<-odata[odata$check,]
dim(odata)
odata$IncomeGroup<-ifelse(odata$IncomeGroup=="High income","High","Low")
table(odata$IncomeGroup)

yearcode<-1:length(vars)
for (code in unique(odata$Country.Code)) {
  country<-as.character(odata[odata$Country.Code==code,"Country.Name"])
  income<-as.character(odata[odata$Country.Code==code,"IncomeGroup"])
  mcs<-as.numeric(odata[odata$Country.Code==code,vars])
  long[[length(long)+1]]<-cbind(name,code,income,year,yearcode,mcs)
}
longdata<-as.data.frame(do.call("rbind",long))
head(longdata)
longdata$year<-as.numeric(as.character(longdata$year))
longdata$yearcode<-as.numeric(as.character(longdata$yearcode))
longdata$mcs<-as.numeric(as.character(longdata$mcs))

write.csv(longdata,file="data/mobileslong.csv",row.names = F)

data<-read.csv(file="data/mobileslong.csv")
head(data)

hist(data$mcs)
data[data$yearcode==1,]

mod<-gamljMixed(formula=mcs~year+income+I(year^2)+income:year+income:I(year^2)+(1+year|code),data=data)

p<-plot(mod,~year:income,plotRandomEffects=T)
p
oo<-ggplot_build(p)$layout$panel_params[[1]]$x
labels<-oo$breaks
mm<-mean(data$year,na.rm = T)
oo$minor_breaks
labels[[1]]
as.character(labels[[1]])
is.integer(10)

test<-all(sapply(labels, function(a) (a %% 1)==0))
newlabs<-labels+round(mm)
p$scales$scales

p+ggplot2::scale_x_continuous(breaks=labels, labels=newlabs)


mod<-gamljMixed(formula=mcs~year+I(year^2)+(1+year+I(year^2)|code),data=data)
mod
p2<-plot(mod,~year,plotRandomEffects=T,plotRaw=T,scaling=c(year="standardized"))

oo<-ggplot_build(p2)$layout$panel_params[[1]]$x
oo$scale_is_discrete
breaks<-oo$breaks


oo$limits
.sd<-sd(data$year)
newlabs<-round((breaks*.sd)+mm)
p2+ggplot2::scale_x_continuous(breaks=breaks, labels=newlabs)
