set.seed(1839)
n <- 150
x1 <- rnorm(n)
x2 <- runif(n)

b01 <- -2
b02 <- -1
b03 <- 0
b04 <- 1

b1 <- 1.5
b2 <- 0.2
b3 <- 0.2
b4 <- 0.2


inv_logit <- function(logit) exp(logit) / (1 + exp(logit))

logodds1 <- b01 + b1 * (x1 )
logodds2 <- b02 + b1 * x1 
logodds3 <- b03 + b1 * x1 
logodds4 <- b04 + b1 * x1 

prob1 <- inv_logit(logodds1)
prob2 <- inv_logit(logodds2)
prob3 <- inv_logit(logodds3)
prob4 <- inv_logit(logodds4)

dat<-data.frame(x1,prob1,prob2,prob3,prob4,logodds1,logodds2,logodds3,logodds4)
gg<-ggplot(dat)
gg<-gg+geom_line(aes(x=x1,y=prob1,color="1 vs 2,3,4,5"),size=1.5)
gg<-gg+geom_line(aes(x=x1,y=prob2,color="1,2 vs 3,4,5"),size=1.5)
gg<-gg+geom_line(aes(x=x1,y=prob3,color="1,2,3 vs 4,5"),size=1.5)
gg<-gg+geom_line(aes(x=x1,y=prob4,color="1,2,3,4 vs 5"),size=1.5)
gg<-gg+ylab("Probability")+xlab("x")
gg<-gg+scale_color_manual(name='Comparisons',
                          breaks=c('1 vs 2,3,4,5', '1,2 vs 3,4,5', '1,2,3 vs 4,5','1,2,3,4 vs 5'),
                          values=c('1 vs 2,3,4,5'='blue', '1,2 vs 3,4,5'='cornflowerblue', '1,2,3 vs 4,5'='deepskyblue','1,2,3,4 vs 5'='aquamarine'))
gg1<-gg+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
         panel.background = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size = 14))


gg<-ggplot(dat)
gg<-gg+geom_line(aes(x=x1,y=logodds1,color="1 vs 2,3,4,5"),size=1.5)
gg<-gg+geom_line(aes(x=x1,y=logodds2,color="1,2 vs 3,4,5"),size=1.5)
gg<-gg+geom_line(aes(x=x1,y=logodds3,color="1,2,3 vs 4,5"),size=1.5)
gg<-gg+geom_line(aes(x=x1,y=logodds4,color="1,2,3,4 vs 5"),size=1.5)
gg<-gg+ylab("Probability")+xlab("x")
gg<-gg+scale_color_manual(name='Comparisons',
                          breaks=c('1 vs 2,3,4,5', '1,2 vs 3,4,5', '1,2,3 vs 4,5','1,2,3,4 vs 5'),
                          values=c('1 vs 2,3,4,5'='blue', '1,2 vs 3,4,5'='cornflowerblue', '1,2,3 vs 4,5'='deepskyblue','1,2,3,4 vs 5'='aquamarine'))

gg2<-gg+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom",
         panel.background = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size = 14))

require(ggpubr)
ggarrange(gg1, gg2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

gg<-ggplot(dat)
gg<-gg+geom_line(aes(x=x1,y=prob1,color="a"))
gg<-gg+geom_line(aes(x=x1,y=prob2),color="b")
gg<-gg+geom_line(aes(x=x1,y=prob3),color="c")
gg<-gg+geom_line(aes(x=x1,y=prob4),color="d")
gg<-gg+ylab("Probability")+xlab("x")
gg<-gg+scale_color_manual(name='Comparisons',
                          values=c('a'='blue', 'b'='cornflowerblue', 'c'='deepskyblue','d'='aquamarine'))
gg

