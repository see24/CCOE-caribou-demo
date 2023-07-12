library(ggplot2)
#Notes
#q: assume more females than males in calf:cow group, so q<1

p=expand.grid(delay=c(T,F),q=c(0,0.3,0.6),w=c(1,3,6),
              u=c(0,0.1,0.2),z=seq(0,0.2,length.out=3),R=seq(0.15,0.6,length.out=10))

getX<-function(R,delay,q,w,u,z){
  X = 0.5*R*w*(q*u+1-u)/((q*u+w-u)*(1-z))
  X=X*(1-delay)+delay*X/(1+X)
  return(X)
}

p$X = getX(p$R,p$delay,p$q,p$w,p$u,p$z)

p$grp = paste(p$u,p$z)
names(p)
p$u=as.factor(p$u)
p$z=as.factor(p$z)

base = ggplot(subset(p,!delay),aes(x=R,y=2*X/R,col=z,linetype=u,group=grp))+
  geom_hline(yintercept=1)+geom_line()+facet_grid(q~w,labeller=label_both)+theme_bw()
plot(base)

base2 = ggplot(subset(p,delay),aes(x=R,y=2*X/R,col=z,linetype=u,group=grp))+
  geom_hline(yintercept=1)+geom_line()+facet_grid(q~w,labeller=label_both)+theme_bw()
plot(base2)
