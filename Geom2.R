#-------------------------------------------------------------------------------
# FATOR GEOMETRICO
#-------------------------------------------------------------------------------
# Angulos
Dk<-function(th,n,k,reduce=TRUE){
   ph<-asin(sin(th)/n)
   dk<-2*(th-ph)+k*(pi-2*ph)
   if(reduce){
      s<-sign(dk)
      dk<-abs(dk)%%(2*pi)
      dk<-ifelse(dk>pi,dk-2*pi,dk)
      dk<-s*dk
   }
   return(dk)
}
Pk<-function(th,n,k,reduce=TRUE){
   ph<-asin(sin(th)/n)
   pk<-2*th+(k+1)*(pi-2*ph)
   if(reduce){
      s<-sign(pk)
      pk<-abs(pk)%%(2*pi)
      pk<-ifelse(pk>pi,pk-2*pi,pk)
      pk<-s*pk
   }
   return(pk)
}
#-------------------------------------------------------------------------------
DkE<-function(n,k){
   dke<-acos(sqrt((n**2-1)/(k*(k+2))))
   return(dke*c(-1,1))
}
#-------------------------------------------------------------------------------
# REFLECTION COEFFICIENTS
#-------------------------------------------------------------------------------
Tpk<-function(th,n,k){
   ph<-asin(sin(th)/n)
   r<-sin(th-ph)/sin(th+ph)
   r<-r**2
   rpk<-((1-r)**2)*(r**k)
}
Tsk<-function(th,n,k){
   ph<-asin(sin(th)/n)
   r<-tan(th-ph)/tan(th+ph)
   r<-r**2
   rpk<-((1-r)**2)*(r**k)
}
#-------------------------------------------------------------------------------
ko<-1
dth<-pi/600
th<-seq(-pi/2,pi/2,dth)
u1<-Pk(th,4/3,ko,reduce=TRUE)
u2<-Pk(th,4/3,ko+1,reduce=TRUE)
# Angles
uk1<-DkE(4/3,ko  )
uk2<-DkE(4/3,ko+1)
tk1<-Pk(uk1,4/3,ko  )
tk2<-Pk(uk2,4/3,ko+1)
#-------------------------------------------------------------------------------
# DRAW
#-------------------------------------------------------------------------------
pdf("Pk.pdf")
plot(  sin(th),u1*180/pi,type='l',ylim=range(180*c(-1,1)),
       xlab="Impact Parameter",ylab=expression(Theta_k))
points(sin(th),u2*180/pi,type='l',col='red')
legend("topleft",c("P1","P2"),col=c("black","red"),lty=1)
abline(h=tk1*180/pi)
abline(h=tk2*180/pi,col='red')
dev.off()
#-------------------------------------------------------------------------------
# REFLECTION COEFFICIENTS
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
TH1<-Pk(th,4/3,ko  )
TH2<-Pk(th,4/3,ko+1)
#-------------------------------------------------------------------------------
Tp1<-Tpk(th,4/3,ko  )
Tp2<-Tpk(th,4/3,ko+1)
Ts1<-Tsk(th,4/3,ko  )
Ts2<-Tsk(th,4/3,ko+1)
T1<-Tp1+Ts1
T2<-Tp2+Ts2
#-------------------------------------------------------------------------------
#DRAW
#-------------------------------------------------------------------------------
#plot(  TH1*180/pi,u1,type='l',ylim=range(c(u1,u2,v1,v2,u,v)),xlim=180*c(-1,1))
#points(TH1*180/pi,v1,type='l',col='cyan')
#points(TH1*180/pi,i1,type='l',col='blue',lty=2)
#points(TH2*180/pi,u2,type='l',col='red')
#points(TH2*180/pi,v2,type='l',col='magenta')
#points(TH2*180/pi,i2,type='l',col='green',lty=2)
#abline(v=tk1*180/pi,col='blue')
#abline(v=tk2*180/pi,col='green')
c1<-cos(TH1)
s1<-sin(TH1)
c2<-cos(TH2)
s2<-sin(TH2)
thj<-seq(-pi,pi,pi/200)
rjm<-.3
dr<-rjm/10
rj<-seq(dr,.3,dr)
#-------------------------------------------------------------------------------
#DRAW
#-------------------------------------------------------------------------------
pdf("ReflectionCoeff.pdf")
plot(  cbind(c1,s1)*Tp1,type='l',lwd=2,asp=1,
       xlim=.8*max(c(T1,T2))*c(-1,1),ylim=.8*max(c(T1,T2))*c(-1,1),
       xlab='',ylab='',col='blue')
points(cbind(c1,s1)*Ts1,type='l',lwd=2,col='cyan')       
points(cbind(c1,s1)*T1,type='l',lwd=2,col='black') 
points(cbind(c2,s2)*Tp2,type='l',lwd=2,col='orange')        
points(cbind(c2,s2)*Ts2,type='l',lwd=2,col='magenta')    
points(cbind(c2,s2)*T2,type='l',lwd=2,col='red')
points(rjm*c(0,cos(tk1[1])),rjm*c(0,sin(tk1[1])),type='l',col='magenta',lwd=2,lty=2)
points(rjm*c(0,cos(tk1[2])),rjm*c(0,sin(tk1[2])),type='l',col='magenta',lwd=2,lty=2)
points(rjm*c(0,cos(tk2[1])),rjm*c(0,sin(tk2[1])),type='l',col='magenta',lwd=2,lty=2)
points(rjm*c(0,cos(tk2[2])),rjm*c(0,sin(tk2[2])),type='l',col='magenta',lwd=2,lty=2)
#-------------------------------------------------------------------------------
for(i in rj){
   points(i*cbind(cos(thj),sin(thj)),col='lightgreen',type='l')
}
rjm<-max(rj)
for(t in seq(0,360,15)*pi/180){
   points(rjm*c(0,cos(t)),rjm*c(0,sin(t)),type='l',col='lightgreen')
}
points(rjm*c(0,cos(tk1[1])),rjm*c(0,sin(tk1[1])),type='l',col='magenta',lwd=2,lty=2)
points(rjm*c(0,cos(tk1[2])),rjm*c(0,sin(tk1[2])),type='l',col='magenta',lwd=2,lty=2)
points(rjm*c(0,cos(tk2[1])),rjm*c(0,sin(tk2[1])),type='l',col='magenta',lwd=2,lty=2)
points(rjm*c(0,cos(tk2[2])),rjm*c(0,sin(tk2[2])),type='l',col='magenta',lwd=2,lty=2)
#-------------------------------------------------------------------------------
legend("topleft",c("Tp1","Ts1","Tp1+Ts1","Tp2","Ts2","Tp2+Ts2"),
              col=c("blue","cyan","black","orange","magenta","red"),
              lty=1,lwd=2)#,pch=c(1,1,3,3,4,4))
dev.off()
