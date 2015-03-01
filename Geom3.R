#-------------------------------------------------------------------------------
# FATOR GEOMETRICO
#-------------------------------------------------------------------------------
# Angulos
PiReduce<-function(th){
   s<-sign(th)
   th<-abs(th)%%(2*pi)
   th<-ifelse(th>pi,th-2*pi,th)
   th<-s*th
   return(th)
}
Phi.k<-function(th,n,k,reduce=TRUE){
   ph<-asin(sin(th)/n)
   dk<-
   dk<-th+(k+1)*(pi-2*ph)
   if(reduce){
      dk<-PiReduce(dk)
   }
   return(dk)
}
Theta.k<-function(th,n,k,reduce=TRUE){
   ph<-asin(sin(th)/n)
   Tk<-2*th+(k+1)*(pi-2*ph)
   if(reduce){
      Tk<-PiReduce(Tk)
   }
   return(Tk)
}
#-------------------------------------------------------------------------------
AngExtreme<-function(n,k,j=2){
   u<-(2/j)*(k+1)
   dke<-acos(sqrt((n**2-1)/(u**2-1)
   return(dke)
}
#-------------------------------------------------------------------------------
# REFLECTION COEFFICIENTS
#-------------------------------------------------------------------------------
Rpk<-function(th,n,k){
   ph<-asin(sin(th)/n)
   r<-ifelse((abs(th)%%pi)<0.01,(n-1)/(n+1),sin(th-ph)/sin(th+ph))
   r<-r**2
   Rpk<-((1-r)**2)*(r**k)
   return(Rpk)
}
Rsk<-function(th,n,k){
   ph<-asin(sin(th)/n)
   r<-ifelse((abs(th)%%pi)<0.01,(n-1)/(n+1),tan(th-ph)/tan(th+ph))
   r<-r**2
   Rpk<-((1-r)**2)*(r**k)
   return(Rpk)
}
#-------------------------------------------------------------------------------
# GEOMETRICAL FACTOR
#-------------------------------------------------------------------------------
dTheta.dtheta<-function(th,n,k,j=2){
   u<-sin(th)
   ft<-sqrt((1-u**2)/(n**2-u**2))
   ddk<-j-2*(k+1)*ft
   return(ddk)
}
#-------------------------------------------------------------------------------
GeometricalFactor<-function(th,n,k,pol='b',dtheta=TRUE,Fresnel=TRUE,j=2){
   # Check the polarizations
   if(!(pol%in%c('s','p','b'))){
      stop("Polarization must be parallel (p), perpendicular (s) or both (b)")
   }
   # Take into acount the (d\Theta/d\theta)^{-1} factor
   if(dtheta){
      de<-1/dTheta.dtheta(th,n,k,j)
   }else{
      de<-1
   }
   # reduce th to 0:2*pi
   th<-PiReduce(th)
   # values for th=0 and th=pi 
   c0<--((-1)^k)/(1-(k+1)/n)
   c1<--((-1)^k)/(1+(k+1)/n)
   # Limits 
   L0<-0.001
   ft<-ifelse(abs(th) < L0,
         c0,
         ifelse(abs(abs(th)-pi) < L0,
            c1,
            sin(2*th)/sin(Tk(th,n,k))))
   # Reflectivity and polarization
   if(pol=='b'){
      ek<-.5*(Rpk(th,n,k)+Rsk(th,n,k))
   }else{
      if(pol=='s'){
         ek<-.5*Rsk(th,n,k)
      }else{
         ek<-.5*Rpk(th,n,k)
      }
   }
   if(Fresnel){
      ek<-ft*de*ek
   }else{
      ek<-ft*de
   }
   return(ek)
}
#-------------------------------------------------------------------------------
ko<-1
dth<-pi/600
th<-seq(-pi/2,pi/2,dth)
#-------------------------------------------------------------------------------
uk1<-DkE(4/3,ko  )
uk2<-DkE(4/3,ko+1)
tk1<-Tk(uk1,4/3,ko  )
tk2<-Tk(uk2,4/3,ko+1)
#-------------------------------------------------------------------------------
TH1<-Tk(th,4/3,ko  )
TH2<-Tk(th,4/3,ko+1)
#-------------------------------------------------------------------------------
u1<-Rpk(th,4/3,ko  )
u2<-Rpk(th,4/3,ko+1)
v1<-Rsk(th,4/3,ko  )
v2<-Rsk(th,4/3,ko+1)
i1<-u1+v1
i2<-u2+v2
#-------------------------------------------------------------------------------
#
c1<-cos(TH1)
c2<-cos(TH2)
s1<-sin(TH1)
s2<-sin(TH2)
#
ek1b<-Ek(th,4/3,ko  ,pol='b')
ek2b<-Ek(th,4/3,ko+1,pol='b')
ek1p<-Ek(th,4/3,ko  ,pol='p')
ek2p<-Ek(th,4/3,ko+1,pol='p')
ek1s<-Ek(th,4/3,ko  ,pol='s')
ek2s<-Ek(th,4/3,ko+1,pol='s')
#
#-------------------------------------------------------------------------------
# LINEAR PLOT
#-------------------------------------------------------------------------------
#x11()
#pdf("Intensity.pdf")
plot(  TH1*180/pi,ek1b,pch=1,type='p',cex=.5,log='y',xlim=180*c(-1,1),
   ylim=c(1e-5,10),xlab="Emerging angle",ylab="Intensity")
points(TH2*180/pi,ek2b,pch=1,type='p',cex=.5,col='red')
#
points(TH1*180/pi,ek1p,pch=3,type='p',cex=.5,col='green')
points(TH2*180/pi,ek2p,pch=3,type='p',cex=.5,col='cyan')
#
points(TH1*180/pi,ek1s,pch=4,type='p',cex=.5,col='blue')
points(TH2*180/pi,ek2s,pch=4,type='p',cex=.5,col='magenta')
#
abline(v=tk1*180/pi,col='magenta',lty=2,lwd=2)
abline(v=tk2*180/pi,col='magenta',lty=2,lwd=2)
#
legend("topright",c("b1","b2","p1","p2","s1","s2"),
              col=c("black","red","green","cyan","blue","magenta"),
              pch=c(1,1,3,3,4,4))
#dev.off()
#-------------------------------------------------------------------------------
# POLAR PLOT
#-------------------------------------------------------------------------------
#pdf("PFZ.pdf")
#x11()
r<-.2
plot  (cbind(c1,s1)*ek1b,type='l',lwd=2,xlim=r*c(-1,1),asp=1,ylim=r*c(-1,1),xlab='',ylab='')
points(cbind(c2,s2)*ek2b,type='l',lwd=2,col='red')
#
points(cbind(c1,s1)*ek1p,type='l',lwd=2,pch=3,col='green')
points(cbind(c2,s2)*ek2p,type='l',lwd=2,pch=3,col='cyan')
#
points(cbind(c1,s1)*ek1s,type='l',lwd=2,pch=4,col='blue')
points(cbind(c2,s2)*ek2s,type='l',lwd=2,pch=4,col='magenta')
#-------------------------------------------------------------------------------
thj<-seq(-pi,pi,pi/200)
lj<-seq(10,1,-2)
rjm<-r
dr<-rjm/10
rj<-seq(dr,rjm,dr)
for(i in rj){
   points(i*cbind(cos(thj),sin(thj)),col='lightgreen',type='l')
}
for(t in seq(0,360,15)*pi/180){
   points(rjm*c(0,cos(t)),rjm*c(0,sin(t)),type='l',col='lightgreen')
}
points(rjm*c(0,cos(tk1[1])),rjm*c(0,sin(tk1[1])),type='l',col='magenta',lwd=2,lty=2)
points(rjm*c(0,cos(tk1[2])),rjm*c(0,sin(tk1[2])),type='l',col='magenta',lwd=2,lty=2)
points(rjm*c(0,cos(tk2[1])),rjm*c(0,sin(tk2[1])),type='l',col='magenta',lwd=2,lty=2)
points(rjm*c(0,cos(tk2[2])),rjm*c(0,sin(tk2[2])),type='l',col='magenta',lwd=2,lty=2)
text(cbind(rj,0)[seq(10,1,-2),],labels=rj[seq(10,1,-2)],cex=.8)
#-------------------------------------------------------------------------------
#legend("topright",c("S1","S2"),
#              col=c("black","red"),
#              lty=1)#,pch=c(1,1,3,3,4,4))
legend("topleft",c("(Tp1+Ts1) S1","(Tp2+Ts2) S2","Tp1 S1","Tp2 S2","Ts1 S1","Ts2 S2"),
              col=c("black","red","green","cyan","blue","magenta"),
              lty=1)#,pch=c(1,1,3,3,4,4))
#-------------------------------------------------------------------------------
#dev.off()
