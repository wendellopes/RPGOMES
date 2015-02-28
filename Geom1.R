# IPs Intensity of Primary Rainbow, orthogonal
# IPp Intensity of Primary Rainbow, parallel
# ISs Intensity of Secondary Rainbow, orthogonal
# ISp Intensity of Secondary Rainbow, parallel
# \theta Incidence angle
# \phi Reflection angle
# IPs<- (1-sin^2(\phi-\theta))
# BIBLIOGRAPHY - E. Hecht - Optics
# n_1\sin\theta_1=n_2\sin\theta_2
# E perpendicular to the plane of incidence
# rs=\frac{n_1\cos\theta_1-n_2\cos\theta_2}{n_1\cos\theta_1+n_2\cos\theta_2}
# ts=\frac{2 n_1\cos\theta_1              }{n_1\cos\theta_1+n_2\cos\theta_2}
# E parallel to the plane of incidence
# rp=\frac{n_2\cos\theta_1-n_1\cos\theta_2}{n_1\cos\theta_2+n_2\cos\theta_1}
# tp=\frac{2 n_1\cos\theta_1              }{n_1\cos\theta_2+n_2\cos\theta_1}
# REFLECTANCE AND TRANSMITANCE
# RP+TP=1
# RS+TS=1
#
# RS=rs**2
# RP=rp**2
# TS=ts**2*\left(\frac{n_2\cos\theta_2}{n_1\cos\theta_1})
# TP=tp**2*\left(\frac{n_2\cos\theta_2}{n_1\cos\theta_1})
#
#-------------------------------------------------------------------------------
snell<-function(sth,n2,n1=1,amplitude=FALSE){
   cth<-sqrt(1-sth**2)
   sph<-(n1/n2)*sth
   cph<-sqrt(1-sph**2)
   rs<-(n1*cth-n2*cph)/(n1*cth+n2*cph)
   ts<-(2*n1*cth     )/(n1*cth+n2*cph)
   rp<-(n2*cth-n1*cph)/(n1*cph+n2*cth)
   tp<-(2*n1*cth     )/(n1*cph+n2*cth)
   if(amplitude){
      fa<-(n2*cph)/(n1*cth)
      rs<-rs**2
      rp<-rp**2
      ts<-fa*ts**2
      tp<-fa*tp**2
   }
   return(data.frame(rs,ts,rp,tp))
}
#-------------------------------------------------------------------------------
dth<-pi/200
th<-seq(0,pi/2-dth,dth)
p<-sin(th)
n<-3/2
u<-snell(p,n,amplitude=TRUE)
#-------------------------------------------------------------------------------
# Coeficientes, grafico linear
plot(th,u$ts,type='l',ylim=c(0,1))
points(th,u$tp,col='red',type='l')
points(th,u$rp,col='blue',type='l')
points(th,u$rs,col='green',type='l')
abline(h=0)
legend("topright",c("ts","tp","rs","rp"),col=c("black","red","blue","green"),lty=1)
#-------------------------------------------------------------------------------
# Coeficientes, grafico polar
x11()
th<-seq(-pi,pi,dth/10)
u<-snell(sin(th),n,amplitude=FALSE)
plot(  u$ts*cbind(cos(th),sin(th)),type='l',lwd=2,ylim=c(-1,1),asp=1,xlab='',ylab='')
points(u$tp*cbind(cos(th),sin(th)),type='l',lwd=2,col='red')
points(u$rp*cbind(cos(th),sin(th)),type='l',lwd=2,col='blue')
points(u$rs*cbind(cos(th),sin(th)),type='l',lwd=2,col='magenta')
legend("topright",c("ts","tp","rp","rs"),col=c("black","red","blue","magenta"),lty=1,lwd=2)
#-------------------------------------------------------------------------------
# POLAR GRID
thj<-seq(-pi,pi,pi/200)
dr<-.2
rj<-seq(dr,1,dr)
for(i in rj){
   points(i*cbind(cos(thj),sin(thj)),col='lightgreen',type='l')
}
rjm<-max(rj)
for(t in seq(0,360,15)*pi/180){
   points(rjm*c(0,cos(t)),rjm*c(0,sin(t)),type='l',col='lightgreen')
}
