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
plot(th,u$ts,type='l',ylim=c(0,1))
points(th,u$tp,col='red',type='l')
points(th,u$rp,col='blue',type='l')
points(th,u$rs,col='green',type='l')
abline(h=0)
legend("topright",c("ts","tp","rs","rp"),col=c("black","red","blue","green"),lty=1)
#-------------------------------------------------------------------------------
th<-seq(-pi,pi,dth/10)
u<-snell(sin(th),n,amplitude=FALSE)
plot(u$ts*cbind(cos(th),sin(th)),type='l',ylim=c(-1,1),asp=1)
points(u$tp*cbind(cos(th),sin(th)),col='red',type='l')
points(u$rp*cbind(cos(th),sin(th)),col='blue',type='l')
points(u$rs*cbind(cos(th),sin(th)),col='green',type='l')
legend("topright",c("ts","tp","rp","rs"),col=c("black","red","blue","green"),lty=1)
#-------------------------------------------------------------------------------
# RAINBOWL
#-------------------------------------------------------------------------------
# R1 == Two refractions, one reflection
# R2 == Two refractions, two reflections
n<-4/3
s.in<-sin(th)
s.ou<-sin(th)/n
u.in<-snell(s.in,  n,amplitude=TRUE)
u.ou<-snell(s.ou,1/n,amplitude=TRUE)
#-------------------------------------------------------------------------------
R1p<-u.in$tp*u.ou$rp*u.ou$tp
R1s<-u.in$ts*u.ou$rs*u.ou$ts
R2p<-u.in$tp*u.ou$rp*u.ou$rp*u.ou$tp
R2s<-u.in$ts*u.ou$rs*u.ou$rs*u.ou$ts
#-------------------------------------------------------------------------------
plot(R1p*cbind(cos(th),sin(th)),type='l',ylim=.15*c(-1,1),asp=1)
points(R1s*cbind(cos(th),sin(th)),col='red',type='l')
points(R2p*cbind(cos(th),sin(th)),col='blue',type='l')
points(R2s*cbind(cos(th),sin(th)),col='green',type='l')
legend("topright",c("R1p","R1s","R2p","R2s"),col=c("black","red","blue","green"),lty=1)