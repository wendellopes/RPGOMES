#-------------------------------------------------------------------------------
R<-1              # Radius of the drop
n<-4/3            # Index of refraction
tr<-1             # Transparency of the rays
nr<-3             # Number of output refracted rays
f<-3              # Amplitude factor. Extends the rays by this value.
# kj<-8           # Uncomment this to show only specified option
# Incident impact factor  
dp<-.005          # increment 
pmin<--sin(pi/4)  # minimum value (-1+dp)
pmax<-sin(pi/4)   # maximum value (1-dp)
pj<-seq(pmin,pmax,dp) # Sequence or specific value(s).
jj<-.7
pj<-seq(jj,jj+.1,.01)
pj<-.7
pj<-c(pj,-pj)
# Other choices
reflec<-TRUE     # Show incident reflecion
normal<-TRUE     # Show the normals
ncheck<-FALSE    # Show outer circle to check normal
angtst<-FALSE    # Set n=1 and p=sin(pi/nr), where nr=4,8,16,
#-------------------------------------------------------------------------------
plot.rainbow(pj,n,nr,tr=1,kj=1:nr,f=1,reflec=TRUE,normal=TRUE,ncheck=TRUE){
   tt<-seq(0,2*pi,pi/199)
   plot(R*cos(tt),R*sin(tt),type='l',asp=1,xlim=2*R*c(-1,1),xlab='',ylab='');grid()      # Circle
   # outer circle to check normals
   #---------------------------------------
   if(ncheck){
      points(f*R*cos(tt),f*R*sin(tt),type='l');        # Circle
   }
   #---------------------------------------
   for(p in pj){
      st<-p
      ct<-sqrt(1-st**2)
      th<-atan2(st,ct)
      # phi
      sp<-st/n
      cp<-sqrt(1-sp**2)
      ph<-atan2(sp,cp)
      # psi
      ps<-(pi-2*ph)
      #---------------------------------------
      co<-cos(2*(th)) # Reflexao
      so<-sin(2*(th))
      points(R*c(1+f,ct),R*c(st,st),type='l',col=rgb(0,0,1,tr))        # Incident beam
      if(reflec){
         points(R*ct+f*c(0,co),R*st+f*c(0,so),type='l',col=rgb(0,1,0,tr))   # Reflection
      }
      #---------------------------------------
      cp<-ct
      sp<-st
      for(k in 0:nr){
         # INTERNAL REFLECTIONS
         ck<-cos(th+k*ps)
         sk<-sin(th+k*ps)
         if(normal){
            points( f*R*c(ck,0),f*R*c(sk,0),type='l',col=rgb(1,0,0,tr))  # N1
         }
         points(R*c(cp,ck),R*c(sp,sk),type='l',col=rgb(0,0,1,tr))   # D1
         cp<-ck
         sp<-sk
         # REFRACTIONS
         if(k>0){
            co<-cos(2*th+k*ps)
            so<-sin(2*th+k*ps)
            if(k %in% kj){
               points(R*ck+c(0,f*co),R*sk+c(0,f*so),type='l',col=rgb(0,1,0,tr))   # REFRACTION
            }
         }
      }
   }
}
   #---------------------------------------
