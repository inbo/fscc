#### SOILSPLINE function v3
#### Creates cm-based vertical mass preserving spline from soil variable based on horizon based input
#### Wrapper based on mpspline2 function, but with extrapolation till max soildepth
#### Input vars:
####      id = name of profile
####      top = vector with layer top (in cm) for each horizon 
####      bot  = vector with layer bottom (in cm) for each horizon 
####      variab = variable to spline
####      msd = max soil depth for spline
####      graph = if TRUE, make a graph
####      NA not allowed in horizon boundaries, NA in variab is possible, if not in first horizon 
####################################################################################


##-- Load libraries
#library(nortest)
#library(RSAGA) 
#library(XML)
library(aqp)
#library(sp)
#library(plyr)
library(mpspline2)

par(mfrow=c(1,1))

## set working directory for spline outputs
SplineDir<-c("C:/R_scripts/_GIT_REPO/fscc/output/stocks/so/splines")



#### Function:

SOILSPLINE <- function(id,top,bot,variab,msd,graph) 
{ 
# prepare a SoilProfileCollection (prof1 is dataframe to promote to SoilProfileCollection object):
prof1<-NULL
prof1 <- na.omit(data.frame(id, top, bot, variab))
#depths(prof1) <- id ~ top + bot     # this statement makes Soilprofilecollection object / not needed for mpsline2
### check if correct object
print(prof1)
### fit a spline using mpspline function (max depth will be bottom lowest boundary)
variab.s <-mpspline_one(site = prof1, var_name = 'variab', lam = 0.1)   
### spline extrapolation to soildepth = msd
x<-variab.s$est_1cm[1:msd]
xlen<-length(x)
y<-(-1)*c(1:xlen)
spfit<-spline(x, y=NULL, method = "natural", xout=c(1:msd))    # xout makes extrapolation beyond lower boundary of obs)
diepte<-(-1)*spfit$x
splinx<-spfit$y  # this is predicted and extrapolated variab
splinx<-ifelse(splinx<0,0,splinx)    ### output variable cannot become negative, less than zero is zero

if (graph==TRUE) { 

### save plot as file (undo if not required)
  
  fname<-paste0(SplineDir,"/",id,".png",sep="")
  png(file=fname)
  
#### plot mpspline
AVD<-(-1)*(top+bot)/2
maxv<-max(variab,na.rm=T)
plot(variab,AVD, xlim=c(0,maxv), ylim=c(-1*msd,0), ylab=c("Depth   (cm)"), col="black", cex=1.2, pch=16, main=id, 
     xlab=expression('Carbon density   (t C ha'^-1*'cm'^-1*')'))
# plot bulk horizons
nhor<-length(top)
xl<-rep(0,nhor)
yb<-(-1)*bot
xr<-variab
yt<-(-1)*top
rect(xl,yb,xr,yt, col=8, border="white",lwd=2)
# plot points and lines
abline(h=-1*msd,col=3,lty=2, lwd=2)   # depthstock line of profile
# lines(variab,AVD, col=2)      # connect lines between horizon midpoints
# points(variab.s$var.1cm[1:msd],-1*c(1:msd), col="blue", pch=".", cex=4)  # 1 cm spline till max depth
lines(splinx[1:msd],-1*c(1:msd), col="blue", lwd=3)  # 1 cm mpspline till max depth
points(splinx,diepte, col=3)
#points(variab.s$var.fitted,-1*variab.s$depths, pch=16, col="red")  # fitted at observed depths (border)
points(variab,AVD, col="black", cex=1.2, pch=16, main=id)    # midpoints of horizons
}
####################
# Saves PNG graph output with id name
dev.off()

# Returns splined variable for each cm 
return(splinx)
}

dev.off()
## End function
 

 
#### Run test function

#naam = c("CD test")
#tophor = as.numeric(c(0,5,10,20,40))
#bothor = as.numeric(c(5,10,20,40,80))
#OC = as.numeric(c(2.7, 1.3, 1, 0.6, 0.3))
#maxdiepte <- 100
#grafiek<-TRUE

#out<-SOILSPLINE(id=naam,tophor,bothor,OC,msd=maxdiepte,graph=grafiek)
#


  

  


