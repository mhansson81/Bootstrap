
Wage<-read.table("wage.txt",header=TRUE)
y=Wage$wage 
x=Wage$age
nx=length(x)

#Exercise 2(i) 
#############################################################
n_train=floor(0.7*nx) #n
n_test=nx-n_train #N-n
pMSE=numeric(10) 
set.seed(321)
indeces <- sample.int(nx,n_train) #find ntrain random indeces from data set
x_tr=x[indeces] #vector with train x-values
x_test=x[-indeces] #vector with test x-values
y_tr=y[indeces] #vector with train y-values 
y_test=y[-indeces] #vector with test y-values
for (i in 1:10) {
  m=lm(y_tr~poly(x_tr,i,raw=TRUE)) #poly regression of order i
  y_est=predict(m,newdata = data.frame(x_tr = x_test)) #model pred train x-values
  pMSE[i]=sum((y_est-y_test)^2)/n_test #pMSE for order i
}
plot(seq(1,10,1),pMSE,type="l",xlab='p',ylab='pMSE',cex.lab = 1.5)

#Exercise 2(ii)
#############################################################
n_att=50 #number of attemps
pMSE=matrix(nrow=10,ncol=n_att)
set.seed(321)
for (j in 1:n_att) {
  indeces <- sample.int(nx,n_train) #find ntrain random indeces from data set
  x_tr=x[indeces] #vector with train x-values
  x_test=x[-indeces] #vector with test x-values
  y_tr=y[indeces] #vector with train y-values 
  y_test=y[-indeces] #vector with test y-values
  for (i in 1:10) {
    m=lm(y_tr~poly(x_tr,i,raw=TRUE)) #poly regression of order i
    y_est=predict(m,newdata = data.frame(x_tr = x_test)) #model pred train x-values
    pMSE[i,j]=sum((y_est-y_test)^2)/n_test #pMSE for order i
  }
}
plot(seq(1,10,1),pMSE[,1],type="l",ylim=c(1300,1900),xlab='p',ylab='pMSE',cex.lab = 1.5)
for (i in 2:50) {
  lines(seq(1,10,1),pMSE[,i],type="l")
}
p=seq(1,10,1)%*%t(rep(1,n_att)) #matrix with p-values
plot(p,pMSE)

#Exercise 2(iii) p=2
#############################################################
m2=lm(y~poly(x,2,raw=TRUE)) #poly regression of order 2
m3=lm(y~poly(x,3,raw=TRUE)) #poly regression of order 3
m9=lm(y~poly(x,9,raw=TRUE)) #poly regression of order 9
y2=predict(m2,newdata=data.frame(x=seq(min(x),max(x),.1)),interval="prediction")
y3=predict(m3,newdata=data.frame(x=seq(min(x),max(x),.1)),interval="prediction")
y9=predict(m9,newdata=data.frame(x=seq(min(x),max(x),.1)),interval="prediction")


plot(x,y,ylim=c(-20,350),xlab='age',ylab='wage',cex.lab = 1.5,main='p = 2')
lines(seq(min(x),max(x),.1),sort(y2[,1]),type="l",col = "magenta")
lines(seq(min(x),max(x),.1),sort(y2[,2]),type="l",col = "red")
lines(seq(min(x),max(x),.1),sort(y2[,3]),type="l",col = "blue")

plot(x,y,ylim=c(-20,350),xlab='age',ylab='wage',cex.lab = 1.5,main='p = 3')
lines(seq(min(x),max(x),.1),sort(y3[,1]),type="l",col = "magenta")
lines(seq(min(x),max(x),.1),sort(y3[,2]),type="l",col = "red")
lines(seq(min(x),max(x),.1),sort(y3[,3]),type="l",col = "blue")

plot(x,y,ylim=c(-20,350),xlab='age',ylab='wage',cex.lab = 1.5,main='p = 9')
lines(seq(min(x),max(x),.1),sort(y9[,1]),type="l",col = "magenta")
lines(seq(min(x),max(x),.1),sort(y9[,2]),type="l",col = "red")
lines(seq(min(x),max(x),.1),sort(y9[,3]),type="l",col = "blue")

#Exercise 4 - Bootstrapping beta0-beta3
#############################################################
B=2000
beta=matrix(,nrow=B,ncol=4)
set.seed(321)
for (i in 1:B) { #loop for bootstrapping
  index=sample.int(nx,nx,replace=T) #indeces for sampling with replacement
  xs=x[index]
  ys=y[index]
  m=lm(ys~poly(xs,3,raw=TRUE))
  beta[i,1]=m$coefficients[1] #Extraxt coefficients
  beta[i,2]=m$coefficients[2]
  beta[i,3]=m$coefficients[3]
  beta[i,4]=m$coefficients[4]
}
#90% confint for bootstrapping parameters
CI_boot=matrix(,nrow=3,ncol=4)
CI_boot[,1]=quantile(beta[,1],probs=c(0.5,0.05,0.95))
CI_boot[,2]=quantile(beta[,2],probs=c(0.5,0.05,0.95))
CI_boot[,3]=quantile(beta[,3],probs=c(0.5,0.05,0.95))
CI_boot[,4]=quantile(beta[,4],probs=c(0.5,0.05,0.95))
#Comparing with confint 90 %
m0=lm(y~poly(x,3,raw=TRUE))
ci=confint(m0,level=0.9)

