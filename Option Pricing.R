n<-1000
S_0<-100
K<-100
T<-0.5
d<-2
r<-0.01
sigma_1<-0.4
sigma_2<-0.6

grid<-matrix(rep(seq(T/d,T,length.out=d),n),nrow=n,byrow=TRUE)

x1<-matrix(rnorm(d*n),nrow=n)
x2<-matrix(rnorm(d*n),nrow=n)

BM1<-t(apply(sqrt(T/d)*x1,1,cumsum))
BM2<-t(apply(sqrt(T/d)*x2,1,cumsum))

S_1<-S_0*exp((r-sigma_1^2/2)*grid+sigma_1*BM1) #Shows S_1 at T/2 and T
S_2<-S_0*exp((r-sigma_2^2/2)*grid+sigma_2*(0.6*BM1+0.8*BM2))

S<-(S_1+S_2)/2 #Divided by 2 since you are adding column of T/2 with T/2 and T with T

discpayoff<-pmax(apply(S,1,mean)-100,0)*exp(-r*T) #The mean in the apply function takes the mean of T/2 and T
price<-mean(discpayoff)
price

nu<-(2.58*1.1*sd(discpayoff)/0.05)^2
nu

#Rerun with new n
n<-996400.3
S_0<-100
K<-100
T<-0.5
d<-2
r<-0.01
sigma_1<-0.4
sigma_2<-0.6

grid<-matrix(rep(seq(T/d,T,length.out=d),n),nrow=n,byrow=TRUE)

x1<-matrix(rnorm(d*n),nrow=n)
x2<-matrix(rnorm(d*n),nrow=n)

BM1<-t(apply(sqrt(T/d)*x1,1,cumsum))
BM2<-t(apply(sqrt(T/d)*x2,1,cumsum))

S_1<-S_0*exp((r-sigma_1^2/2)*grid+sigma_1*BM1) #Shows S_1 at T/2 and T
S_2<-S_0*exp((r-sigma_2^2/2)*grid+sigma_2*(0.6*BM1+0.8*BM2))

S<-(S_1+S_2)/2 #Divided by 2 since you are adding column of T/2 with T/2 and T with T

discpayoff<-pmax(apply(S,1,mean)-100,0)*exp(-r*T) #The mean in the apply function takes the mean of T/2 and T
price<-mean(discpayoff)
price
