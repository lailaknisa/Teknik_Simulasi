#Variabel Random Eksponensial
#Eksponensial (lambda=4)
i<-1000
lambda<-4
U<-runif(i)
X<--log(U)/lambda
hist(X)

#x ~ eksponensial (4) sebanyak 16 bilangan acak
x<- rexp(16,4)
x

#Variabel Random Gamma
#Gamma (a=4,B=3)
i<-1000
lambda<-3
alpha<-3
U<-log(runif(i*alpha))
Um<-matrix(U,i)
Y<-apply(Um,1,sum)
Gamma<--Y/lambda
hist(Gamma)

#x ~ Gamma (4,3) sebanyak 16 bilangan acak
x<-rgamma(16,4,3)
x

#Variabel Random Chisquare
#Chi-square (10)
i<-1000
lambda<-2
alpha<-5
U<-log(runif(i*alpha))
Um<-matrix(U,i)
Y<-apply(Um,1,sum)
Chi<--Y/lambda
hist(Chi)

#x ~ Chisquare (10) sebanyak 16 bilangan acak
x<-rchisq(16,10)
x

#Chi-square (11)
i<-1000
lambda<-2
alpha<-5
U<-log(runif(i*alpha))
Um<-matrix(U,i)
Y<-apply(Um,1,sum)
Chi<--Y/lambda
Chi<-Chi+(rnorm(i))^2
hist(Chi)

#x ~ Chisquare (11) sebanyak 16 bilangan acak
x<-rchisq(16,11)
x