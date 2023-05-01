#Nama Kelompok: Fenny Amalia (003), Laila Khoirun Nisa (034), Wulan Nur R(038)
#KelasA


# Additive dengan variabel random binomial negatif
Additive_RNG<-function(a,z0,c,m,n) {
  xi<-matrix(NA,n,3)
  colnames(xi)<-c("aZ(i-1)+c","Xi","Ui")
  for (i in 1:n)
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
  }
  hist(xi[,3])
  View(xi)
  U<- (xi[,3])
  m<- 5
  p<- 0.5
  F<- pnbinom(1:20, size=m,p)
  negative.binom<-NULL
  for(i in 1:n){
    negative.binom[i]<- min(which(U[i]<F))-1
  }
  table (negative.binom)
}
Additive_RNG(35,11123,437,138,100)

# Additive dengan variabel random poisson
Additive_RNG<-function(a,z0,c,m,n) {
  xi<-matrix(NA,n,3)
  colnames(xi)<-c("aZ(i-1)+c","Xi","Ui")
  for (i in 1:n)
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
  }
  hist(xi[,3])
  View(xi)
  lambda<-1
  K<- (xi[,3])
  for (z in 1:i){
    k<-0
    sk<- 1
    while(sk>= exp(-lambda)){
      u<-runif(1)
      sk<-sk*u
      k<-k+1
    }
    K[z]<-k
  }
  K
  (tabel1<-table(K)/length(K))
}
Additive_RNG(35,11123,437,138,100)