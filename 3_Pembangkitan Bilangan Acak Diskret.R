#Pembangkitan dari bilangan acak Binomial
Binomial_sim<-function(n,p) {
  i<-1000
  n<-n
  p<-p
  Binom<-NULL
  for (z in 1:i) {
    m<-0
    for (k in 1:n){
      y<-(runif(1)<=p)+0
      m<-m+y
    }
    Binom[z]<-m
  }
  (tabel<-table(Binom)/length(Binom))
  print(Binom)
}

Binomial_sim(100,0.65)

#Pembangkitan dari bilangan acak uniform
i<-1000
X<-runif(i)
Binom<-as.numeric(cut(X,breaks=c(0,1/8,4/8,7/8,1),include.lowest=T))-1
(tabel<-table(Binom)/length(Binom))
View(Binom)

#Pembangkitan dengan menggunakan fungsi di R
#Fungsi R
x<-rbinom(16,4,0.5)
x

#Fungsi inverse transformation method
#Variabel Random geometri
#transformation
i<-1000
p<-0.5
R<-runif(i)
X<-log(1-R)/log(1-p)
hist(X)

#Input nilai p (peluang sukses)
i<-100
sebaran_geom<-function(p) {
  R<-runif(i)
  X<-log(1-R)/log(1-p)
  print(X)
}
sebaran_geom(0.5)

#Sebaran Bernoulli
K<-1
p<-0.5
while(runif(i)>p)
  K=K+1;
K

#Fungsi di R
#x ~ geometrik (0.4) sebanyak 16 bilangan acak
x<-rgeom(16,0.4)
x

#Variabel Random Binomial Negatif
#Sebaran geometri
K<-1
p<-0.5
r<-3
R<-runif(i)
s<-0
while(s<r) {
  if (runif(1)>p)
  {K=K+1;
  print=0
  }
  else
  {s=s+1;
  print=1}
}
K+r-1

#Sebaran uniform
n<-1000
U<-runif(n)
m<-5
p<-0.5
F<-pnbinom(1:20,size=m,p)
negative.binom<-NULL
for (i in 1:n) {
  negative.binom[i]<-min(which(U[i]<F))-1
}
table(negative.binom)

#Menggunakan fungsi di R
#x ~ negative binom (4, 0.5) sebanyak 16 bilangan acak
x<-rnbinom(16,4,0.5)
x

#Variabel Random Poisson
#Sebaran Uniform
i<-100
lambda<-1
K<-NULL
for (z in 1:i) {
  k<-0
  sk<-1
  while (sk>=exp(-lambda)) {
    u<-runif(i)
    sk<-sk*u
    k<-k+1
  }
  K[z]<-k
}
K
(tabel<-table(K)/length(K))

#Sebaran Eksponensial
i<-100
lambda<-1
K<-NULL
for (z in 1:i) {
  sk<-0
  k<-0
  while (sk<=1) {
    u<-runif(1)
    y<-log(u)/lambda
    sk<-y+sk
    k<-k+1
  }
  K[z]<-k+1
}
K
(tabel2<-table(K)/length(K))

#Fungsi di R
#x ~ poisson (4) sebanyak 16 bilangan acak
x<-rpois(16,4)
x