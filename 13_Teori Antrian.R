library(readxl)
library(MASS)
library(queueing)
library(dplyr)

#Import data
antrian <- read_excel("Kuliah/Teknik Simulasi/Data Teori Antrian.xlsx")
head(antrian)

##Cek distribusi arrival dan service
# mendapatkan nilai rate 
fit <- fitdistr(antrian$`Waktu Antar Kedatangan Konsumen`, "exponential")
fit2 <- fitdistr(antrian$`Lama Penerimaan Pemesanan`, "exponential")

# goodness of fit test
# H0 : F0=Fh Distribusi data sama dengan distribusi eksponensial (acuan)
# H1 : F0=/Fh Distribusi data tidak sama dengan distribusi eksponensial (acuan)
# p-value < 0,5 maka H0 ditolak
ks.test(antrian$`Waktu Antar Kedatangan Konsumen`, "pexp", fit$estimate)
ks.test(antrian$`Lama Penerimaan Pemesanan`, "pexp", fit2$estimate)

steady_state <- function(x){
  if ((x$`Waktu Antar Kedatangan Konsumen` %>% mean()) < (x$`Lama Penerimaan Pemesanan` %>% mean())) {
    print("steady state")
  } else {
    print("not steady state")
  }
}
steady_state(antrian)

##Analisis Antrian
# define input 
server <- 3 # banyak server
lambda <- mean(antrian$`Waktu Antar Kedatangan Konsumen`) #rata-rata waktu antar kedatangan
mu <- mean(antrian$`Lama Penerimaan Pemesanan`) #rata-rata waktu pelayanan
input_mm1 <- queueing::NewInput.MM1(lambda = lambda, mu = mu)
input_mmc <- queueing::NewInput.MMC(lambda = lambda, mu = mu, c = server)

# check input
CheckInput.i_MM1(input_mm1)
CheckInput.i_MMC(input_mmc)

# create model
model_mm1 <- QueueingModel.i_MM1(input_mm1)
model_mmc <- QueueingModel.i_MMC(input_mmc)

# print model
summary(model_mm1)
summary(model_mmc)

# mendapatkan nilai faktor utilisasi atau rho
library(queuecomputer)
que <- queue_step(arrivals = antrian %>% pull(`Waktu Antar Kedatangan Konsumen`), 
                  service = antrian %>% pull(`Lama Penerimaan Pemesanan`), servers = 3)
summary(que)[10]

