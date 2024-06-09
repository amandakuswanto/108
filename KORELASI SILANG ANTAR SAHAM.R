# Download library
library(readxl)
library(ggplot2)
library(psych)
library(tseries)
library(forecast)
library(CircStats)
library(astsa)

# Membaca excel yang akan digunakan
Book2 <- read_excel("Book2.xlsx")
View(Book2)


BBRI= Book2$BRI
BMRI= Book2$Mandiri
BBNI= Book2$BNI
BBTN= Book2$BTN

BBRI= ts(BBRI)
BMRI= ts(BMRI)
BBNI= ts(BBNI)
BBTN= ts(BBTN)

#Plot koefisien korelasi (untuk buat plot cross corr function)
ccfvalues1 =ccf(BBRI,BMRI, lag.max = 30)
lags = ccfvalues1$lag
corr = ccfvalues1$acf
pos = lags[lags >= 0 & lags <= 30]
pos1 = corr[lags >= 0 & lags <= 30]
ccfdata = data.frame(Lag = pos, Korelasi = pos1)
ccfdata
plot(pos,pos1, type ='h', xlab = 'Lag ke-', ylab = 'Nilai Koefisien Korelasi', main = 'Koefisien Korelasi BBRI dan BMRI')

ccfvalues2 =ccf(BBRI,BBNI, lag.max = 30)
lags2 = ccfvalues2$lag
corr2 = ccfvalues2$acf
pos2 = lags2[lags2 >= 0 & lags2 <= 30]
pos12 = corr2[lags2 >= 0 & lags2 <= 30]
ccfdata2 = data.frame(Lag = pos2, Korelasi = pos12)
ccfdata2
plot(pos2,pos12, type ='h', xlab = 'Lag ke-', ylab = 'Nilai Koefisien Korelasi', main = 'Koefisien Korelasi BBRI dan BBNI')

# Menghitung cross-correlation function (CCF) antara BBRI dan BMRI
ccfvalues1 = ccf(BBRI, BMRI, lag.max = 30, plot = FALSE)
lags = ccfvalues1$lag
corr = ccfvalues1$acf

# Memfilter lag dan nilai korelasi untuk lag >= 0 dan <= 30
pos = lags[lags >= 0 & lags <= 30]
pos1 = corr[lags >= 0 & lags <= 30]

# Membuat data frame untuk hasil CCF
ccfdata = data.frame(Lag = pos, Korelasi = pos1)

# Menampilkan data frame
print(ccfdata)

# Menghitung interval kepercayaan 95%
n = length(BBRI) # Asumsikan panjang data BBRI sama dengan BMRI
conf_interval = qnorm(0.975) / sqrt(n)

# Menentukan korelasi tertinggi dan terendah
max_corr = max(pos1)
min_corr = min(pos1)
max_lag = pos[which.max(pos1)]
min_lag = pos[which.min(pos1)]

# Membuat plot dengan warna biru dan menambahkan interval kepercayaan
plot(pos, pos1, type = 'h', col = 'lightblue', lwd = 2,
     xlab = 'Lag ke-', ylab = 'Nilai Koefisien Korelasi',
     main = 'Koefisien Korelasi BBRI dan BMRI',
     ylim = c(-1, 1)) # Zoom out untuk menampilkan interval kepercayaan

# Menambahkan garis horizontal untuk interval kepercayaan
abline(h = c(-conf_interval, conf_interval), col = 'red', lwd = 2)

# Menambahkan garis horizontal pada y = 0 untuk referensi
abline(h = 0, col = 'black', lwd = 2)

# Menambahkan garis horizontal untuk korelasi tertinggi dan terendah
abline(h = max_corr, col = 'darkgreen', lwd = 2, lty = 2)
abline(h = min_corr, col = 'darkorchid2', lwd = 2, lty = 2)

# Menambahkan legenda untuk garis-garis yang ditambahkan
legend("topright", legend = c("Selang Kepercayaan 95%", "Nilai Korelasi Tertinggi", "Nilai Korelasi Terendah"),
       col = c("red", "darkgreen", "darkorchid2"), lwd = 2, lty = c(1, 2, 2))

# Menambahkan grid untuk referensi visual yang lebih baik
grid()

# Menghitung cross-correlation function (CCF) antara BBRI dan BBTN
ccfvalues3 = ccf(BBRI, BBTN, lag.max = 30, plot = FALSE)
lags3 = ccfvalues3$lag
corr3 = ccfvalues3$acf

# Memfilter lag dan nilai korelasi untuk lag >= 0 dan <= 30
pos3 = lags3[lags3 >= 0 & lags3 <= 30]
pos13 = corr3[lags3 >= 0 & lags3 <= 30]

# Membuat data frame untuk hasil CCF
ccfdata3 = data.frame(Lag = pos3, Korelasi = pos13)

# Menampilkan data frame
print(ccfdata3)

# Menghitung interval kepercayaan 95%
n = length(BBRI) # Asumsikan panjang data BBRI sama dengan BBTN
conf_interval = qnorm(0.975) / sqrt(n)

# Menentukan korelasi tertinggi dan terendah
max_corr3 = max(pos13)
min_corr3 = min(pos13)
max_lag3 = pos3[which.max(pos13)]
min_lag3 = pos3[which.min(pos13)]

# Membuat plot dengan warna biru dan menambahkan interval kepercayaan
plot(pos3, pos13, type = 'h', col = 'lightblue', lwd = 2,
     xlab = 'Lag ke-', ylab = 'Nilai Koefisien Korelasi',
     main = 'Koefisien Korelasi BBRI dan BBTN',
     ylim = c(-1, 1)) # Zoom out untuk menampilkan interval kepercayaan

# Menambahkan garis horizontal untuk interval kepercayaan
abline(h = c(-conf_interval, conf_interval), col = 'red', lwd = 2)

# Menambahkan garis horizontal pada y = 0 untuk referensi
abline(h = 0, col = 'black', lwd = 2)

# Menambahkan garis horizontal untuk korelasi tertinggi dan terendah
abline(h = max_corr3, col = 'darkgreen', lwd = 2, lty = 2)
abline(h = min_corr3, col = 'darkorchid2', lwd = 2, lty = 2)

# Menambahkan legenda untuk garis-garis yang ditambahkan
legend("topright", legend = c("Selang Kepercayaan 95%", "Nilai Korelasi Tertinggi", "Nilai Korelasi Terendah"),
       col = c("red", "darkgreen", "darkorchid2"), lwd = 2, lty = c(1, 2, 2))

# Menambahkan grid untuk referensi visual yang lebih baik
grid()


# Menghitung cross-correlation function (CCF) antara BBRI dan BBNI
ccfvalues2 = ccf(BBRI, BBNI, lag.max = 30, plot = FALSE)
lags2 = ccfvalues2$lag
corr2 = ccfvalues2$acf

# Memfilter lag dan nilai korelasi untuk lag >= 0 dan <= 30
pos2 = lags2[lags2 >= 0 & lags2 <= 30]
pos12 = corr2[lags2 >= 0 & lags2 <= 30]

# Membuat data frame untuk hasil CCF
ccfdata2 = data.frame(Lag = pos2, Korelasi = pos12)

# Menampilkan data frame
print(ccfdata2)

# Menghitung interval kepercayaan 95%
n = length(BBRI) # Asumsikan panjang data BBRI sama dengan BBNI
conf_interval = qnorm(0.975) / sqrt(n)

# Menentukan korelasi tertinggi dan terendah
max_corr = max(pos12)
min_corr = min(pos12)
max_lag = pos2[which.max(pos12)]
min_lag = pos2[which.min(pos12)]

# Membuat plot dengan warna biru dan menambahkan interval kepercayaan
plot(pos2, pos12, type = 'h', col = 'lightblue', lwd = 2,
     xlab = 'Lag ke-', ylab = 'Nilai Koefisien Korelasi',
     main = 'Koefisien Korelasi BBRI dan BBNI',
     ylim = c(-1, 1)) # Zoom out untuk menampilkan interval kepercayaan

# Menambahkan garis horizontal untuk interval kepercayaan
abline(h = c(-conf_interval, conf_interval), col = 'red', lwd = 2)

# Menambahkan garis horizontal pada y = 0 untuk referensi
abline(h = 0, col = 'black', lwd = 2)

# Menambahkan garis horizontal untuk korelasi tertinggi dan terendah
abline(h = max_corr, col = 'darkgreen', lwd = 2, lty = 2)
abline(h = min_corr, col = 'darkorchid2', lwd = 2, lty = 2)

# Menambahkan legenda untuk garis-garis yang ditambahkan
legend("topright", legend = c("Selang Kepercayaan 95%", "Nilai Korelasi Tertinggi", "Nilai Korelasi Terendah"),
       col = c("red", "darkgreen", "darkorchid2"), lwd = 2, lty = c(1, 2, 2))
# Menambahkan grid untuk referensi visual yang lebih baik
grid()

# BBRI BBTN
ccfvalues3 =ccf(BBRI,BBTN, lag.max = 30)
lags3 = ccfvalues3$lag
corr3 = ccfvalues3$acf
pos3 = lags3[lags3 >= 0 & lags3 <= 30]
pos13 = corr3[lags3 >= 0 & lags3 <= 30]
ccfdata3 = data.frame(Lag = pos3, Korelasi = pos13)
ccfdata3
plot(pos3,pos13, type ='h', xlab = 'Lag ke-', ylab = 'Nilai Koefisien Korelasi', main = 'Koefisien Korelasi BBRI dan BBTN')

alldata1= ts.intersect(BBRI, BBRI1 = lag(BBRI, -1), BBTN1 = lag(BBTN,-4),BBTN2= lag(BBTN,-5),
                       BBTN3= lag(BBTN,-6),BBTN4= lag(BBTN,-7),BBTN5= lag(BBTN,-8))
tryit1= lm(BBRI~BBTN1+BBTN4+BBTN5, data = alldata1)
summary(tryit1)
acf2(residuals(tryit1), main = "Plot ACF dan PACF Residual")
tryit2= lm(BBRI~BBRI1+BBTN1+BBTN4+BBTN5, data = alldata1)
summary(tryit2)

# BBRI BBNI
alldata2= ts.intersect(BBRI, BBRI1 = lag(BBRI, -1), BBRI2= lag(BBRI, -2),BBRI3= lag(BBRI, -3),BBRI4= lag(BBRI, -4),BBRI5= lag(BBRI, -5),
                       BBRI6= lag(BBRI, -6),BBRI7= lag(BBRI, -7), BBRI8= lag(BBRI, -8), BBNI1 = lag(BBNI,-3),BBNI2= lag(BBNI,-4),BBNI3= lag(BBNI,-5),
                       BBNI4= lag(BBNI,-6),BBNI5= lag(BBNI,-7), BBNI6= lag(BBNI,-8), BBNI7= lag(BBNI,-9),BBNI8= lag(BBNI,-10))
tryit3= lm(BBRI~BBNI2+BBNI6+BBNI8, data = alldata2)
summary(tryit3)
acf2(residuals(tryit3), main= "Plot ACF dan PACF Residual")
tryit4= lm(BBRI~BBRI1+BBNI2+BBNI6+BBNI8, data = alldata2)
summary(tryit4)


# BBRI BMRI
alldata3= ts.intersect(BBRI,BBRI1 = lag(BBRI, -1),BMRI1 = lag(BMRI,-25),BMRI2= lag(BMRI,-26),
                       BMRI3= lag(BMRI,-27),BMRI4= lag(BMRI,-28),BMRI5= lag(BMRI,-29),BMRI6= lag(BMRI,-30))
tryit5= lm(BBRI~BMRI1+BMRI4+BMRI6, data = alldata3)
summary(tryit5)
acf2(residuals(tryit5), main= "Plot ACF dan PACF Residual")
tryit6= lm(BBRI~BBRI1+BMRI1+BMRI4+BMRI6, data = alldata3)
summary(tryit6)
