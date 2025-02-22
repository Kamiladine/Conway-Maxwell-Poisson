# Memasang paket yang diperlukan
install.packages("MASS")
install.packages("COMPoissonReg")
install.packages("outliers")
install.packages("ggplot")

library(outliers)
library(MASS)
library(readxl)
library(car)
library(COMPoissonReg)
library(lmtest)
library(ggplot2)
library(vcd)


#input data
data <- read_excel("C:/skripsi_2/Jumlah kematian bayi jabar 2023.xlsx")

#eksplorasi data
View(data)
summary(data)

rangeY <- max(data$Y)-min(data$Y)
rangeX1 <- max(data$X1)-min(data$X1)
rangeX2 <- max(data$X2)-min(data$X2)
rangeX3 <- max(data$X3)-min(data$X3)
rangeX4 <- max(data$X4)-min(data$X4)
rangeX5 <- max(data$X5)-min(data$X5)
rangeX6 <- max(data$X6)-min(data$X6)
rangeX7 <- max(data$X7)-min(data$X7)
rangeX8 <- max(data$X8)-min(data$X8)
range_all <- data.frame(cbind(rangeY,rangeX1,rangeX2,rangeX3,rangeX4,
                              rangeX5,rangeX6, rangeX7, rangeX8))
range_all

sdvy <- sd(data$Y)
sdvX1 <- sd(data$X1)
sdvX2 <- sd(data$X2)
sdvX3 <- sd(data$X3)
sdvX4 <- sd(data$X4)
sdvX5 <- sd(data$X5)
sdvX6 <- sd(data$X6)
sdvX7 <- sd(data$X7)
sdvX8 <- sd(data$X8)
SDV_all <- data.frame(cbind(sdvy,sdvX1, sdvX2 , sdvX3, sdvX4,
                            sdvX5, sdvX6, sdvX7, sdvX8))
SDV_all

#standarisasi data
data2 <- read_excel("C:/skripsi_2/Jumlah kematian bayi jabar 2023.xlsx")
#variabel yang sudah distandarisasi
data_scaled2 <- as.data.frame(scale(data2[, c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")]))
data_scaled2$Y <- data2$Y
data_scaled2$t <- data2$t
View(data_scaled2)

# Cek multikolinearitas
model <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data = data)
vif(model)

#cek data berdistribusi poisson atau tidak
lambda <- mean(data2$Y)
hist(data2$Y, 
     freq = FALSE, 
     main = "Observed vs Theoretical Poisson", 
     xlab = "Jumlah Kematian Bayi", 
     col = "lightblue")
points(0:max(data2$Y), 
       dpois(0:max(data2$Y), lambda), 
       col = "red", 
       type = "b", pch = 70)
legend("topright", legend = c("Observed", "Theoretical Poisson"), col = c("blue", "red"), pch = 19)

#cek distribusi pada data
model_poisson <- glm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + offset(log(t)),
                     family = poisson, data = data)
vif(model_poisson)
# Prediksi nilai lambda
lambda <- fitted(model_poisson)
# Frekuensi aktual
observed <- table(data$Y) 
# Frekuensi harapan berdasarkan distribusi Poisson
expected <- dpois(as.numeric(names(observed)), lambda = mean(data$Y)) * sum(observed)
# Pastikan jumlah kategori mencukupi
expected <- ifelse(expected < 5, 5, expected) # Gabungkan kategori kecil jika diperlukan
# Uji Chi-Square
chisq_test <- chisq.test(x = observed, p = expected / sum(expected))
print(chisq_test)

# Fitting model regresi Poisson
model_poisson2 <- glm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 +X7 +X8,offset(log(t)), data = data_scaled2, family = poisson())
summary(model_poisson2)
lambda <- fitted(model_poisson2)
vif(model_poisson2)
# Menghitung deviance dan degrees of freedom
deviance <- model_poisson2$deviance
print(deviance)
# Menghitung Pearson Chi-Square
pearson_chisq <- sum((data_scaled2$Y - lambda)^2 / lambda)
print(pearson_chisq)
# Menghitung degrees of freedom
df_residual <- model_poisson2$df.residual
print(df_residual)
# Menghitung ratio deviance per degrees of freedom (dispersion parameter)
dispersion <- deviance / df_residual
print(dispersion)
# Menghitung nilai dispersi dengan Pearson Chi-Square
dispersion_pearson <- pearson_chisq / df_residual
print(dispersion_pearson)

#FIT CMP
com_pois_model <- glm.cmp(Y ~ X1 + X2 + X3 + X4 + X5 + X6+ X7+ X8+offset(log(t)), data = data_scaled2)
summary(com_pois_model)

# Hitung derajat bebas (db)
n <- nrow(data_scaled2)  # Jumlah observasi
p <- length(coef(model_poisson2))  # Jumlah parameter dalam model
p
df <- n - p  # Derajat bebas
cat("Degrees of Freedom (df):", df, "\n")

# Hitung nilai deviance
# Deviance dapat dihitung menggunakan fungsi log-likelihood model
loglik_saturated <- sum(dpois(data_scaled2$Y, lambda = data_scaled2$Y, log = TRUE))  # Log-likelihood model saturasi
loglik_fitted <- logLik(com_pois_model)  # Log-likelihood model fitted
deviance_value <- 2 * (loglik_saturated - loglik_fitted)  # Deviance
cat("Deviance:", deviance_value, "\n")

# Hitung Pearson Chi-Square
mu_hat <- predict(com_pois_model, type = "response")  # Prediksi rata-rata (μ)
residual_pearson <- (data_scaled2$Y - mu_hat) / sqrt(mu_hat)  # Residual Pearson
chi_square <- sum(residual_pearson^2)  # Pearson Chi-Square
cat("Pearson Chi-Square:", chi_square, "\n")

# Hitung nilai dispersi chi-square
dispersion <- chi_square / df
cat("Nilai Dispersi (v):", dispersion, "\n")

# Hitung nilai dispersi berbasis deviance
dispersion_deviance <- deviance_value / df
cat("Nilai Dispersi Berbasis Deviance (v):", dispersion_deviance, "\n")

cmp_null <- glm.cmp(Y ~ offset(log(t)), data = data_scaled2)
# Log-likelihood model tereduksi
loglik_null <- logLik(cmp_null)
# Log-likelihood model penuh
loglik_full <- logLik(com_pois_model)
# Hitung G-statistic
G_stat <- 2 * (loglik_full - loglik_null)
# Derajat kebebasan (jumlah parameter tambahan di model penuh)
df <- length(com_pois_model$coefficients) - length(cmp_null$coefficients)
# P-value untuk G-statistic
p_value <- pchisq(G_stat, df = df, lower.tail = FALSE)
cat("G-statistic:", G_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-value:", p_value, "\n")

if (p_value < 0.05) {
  cat("Hasil: Model penuh memberikan peningkatan signifikan dibandingkan model tereduksi.\n")
} else {
  cat("Hasil: Model penuh tidak memberikan peningkatan signifikan dibandingkan model tereduksi.\n")
}

# Log-Likelihood
loglik_null <- logLik(cmp_null)
loglik_full <- logLik(com_pois_model)

# G-Statistic
G_stat <- 2 * (loglik_full - loglik_null)

# Degrees of Freedom (Jumlah parameter prediktor)
df <- length(com_pois_model$coefficients) - length(cmp_null$coefficients)

# Nilai Kritis Chi-Square
alpha <- 0.05
chi_critical <- qchisq(1 - alpha, df = df)

# Keputusan
cat("G-Statistic:", G_stat, "\n")
cat("Chi-Square Critical Value (df =", df, "):", chi_critical, "\n")

if (G_stat > chi_critical) {
  cat("Keputusan: Tolak H0. Setidaknya ada satu variabel prediktor yang signifikan.\n")
} else {
  cat("Keputusan: Gagal menolak H0. Tidak ada bukti bahwa prediktor signifikan.\n")
}

# AIC dan BIC untuk model Poisson
AIC(model_poisson2)
BIC(model_poisson2)
# AIC dan BIC untuk model Conway-Maxwell Poisson
AIC(com_pois_model)
BIC(com_pois_model)

