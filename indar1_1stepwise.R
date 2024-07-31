#====================packages
#instalasi dan pemanggilan paket yang diperlukan
install.packages("dplyr")
install.packages("readr")
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")
install.packages("vars")
install.packages("readr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("caret")
install.packages("car")
install.packages("tseries")
install.packages("vars")
install.packages("tdyr")

library(dplyr)
library(readr)
library(forecast)
library(tseries)
library(ggplot2)
library(vars)
library(readr)
library(ggplot2)
library(lubridate)
library(caret)
library(car)
library(tseries)
library(vars)
library(tidyr)
#====================dataset
#membaca file csv
arm_tj<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/jumlah_armada_tj.csv")
colnames(arm_tj)
head(arm_tj)

pen_lrt<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/jumlah_penumpang_lrt.csv")
colnames(pen_lrt)<-c("bulan","tahun","jumlah_penumpang_lrt")
colnames(pen_lrt)
head(pen_lrt)

pen_mrt<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/jumlah_penumpang_mrt.csv")
colnames(pen_mrt)<-c("bulan","tahun","jumlah_penumpang_mrt")
colnames(pen_mrt)
head(pen_mrt)

perj_lrt<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/jumlah_perjalanan_lrt.csv")
colnames(perj_lrt)<-c("bulan","tahun","jumlah_perjalanan_lrt")
colnames(perj_lrt)
head(perj_lrt)

perj_mrt<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/jumlah_perjalanan_mrt.csv")
colnames(perj_mrt)<-c("bulan","tahun","jumlah_perjalanan_mrt")
colnames(perj_mrt)
head(perj_mrt)

#lihat data test & samsub
samsub<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/sample_submision.csv")
head(samsub)
test_data<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/testing_jumlah_penumpang_tj.csv")

#data training (data y)
train_pen_tj<-read.csv2("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/training_jumlah_penumpang_tj.csv")
head(train_pen_tj) #data 1/2015 sd 12/2023
tail(train_pen_tj)

#=================
#data x
#menggabungkan csv based bulan tahun
datax <- arm_tj %>%
  left_join(pen_lrt, by = c("bulan", "tahun")) %>%
  left_join(perj_lrt, by = c("bulan", "tahun")) %>%
  left_join(pen_mrt, by = c("bulan", "tahun")) %>%
  left_join(perj_mrt, by = c("bulan", "tahun"))

#lihat hasil penggabungan
head(datax) #data 1/2023 sd 5/2024

#=================
#menggabungkan datax & data training
#1. isi na pada datax periode sebelum 1/2023
full_dates<-expand.grid(
  bulan=1:12,
  tahun=2015:2024
)

#gabung data y dengan tanggal penuh
train_data_full<-full_dates %>%
  left_join(train_pen_tj,by=c("bulan","tahun"))
head(train_data_full)
tail(train_data_full)

#gabung data x dengan tanggal penuh dan isi NA dengan nile yang proper
datax_full<-full_dates %>%
  left_join(datax,by=c("bulan","tahun")) %>%
  fill(-bulan,-tahun, .direction="downup")
head(datax_full)
tail(datax_full)

#gabung datax & datay based bulan & tahun
train_data_combined<-train_data_full %>%
  left_join(datax_full,by=c("bulan","tahun"))
train_data_subset <- train_data_combined %>%
  filter(
    (tahun == 2023 & bulan >= 1) | 
      (tahun == 2024 & bulan <= 5)
  )

head(train_data_subset)
tail(train_data_subset)

#memisahkan data x & data y dalam data training
train_X <- dplyr::select(train_data_subset, -jumlah_penumpang)
train_y <- train_data_subset$jumlah_penumpang

head(train_data_subset)
head(train_X)
tail(train_X)
head(train_y)
tail(train_y)

#gabung data x & y utk buat model
train_combined<-cbind(train_X, jumlah_penumpang = train_y)
head(train_combined)
tail(train_combined)

#model awal semua fitur
initial_model<-lm(jumlah_penumpang~.,data = train_combined)

#do stepwise regression
stepwise_model<-step(initial_model,direction = "both")

#ringkasan model akhir
summary(stepwise_model)

#=======prediksi
# Misalkan ini adalah train_data_subset yang sudah ada
head(train_data_subset)
train_data_subset<-data.frame(train_data_subset)

#ubah NA jadi 0
train_data_subset[is.na(train_data_subset)]<-0

# Misalkan test_data hanya memiliki kolom bulan dan tahun
test_data <- data.frame(
  bulan = 1:6,
  tahun = rep(2024, 6)
)

# Menambahkan kolom yang diperlukan ke test_data
required_columns <- setdiff(names(train_data_subset), "jumlah_penumpang")

for (col in required_columns) {
  if (!(col %in% names(test_data))) {
    test_data[[col]] <- NA
  }
}

# Memastikan test_data memiliki semua kolom yang diperlukan oleh model
test_data <- test_data %>%
  select(all_of(required_columns))

# Memeriksa dan mengisi NA pada test_data jika diperlukan
# Jika Anda memiliki nilai default atau cara mengisi nilai NA, tambahkan di sini

# Misalkan nilai default adalah nilai rata-rata dari training data
# Contoh pengisian nilai NA dengan nilai rata-rata kolom yang sesuai
for (col in required_columns) {
  if (any(is.na(test_data[[col]]))) {
    test_data[[col]] <- mean(train_data_subset[[col]], na.rm = TRUE)
  }
}

# Lakukan prediksi dengan model stepwise
predictions <- predict(stepwise_model, newdata = test_data)

# Tambahkan hasil prediksi ke test_data
test_data$predicted_jumlah_penumpang <- predictions

# Tampilkan beberapa baris hasil prediksi untuk verifikasi
print(head(test_data))

#========untuk keperluan menghitung RMSE
#test_rmse hanya memiliki kolom bulan dan tahun
test_rmse <- data.frame(
  bulan = 1:12,
  tahun = rep(2023, 12)
)

# Menambahkan kolom yang diperlukan ke test_data
required_columns_rmse <- setdiff(names(train_data_subset), "jumlah_penumpang")

for (col in required_columns_rmse) {
  if (!(col %in% names(test_rmse))) {
    test_rmse[[col]] <- NA
  }
}

# Memastikan test_data memiliki semua kolom yang diperlukan oleh model
test_rmse <- test_rmse %>%
  select(all_of(required_columns))

#misalkan nilai default adalah nilai rata-rata dari training data
for (col in required_columns_rmse) {
  if (any(is.na(test_rmse[[col]]))) {
    test_rmse[[col]] <- mean(train_data_subset[[col]], na.rm = TRUE)
  }
}

# Lakukan prediksi dengan model stepwise
predictions_rmse <- predict(stepwise_model, newdata = test_rmse)

# Tambahkan hasil prediksi ke test_data
test_rmse$predicted_jumlah_penumpang <- predictions

# Tampilkan beberapa baris hasil prediksi untuk verifikasi
print(head(test_rmse))
print(tail(test_rmse))

train_data_rmse <- train_data_full %>%
  filter(
    (tahun == 2023 & bulan >= 1) | 
      (tahun == 2023 & bulan <= 12)
  )

print(head(train_data_rmse))
print(tail(train_data_rmse))

actual<-train_data_rmse$jumlah_penumpang
predicted<-test_rmse$predicted_jumlah_penumpang

#rmse
#fungsi untuk menghitung RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Menghitung RMSE
rmse_value <- rmse(actual, predicted)

# Tampilkan nilai RMSE
print(paste("RMSE:", rmse_value))

#save csv hasil jumlah_penumpang ke samsub
head(samsub)
colnames(samsub)<-c("bulan","jumlah_penumpang")
colnames(samsub)

print(test_data$predicted_jumlah_penumpang)

#gabungkan data2 dengan data1 berdasarkan bulan
samsub$jumlah_penumpang <- test_data$predicted_jumlah_penumpang

# Tampilkan hasil
print(samsub)

#simpan data frame ke file CSV
write.csv(samsub, file = "D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/Indar1_1 Stepwise/sample_submision.csv", row.names = FALSE)
samsub<-read.csv("D:/@ALMIRA/14. Indaton/4 Round 1/3 Data Running/Indar1_1 Stepwise/sample_submision.csv")
head(samsub)
colnames(samsub)<-c("id","jumlah_penumpang")
