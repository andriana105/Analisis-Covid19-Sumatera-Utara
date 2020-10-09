library(httr)
library(dplyr)
library(hrbrthemes)
library(ggplot2)
library(tidyr)

#Access API
resp <- GET ("https://data.covid19.go.id/public/api/update.json")

#Check status code
status_code (resp)

#Get Data for North Sumatera Province
resp_sumut <- GET("https://data.covid19.go.id/public/api/prov_detail_SUMATERA_UTARA.json")
cov_sumut_raw <- content(resp_sumut, as="parsed", simplifyVector = TRUE)
names(cov_sumut_raw)

#Berapa jumlah total kasus COVID-19 di Sumatera Utara?
cov_sumut_raw$kasus_total

#Berapa persentase kematian akibat COVID-19 di Sumatera Utara?
cov_sumut_raw$meninggal_persen

#Berapa persentase tingkat kesembuhan dari COVID-19 di Sumatera Utara?
cov_sumut_raw$sembuh_persen

cov_sumut <- cov_sumut_raw$list_perkembangan
str(cov_sumut)

head(cov_sumut)

new_cov_sumut <- 
  cov_sumut %>%
  select(-contains("DIRAWAT_OR_ISOLASI")) %>%
  select(-starts_with("AKUMULASI")) %>%
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>%
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )

str(new_cov_sumut)

ggplot(new_cov_sumut, aes(tanggal, kasus_baru)) +
  geom_col(fill = "salmon") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Positif COVID-19 di Sumatera Utara",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

ggplot(new_cov_sumut, aes(tanggal, sembuh)) +
  geom_col(fill = "olivedrab2") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Sembuh Dari COVID-19 di Sumatera Utara",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

ggplot(new_cov_sumut, aes(tanggal, meninggal)) +
  geom_col(fill = "darkslategray4") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Meninggal Akibat COVID-19 di Jawa Barat",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
