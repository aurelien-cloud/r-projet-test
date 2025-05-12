

library(readr)
library(data.table)
library(tidyverse)
library(stringr)
library(forecast)

ma_tab <- read_tsv("C:/Users/cepe-s4-09/Desktop/ravary_12052025/estat_ei_cphi_m.tsv")


don <- read.csv("C:/Users/cepe-s4-09/Desktop/ravary_12052025/estat_ei_cphi_m.tsv")

don <- fread("C:/Users/cepe-s4-09/Desktop/ravary_12052025/estat_ei_cphi_m.tsv")


names(don)

don2 <- don |> 
  rename(serie_chrono = "freq,unit,s_adj,indic,geo\\TIME_PERIOD") |>
  pivot_longer(-serie_chrono) |>
  mutate(value2 = if_else(str_detect(value, ";"), NA_real_, as.numeric(value)))


don3 <- don2 |>
  filter(serie_chrono == "M,HICP2015,NSA,CP-HI11,FR")


summary(ma_tab)

ts_data <- ts(don3$value)
modele <- auto.arima(ts_data)
summary(modele)

grep(" d", don[1,])
grep(" d", don)


for (ii in 1:nrow(don)) {
  ind = grep(" d", don[ii,])
  tmp <- don[ii, ind]
  tmp2 <- str_replace(tmp, " d", "")
  don2[ii, ind] <- tmp2
  don2[ii,] <- as.numeric(don2[ii,])
}

don2[1:4, 1:4]
don3 <- as.numeric(don2)





df <- read_tsv("C:/Users/cepe-s4-09/Desktop/ravary_12052025/estat_ei_cphi_m.tsv")


# Séparation de la première colonne en plusieurs
col_names <- strsplit(names(df), ",")[[1]]
df_sep <- df %>% 
  separate(`freq,unit,s_adj,indic,geo\\TIME_PERIOD`, into = col_names, sep = ",")

colnames(df_sep)[5] <- "geo"

# Suppression des "d"
df_clean <- df_sep %>%
  mutate(across(-c(1:5), ~ parse_number(.)))

sapply(df_clean, class)
table(df_clean[,"freq"])
table(df_clean[,"unit"])
table(df_clean[,"s_adj"])
table(df_clean[,"indic"])
table(df_clean[,"geo"])




library(tidyr)
library(dplyr)
library(lubridate)

file <- "estat_ei_cphi_m.tsv"

raw <- read.delim("C:/Users/cepe-s4-09/Desktop/ravary_12052025/estat_ei_cphi_m.tsv", header=TRUE, row.names=NULL, na.strings = ":")

# Tidy et format long
d01 <- raw %>%
  separate(freq.unit.s_adj.indic.geo.TIME_PERIOD, sep = ",", into = c("freq", "unit", "s_adj", "indic", "geo")) %>%
  pivot_longer(-c(freq, unit, s_adj, indic, geo), names_to = "date") %>%
  mutate(date = ym(gsub("^X", "", date)),
         value = as.numeric(gsub("([a-z])|([[:space:]])", "", value)))

# Que des donnees NSA mensuelles, 3 unit différents
table(d01$s_adj)
table(d01$freq)
table(d01$indic)
table(d01$geo)
table(d01$unit)


# Passer l'inflation en colonne
d02 <- d01 %>%
  filter(unit == "HICP2015") %>%
  pivot_wider(names_from = indic, values_from = value, id_cols = c(geo, date))

nom <- names(d02)
nom2 <- str_replace(nom,"-","_")
names(d02) <- nom2
ggplot(d02,aes(x=date,y=CP_HI00,col=geo))+geom_line()



#### Y a t il des groupes de pays qui se ressemblent 
#### en considérant la série CP_HI00
tmp <- d02[,1:3]
CPHI00 <- pivot_wider(tmp,names_from = geo,values_from = CP_HI00)
summary(CPHI00)
nbna <- apply(is.na(CPHI00), 2, sum)




# Premier point dispo pour chaque pays x variable ?
out1 <- d02 %>%
  pivot_longer(-c(geo, date)) %>%
  filter(!is.na(value)) %>%
  group_by(geo, name) %>%
  summarise(min = min(date)) %>%
  ungroup() %>%
  pivot_wider(values_from = min)

# Retirer les groupes de pays + les pays pour lesquels on n'a pas d'obs avant 2000 + le UK car plus d'obs après 2020
# On garde les obs entre 2000 et 2024, comme ça pas de NA
out2 <- out1 %>%
  pivot_longer(-c(geo)) %>%
  mutate(if_else(is.na(value), today(), value)) %>%
  group_by(geo) %>%
  summarise(min = max(value)) %>%
  ungroup() %>%
  filter(year(min) < 2000)

d03 <- d02 %>%
  filter(!geo %in% c("EA20", "EU27_2020", "UK"),
         geo %in% out2$geo,
         year(date) >= 2000,
         year(date) < 2025) %>%
  arrange(geo, date)

# Check pas de NA
sum(is.na(d03))


#### Y a t il des groupes de pays qui se ressemblent 
#### en considérant la série CP_HI00
tmp <- d02[,1:3]
CPHI00 <- pivot_wider(tmp,names_from = geo,values_from = CP_HI00)
summary(CPHI00)
nbna <- apply(is.na(CPHI00),2,sum)
donT <- CPHI00[,nbna<2]
dim(donT)
summary(donT)
plot(apply(is.na(donT),1,sum)) #le NA est bien sur la dernière obs
donT <- donT[-nrow(donT),]
summary(donT)
matplot(,donT[,-1],type="l")
#### classif
#### quelle distance ? quelle méthode ? Que faire de TR ?

donT_2 <- donT %>%
  pivot_longer(-1)

donT_2_sansTR <- donT_2 %>%
  filter(name != "TR")


donT_2_TR <- donT_2 %>%
  filter(name == "TR")

dondate <- donT

donpays <- t(donT[,-1])

colnames(donpays) <- donT$date

matd1 <- dist(donpays, "manhattan")
cahs <- hclust(matd1, "single")
plot(as.dendrogram(cahs))
plot(sort(cahs$height, dec = T), type = "h")

gp4 <- cutree(cahs, k=4)

matplot(,donT[,-1], type = "l", col=gp4, lwd=2)

plot(donT_2_sansTR)

donTsT <- select(donT, -TR)
matd2 <- dist(donTsT)
caha <- hclust(matd1, "average")
plot(as.dendrogram(caha))
plot(sort(caha$height, dec=T), type="h")

gp5 <- cutree(caha, k=5)
plot(gp5)


split(rownames)

donfr <- select(CPHI00, date, FR)
plot(donfr)
donfr

library(ggplot2)

ggplot(donfr, aes(x=date, y=FR)) +
         geom_line()

don <- data.frame(date = donfr[-1, "date"], infl = diff(donfr$FR))

ggplot(don, aes(x = date, y = infl)) + geom_line()

infts <- ts(donfr$FR, start=c(1996, 01), frequency=12)
plot(infts)
tmp <- stl(infts, s.window = 12, t.window=1000)


plot(tmp)


#########################################
#########################################


tsapp <- ts(diff(donfr$FR[1:324]), start=c(1996, 02), frequency=12)

tstest <- ts(diff(donfr$FR[324:nrow(donfr)]), start=c(2023, 01), frequency=12)

Y <- as.vector(tsapp)

don <- data.frame(Y=Y)

for (ii in 1:12) {
  don[, ii + 1] <- lag(Y, n=ii)
}

dim(don)
names(don) <- c("Y", paste("xd", 1:12, sep=""))
head(don)

don <- don[-(1:13),]


date <- rep(1996:2022, each=12)




