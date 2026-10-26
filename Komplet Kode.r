library(dkstat)
library(tidyr)
library(ggplot2)



# ========================================================================
# OPGAVE 1.1 | FORV1 OG KLARGØRING AF DATA
# ========================================================================

#### FORV1 - FORBRUGERFORVENTNINGER

#henter metadata
forv1_meta <- dst_meta(table = "FORV1", lang = "da")

#vælger de 11 spørgsmål
spoergsmaal <- forv1_meta$values$INDIKATOR$text

spoergsmaal <- spoergsmaal[
  !grepl("Forbrugertillidsindikatoren|Anser det som fornuftigt", spoergsmaal)
]

#henter data
forv1 <- dst_get_data(
  table = "FORV1",
  query = list(
    INDIKATOR = spoergsmaal,
    Tid = "*"
  ),
  meta_data = forv1_meta,
  lang = "da",
  parse_dst_tid = FALSE
)

#beholder januar 2000 og frem
forv1 <- forv1[forv1$TID >= "2000M01", ]

#laver spørgsmålene til kolonner
forv1 <- pivot_wider(
  forv1,
  id_cols = TID,
  names_from = INDIKATOR,
  values_from = value
)

forv1 <- as.data.frame(forv1)
forv1 <- forv1[order(forv1$TID), ]
rownames(forv1) <- NULL

#laver måneder til kvartaler
maaned <- as.numeric(substr(forv1$TID, 6, 7))

kvartal <- paste0(
  substr(forv1$TID, 1, 4),
  "K",
  ceiling(maaned / 3)
)

#beregner gennemsnit pr. kvartal
forv1_kvt <- aggregate(
  forv1[, -1],
  by = list(TID = kvartal),
  FUN = mean
)

#vender fortegnene på f6 og f8
vend_spg <- grep("^F(6|8) ", names(forv1_kvt))

#vender fortegnene
forv1_kvt[,vend_spg] <- -forv1_kvt[,vend_spg]

#viser hvilke spørgsmål der er vendt
names(forv1_kvt)[vend_spg]

View(forv1_kvt)



# ========================================================================
# OPGAVE 1.1 | KOMBINATIONSALGORITME
# ========================================================================

#### BEREGNER ALLE 2047 INDIKATORER

#indikatorer med 1 spørgsmål
z1 <- matrix(0, ncol = 11, nrow = nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  z1[i,] <- as.numeric(forv1_kvt[i, 2:ncol(forv1_kvt)])
}

#indikatorer med 2 spørgsmål
z2 <- matrix(0, ncol = 55, nrow = nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  z2[i,] <- colMeans(combn(
    as.numeric(forv1_kvt[i, 2:ncol(forv1_kvt)]), 2
  ))
}

#indikatorer med 3 spørgsmål
z3 <- matrix(0, ncol = 165, nrow = nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  z3[i,] <- colMeans(combn(
    as.numeric(forv1_kvt[i, 2:ncol(forv1_kvt)]), 3
  ))
}

#indikatorer med 4 spørgsmål
z4 <- matrix(0, ncol = 330, nrow = nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  z4[i,] <- colMeans(combn(
    as.numeric(forv1_kvt[i, 2:ncol(forv1_kvt)]), 4
  ))
}

#indikatorer med 5 spørgsmål
z5 <- matrix(0, ncol = 462, nrow = nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  z5[i,] <- colMeans(combn(
    as.numeric(forv1_kvt[i, 2:ncol(forv1_kvt)]), 5
  ))
}

#indikatorer med 6 spørgsmål
z6 <- matrix(0, ncol = 462, nrow = nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  z6[i,] <- colMeans(combn(
    as.numeric(forv1_kvt[i, 2:ncol(forv1_kvt)]), 6
  ))
}

#indikatorer med 7 spørgsmål
z7 <- matrix(0, ncol = 330, nrow = nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  z7[i,] <- colMeans(combn(
    as.numeric(forv1_kvt[i, 2:ncol(forv1_kvt)]), 7
  ))
}

#indikatorer med 8 spørgsmål
z8 <- matrix(0, ncol = 165, nrow = nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  z8[i,] <- colMeans(combn(
    as.numeric(forv1_kvt[i, 2:ncol(forv1_kvt)]), 8
  ))
}

#indikatorer med 9 spørgsmål
z9 <- matrix(0, ncol = 55, nrow = nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  z9[i,] <- colMeans(combn(
    as.numeric(forv1_kvt[i, 2:ncol(forv1_kvt)]), 9
  ))
}

#indikatorer med 10 spørgsmål
z10 <- matrix(0, ncol = 11, nrow = nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  z10[i,] <- colMeans(combn(
    as.numeric(forv1_kvt[i, 2:ncol(forv1_kvt)]), 10
  ))
}

#indikatoren med alle 11 spørgsmål
z11 <- matrix(0, ncol = 1, nrow = nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  z11[i,] <- mean(as.numeric(forv1_kvt[i, 2:ncol(forv1_kvt)]))
}

#samler alle indikatorer i én matrix
zALLE <- cbind(z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11)

#giver indikatorerne navne fra x1 til x2047
colnames(zALLE) <- paste0("X", 1:2047)

#tilføjer kvartalerne som første kolonne
zALLE <- data.frame(
  TID = forv1_kvt$TID,
  zALLE
)

#kontrollerer antal indikatorer
stopifnot(ncol(zALLE) == 2048)

View(zALLE)



# ========================================================================
# OPGAVE 1.1 | REALVÆKST I PRIVATFORBRUG
# ========================================================================

#### NKN1 - HUSHOLDNINGERNES PRIVATFORBRUG

#henter metadata
privatforbrug_meta <- dst_meta("NKN1", lang = "da")

variable <- privatforbrug_meta$variables

ids <- variable$id[match(
  c("transaktion", "prisenhed", "sæsonkorrigering", "tid"),
  tolower(trimws(variable$text))
)]

stopifnot(!anyNA(ids))

#henter sæsonkorrigeret forbrug i kædede 2020-priser
privatforbrug_filters <- setNames(
  list(
    "P.31 Husholdningernes forbrugsudgifter",
    "2020-priser, kædede værdier, (mia. kr.)",
    "Sæsonkorrigeret",
    "*"
  ),
  ids
)

privatforbrug_raw <- dst_get_data(
  table = "NKN1",
  query = privatforbrug_filters,
  meta_data = privatforbrug_meta,
  lang = "da",
  parse_dst_tid = FALSE
)

#vælger perioden fra 1999k1
privatforbrug_raw <- privatforbrug_raw[
  privatforbrug_raw$TID >= "1999K1",
]

#sorterer kvartalerne
privatforbrug_raw <- privatforbrug_raw[
  order(privatforbrug_raw$TID),
]

#opretter dataframe
privatforbrug <- data.frame(
  Tidsinterval = as.character(privatforbrug_raw$TID),
  Privatforbrug = privatforbrug_raw$value
)

#kontrollerer at der ikke mangler kvartaler
kvartalsnummer <- as.integer(
  substr(privatforbrug$Tidsinterval, 1, 4)
) * 4 +
  as.integer(substr(privatforbrug$Tidsinterval, 6, 6))

stopifnot(
  nrow(privatforbrug) > 4,
  all(diff(kvartalsnummer) == 1)
)

#beregner årlig realvækst i forhold til samme kvartal året før
privatforbrug$Realvaekst <- c(
  rep(NA, 4),
  (
    privatforbrug$Privatforbrug[5:nrow(privatforbrug)] /
      privatforbrug$Privatforbrug[1:(nrow(privatforbrug) - 4)] - 1
  ) * 100
)

View(privatforbrug)



# ========================================================================
# OPGAVE 1.2 | REGRESSIONER OG R2
# ========================================================================

#### REGRESSIONER FOR ALLE 2047 INDIKATORER

#samler forbrugsvækst og indikatorer
regressionsdata <- merge(
  privatforbrug,
  zALLE,
  by.x = "Tidsinterval",
  by.y = "TID"
)

#finder de 2047 indikatorer
indikatorer <- names(zALLE)[-1]

#kontrollerer datagrundlaget
dim(regressionsdata)

stopifnot(length(indikatorer) == 2047)

stopifnot(
  !anyNA(regressionsdata[, c("Realvaekst", indikatorer)])
)

#opretter en tabel til resultater
resultater <- data.frame(
  indikator = indikatorer,
  R2 = NA_real_,
  koefficient = NA_real_,
  p_vaerdi = NA_real_
)

#beregner en lineær regression for hver indikator
for(i in 1:length(indikatorer)) {
  
  x <- regressionsdata[[indikatorer[i]]]
  
  model <- lm(regressionsdata$Realvaekst ~ x)
  
  resultater$R2[i] <- summary(model)$r.squared
  resultater$koefficient[i] <- coef(model)[2]
  resultater$p_vaerdi[i] <- summary(model)$coefficients[2,4]
}

#sorterer efter højeste r2
resultater <- resultater[order(-resultater$R2), ]

#viser de 10 bedste
print(head(resultater, 10))

View(resultater)

# ========================================================================
# OPGAVE 1.2 | BEDSTE MODEL
# ========================================================================

#### FINDER DEN BEDSTE MODEL

#finder automatisk indikatoren med højeste r2
bedste_navn <- resultater$indikator[1]

#beregner den bedste regression
bedste_model <- lm(
  reformulate(bedste_navn, response = "Realvaekst"),
  data = regressionsdata
)

#viser regressionsresultatet
print(summary(bedste_model))

# ========================================================================
# OPGAVE 1.3 | SPØRGSMÅL BAG INDIKATORER
# ========================================================================

#### FINDER SPØRGSMÅLENE BAG INDIKATORERNE

#finder spørgsmålene bag et x-nummer
find_spg <- function(n) {
  
  antal <- choose(11, 1:11)
  
  k <- which(n <= cumsum(antal))[1]
  
  placering <- n - sum(antal[1:11 < k])
  
  paste(
    sub(" .*", "", combn(names(forv1_kvt)[-1], k)[, placering]),
    collapse = ", "
  )
}

#tilføjer spørgsmålene til regressionsresultaterne
resultater$Spoergsmaal <- sapply(
  as.numeric(sub("X", "", resultater$indikator)),
  find_spg
)

#viser hvilken indikator der er bedst
cat("Bedste indikator:", bedste_navn, "\n")

#viser spørgsmålene i den bedste indikator
cat(
  "Spørgsmål:",
  resultater$Spoergsmaal[1],
  "\n"
)



# ========================================================================
# OPGAVE 1.3 | RANGERING AF INDIKATORER
# ========================================================================

#### TOP 10, TOP 50 OG TOP 100

#opretter tabellerne
top10 <- head(resultater, 10)
top50 <- head(resultater, 50)
top100 <- head(resultater, 100)

#viser top 10
print(top10)

View(top10)
View(top50)
View(top100)



# ========================================================================
# OPGAVE 1.3 | HYPPIGHED I TOP 100
# ========================================================================

#### SPØRGSMÅLSHYPPIGHED I TOP 100

#tæller hvor ofte hvert spørgsmål optræder
hyppighed100 <- sort(
  table(
    unlist(strsplit(top100$Spoergsmaal, ", "))
  ),
  decreasing = TRUE
)

#opretter en tabel med antal
hyppighed100 <- data.frame(
  Spoergsmaal = names(hyppighed100),
  Antal = as.vector(hyppighed100)
)

#viser hyppigheden
print(hyppighed100)

View(hyppighed100)


#### SIDSTE KONTROL

#kontrollerer at de rigtige spørgsmål er vendt
print(names(forv1_kvt)[vend_spg])

#kontrollerer at vi har 2047 indikatorer
print(length(indikatorer))

#kontrollerer at ingen regressioner mangler r2
print(sum(is.na(resultater$R2)))

#kontrollerer at regressionsdata bruger de nye værdier
print(all.equal(
  as.numeric(regressionsdata$X1806),
  as.numeric(zALLE$X1806[
    match(regressionsdata$Tidsinterval, zALLE$TID)
  ])
))

# ========================================================================
# OPGAVE 1.4 | FORUDSIG K3 OG K4
# ========================================================================

#henter de nyeste forbrugerdata
forv1_ny <- dst_get_data(
  table = "FORV1",
  query = list(
    INDIKATOR = spoergsmaal,
    Tid = "*"
  ),
  meta_data = forv1_meta,
  lang = "da",
  parse_dst_tid = FALSE
)

#beholder juli til oktober 2026
forv1_ny <- forv1_ny[
  forv1_ny$TID %in% c(
    "2026M07", "2026M08",
    "2026M09", "2026M10"
  ),
]

#laver spørgsmålene til kolonner
forv1_ny <- pivot_wider(
  forv1_ny,
  id_cols = TID,
  names_from = INDIKATOR,
  values_from = value
)

#sorterer månederne
forv1_ny <- as.data.frame(forv1_ny)
forv1_ny <- forv1_ny[order(forv1_ny$TID), ]

#kontrollerer at tredje kvartal er komplet
stopifnot(all(
  c("2026M07", "2026M08", "2026M09") %in% forv1_ny$TID
))

#laver måneder til kvartaler
maaned_ny <- as.numeric(substr(forv1_ny$TID, 6, 7))

kvartal_ny <- paste0(
  substr(forv1_ny$TID, 1, 4),
  "K",
  ceiling(maaned_ny / 3)
)

#beregner kvartalsgennemsnit
forv1_ny_kvt <- aggregate(
  forv1_ny[, -1],
  by = list(TID = kvartal_ny),
  FUN = mean
)

#vender fortegnene på F6 og F8
vend_spg_ny <- grep("^F(6|8) ", names(forv1_ny_kvt))

forv1_ny_kvt[, vend_spg_ny] <-
  -forv1_ny_kvt[, vend_spg_ny]

#finder spørgsmålene i X1450
spg1450 <- strsplit(
  find_spg(1450), ", "
)[[1]]

#finder de tilsvarende kolonner
kolonner1450 <- match(
  spg1450,
  sub(" .*", "", names(forv1_ny_kvt))
)

stopifnot(!anyNA(kolonner1450))

#beregner X1450 for de nye kvartaler
forv1_ny_kvt$X1450 <- rowMeans(
  forv1_ny_kvt[, kolonner1450, drop = FALSE]
)

#bruger den oprindelige regressionsmodel
model1450 <- lm(
  Realvaekst ~ X1450,
  data = regressionsdata[
    regressionsdata$Tidsinterval <= "2026K2",
  ]
)

#beregner forudsigelser med 95 procent prædiktionsinterval
forudsigelse <- predict(
  model1450,
  newdata = forv1_ny_kvt,
  interval = "prediction",
  level = 0.95
)

#samler resultaterne
prognose <- data.frame(
  Kvartal = forv1_ny_kvt$TID,
  X1450 = forv1_ny_kvt$X1450,
  Realvaekst = forudsigelse[, "fit"],
  Nedre95 = forudsigelse[, "lwr"],
  Oevre95 = forudsigelse[, "upr"]
)

#viser prognosen
print(round(prognose[, -1], 2))
View(prognose)


#### OMREGNER REALVÆKST TIL FORBRUGSUDGIFTER

#finder forbruget i samme kvartaler i 2025
forbrug2025 <- privatforbrug[
  privatforbrug$Tidsinterval %in%
    c("2025K3", "2025K4"),
  c("Tidsinterval", "Privatforbrug")
]

#sorterer kvartalerne
forbrug2025 <- forbrug2025[
  order(forbrug2025$Tidsinterval),
]

stopifnot(nrow(forbrug2025) == nrow(prognose))

#beregner forventede forbrugsudgifter
prognose$Forbrugsudgift <- forbrug2025$Privatforbrug *
  (1 + prognose$Realvaekst / 100)

#beregner nedre og øvre grænse
prognose$Forbrug_Nedre95 <- forbrug2025$Privatforbrug *
  (1 + prognose$Nedre95 / 100)

prognose$Forbrug_Oevre95 <- forbrug2025$Privatforbrug *
  (1 + prognose$Oevre95 / 100)

#viser den samlede prognose
print(prognose)
View(prognose)


#### GRAF OVER HISTORISK REALVÆKST OG PROGNOSE

#vælger historiske observationer
historisk <- regressionsdata[
  regressionsdata$Tidsinterval >= "2020K1" &
    regressionsdata$Tidsinterval <= "2026K2",
  c("Tidsinterval", "Realvaekst")
]

#samler historiske værdier og prognoser
grafdata <- data.frame(
  Kvartal = c(historisk$Tidsinterval, prognose$Kvartal),
  Realvaekst = c(historisk$Realvaekst, prognose$Realvaekst),
  Type = c(
    rep("Historisk", nrow(historisk)),
    rep("Prognose", nrow(prognose))
  )
)

#laver grafen
ggplot(grafdata, aes(x = Kvartal, y = Realvaekst)) +
  geom_line(aes(group = 1), color = "steelblue") +
  geom_point(aes(color = Type), size = 2) +
  scale_x_discrete(breaks = grafdata$Kvartal[
    seq(1, nrow(grafdata), by = 4)
  ]) +
  labs(
    title = "Husholdningernes årlige realvækst",
    subtitle = "Prognose for 2026K3 og 2026K4 med X1450",
    x = "Kvartal",
    y = "Realvækst i procent",
    caption = "Kilde: DST, Egne Beregninger",
    color = "Observation"
  ) +
  theme_minimal()


# ========================================================================
# OPGAVE 1.5 | MIKROØKONOMISK INDIKATOR
# ========================================================================

####finder den bedste mikroøkonomiske indikator

#vælger de mikroøkonomiske spørgsmål
mikro_spg <- c("F2", "F3", "F9", "F10", "F12", "F13")

#finder indikatorer der kun indeholder mikrospørgsmål
mikro_resultater <- resultater[
  sapply(strsplit(resultater$Spoergsmaal, ", "), function(x) {
    all(x %in% mikro_spg)
  }),
]

#viser de 10 bedste mikroøkonomiske indikatorer
top10_mikro <- head(mikro_resultater, 10)

print(top10_mikro)
View(top10_mikro)


# ========================================================================
# EKSTRA ANALYSE | STABILITETSTEST
# ========================================================================

####stabilitetstest med fem perioder (CHOPCHOP af fem perioder på ca. 5år og 3måneder)

#sorterer data efter kvartal
stabilitetsdata <- regressionsdata[
  order(regressionsdata$Tidsinterval), 
]

#fordeler 106 kvartaler på fem perioder
periode <- rep(1:5, c(21,21,21,21,22))

#kontrollerer datagrundlaget
stopifnot(nrow(stabilitetsdata) == 106)

#opretter en tabel til resultaterne
stabilitet <- data.frame(
  indikator = indikatorer,
  P1 = NA_real_,
  P2 = NA_real_,
  P3 = NA_real_,
  P4 = NA_real_,
  P5 = NA_real_
)
#tilføjer spørgsmålene til stabilitetsmatrixen
stabilitet$Spoergsmaal <- resultater$Spoergsmaal[
  match(stabilitet$indikator, resultater$indikator)
]
#beregner r2 for alle indikatorer i hver periode
for(p in 1:5) {
  
  data_periode <- stabilitetsdata[periode == p, ]
  
  for(i in 1:length(indikatorer)) {
    
    x <- data_periode[[indikatorer[i]]]
    
    model <- lm(data_periode$Realvaekst ~ x)
    
    stabilitet[i,p+1] <- summary(model)$r.squared
  }
}

#beregner gennemsnitlig r2
stabilitet$Gennemsnit_R2 <- rowMeans(stabilitet[,2:6])

#beregner den laveste r2
stabilitet$Laveste_R2 <- apply(stabilitet[,2:6],1,min)

#beregner standardafvigelsen
stabilitet$SD_R2 <- apply(stabilitet[,2:6],1,sd)

#sorterer efter højeste gennemsnitlige r2
stabilitet <- stabilitet[order(-stabilitet$Gennemsnit_R2), ]

#viser de 10 bedste indikatorer
print(head(stabilitet,10))

#viser hele tabellen
View(stabilitet)









#finder de tre oprindelige spørgsmål
spg <- grep("^F(6|7|8) ", names(forv1))

#beregner de oprindelige kvartalsgennemsnit
sp <- aggregate(
  forv1[,spg],
  by = list(TID = kvartal),
  FUN = mean
)

#samler spørgsmålene med realvæksten
kontrol <- merge(
  sp,
  privatforbrug,
  by.x = "TID",
  by.y = "Tidsinterval"
)

#viser korrelationen med forbruget
round(cor(kontrol[,2:4], kontrol$Realvaekst), 3)



# ========================================================================
# EKSTRA ANALYSE | TOP 10, 50 OG 100
# ========================================================================

#### TOP 10, TOP 50 OG TOP 100

#opretter tabellerne
top10stabil <- head(stabilitet, 10)
top50stabil <- head(stabilitet, 50)
top100stabil <- head(stabilitet, 100)



# ========================================================================
# EKSTRA ANALYSE | HYPPIGHED I TOP 100
# ========================================================================

#### SPØRGSMÅLSHYPPIGHED I TOP 100

#tæller hvor ofte hvert spørgsmål optræder
hyppighed100stabil <- sort(
  table(
    unlist(strsplit(top100stabil$Spoergsmaal, ", "))
  ),
  decreasing = TRUE
)
#opretter en tabel med antal
hyppighed100stabil <- data.frame(
  Spoergsmaal = names(hyppighed100stabil),
  Antal = as.vector(hyppighed100stabil)
)

top100

print(top10_mikro)


#kontrollerer antal indikatorer
ncol(zALLE) - 1

#kontrollerer antal kvartaler
nrow(regressionsdata)

#viser første og sidste kvartal
range(regressionsdata$Tidsinterval)

#viser de ti bedste indikatorer
print(top10)



#viser stabiliteten for x1450
stabilitet[stabilitet$indikator == "X1450", ]

#finder x1450's placering i stabilitetsranglisten
which(stabilitet$indikator == "X1450")

#-----------------------------------------------
# Opgave 2.1 - Multipel lineær regression med alle 11 spørgsmål
#-----------------------------------------------
model_multipel <- lm(Realvaekst ~ ., data = multipel_data)

summary(model_multipel)

library(pls)

# Brug alle 11 spørgsmål og realvæksten
pca_data <- na.omit(multipel_data)

# PCA-regression
set.seed(123)

pcr.fit11 <- pcr(
  Realvaekst ~ .,
  data = pca_data,
  scale = TRUE,
  validation = "CV"
)

summary(pcr.fit11)

# Loadings på første komponent
loadings1 <- as.matrix(pcr.fit$loadings)[, 1]

# Kvadrerede loadings som procentandele
pca_resultater <- data.frame(
  Spoergsmaal = names(loadings1),
  Loading = loadings1,
  Vaegt_procent = 100 * loadings1^2
)

# Sortér største vægt øverst
pca_resultater <- pca_resultater[
  order(pca_resultater$Vaegt_procent, decreasing = TRUE),
]

View(pca_resultater)

# ==============
# Opgave 2.3
# ==============

forudsigelse <- predict(
  pcr.fit11,
  newdata = tail(forv1_kvt, 1),
  ncomp = 1
)

forudsigelse

# ==============
# Opgave 2.4
# ==============

# Samlet realt privatforbrug i 2015 og 2016
forbrug_2015 <- sum(privatforbrug$Privatforbrug[
  substr(privatforbrug$Tidsinterval, 1, 4) == "2015"
])

forbrug_2016 <- sum(privatforbrug$Privatforbrug[
  substr(privatforbrug$Tidsinterval, 1, 4) == "2016"
])

# Realvækst for hele 2016
realvaekst_2016 <- (forbrug_2016 / forbrug_2015 - 1) * 100

realvaekst_2016

# OLA 3
# Opgave 3.1 - Husholdningernes forbrug i 4. kvartal
# Er forbruget i 2026K4 større end i 2025K4?

library(dkstat)
library(dplyr)
library(tidyr)
library(ggplot2)

# Henter metadata

forbrug_meta <- dst_meta(table = "NKN1", lang = "da")

# Henter husholdningernes forbrug i faste priser uden sæsonkorrektion
# Samme kvartal sammenlignes med året før.

forbrug_filter <- list(
  TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON = "Ikke sæsonkorrigeret",
  Tid = "*"
)

forbrug_raw <- dst_get_data(
  table = "NKN1",
  query = forbrug_filter,
  meta_data = forbrug_meta,
  lang = "da",
  parse_dst_tid = FALSE
)

# Beholder 1999 til 2026K2. 1999 bruges til at beregne væksten i 2000.
# Vi bruger kun oplysninger frem til 2026K2, også ved senere kørsler.

forbrug <- forbrug_raw %>%
  filter(TID >= "1999K1", TID <= "2026K2") %>%
  select(TID, value) %>%
  rename(Tidsinterval = TID, Forbrug = value) %>%
  arrange(Tidsinterval)

forbrug$Forbrug <- as.numeric(forbrug$Forbrug)
forbrug$Aar <- as.numeric(substr(forbrug$Tidsinterval, 1, 4))
forbrug$Kvartal <- as.numeric(substr(forbrug$Tidsinterval, 6, 6))

# Laver kvartalerne til kolonner

forbrug_aar <- forbrug %>%
  select(Aar, Kvartal, Forbrug) %>%
  pivot_wider(
    names_from = Kvartal,
    values_from = Forbrug,
    names_prefix = "K"
  ) %>%
  arrange(Aar)

# Beregner realvæksten i første halvår og i 4. kvartal
# Første halvår er summen af K1 og K2.

forbrug_aar <- forbrug_aar %>%
  mutate(
    Foerste_halvaar = K1 + K2,
    Vaekst_halvaar = (Foerste_halvaar / lag(Foerste_halvaar) - 1) * 100,
    Vaekst_K4 = (K4 / lag(K4) - 1) * 100
  )

# Træner kun på afsluttede år før 2026

traening <- forbrug_aar %>%
  filter(Aar >= 2000, Aar <= 2025) %>%
  filter(complete.cases(Vaekst_halvaar, Vaekst_K4))

# Simpel regression: vækst i første halvår forklarer vækst i K4

model <- lm(Vaekst_K4 ~ Vaekst_halvaar, data = traening)

print(summary(model))

# Forudsiger væksten i 2026K4 med den fulde model fra 2000-2025

forbrug_2026 <- forbrug_aar %>% filter(Aar == 2026)
forbrug_2025 <- forbrug_aar %>% filter(Aar == 2025)

prognose <- predict(
  model,
  newdata = forbrug_2026,
  interval = "prediction",
  level = 0.95
)

# Omregner den forventede vækst til forbrug i mia. kr.

resultat <- data.frame(
  Forbrug_2025K4 = forbrug_2025$K4,
  Prognose_2026K4 = forbrug_2025$K4 * (1 + prognose[1, "fit"] / 100),
  Realvaekst_pct = prognose[1, "fit"],
  Nedre_95_pct = prognose[1, "lwr"],
  Oevre_95_pct = prognose[1, "upr"]
)

resultat$Forskel_mia_kr <- resultat$Prognose_2026K4 - resultat$Forbrug_2025K4

rownames(resultat) <- NULL
print(round(resultat, 2))

# Viser tabellerne i RStudio

View(resultat)

# Graf over den faktiske og forventede realvækst i 4. kvartal

graf <- ggplot(traening, aes(x = Aar, y = Vaekst_K4)) +
  geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed") +
  geom_line(colour = "#1595D3", linewidth = 1) +
  geom_point(colour = "#1595D3", size = 2) +
  annotate(
    "errorbar", x = 2026,
    ymin = resultat$Nedre_95_pct,
    ymax = resultat$Oevre_95_pct,
    width = 0.4, colour = "#D97706"
  ) +
  annotate(
    "point", x = 2026, y = resultat$Realvaekst_pct,
    colour = "#D97706", size = 3
  ) +
  scale_x_continuous(breaks = c(seq(2000, 2025, by = 5), 2026)) +
  labs(
    title = "Husholdningernes forbrug ventes at stige 1,9 % i 4. kvartal 2026",
    subtitle = "Blå: faktisk realvækst. Orange: prognose for 2026 med 95 % prædiktionsinterval",
    x = "År",
    y = "Realvækst i forhold til samme kvartal året før (%)",
    caption = "Kilde: Danmarks Statistik, NKN1. Egne beregninger."
  ) +
  theme_minimal()

print(graf)

# ========================================================================
# 4.1 | STABILITETSTEST
# ========================================================================

####stabilitetstest med fem perioder (CHOPCHOP af fem perioder på ca. 5år og 3måneder)

#sorterer data efter kvartal
stabilitetsdata <- regressionsdata[
  order(regressionsdata$Tidsinterval), 
]

#fordeler 106 kvartaler på fem perioder
periode <- rep(1:5, c(21,21,21,21,22))

#kontrollerer datagrundlaget
stopifnot(nrow(stabilitetsdata) == 106)

#opretter en tabel til resultaterne
stabilitet <- data.frame(
  indikator = indikatorer,
  P1 = NA_real_,
  P2 = NA_real_,
  P3 = NA_real_,
  P4 = NA_real_,
  P5 = NA_real_
)
#tilføjer spørgsmålene til stabilitetsmatrixen
stabilitet$Spoergsmaal <- resultater$Spoergsmaal[
  match(stabilitet$indikator, resultater$indikator)
]
#beregner r2 for alle indikatorer i hver periode
for(p in 1:5) {
  
  data_periode <- stabilitetsdata[periode == p, ]
  
  for(i in 1:length(indikatorer)) {
    
    x <- data_periode[[indikatorer[i]]]
    
    model <- lm(data_periode$Realvaekst ~ x)
    
    stabilitet[i,p+1] <- summary(model)$r.squared
  }
}

#beregner gennemsnitlig r2
stabilitet$Gennemsnit_R2 <- rowMeans(stabilitet[,2:6])

#beregner den laveste r2
stabilitet$Laveste_R2 <- apply(stabilitet[,2:6],1,min)

#beregner standardafvigelsen
stabilitet$SD_R2 <- apply(stabilitet[,2:6],1,sd)

#sorterer efter højeste gennemsnitlige r2
stabilitet <- stabilitet[order(-stabilitet$Gennemsnit_R2), ]

#viser de 10 bedste indikatorer
print(head(stabilitet,10))

#viser hele tabellen
View(stabilitet)





#finder de tre oprindelige spørgsmål
spg <- grep("^F(6|7|8) ", names(forv1))

#beregner de oprindelige kvartalsgennemsnit
sp <- aggregate(
  forv1[,spg],
  by = list(TID = kvartal),
  FUN = mean
)

#samler spørgsmålene med realvæksten
kontrol <- merge(
  sp,
  privatforbrug,
  by.x = "TID",
  by.y = "Tidsinterval"
)

#viser korrelationen med forbruget
round(cor(kontrol[,2:4], kontrol$Realvaekst), 3)

# ==================
# Opgave 5.2
# ==================


library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

# ==========================================
# 1. GLOBALE VARIABLE
# ==========================================

windirparam = "wind_dir"
windforceparam = "wind_speed"

sid = "06033"
asid = "06180"

baseurl = paste0(
  "https://opendataapi.dmi.dk/v2/metObs/",
  "collections/observation/items?"
)

# Stormen Dave
fd = "2026-04-04T00:00:00Z"
td = "2026-04-07T00:00:00Z"


# ==========================================
# 2. HENT DATA FRA DMI
# ==========================================

getdatafromdmi <- function(param, station, frd, tod) {
  
  varurl = paste0(
    "parameterId=", param,
    "&stationId=", station,
    "&datetime=", frd, "/", tod,
    "&limit=20000"
  )
  
  totalurl = paste0(baseurl, varurl)
  
  res = GET(totalurl)
  
  print(res$status_code)
  stop_for_status(res)
  
  resraw = content(
    res,
    as = "text",
    encoding = "UTF-8"
  )
  
  resdf = fromJSON(resraw)
  
  obsdf = resdf$features$properties
  
  if (is.null(obsdf) || nrow(obsdf) == 0) {
    stop(paste(
      "Ingen data for station",
      station,
      "med parameter",
      param
    ))
  }
  
  winddf = obsdf[, c("observed", "value")]
  
  winddf$observed = as.POSIXct(
    winddf$observed,
    format = "%Y-%m-%dT%H:%M:%S",
    tz = "UTC"
  )
  
  winddf = arrange(winddf, observed)
  
  return(winddf)
}


# ==========================================
# 3. HENT DATA FRA HIRTSHALS
# ==========================================

windforcedf = getdatafromdmi(
  windforceparam, sid, fd, td
)

winddirdf = getdatafromdmi(
  windirparam, sid, fd, td
)

totalwind = inner_join(
  windforcedf,
  winddirdf,
  by = "observed"
)

colnames(totalwind) = c(
  "observed", "force", "dir"
)

totalwind$station = "Hirtshals"


# ==========================================
# 4. HENT DATA FRA KØBENHAVN
# ==========================================

windforcedfa = getdatafromdmi(
  windforceparam, asid, fd, td
)

winddirdfa = getdatafromdmi(
  windirparam, asid, fd, td
)

totalwinda = inner_join(
  windforcedfa,
  winddirdfa,
  by = "observed"
)

colnames(totalwinda) = c(
  "observed", "force", "dir"
)

totalwinda$station = "København Lufthavn"


# ==========================================
# 5. SAML DATA
# ==========================================

begge = rbind(
  totalwind,
  totalwinda
)

begge = arrange(
  begge,
  station,
  observed
)


# ==========================================
# 6. BEREGN VINDSTYRKE
# ==========================================

sammenligning = begge %>%
  group_by(station) %>%
  summarise(
    antal = n(),
    gennemsnit = mean(force, na.rm = TRUE),
    maksimum = max(force, na.rm = TRUE),
    minimum = min(force, na.rm = TRUE),
    .groups = "drop"
  )

print(sammenligning)

hmax = sammenligning$maksimum[
  sammenligning$station == "Hirtshals"
]

kmax = sammenligning$maksimum[
  sammenligning$station == "København Lufthavn"
]

forskel = round(
  abs(hmax - kmax),
  1
)

# Konkluderende overskrift til graf 1

if (hmax > kmax) {
  
  titel1 = paste0(
    "Hirtshals målte ",
    forskel,
    " m/s mere end København"
  )
  
} else if (kmax > hmax) {
  
  titel1 = paste0(
    "København målte ",
    forskel,
    " m/s mere end Hirtshals"
  )
  
} else {
  
  titel1 = "Begge stationer målte samme maksimale vindstyrke"
  
}


# ==========================================
# 7. BEREGN VINDRETNING
# ==========================================

# Vindretning i grader
# 0 = Nord
# 90 = Øst
# 180 = Syd
# 270 = Vest

begge$dir_rad = begge$dir * pi / 180

# Lav pile til grafen

begge$xd = begge$observed -
  sin(begge$dir_rad) * 1800

begge$yd = begge$force -
  cos(begge$dir_rad)


# Find dominerende vindretning
# Brug vektorer så 0 og 360 grader
# behandles som samme retning

retning_data = begge %>%
  filter(!is.na(dir)) %>%
  group_by(station) %>%
  summarise(
    sin_mean = mean(sin(dir * pi / 180)),
    cos_mean = mean(cos(dir * pi / 180)),
    .groups = "drop"
  ) %>%
  mutate(
    grader = (
      atan2(sin_mean, cos_mean) * 180 / pi + 360
    ) %% 360
  )

print(retning_data)


# Funktion til at oversætte grader

vindretning_navn = function(grader) {
  
  retninger = c(
    "nord", "nordøst", "øst", "sydøst",
    "syd", "sydvest", "vest", "nordvest"
  )
  
  nr = floor((grader + 22.5) / 45) %% 8 + 1
  
  return(retninger[nr])
}

retning_data$retning = vindretning_navn(
  retning_data$grader
)

hretning = retning_data$retning[
  retning_data$station == "Hirtshals"
]

kretning = retning_data$retning[
  retning_data$station == "København Lufthavn"
]

titel3 = paste0(
  "Vinden kom primært fra ",
  hretning,
  " i Hirtshals og ",
  kretning,
  " i København"
)


# ==========================================
# 8. GRAF 1, VINDSTYRKE
# ==========================================

p1 = ggplot(
  data = begge,
  aes(
    x = observed,
    y = force,
    color = station
  )
) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "Hirtshals" = "red",
      "København Lufthavn" = "blue"
    )
  ) +
  labs(
    title = titel1,
    subtitle = "Maksimal målt vindstyrke under Dave",
    x = "Dato",
    y = "Vindstyrke (m/s)",
    color = "Station"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 13
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(p1)


# ==========================================
# 9. GRAF 2, VINDSTYRKE MED PILE
# ==========================================

piledata = begge %>%
  group_by(station) %>%
  filter(row_number() %% 10 == 1) %>%
  ungroup()

p2 = ggplot(
  data = begge,
  aes(
    x = observed,
    y = force,
    color = station
  )
) +
  geom_line(linewidth = 0.7) +
  geom_segment(
    data = piledata,
    aes(
      xend = xd,
      yend = yd
    ),
    arrow = arrow(
      type = "closed",
      length = unit(0.05, "inches")
    ),
    linewidth = 0.4
  ) +
  scale_color_manual(
    values = c(
      "Hirtshals" = "red",
      "København Lufthavn" = "blue"
    )
  ) +
  labs(
    title = "Vindretningen ændrede sig under stormen Dave",
    subtitle = "Pilene viser vindens bevægelsesretning",
    x = "Dato",
    y = "Vindstyrke (m/s)",
    color = "Station"
  ) +
  theme_minimal()

print(p2)


# ==========================================
# 10. GRAF 3, VINDRETNING
# ==========================================

p3 = ggplot(
  data = begge,
  aes(
    x = observed,
    y = dir,
    color = station
  )
) +
  geom_point(size = 0.7) +
  scale_color_manual(
    values = c(
      "Hirtshals" = "red",
      "København Lufthavn" = "blue"
    )
  ) +
  scale_y_continuous(
    breaks = c(0, 90, 180, 270, 360),
    labels = c("N", "Ø", "S", "V", "N"),
    limits = c(0, 360)
  ) +
  labs(
    title = titel3,
    subtitle = "Dominerende vindretning under Dave",
    x = "Dato",
    y = "Vindretning",
    color = "Station"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 12
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(p3)


# ==========================================
# 11. SAMLET GRAF
# ==========================================

gridExtra::grid.arrange(
  p1,
  p3,
  ncol = 1,
  
  top = grid::textGrob(
    "Dave ramte Hirtshals og København forskelligt",
    gp = grid::gpar(
      fontsize = 16,
      fontface = "bold"
    )
  ),
  
  bottom = grid::textGrob(
    "Kilde: DMI, egne beregninger",
    x = 0.98,
    hjust = 1,
    gp = grid::gpar(
      fontsize = 9,
      col = "grey45"
    )
  )
)


# ==========================================
# 12. KONKLUSION I KONSOLLEN
# ==========================================

cat("\nKONKLUSION\n")

cat(titel1, "\n")

cat(titel3, "\n")

cat(
  "Hirtshals maksimum:",
  round(hmax, 1),
  "m/s\n"
)

cat(
  "København maksimum:",
  round(kmax, 1),
  "m/s\n"
)

cat(
  "Forskel:",
  forskel,
  "m/s\n"
)

print(sammenligning)

# Gem den samlede graf

samlet_graf = gridExtra::arrangeGrob(
  p1,
  p3,
  ncol = 1,
  top = grid::textGrob(
    "Dave ramte Hirtshals og København forskelligt",
    gp = grid::gpar(
      fontsize = 16,
      fontface = "bold"
    )
  ),
  bottom = grid::textGrob(
    "Kilde: DMI, egne beregninger",
    x = 0.98,
    hjust = 1,
    gp = grid::gpar(fontsize = 9)
  )
)

ggsave(
  filename = "Stormen_Dave.png",
  plot = samlet_graf,
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)
