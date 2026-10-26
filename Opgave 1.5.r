
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
