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

