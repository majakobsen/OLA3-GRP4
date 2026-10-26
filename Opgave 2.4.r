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
