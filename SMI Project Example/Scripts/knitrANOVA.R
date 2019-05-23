# Produce a Knitr Kable showing the results of an ANOVA analysis

knitrANOVA <- function(aovOut)
  kable(aovOut[[1]], digits = 3)
