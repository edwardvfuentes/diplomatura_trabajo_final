library(plm)
library(Formula)

data("EmplUK", package="plm")
data("Produc", package="plm")
data("Grunfeld", package="plm")
data("Wages", package="plm")


E <- pdata.frame(EmplUK, index=c("firm","year"), drop.index=TRUE, row.names=TRUE)
head(E)
grunf_p <- pdata.frame(Grunfeld, index=c("firm","year"), drop.index=TRUE, row.names=TRUE)
head(grunf_p)


# Ejemplos de fórmulas con paquete Formula
emp ~ wage + capital | lag(wage, 1) + capital
emp ~ wage + capital | . -wage + lag(wage, 1)


# Ejemplo de estimación de panel de efectos fijos
grun.fe <- plm(inv~value+capital, data = Grunfeld, model = "within")


# Si quieres extraer los efectos fijos:
fixef(grun.fe, type = "dmean")
summary(fixef(grun.fe, type = "dmean"))




pdata.frame(famecon_complete, index = c("year", "provcd"), drop.index=TRUE, row.names=TRUE)

plm(fincome1_per ~ food + pce + house, data = famecon_complete, model = "within")