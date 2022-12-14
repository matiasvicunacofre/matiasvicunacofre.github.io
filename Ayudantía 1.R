#***********************************************#
#
# Introducción a la Macroeconomía
# Matías Vicuña Cofré
# Ayudantía 1
#
#***********************************************#

setwd("~/OneDrive - Universidad Alberto Hurtado/UNIVERSIDAD UAH/4to Año/8vo Semestre/Introducción a la Macroeconomía/Ayudantías/Ayudantías 2020")

library(readxl)
library(tidyverse)
library(lubridate)
library(geomtextpath)
library(plotly)
library(xts)
library(zoo)
library(dygraphs)
library(gganimate)
library(plotly)
library(esquisse)

#********************** EJEMPLO PIB ***************************#
# Visualizar la data
db_imc <- read_xlsx("Ayudantía 1.xlsx", sheet = "DATOS PIB")
View(db_imc)
db_imc$Periodo <- NULL
db_imc$`Deflactor PIB` <- NULL

# Time-Series trimestral
db_ts_ims <- ts(data = db_imc, start = c(1996,1), frequency = 4)

#******* Gráfica interactiva trimestral *******#
epib <- dygraph(db_ts_ims, main = "Evolución PIB, 1996-2022", xlab = "Tiempo en Trimestres", ylab = "Miles de Pesos")

# Serie de tiempo en formato anual
anual_ts_pib <- aggregate(db_ts_ims, nfrequency = 1, FUN = sum)

#******* Grafica interactiva anual *******#
anual_pib <- dygraph(anual_ts_pib, main = "Evolución PIB, 1996-2022", xlab = "Tiempo en Años", ylab = "Miles de Pesos")

#******* Grafica Estatica anual *******#
anual_ts <- ts.plot(
  anual_ts_pib,
  main = "PIB Real vs PIB Nominal 1996 - 2022",
  xlab = "Tiempo en Años",
  ylab = "Miles de Pesos",
  col = c("blue", "red"),
  lwd = 3)
legend(x = "bottomright", legend = c("PIB Nominal", "PIB Real"), fill = c("blue", "red"), col = c("blue", "red"))

esquisser()

anual <- as.data.frame(anual_ts_pib)

data <- as.data.frame(cbind(fecha,PIB_Real,PIB_Nominal))

pib.anual <- ggplot(data) +
  geom_line(
    aes(x = fecha, y = PIB_Nominal),
    col = "blue",
    alpha = 1,
    size = 1
  ) +
  geom_line(
    aes(x = fecha, y = PIB_Real),
    col = "red",
    alpha = 1,
    size = 1
  ) +
  labs(
    x = "Tiempo en Años",
    y = "Miles de Pesos",
    title = "PIB Real vs PIB Nominal",
    subtitle = "1996 a 2021"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 15L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12L, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 15L, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 15L, face = "bold", hjust = 0.5)
  )

#********************** EJEMPLO IPC ***************************#

db_ipc <- read_xlsx("Ayudantía 1.xlsx", sheet = "DATOS IPC")

db_ipc$Periodo <- NULL

# Time-Series mensual (89-22)
db_ts_ipc <- ts(db_ipc, start = c(1989,1), frequency = 12)

#******* Grafica interactiva mensual *******#
mensual_ipc <- dygraph(db_ts_ipc, main = "Evolución IPC, 1989-2022", xlab = "Tiempo en Meses", ylab = "Variación %")

# Time-Series anual
anual_ipc <- aggregate(db_ts_ipc, nfrequency = 1, FUN = sum)

#******* Grafica interactiva anual *******#
anipc <- dygraph(anual_ipc, main = "Evolución IPC, 1989-2022", xlab = "Tiempo en Años", ylab = "Variación %")

