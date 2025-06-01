setwd("C:/Users/ferna/OneDrive - Universidad Católica de Chile/UC/Desafios")

library(readxl)
library(ggplot2)
library(tidyr)  # para convertir a formato largo
Series_R <- read_excel("Series R.xlsx")
Series_R$Fecha = as.Date(Series_R$Fecha)

#data_larga <- pivot_longer(Series_R, cols = -Fecha, names_to = "Serie", values_to = "Valor")

# Crear vector de fechas
fechas_retiros <- as.Date(c("2020-07-23", "2020-12-03", "2021-04-27","2025-04-02"))

# Graficar usando geom_vline con data
ggplot(Series_R) +
  geom_line(aes(x = Fecha, y = `Bonos corporativos con riesgo AAA`, color = "riesgo AAA"), size = 1) +
  geom_line(aes(x = Fecha, y = `Bonos corporativos con riesgo AA`, color = "riesgo AA"), size = 1) +
  geom_line(aes(x = Fecha, y = `Bonos corporativos con riesgo A`, color = "riesgo A"), size = 1) +
  geom_line(aes(x = Fecha, y = `BTU-10`, color = "BTU-10"), size = 1) +
  
  geom_vline(data = data.frame(fechas_retiros), aes(xintercept = fechas_retiros),
             linetype = "dashed", color = "black") +
  
  labs(title = "Evolución tasa corporativa a 10 años",
       x = "Fecha", y = "Tasas", color = "Serie") +
  theme_minimal() +
  theme(legend.position = "bottom")




cl_soberanos <- read_excel("Series R.xlsx", sheet = "Hoja2")
cl_soberanos$Fecha <- as.Date(cl_soberanos$Fecha)
usa_soberanos <- read_excel("Series R.xlsx", sheet = "Hoja3")
usa_soberanos$Fecha <- as.Date(usa_soberanos$Fecha)
TCN <- read_excel("Series R.xlsx", sheet = "Hoja4")
TCN$Fecha <- as.Date(TCN$Fecha)

soberanos <- merge(cl_soberanos, usa_soberanos, by = "Fecha")
soberanos <- merge(soberanos, TCN, by="Fecha")

soberanos$cambio_esperado_5 <- soberanos$TCN*((100+soberanos$`BTU-5`)/(100+soberanos$`5 real`))
soberanos$cambio_esperado_10 <- soberanos$TCN*((100+soberanos$`BTU-10`)/(100+soberanos$`10 real`))

soberanos$rendimiento_5_usa <- soberanos$`5 real`+(soberanos$cambio_esperado_5/soberanos$TCN-1)*100

soberanos$rendimiento_10_usa <- soberanos$`10 real`+(soberanos$cambio_esperado_10/soberanos$TCN-1)*100




# Crear vector de fechas
fechas_retiros <- as.Date(c("2020-07-23", "2020-12-03", "2021-04-27","2025-04-02"))

# Graficar usando geom_vline con data
ggplot(soberanos) +
  geom_line(aes(x = Fecha, y = rendimiento_5_usa, color = "rendimiento 5 años EEUU"), size = 1) +
  geom_line(aes(x = Fecha, y = rendimiento_10_usa, color = "rendimiento 10 años EEUU"), size = 1) +
  geom_line(aes(x = Fecha, y = `BTU-5`, color = "BTU-5"), size = 1) +
  geom_line(aes(x = Fecha, y = `BTU-10`, color = "BTU-10"), size = 1) +
  
  geom_vline(data = data.frame(fechas_retiros), aes(xintercept = fechas_retiros),
             linetype = "dashed", color = "black") +
  
  labs(title = "Evolución de rendimientos soberanos ajustados por expectativas",
       x = "Fecha", y = "Tasas", color = "Serie") +
  theme_minimal() +
  theme(legend.position = "bottom")




soberanos$spread_10 <- soberanos$`BTU-10`- soberanos$rendimiento_10_usa
soberanos$spread_5 <- soberanos$`BTU-5`- soberanos$rendimiento_5_usa


fechas_retiros <- as.Date(c("2020-07-23", "2020-12-03", "2021-04-27","2025-04-02"))

# Graficar usando geom_vline con data
ggplot(soberanos) +
  geom_line(aes(x = Fecha, y = spread_10, color = "spread 10"), size = 1) +
  geom_line(aes(x = Fecha, y = spread_5, color = "spread 5"), size = 1) +
  
  geom_vline(data = data.frame(fechas_retiros), aes(xintercept = fechas_retiros),
             linetype = "dashed", color = "black") +
  
  labs(title = "Spread ajustado por expectativas",
       x = "Fecha", y = "Spread de tasas", color = "Serie") +
  theme_minimal() +
  theme(legend.position = "bottom")


soberanos_2 <- merge(soberanos, Series_R, by = "Fecha")
soberanos_2$spreadAAA <- soberanos_2$`Bonos corporativos con riesgo AAA`- soberanos_2$rendimiento_10_usa
soberanos_2$spreadAA <- soberanos_2$`Bonos corporativos con riesgo AA`- soberanos_2$rendimiento_10_usa
soberanos_2$spreadA <- soberanos_2$`Bonos corporativos con riesgo A`- soberanos_2$rendimiento_10_usa




# Graficar usando geom_vline con data
ggplot(soberanos_2) +
  geom_line(aes(x = Fecha, y = spreadAAA, color = "spread AAA"), size = 1) +
  geom_line(aes(x = Fecha, y = spreadAA, color = "spread AA"), size = 1) +
  geom_line(aes(x = Fecha, y = spreadA, color = "spread A"), size = 1) +
  
  geom_vline(data = data.frame(fechas_retiros), aes(xintercept = fechas_retiros),
             linetype = "dashed", color = "black") +
  
  labs(title = "Spread ajustado por expectativas",
       x = "Fecha", y = "Spread de tasas", color = "Serie") +
  theme_minimal() +
  theme(legend.position = "bottom")









TPM <-read_excel("Series R.xlsx", sheet = "Hoja5")
TPM$Fecha <- as.Date(TPM$Fecha)
soberanos_2 <- merge(TPM,soberanos_2, by= "Fecha")







library(readr)
a <- read_csv("IPSA.csv")
IPSA=data.frame(1:3021)
IPSA$Fecha <- as.Date(a$Date, format ="%m/%d/%Y")
IPSA$IPSA <- a$`Change %`
IPSA$IPSA <- substr(IPSA$IPSA, 1, nchar(IPSA$IPSA) - 2)
IPSA$IPSA <- as.numeric(IPSA$IPSA)
soberanos_2 <- merge(soberanos_2, IPSA, by="Fecha")









# Graficar usando geom_vline con data
ggplot(soberanos_2) +
  geom_line(aes(x = Fecha, y = `Bonos corporativos con riesgo AAA`, color = "riesgo AAA"), size = 1) +
  geom_line(aes(x = Fecha, y = `BTU-10.x`, color = "BTU-10"), size = 1) +
  geom_line(aes(x = Fecha, y = `BTU-5.x`, color = "BTU-5"), size = 1) +
  geom_line(aes(x = Fecha, y = `10 real`, color = "Bonos USA 10 años"), size = 1) +
  geom_line(aes(x = Fecha, y = spread_10*100, color = "Spread 10 años"), size = 1) +
  geom_line(aes(x = Fecha, y = TPM/10, color = "TPM"), size = 1) +
  geom_line(aes(x = Fecha, y = IPSA/10, color = "IPSA"), size = 1) +
  
  geom_vline(data = data.frame(fechas_retiros), aes(xintercept = fechas_retiros),
             linetype = "dashed", color = "black") +
  
  labs(title = "Evolución tasa corporativa a 10 años",
       x = "Fecha", y = "Tasas", color = "Serie") +
  theme_minimal() +
  theme(legend.position = "bottom")

library(writexl)
write_xlsx(soberanos_2, "BBDD_ML.xlsx")
install.packages("openxlsx")
library(openxlsx)

write.xlsx(df, "mi_archivo.xlsx")
