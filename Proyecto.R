# Pregunta Objetivo

# Identificar qué condiciones ambientales, del entorno y del conductor están asociadas con accidentes de alta fatalidad (más de 1 muerto) en Estados Unidos en 2022.

# Seteamos la base de datos
attach(Data)

# Librerias usadas
library(dplyr) # Agrupacion de datos (ordenar,filtrsar, etc)
library(moments) # Medidas de forma estadisticas (Kurtosis, Skewness)
library(ggplot2) # Libreria para graficar

# Conversión de tipos de datos
Decesos = as.numeric(Decesos)
Hora = as.integer(Hora)
Mes = as.integer(Mes)
DiaSemanaNum = as.integer(Dia)
Fecha = as.POSIXct(Fecha)

# Creacion nueva columna basado en accidente de categoria alta o baja
Data <- Data %>%
  mutate(Fatalidad = ifelse(Decesos == 1, "Baja",
                  ifelse(Decesos >= 2, "Alta", NA)))


# Descripcion de la variable objetivo con calculos estadisticos 

# Medidas de tendencia central
Media = mean(Decesos)
Mediana = median(Decesos)
# Medidas de dispersión
VariacionEstandar = var(Decesos)
DesviacionEstandar = sd(Decesos)
IQR_D = IQR(Decesos)
# Medidas de posicion
Rango = range(Decesos)
quantile(Decesos)
boxplot(Decesos, horizontal = TRUE)
quantile(Decesos, probs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99,1))
# Medidas de forma
hist(Decesos)
Skew = skewness(Decesos)
Kurtosis = kurtosis(Decesos)

# Creacion de un tabla resumen para mostrar los datos estadisticos
Tabla_Resumen_V = data.frame(
  Descripcion = c(
    "Media",
    "Mediana",
    "Desviación Estándar",
    "Varianza",
    "IQR",
    "Mínimo",
    "Máximo",
    "Skewness",
    "Kurtosis"
  ),
  Valor = c(
    Media,
    Mediana,
    DesviacionEstandar,
    VariacionEstandar,
    IQR_D,
    Rango[1],
    Rango[2],
    Skew,
    Kurtosis
  )
)

Tabla_Resumen_V

# Creación grafico de histograma con la curtosis
p = 
  hist(Decesos, 
     main = paste("Curtosis =", round(kurtosis(Decesos), 2)),
     col = "lightblue", 
     border = "black")
  abline(v = mean(Decesos), col = "red", lwd = 2)

# Con la linea de abajo se guarda la imagen obtenida  
#png("Kurtosis.png", width = 1200, height = 800, res = 150)


#==================================================================================
#==================================================================================
# Creamos una lista para las varialbes que tienen mayor impacto en los accidentes

Ganadores = list()

#==================================================================================
#==================================================================================

# Aqui comenzamos a mirar las frecuencias de cada variable y combinada con Fatalidad
# Cada analisis de la variblwe tuvo tres etapas
# Analisis independiente
# Analsis cruzado con la variable de fatalidad
# Obtención de la varialbe con mayor impacto
# Graficación de la información
# Guardar imagen graficada

# Ubicacion
cat("\nEstado:\n")
FreAbsoluta_Lugar = table(`Nombre de Estado`)
FreRelativa_Lugar = prop.table(FreAbsoluta_Lugar)
FreAcumulada_Lugar = cumsum(FreAbsoluta_Lugar)
FreRelAcumulada_Lugar = cumsum(FreRelativa_Lugar)

Tabla_Lugar = data.frame(
  Luz = names(FreAbsoluta_Lugar),
  FreAbsoluta_Lugar = as.vector(FreAbsoluta_Lugar),
  FreRelativa_Lugar = as.vector(FreRelativa_Lugar),
  FreAcumulada_Lugar = as.vector(FreAcumulada_Lugar),
  FreRelAcumulada_Lugar = as.vector(FreRelAcumulada_Lugar)
)

Tabla_Lugar


cat("\nLugar vs Fatalidad:\n")
FreAbsoluta_Lugar_Fatalidad= table(`Nombre de Estado`, Fatalidad)
FreRelativa_Lugar_Fatalidad = prop.table(FreAbsoluta_Lugar_Fatalidad, 2)

Tabla_Lugar_Fatalidad <- data.frame(
  Conteo_Alta = FreAbsoluta_Lugar_Fatalidad[, "Alta"],
  Conteo_Baja = FreAbsoluta_Lugar_Fatalidad[, "Baja"],
  Porcentaje_Alta = round(FreRelativa_Lugar_Fatalidad[, "Alta"] * 100, 2),
  Porcentaje_Baja = round(FreRelativa_Lugar_Fatalidad[, "Baja"] * 100, 2)
)

Tabla_Lugar_Fatalidad

# obtener el estado con mayor fatalidad alta
ganadorCatg = Tabla_Lugar_Fatalidad[which.max(Tabla_Lugar_Fatalidad$Porcentaje_Alta), ]
# Ingresarlo a la lista de ganadores
Ganadores[["Lugar"]] = ganadorCatg

# Creamos un datafrane solo con los datos de alta fatalidad

df_pct <- data.frame(
  Estado = rownames(Tabla_Lugar_Fatalidad),
  Porcentaje_Alta = Tabla_Lugar_Fatalidad$Porcentaje_Alta,
  Conteo_Alta = Tabla_Lugar_Fatalidad$Conteo_Alta
)

# Graficar la varialbe combinada

p = 
  ggplot(df_pct, aes(x = Estado, y = Porcentaje_Alta)) +
  geom_bar(stat = "identity", fill = "#3c6e99") +
  geom_text(aes(label = Conteo_Alta), vjust = -0.5, size = 3) +
  labs(title = "Porcentaje de Fatalidad Alta por Estado",
       x = "Estado",
       y = "Porcentaje de fatalidad alta (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p

# Guardamos la imagen
# ggsave("fatalidad_por_estado.png", plot = p, width = 10, height = 6, dpi = 300)

#==================================================================================
#==================================================================================


# Variables de ambiente ( Luz y Clima )

# Luz
cat("\nLuz:\n")
FreAbsoluta_Luz = table(Luz)
FreRelativa_Luz = prop.table(FreAbsoluta_Luz)
FreAcumulada_Luz = cumsum(FreAbsoluta_Luz)
FreRelAcumulada_Luz = cumsum(FreRelativa_Luz)

Tabla_Luz = data.frame(
  Luz = names(FreAbsoluta_Luz),
  FreAbsoluta_Luz = as.vector(FreAbsoluta_Luz),
  FreRelativa_Luz = as.vector(FreRelativa_Luz),
  FreAcumulada_Luz = as.vector(FreAcumulada_Luz),
  FreRelAcumulada_Luz = as.vector(FreRelAcumulada_Luz)
)

Tabla_Luz


cat("\nLuz vs Fatalidad:\n")
FreAbsoluta_Luz_Fatalidad= table(Luz, Fatalidad)
FreRelativa_Luz_Fatalidad = prop.table(FreAbsoluta_Luz_Fatalidad, 2)

Tabla_Luz_Fatalidad <- data.frame(
  Conteo_Alta = FreAbsoluta_Luz_Fatalidad[, "Alta"],
  Conteo_Baja = FreAbsoluta_Luz_Fatalidad[, "Baja"],
  Porcentaje_Alta = round(FreRelativa_Luz_Fatalidad[, "Alta"] * 100, 2),
  Porcentaje_Baja = round(FreRelativa_Luz_Fatalidad[, "Baja"] * 100, 2)
)

Tabla_Luz_Fatalidad

ganadorCatg = Tabla_Luz_Fatalidad[which.max(Tabla_Luz_Fatalidad$Porcentaje_Alta), ]

Ganadores[["Luz"]] = ganadorCatg


df_pct <- data.frame(
  Estado = rownames(Tabla_Luz_Fatalidad),
  Porcentaje_Alta = Tabla_Luz_Fatalidad$Porcentaje_Alta,
  Conteo_Alta = Tabla_Luz_Fatalidad$Conteo_Alta
)

p = 
  ggplot(df_pct, aes(x = Estado, y = Porcentaje_Alta)) +
    geom_bar(stat = "identity", fill = "#3c6e99") +
    geom_text(aes(label = Conteo_Alta), vjust = -0.5, size = 3) +
    labs(title = "Porcentaje de Fatalidad Alta por Luz",
         x = "Franja Horaria",
         y = "Porcentaje de fatalidad alta (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
p

ggsave("fatalidad_por_luz.png", plot = p, width = 10, height = 6, dpi = 300)


#==================================================================================
#==================================================================================


# Clima

cat("\nClima:\n")
FreAbsoluta_Clima = table(Clima)
FreRelativa_Clima = prop.table(FreAbsoluta_Clima)
FreAcumulada_Clima = cumsum(FreAbsoluta_Clima)
FreRelAcumulada_Clima = cumsum(FreRelativa_Clima)

Tabla_Clima = data.frame(
  TipoClima = names(FreAbsoluta_Clima),
  FreAbsoluta_Clima = as.vector(FreAbsoluta_Clima),
  FreRelativa_Clima = as.vector(FreRelativa_Clima),
  FreAcumulada_Clima = as.vector(FreAcumulada_Clima),
  FreRelAcumulada_Clima = as.vector(FreRelAcumulada_Clima)
)

Tabla_Clima

cat("\nClima vs Fatalidad:\n")
FreAbsoluta_Clima_Fatalidad= table(Clima, Fatalidad)
FreRelativa_Clima_Fatalidad = prop.table(FreAbsoluta_Clima_Fatalidad, 2)

Tabla_Clima_Fatalidad <- data.frame(
  Conteo_Alta = FreAbsoluta_Clima_Fatalidad[, "Alta"],
  Conteo_Baja = FreAbsoluta_Clima_Fatalidad[, "Baja"],
  Porcentaje_Alta = round(FreRelativa_Clima_Fatalidad[, "Alta"] * 100, 2),
  Porcentaje_Baja = round(FreRelativa_Clima_Fatalidad[, "Baja"] * 100, 2)
)

Tabla_Clima_Fatalidad

ganadorCatg = Tabla_Clima_Fatalidad[which.max(Tabla_Clima_Fatalidad$Porcentaje_Alta), ]

Ganadores[["Clima"]] = ganadorCatg


df_pct <- data.frame(
  Estado = rownames(Tabla_Clima_Fatalidad),
  Porcentaje_Alta = Tabla_Clima_Fatalidad$Porcentaje_Alta,
  Conteo_Alta = Tabla_Clima_Fatalidad$Conteo_Alta
)

p = 
  ggplot(df_pct, aes(x = Estado, y = Porcentaje_Alta)) +
    geom_bar(stat = "identity", fill = "#3c6e99") +
    geom_text(aes(label = Conteo_Alta), vjust = -0.5, size = 3) +
    labs(title = "Porcentaje de Fatalidad Alta por Tipo de Clima",
         x = "Tipo de Clima",
         y = "Porcentaje de fatalidad alta (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

p


ggsave("fatalidad_por_clima.png", plot = p, width = 10, height = 6, dpi = 300)


#==================================================================================
#==================================================================================


# Variables de causalidad (tipo y causa de accidente)

# Causa
cat("\nCausa:\n")
FreAbsoluta_Causa = table(CausaAccidente)
FreRelativa_Causa = prop.table(FreAbsoluta_Causa)
FreAcumulada_Causa = cumsum(FreAbsoluta_Causa)
FreRelAcumulada_Causa = cumsum(FreRelativa_Causa)

Tabla_Causa = data.frame(
  Causa = names(FreAbsoluta_Causa),
  FreAbsoluta_Causa = as.vector(FreAbsoluta_Causa),
  FreRelativa_Causa = as.vector(FreRelativa_Causa),
  FreAcumulada_Causa = as.vector(FreAcumulada_Causa),
  FreRelAcumulada_Causa = as.vector(FreRelAcumulada_Causa)
)

Tabla_Causa

cat("\nCausa vs Fatalidad:\n")
FreAbsoluta_Causa_Fatalidad= table(CausaAccidente, Fatalidad)
FreRelativa_Causa_Fatalidad = prop.table(FreAbsoluta_Causa_Fatalidad, 2)

Tabla_Causa_Fatalidad <- data.frame(
  Conteo_Alta = FreAbsoluta_Causa_Fatalidad[, "Alta"],
  Conteo_Baja = FreAbsoluta_Causa_Fatalidad[, "Baja"],
  Porcentaje_Alta = round(FreRelativa_Causa_Fatalidad[, "Alta"] * 100, 2),
  Porcentaje_Baja = round(FreRelativa_Causa_Fatalidad[, "Baja"] * 100, 2)
)

Tabla_Causa_Fatalidad

ganadorCatg = Tabla_Causa_Fatalidad[which.max(Tabla_Causa_Fatalidad$Porcentaje_Alta), ]

Ganadores[["Causa"]] = ganadorCatg



df_pct <- data.frame(
  Estado = rownames(Tabla_Causa_Fatalidad),
  Porcentaje_Alta = Tabla_Causa_Fatalidad$Porcentaje_Alta,
  Conteo_Alta = Tabla_Causa_Fatalidad$Conteo_Alta
)

p = 
ggplot(df_pct, aes(x = Porcentaje_Alta, y = Estado)) +
  geom_bar(stat = "identity", fill = "#3c6e99") +
  geom_text(aes(label = Conteo_Alta), 
            hjust = -0.2,  # mueve la etiqueta a la derecha
            size = 3) +
  labs(title = "Porcentaje de Fatalidad Alta por Causa de Accidente",
       x = "Porcentaje de fatalidad alta (%)",
       y = "Causa de Accidente") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

p


ggsave("fatalidad_por_causa.png", plot = p, width = 10, height = 6, dpi = 300)


#==================================================================================
#==================================================================================


# Variables de entorno (Rural o urbano, por horas, por mes y por dia)
#Tipo de entorno
cat("\nEntorno:\n")
FreAbsoluta_Entorno = table(`Rural o Urbanoo`)
FreRelativa_Entorno = prop.table(FreAbsoluta_Entorno)
FreAcumulada_Entorno = cumsum(FreAbsoluta_Entorno)
FreRelAcumulada_Entorno = cumsum(FreRelativa_Entorno)

Tabla_Entorno = data.frame(
  Entorno = names(FreAbsoluta_Entorno),
  FreAbsoluta_Entorno = as.vector(FreAbsoluta_Entorno),
  FreRelativa_Entorno = as.vector(FreRelativa_Entorno),
  FreAcumulada_Entorno = as.vector(FreAcumulada_Entorno),
  FreRelAcumulada_Entorno = as.vector(FreRelAcumulada_Entorno)
)

Tabla_Entorno

cat("\nEntorno vs Fatalidad:\n")
FreAbsoluta_Entorno_Fatalidad= table(`Rural o Urbanoo`, Fatalidad)
FreRelativa_Entorno_Fatalidad = prop.table(FreAbsoluta_Entorno_Fatalidad, 2)

Tabla_Entorno_Fatalidad <- data.frame(
  Conteo_Alta = FreAbsoluta_Entorno_Fatalidad[, "Alta"],
  Conteo_Baja = FreAbsoluta_Entorno_Fatalidad[, "Baja"],
  Porcentaje_Alta = round(FreRelativa_Entorno_Fatalidad[, "Alta"] * 100, 2),
  Porcentaje_Baja = round(FreRelativa_Entorno_Fatalidad[, "Baja"] * 100, 2)
)

Tabla_Entorno_Fatalidad

ganadorCatg = Tabla_Entorno_Fatalidad[which.max(Tabla_Entorno_Fatalidad$Porcentaje_Alta), ]

Ganadores[["Entorno"]] = ganadorCatg


df_pct <- data.frame(
  Estado = rownames(Tabla_Entorno_Fatalidad),
  Porcentaje_Alta = Tabla_Entorno_Fatalidad$Porcentaje_Alta
)

df_pct$label <- paste0(df_pct$Porcentaje_Alta, "%")

p = 
ggplot(df_pct, aes(x = "", y = Porcentaje_Alta, fill = Estado)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(title = "Porcentaje de Fatalidad Alta por Entorno") +
  theme_void()


p


ggsave("fatalidad_por_entorno.png", plot = p, width = 10, height = 6, dpi = 300)


#=================================================================================
#==================================================================================


#Horas
cat("\nHoras:\n")
FreAbsoluta_Hora = table(Hora)
FreRelativa_Hora = prop.table(FreAbsoluta_Hora)
FreAcumulada_Hora = cumsum(FreAbsoluta_Hora)
FreRelAcumulada_Hora = cumsum(FreRelativa_Hora)

Tabla_Hora = data.frame(
  Entorno = names(FreAbsoluta_Hora),
  FreAbsoluta_Hora = as.vector(FreAbsoluta_Hora),
  FreRelativa_Hora = as.vector(FreRelativa_Hora),
  FreAcumulada_Hora = as.vector(FreAcumulada_Hora),
  FreRelAcumulada_Hora = as.vector(FreRelAcumulada_Hora)
)

Tabla_Hora

cat("\nHhora vs Fatalidad:\n")
FreAbsoluta_Hora_Fatalidad= table(Hora, Fatalidad)
FreRelativa_Hora_Fatalidad = prop.table(FreAbsoluta_Hora_Fatalidad, 2)

Tabla_Hora_Fatalidad <- data.frame(
  Conteo_Alta = FreAbsoluta_Hora_Fatalidad[, "Alta"],
  Conteo_Baja = FreAbsoluta_Hora_Fatalidad[, "Baja"],
  Porcentaje_Alta = round(FreRelativa_Hora_Fatalidad[, "Alta"] * 100, 2),
  Porcentaje_Baja = round(FreRelativa_Hora_Fatalidad[, "Baja"] * 100, 2)
)

Tabla_Hora_Fatalidad

ganadorCatg = Tabla_Hora_Fatalidad[which.max(Tabla_Hora_Fatalidad$Porcentaje_Alta), ]

Ganadores[["Hora"]] = ganadorCatg



df_hora <- data.frame(
  Hora = as.numeric(rownames(Tabla_Hora_Fatalidad)),
  Porcentaje_Alta = Tabla_Hora_Fatalidad$Porcentaje_Alta,
  Conteo_Alta = Tabla_Hora_Fatalidad$Conteo_Alta
)

p = 
ggplot(df_hora, aes(x = Hora, y = Porcentaje_Alta)) +
  geom_line(color = "#3c6e99", size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = Conteo_Alta),
            vjust = -0.8,
            size = 3) +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Porcentaje de Fatalidad Alta por Hora",
       x = "Hora del día",
       y = "Porcentaje de fatalidad alta (%)") +
  theme_minimal()


p


ggsave("fatalidad_por_hora.png", plot = p, width = 10, height = 6, dpi = 300)


#==================================================================================
#==================================================================================


# DiaSemana
cat("\nDiaSemana:\n")
FreAbsoluta_DiaSemana = table(DiaSemana)
FreRelativa_DiaSemana = prop.table(FreAbsoluta_DiaSemana)
FreAcumulada_DiaSemana = cumsum(FreAbsoluta_DiaSemana)
FreRelAcumulada_DiaSemana = cumsum(FreRelativa_DiaSemana)

Tabla_DiaSemana = data.frame(
  Entorno = names(FreAbsoluta_DiaSemana),
  FreAbsoluta_DiaSemana = as.vector(FreAbsoluta_DiaSemana),
  FreRelativa_DiaSemana = as.vector(FreRelativa_DiaSemana),
  FreAcumulada_DiaSemana = as.vector(FreAcumulada_DiaSemana),
  FreRelAcumulada_DiaSemana = as.vector(FreRelAcumulada_DiaSemana)
)

Tabla_DiaSemana

cat("\nDiaSemana vs Fatalidad:\n")
FreAbsoluta_DiaSemana_Fatalidad = table(DiaSemana, Fatalidad)
FreRelativa_DiaSemana_Fatalidad = prop.table(FreAbsoluta_DiaSemana_Fatalidad, 2)

Tabla_DiaSemana_Fatalidad <- data.frame(
  Conteo_Alta = FreAbsoluta_DiaSemana_Fatalidad[, "Alta"],
  Conteo_Baja = FreAbsoluta_DiaSemana_Fatalidad[, "Baja"],
  Porcentaje_Alta = round(FreRelativa_DiaSemana_Fatalidad[, "Alta"] * 100, 2),
  Porcentaje_Baja = round(FreRelativa_DiaSemana_Fatalidad[, "Baja"] * 100, 2)
)

Tabla_DiaSemana_Fatalidad


ganadorCatg = Tabla_DiaSemana_Fatalidad[which.max(Tabla_DiaSemana_Fatalidad$Porcentaje_Alta), ]

Ganadores[["Dia"]] = ganadorCatg



df_pct <- data.frame(
  Estado = rownames(Tabla_DiaSemana_Fatalidad),
  Porcentaje_Alta = Tabla_DiaSemana_Fatalidad$Porcentaje_Alta,
  Conteo_Alta = Tabla_DiaSemana_Fatalidad$Conteo_Alta
)

p = 
ggplot(df_pct, aes(x = Estado, y = Porcentaje_Alta)) +
  geom_bar(stat = "identity", fill = "#3c6e99") +
  geom_text(aes(label = Conteo_Alta), vjust = -0.5, size = 3) +
  labs(title = "Porcentaje de Fatalidad Alta por Dia",
       x = "Dia de la semana",
       y = "Porcentaje de fatalidad alta (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p


ggsave("fatalidad_por_dia.png", plot = p, width = 10, height = 6, dpi = 300)


#==================================================================================
#==================================================================================

# Aqui lo que hacemos en mediante un ciclo mostramos las varialbes ganadoras
# que fuimos ingresando  a la lista durant el analsis de cada variable

Ganadores

# Convertir los ganadores en un dataframe limpio
df_ganadores = do.call(rbind, lapply(names(Ganadores), function(n) {
  fila = Ganadores[[n]]
  data.frame(
    Variable = n,
    Ganador = rownames(fila),
    Porcentaje_Alta = fila$Porcentaje_Alta,
    Conteo_Alta = fila$Conteo_Alta
  )
}))

rownames(df_ganadores) = NULL
df_ganadores



#==================================================================================
#==================================================================================

# Aqui podemos observar las variables de perfil que son categoricas de si algun factor estuvo o no presente
# en el accidente

# Perfil del conductor
perfil = c("Camion Grande","Adolescente (15-20)","Joven (20-24)",
            "Anciano (65+)","Distraido","Cansancio", "Se fugo",
            "Exceso de Velocidad","Motocicleta","Persecución Policial")

lista_tablas = list()

for (v in perfil) {
  
  cat("\n===========================\n")
  cat(v, "vs Fatalidad\n")
  cat("===========================\n")
  
  FreAbs <- table(Data[[v]], Fatalidad)
  FreRel <- prop.table(FreAbs, 2)
  
  Tabla_Perfiles_Fatalidad = data.frame(
    Conteo_Alta = FreAbs[, "Alta"],
    Conteo_Baja = FreAbs[, "Baja"],
    Porcentaje_Alta = round(FreRel[, "Alta"] * 100, 2),
    Porcentaje_Baja = round(FreRel[, "Baja"] * 100, 2)
  )
  
  print(Tabla_Perfiles_Fatalidad)
  
  lista_tablas[[v]] <- Tabla_Perfiles_Fatalidad
}


# Unir las tablas en una
df_heat = bind_rows(lapply(names(lista_tablas), function(v) {
  t <- lista_tablas[[v]]
  data.frame(
    Perfil = v,
    No_Si = rownames(t),
    Porcentaje_Alta = t$Porcentaje_Alta,
    Conteo_Alta = t$Conteo_Alta
  )
}), .id = "variable")


df_heat_si = df_heat %>% filter(No_Si == "Si")


p = 
ggplot(df_heat_si, aes(x = "Fatalidad Alta", 
                       y = Perfil, 
                       fill = Porcentaje_Alta)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(Porcentaje_Alta, "%")), color = "black") +
  scale_fill_gradient(low = "#e8f5ff", high = "#3c6e99") +
  labs(title = "Heatmap de Perfiles vs Porcentaje de Fatalidad Alta",
       x = "",
       y = "Perfil") +
  theme_minimal()

p


ggsave("heatmap.png", plot = p, width = 10, height = 6, dpi = 300)


#==================================================================================
#==================================================================================
#==================================================================================

Data %>%
  summarise(
    Adolecente = sum(`Adolescente (15-20)` == "Si"),
    Joven = sum(`Joven (20-24)` == "Si"),
    Anciano = sum(`Anciano (65+)` == "Si"),
    Distraido = sum(Distraido == "Si"),
    Cansancio = sum(Cansancio == "Si"),
    Fuga = sum(`Se fugo` == "Si"),
    Velocidad = sum(`Exceso de Velocidad` == "Si"),
    Moto = sum(Motocicleta == "Si"),
    Camion = sum(`Camion Grande` == "Si"),
    Persecucion = sum(`Persecución Policial` == "Si"),
  )



#==================================================================================
#==================================================================================
#==================================================================================





