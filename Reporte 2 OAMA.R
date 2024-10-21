Sys.setlocale("LC_CTYPE", "Spanish_Mexico.UTF-8")
# Reporte 2
studentPerformance <- read.csv("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/DatosMEB_OAMA_Ajuste.csv")
studentPerformance

###################################### A ###########################################
#Observación 
#Calificacion
#edad 
#relacion.familiar
#tiempo.libre 
#salidas 
#alc_entresemana 
#alc_finsemana 
#salud 
#clases.extra

# VR: Calificacion
data <- studentPerformance$Calificacion
summary(data)
sd(data)

png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/VR_Calificacion.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de Calificacion",
                   xlab = "Calificación",
                   ylab = "Frecuencia",
                   col = "darkgreen",
                   border = "black",
                   ylim = c(0, 100),
                   axes = FALSE
)
axis(2)
axis(1, at = seq(0, 20, by = 2))
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/VR_Calificacion.csv", row.names = FALSE)
png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/VR_CalificacionBoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de calificación",
        xlab = "",
        ylab = "Calificación",
        col = "darkgreen",
        border = "black"
)
dev.off()

# V1: edad
data <- studentPerformance$edad
summary(data)
sd(data)

png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V1_edad.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de Edad",
                   xlab = "Edad",
                   ylab = "Frecuencia",
                   col = "cadetblue",
                   border = "black",
                   xlim = c(14, 23),
                   breaks = 9
)
axis(2)
axis(1, at = seq(14, 23, by = 1))
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V1_edad.csv", row.names = FALSE)
dev.off()
png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V1_edad_BoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de Edad",
        xlab = "Edad",
        ylab = "",
        col = "cadetblue",
        border = "black",
        ylim = c(14, 23)
)
dev.off()

# V2: relación_familiar
data <- studentPerformance$relacion.familiar
summary(data)
sd(data)

png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V2_relación_familiar.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de Relación familiar",
                   xlab = "Score de relación familiar",
                   ylab = "Frecuencia",
                   col = "cadetblue",
                   border = "black",
                   xlim = c(0, 5),
                   breaks = 10
)

dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V2_relación_familiar.csv", row.names = FALSE)
dev.off()
png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V2_relación_familiar_BoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de Relación Familiar",
        xlab = "Score de relación familiar",
        ylab = "",
        col = "cadetblue",
        border = "black",
        ylim = c(0, 5)
)
dev.off()

# V3: tiempo.libre
data <- studentPerformance$ tiempo.libre
summary(data)
sd(data)

png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V3_tiempo_libre.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de Tiempo libre",
                   xlab = "Score de tiempo libre",
                   ylab = "Frecuencia",
                   col = "cadetblue",
                   border = "black",
                   xlim = c(0, 5),
                   breaks = 10
)
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V3_tiempo_libre.csv", row.names = FALSE)
png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V3_tiempo_libre_BoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de Tiempo Libre",
        xlab = "Score de tiempo libre",
        ylab = "",
        col = "cadetblue",
        border = "black",
        ylim = c(0, 5)
)
dev.off()

# V4: salidas
data <- studentPerformance$ salidas
summary(data)
sd(data)

png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V4_ salidas.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de salidas ",
                   xlab = "Score de salidas ",
                   ylab = "Frecuencia",
                   col = "cadetblue",
                   border = "black",
                   xlim = c(0, 5),
                   breaks = 10
)
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V4_ salidas.csv", row.names = FALSE)
png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V4_ salidas_BoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de salidas ",
        xlab = "Score de salidas ",
        ylab = "",
        col = "cadetblue",
        border = "black",
        ylim = c(0, 5)
)
dev.off()

# V5: alcohol entre semana
data <- studentPerformance$ alc_entresemana
summary(data)
sd(data)

png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V5_ alc_entresemana.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de alcohol entre semana ",
                   xlab = "Score de alcohol entre semana ",
                   ylab = "Frecuencia",
                   col = "cadetblue",
                   border = "black",
                   xlim = c(0, 5),
                   breaks = 10
)
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V5_ alc_entresemana.csv", row.names = FALSE)
png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V5_alc_entresemana _BoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de alcohol entre semana ",
        xlab = "Score de alcohol entre semana ",
        ylab = "",
        col = "cadetblue",
        border = "black",
        ylim = c(0, 5)
)
dev.off()

# V6: alcohol fin de semana
data <- studentPerformance$ alc_finsemana
summary(data)
sd(data)

png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V6_ alc_finsemana.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de alcohol fin de semana ",
                   xlab = "Score de alcohol fin de semana ",
                   ylab = "Frecuencia",
                   col = "cadetblue",
                   border = "black",
                   xlim = c(0, 5),
                   breaks = 10
)
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V6_ alc_finsemana.csv", row.names = FALSE)
png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V6_alc_finsemana _BoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de alcohol fin de semana ",
        xlab = "Score de alcohol fin de semana ",
        ylab = "",
        col = "cadetblue",
        border = "black",
        ylim = c(0, 5)
)
dev.off()

# V7: Salud
data <- studentPerformance$ salud
summary(data)
sd(data)

png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V7_salud.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de salud ",
                   xlab = "Score de salud ",
                   ylab = "Frecuencia",
                   col = "cadetblue",
                   border = "black",
                   xlim = c(0, 5),
                   breaks = 10
)
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V7_salud.csv", row.names = FALSE)
png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V7_salud_BoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de salud ",
        xlab = "Score de salud",
        ylab = "",
        col = "cadetblue",
        border = "black",
        ylim = c(0, 5)
)
dev.off()

# V8: Clases Extra
data <- studentPerformance$ clases.extra
summary(data)
sd(data)

png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V8_clases_extra.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de clases extra ",
                   xlab = "Score de clases extra ",
                   ylab = "Frecuencia",
                   col = "cadetblue",
                   border = "black",
                   xlim = c(0, 1),
                   breaks = 2
)
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V8_ clases_extra.csv", row.names = FALSE)
png("C:/Users/oscal/OneDrive/Documentos/Educación formal/FCFM/1er Tetra/Métodos Estadísticos Básicos/Tareas/Proyecto/V8_ clases_extra_BoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de clases extra ",
        xlab = "Score de clases extra ",
        ylab = "",
        col = "cadetblue",
        border = "black",
        ylim = c(0, 5)
)
dev.off()



################################### B #########################################
# Life Expectancy

