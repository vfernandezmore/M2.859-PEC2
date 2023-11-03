# Limpieza de la memoria ----

rm(list = ls(all.names = T))
gc()

# Librarias ----
library(data.table)
library(dplyr)
library(ggplot2)
library(datasets)
library(ggridges)

# --------------------------- BAR CHART ----------------------------------------

# Explormos los datos ----

dt <- esoph  |> data.table() # Smoking, Alcohol and (O)esophageal Cancer
dt |> str()
dt |> View()


dtFuma1 <- dt |> 
  mutate(tobgp = gsub('g/day', '', tobgp)) |> 
  group_by(tobgp) |> 
  summarize(n = sum(ncases),
            Tipo = 'Casos') |> 
  data.table()

dtFuma2 <- dt |> 
  mutate(tobgp = gsub('g/day', '', tobgp)) |> 
  group_by(tobgp) |> 
  summarize(n = sum(ncontrols),
            Tipo = 'Controles') |> 
  data.table()

dtFuma <- rbind(dtFuma1, dtFuma2)

# Bar Chart ----

barplot <- ggplot(data=dtFuma, aes(x=tobgp, y=n, fill = Tipo)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=n), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3.5)+
  labs(title = "Comparación de Casos y Controles por Consumo de Tabaco",
       x = "Consumo de Tabaco (g/día)",
       y = "Recuento")

barplot
# ggsave(filename = "C:/Users/ZeDNa/OneDrive/Documentos/GitHub/M2.859-PEC2/M2.859-PEC2/Imágenes/barplot.png",
#        plot = barplot, width = 6, height = 4)

# ------------------------------- RIDEGE PLOT ----------------------------------

dt <- iris  |> data.table() # Iris
dt |> str()
dt |> View()


ridgeplot <- ggplot(dt, aes(x = Petal.Length, y = Species, fill = Species)) +
  geom_density_ridges(alpha = .8, color = "white",
                      scale = 2.5, rel_min_height = .01) +
  labs(x = "Longitud del Petalo", y = "Especie") +
  theme_ridges()

ridgeplot
# ggsave(filename = "C:/Users/ZeDNa/OneDrive/Documentos/GitHub/M2.859-PEC2/M2.859-PEC2/Imágenes/ridgeplot.png",
#        plot = ridgeplot, width = 6, height = 4)

