
# Alumno: Hugo Calixto Linares

# En este Trabajo Práctico 1 se pretende calcular los porcentajes de hogares con Necesidades Básicas Insatisfechas
# por Comuna de la Ciudad de Buenos Aires


#La información tomada de datos abiertos es la siguiente:
# Información Censal por Radio 2010 
#Contiene desagregados por unidad espacial de Radios: Total poblacional, Total de viviendas, Parciales por tipo Particular o Colectiva,
#Total hogares, Hogares con/sin Necesidades Básicas Insatisfechas


# Instalar paquetes:

# instalar paquete tidyverse
install.packages("tidyverse")

# cargar paquete tidyverse
library(tidyverse)

# instalar paquete readr
install.packages("readr")

# cargar paquete readr
library(readr)


#Importando la base de datos
data1 <- read_csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/direccion-general-de-estadisticas-y-censos/informacion-censal-por-radio/informacion-censal-por-radio-2010.csv")


# Seleccionamos columnas de nuestro interés (población, vivienda, hogar, hogar con nbi y hogar sin nbi)
data_BA <- data1 %>% 
  select(COMUNA, TOTAL_POB, T_VIVIENDA, T_HOGAR, H_CON_NBI,H_SIN_NBI)

# Revisamos los nombres de las columnas seleccionadas
colnames(data_BA)


#Calculamos el total de valores 
data_comuna <- data_BA %>% group_by(COMUNA) %>%
                          summarise(total_pobl= sum(TOTAL_POB),
                                    total_viv= sum(T_VIVIENDA),
                                    total_hog= sum(T_HOGAR),
                                    total_nbi= sum(H_CON_NBI),
                                    total_snbi= sum(H_SIN_NBI))
view(data_comuna)
                        
summary(data_comuna$total_nbi)

# Se obtiene que el promedio de hogares con nbi de las 15 comunas de Buenos Aires alcanza 4585

# Categorizamos a las comunas de acuerdo si estan por encima o por debajo del promedio 
# de hogares con NBI

data_comuna <- data_comuna %>% 
  mutate(nbi_rango= case_when(total_nbi>=0 & total_nbi < 4585 ~ "Menor al promedio",
                              total_nbi>= 4585 ~ "Mayor o igual al promedio") )

table(data_comuna$nbi_rango)

# Existen 6 comunas que tienen igual o mayor hogares con NBI


data_comuna <- data_comuna %>%
  mutate(part_nbi_hog=(total_nbi/total_hog)*100)
view(data_comuna)

# Me queda pendiente redondear los valores de la participación de hogares con NBI  

# Cinco Comunas con mayor porcentaje de hogares con NBI
data_comuna %>%
  slice_max(order_by = part_nbi_hog, n=5)

# Los resultados muestran que las comunas 1, 4, 3, 8 y 7 son las que tienen mayor porcentaje de hogares con NBI

# Cinco Comunas con menor porcentaje de hogares con NBI
data_comuna %>% 
  slice_min(order_by = part_nbi_hog, n=5)

# Los resultados muestran que las comunas 12, 13, 11, 2 y 6 son las que tienen menor porcentaje de hogares con NBI

