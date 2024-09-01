# Loading packages --------------------------------------------------------

to_be_loaded <- c("readxl",
                  "dplyr",
                  "VIM",
                  "patchwork",
                  "ggplot2",
                  "gtsummary",
                  "gridExtra",
                  "lubridate",
                  "aspace",
                  #"adehabitat",
                  "dplyr",
                  "grDevices",
                  "grid",
                  "maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13",
                  "maps",
                  "mapdata",
                  "rgdal",
                  "readxl",
                  "spsurvey",
                  "sp",
                  "SpatialEpi",
                  "spatstat",
                  "spdep"
)

for (pck in to_be_loaded) {
  if (!require(pck, character.only = T)) {
    install.packages(pck, repos="http://cran.rstudio.com/")
    stopifnot(require(pck, character.only = T))
  }
}

library(readxl)
library(dplyr)
library(VIM)
library(patchwork)
library(ggplot2)
library(gtsummary)
library(gridExtra)
library(lubridate)
library(aspace)
#library(adehabitat)
#library(GeoXp)
library(dplyr)
library(grDevices)
library(grid)
library(maptools)
library(maps)
library(mapdata)
#library(rgdal)
library(readxl)
library(spsurvey)
library(sp)
library(SpatialEpi)
library(spatstat)
library(spdep)

# EDA -------------------------------------------------------------------------

# Loading the data
data_2009 <- read.csv("AccidentesFatales_2009.csv", sep = ";")
data_2010 <- read.csv("AccidentesFatales_2010.csv", sep = ";")


# Fatal accidents in 2009
data_2009
dim(data_2009)
names(data_2009)

# Fatal accidents in 2010
data_2010
dim(data_2010)
names(data_2010)


# Data standardization ----------------------------------------------------

# Checking columns
columns_2009 <- names(data_2009)
columns_2010 <- names(data_2010)

if (setequal(columns_2009, columns_2010)) {
  cat("\nBoth datasets have the same columns.\n")
} else {
  cat("\nThere are differences in the columns between the datasets:\n")
  cat("In 2009 but not in 2010:\n")
  print(setdiff(columns_2009, columns_2010))
  cat("In 2010 but not in 2009:\n")
  print(setdiff(columns_2010, columns_2009))
}

# Drop unnecesary columns
data_2010 <- data_2010 %>% select(-c("FECHA.INSP.", "Ciudad"))
data_2009  <-  data_2009 %>% select(-c("Ciudad"))

# Adding "año" column
data_2009$Ano <- 2009
data_2010$Ano <- 2010

names(data_2009)
names(data_2010)


# Combining data
accidents_data <- rbind(data_2009, data_2010)

# Changing columns standar
colnames(accidents_data) <- tolower(colnames(accidents_data))
colnames(accidents_data) <- gsub("\\.", "_", colnames(accidents_data))
colnames(accidents_data) <- gsub("__", "_", colnames(accidents_data))
colnames(accidents_data) <- gsub("_$", "", colnames(accidents_data))
colnames(accidents_data)[colnames(accidents_data) == "com"] <- "comuna"
colnames(accidents_data)[colnames(accidents_data) == "condiccion"] <- "condicion"
colnames(accidents_data)


# Add new column of case count per commune
accidents_data <- accidents_data %>%
  group_by(comuna) %>%
  mutate(casos = n()) %>%
  ungroup()


# Cleaning  ---------------------------------------------------------------

# Checking null data
colSums(is.na(accidents_data))

na_values <- c("", "NA", "N/A", "NULL", "No aplica", "Desconocido", "-") # Checking for other notations of "NA"
accidents_data <- accidents_data %>%
  mutate_all(~ replace(., . %in% na_values, NA))
colSums(is.na(accidents_data))


accidents_data  <-  accidents_data %>% select(-c("profesion", "lugar_insp")) # We´re not gonna use this columns

accidents_data <- accidents_data %>%  # We´re not gonna use this rows
  filter(!is.na(coordenada_x_metros) &
           !is.na(coordenada_y_metros) &
           !is.na(coordenada_x_km) &
           !is.na(coordenada_y_km))

colSums(is.na(accidents_data))


# Checking categories of each column
unique(accidents_data$mes_fallecimiento)
unique(accidents_data$mes_accidente) # Needs cleaning
unique(accidents_data$barrio) # Needs cleaning
unique(accidents_data$comuna)
unique(accidents_data$sexo) # Needs null handling
unique(accidents_data$edad) # Needs null handling
unique(accidents_data$edad_agrupada) # Needs cleaning and null handling
unique(accidents_data$fecha_accidente)
unique(accidents_data$fecha_fallecimiento)
unique(accidents_data$hora_fallecimiento) # Needs cleaning (time format) and null handling 
unique(accidents_data$hora_accidente) # Needs cleaning (time format) and null handling - Create ranges to use in the EDA ("mañana", "tarde", "noche")
unique(accidents_data$dia_semana_fallecimiento)
unique(accidents_data$dia_semana_accidente)  # Needs cleaning
unique(accidents_data$condicion) # Needs cleaning (agrupation)
unique(accidents_data$vehiculos)
unique(accidents_data$ano)


# Cleaning "mes_accidente"
accidents_data$mes_accidente[accidents_data$mes_accidente == "OCTOBRE"] <- "OCTUBRE"
unique(accidents_data$mes_accidente)


# Cleaning "barrio"
table(accidents_data$barrio)
accidents_data<- accidents_data %>% select(-c("barrio"))# after extensive analysis, it was determined to not use "barrio" variable


# Cleaning "sexo"
table(accidents_data$sexo) # F 113  M 484
accidents_data$sexo[is.na(accidents_data$sexo)] <- "M" # Mode based imputation
table(accidents_data$sexo) # F 113  M 486


# Cleaning "edad"
accidents_data$edad <- as.numeric(accidents_data$edad)
#accidents_data$edad <- as.integer(accidents_data$edad)
summary(accidents_data$edad)

# KNN imputation (k-Nearest Neighbour Imputation)
accidents_data_imputed <- kNN(accidents_data, variable = "edad", k=5) # put results in new df for comparison purposes

summary(accidents_data_imputed$edad)# imputation result

p_combined <- ggplot() +
  geom_histogram(data = accidents_data, aes(x = edad, fill = "Before Imputation"), 
                 binwidth = 5, alpha = 0.5) +
  geom_histogram(data = accidents_data_imputed, aes(x = edad, fill = "After Imputation"), 
                 binwidth = 5, alpha = 0.5) +
  labs(x = "Age", y = "Count") +
  scale_fill_manual(name = "Dataset", 
                    values = c("Before Imputation" = "black", "After Imputation" = "red")) +
  theme_minimal() +
  theme(legend.position = "top")

p_combined


accidents_data <- accidents_data_imputed # replace old dataframe
accidents_data <- accidents_data %>% select(-c("edad_imp")) # Droping unnecessary column


# Cleaning and handling NA "edad_agrupada"
unique(accidents_data$edad_agrupada)

accidents_data$edad_agrupada[accidents_data$edad_agrupada %in% c("80 Y +", "80Y+")] <- "80Y+"

accidents_data$edad_agrupada <- cut(accidents_data$edad, # Change intervals to 10-year intervals
                                    breaks = c(-Inf, 10, 20, 30, 40, 50, 60, 70, 80, Inf),
                                    labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80Y+"),
                                    right = FALSE)

unique(accidents_data$edad_agrupada)
table(accidents_data$edad_agrupada)

sum(is.na(accidents_data$edad_agrupada)) # 0


# Cleaning "hora_fallecimiento"

class(accidents_data$hora_fallecimiento) # "character"
unique(accidents_data$hora_fallecimiento)

accidents_data$hora_fallecimiento <- format(as.POSIXct(accidents_data$hora_fallecimiento, format = "%H:%M"), "%H")

class(accidents_data$hora_fallecimiento)
unique(accidents_data$hora_fallecimiento)

# Handling NA "hora_fallecimiento"
# Convert "HH" to minutes since midnight
convert_to_minutes <- function(time_str) {
  if (is.na(time_str)) return(NA)
  hours <- as.numeric(time_str)
  return(hours * 60)
}

accidents_data$hora_fallecimiento_minutes <- sapply(accidents_data$hora_fallecimiento, convert_to_minutes)

imputed_data <- kNN(accidents_data, variable = "hora_fallecimiento_minutes", k = 5) # Perform KNN imputation on the minutes column

convert_to_time <- function(minutes) {
  if (is.na(minutes)) return(NA)
  hours <- floor(minutes / 60)
  return(sprintf("%02d", hours))  # Convert back to "HH" format
}

imputed_data$hora_fallecimiento <- sapply(imputed_data$hora_fallecimiento_minutes, convert_to_time)

accidents_data <- imputed_data # Replace old dataframe with the imputed data

accidents_data <- accidents_data %>% select(-c("hora_fallecimiento_minutes_imp", "hora_fallecimiento_minutes"))

unique(accidents_data$hora_fallecimiento)
sum(is.na(accidents_data$hora_fallecimiento)) 


# Cleaning "hora_accidente"
class(accidents_data$hora_accidente) # "character"
unique(accidents_data$hora_accidente)

accidents_data$hora_accidente <- format(as.POSIXct(accidents_data$hora_accidente, format = "%H:%M"), "%H")

class(accidents_data$hora_accidente)
unique(accidents_data$hora_accidente)


# Handling NA "hora_accidente"
accidents_data$hora_accidente_minutes <- sapply(accidents_data$hora_accidente, convert_to_minutes)

imputed_data <- kNN(accidents_data, variable = "hora_accidente_minutes", k = 5) # Perform KNN imputation on the minutes column

imputed_data$hora_accidente <- sapply(imputed_data$hora_accidente_minutes, convert_to_time)

accidents_data <- imputed_data # Replace old dataframe with the imputed data

accidents_data <- accidents_data %>% select(-c("hora_accidente_minutes_imp", "hora_accidente_minutes"))

unique(accidents_data$hora_accidente)
sum(is.na(accidents_data$hora_accidente)) 


# Creating ranges
accidents_data <- accidents_data %>%
  mutate(periodo_dia_accidente = case_when(
    hora_accidente >= "06" & hora_accidente < "12" ~ "MAÑANA",
    hora_accidente >= "12" & hora_accidente < "18" ~ "TARDE",
    hora_accidente >= "18" & hora_accidente < "24" ~ "NOCHE",
    hora_accidente >= "00" & hora_accidente < "06" ~ "MADRUGADA"
  ))

unique(accidents_data$periodo_dia_accidente)


# Cleaning "dia_semana_accidente"
unique(accidents_data$dia_semana_accidente)

accidents_data$dia_semana_accidente <- toupper(accidents_data$dia_semana_accidente)
unique(accidents_data$dia_semana_accidente)


# Cleaning "condicion"
unique(accidents_data$condicion) # Needs cleaning (agrupation)

clean_condicion <- function(cond) {
  cond <- toupper(cond)  # Convert to uppercase for consistency
  if (grepl("CICLISTA", cond)) {
    return("CICLISTA")
  } else if (grepl("MOTO|MOTOCARRO", cond)) {
    return("MOTO")
  } else if (grepl("PEATON", cond)) {
    return("PEATON")
  } else {
    return("VEHICULO")  # All other categories are grouped as "VEHICULO"
  }
}

# Apply the function to the 'condiccion' column
accidents_data$condicion <- sapply(accidents_data$condicion, clean_condicion)

unique(accidents_data$condicion)
table(accidents_data$condicion)
sum(is.na(accidents_data$condicion))


# Checking duplicates values
duplicated_rows <- accidents_data[duplicated(accidents_data), ]
nrow(duplicated_rows) # 0 - No duplicate values


# Viewing the final result of the dataset
head(accidents_data)
colSums(is.na(accidents_data))
dim(accidents_data)


# Define month order 
accidents_data$dia_semana_accidente <- factor(accidents_data$dia_semana_accidente,
                                              levels = c("LUNES", "MARTES", "MIERCOLES", "JUEVES", "VIERNES", "SABADO", "DOMINGO"))

# Define month order
accidents_data$mes_accidente <- factor(accidents_data$mes_accidente,
                                       levels = c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO",
                                                  "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE"))


# Statistical analysis ----------------------------------------------------

CV <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
}

accidents_data$comuna <- factor(accidents_data$comuna)

table1 <- accidents_data %>%
  tbl_summary( by = "ano",
               include = c(condicion, dia_semana_accidente, sexo, 
                           edad_agrupada, periodo_dia_accidente,
                           mes_accidente),
               statistic = list(
                 all_continuous() ~ "{mean} ({min}, {max}) {sd} {CV}",
                 all_categorical() ~ "{n} ({p}%)"
               )
  )

table1


# Visualizations ----------------------------------------------------------

# Age Distribution
p1 <- ggplot(accidents_data, aes(x = "", y = edad)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(x = "", y = "Age")

p2 <- ggplot(accidents_data, aes(x = edad)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "darkblue", size = 1) +
  theme_minimal() +
  labs(x = "Age", y = "Density")

(p1 | p2)


# Distribution of accidents by month with year comparison
ggplot(accidents_data, aes(x = mes_accidente, fill = factor(ano))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Month of Accident",
       y = "Number of Accidents",
       fill = "Year") +
  scale_fill_manual(values = c("2009" = "lightpink4", "2010" = "lightpink"))+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Distribution of accidents by comuna with year comparison
ggplot(accidents_data, aes(x = as.factor(comuna), fill = factor(ano))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Comuna",
       y = "Number of Accidents",
       fill = "Year") +
  scale_fill_manual(values = c("2009" = "darkgreen", "2010" = "lightgreen"))


# Distribution of accidents by gender with year comparison
ggplot(accidents_data, aes(x = factor(ano), fill = sexo)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Year",
       y = "Number of Accidents",
       fill = "Gender") +
  scale_fill_manual(values = c("M" = "rosybrown", "F" = "rosybrown1")) +
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            color = "black")


# Distribution of accidents by age group with year comparison
ggplot(accidents_data, aes(x = edad_agrupada, fill = factor(ano))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Age Group",
       y = "Number of Accidents",
       fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("2009" = "orange", "2010" = "yellow"))


# Distribution of accidents by day of the week with year comparison
ggplot(accidents_data, aes(x = dia_semana_accidente, fill = factor(ano))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Day of the Week",
       y = "Number of Accidents",
       fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("2009" = "lightcoral", "2010" = "darkred"))


# Distribution of accidents by hour of the day with year comparison
ggplot(accidents_data, aes(x = hora_accidente, fill = factor(ano))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Hour of the Day",
       y = "Number of Accidents",
       fill = "Year") +
  scale_fill_manual(values = c("2009" = "lightcoral", "2010" = "darkred"))


# Distribution of accidents by period  of the day with year comparison
ggplot(accidents_data, aes(x = periodo_dia_accidente, fill = factor(ano))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Period of the Day",
       y = "Number of Accidents",
       fill = "Year") +
  scale_fill_manual(values = c("2009" = "lightcoral", "2010" = "darkred"))


# Distribution of accidents by condition of the person with year comparison
ggplot(accidents_data, aes(x = condicion, fill = factor(ano))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Condition of the Person",
       y = "Number of Accidents",
       fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("2009" = "dodgerblue", "2010" = "lightblue"))


# Homicides by traffic accidents, broken down by month and gender with year comparison
ggplot(accidents_data, aes(x = mes_accidente, fill = sexo)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ ano, scales = "free_x") +
  theme_minimal() +
  labs(x = "Month of Accident",
       y = "Number of Homicides",
       fill = "Gender") +
  scale_fill_manual(values = c("M" = "royalblue", "F" = "lightpink")) +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.line = element_line(color = "grey"),
    panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Homicides by traffic accidents, broken down by condition and gender with year comparison
ggplot(accidents_data, aes(x = condicion, fill = sexo)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ ano, scales = "free_x") +
  theme_minimal() +
  labs(x = "Condition of the Person",
       y = "Number of Homicides",
       fill = "Gender") +
  scale_fill_manual(values = c("M" = "royalblue", "F" = "lightpink")) +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.line = element_line(color = "grey"),
    panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Homicides by traffic accidents, broken down by age group and condition with year comparison
ggplot(accidents_data, aes(x = edad_agrupada, fill = condicion)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ ano) +
  theme_minimal() +
  labs(x = "Age Group",
       y = "Number of Homicides",
       fill = "Condition") +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.line = element_line(color = "grey"),
    panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "RdYlBu")


# Boxplot of age by condition with year comparison
ggplot(accidents_data, aes(x = condicion, y = edad, fill = factor(ano))) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Condition of the Person",
       y = "Age",
       fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("2009" = "lightblue", "2010" = "royalblue3"))


# Analysis of point patterns by gender, age group and type of vehicle --------

# transforming coords data
options(digits=8)
accidents_data$coordenada_x_km <- as.numeric(gsub(",", ".", accidents_data$coordenada_x_km))
accidents_data$coordenada_y_km <- as.numeric(gsub(",", ".", accidents_data$coordenada_y_km))

# trasforming age 
accidents_data$edad_agrupada <- cut(accidents_data$edad,
                       breaks = c(-Inf, 11, 18, 26, 59, Inf),
                       labels = c("0-11",    # infancia
                                  "12-18", # adolescencia
                                  "18-26", # juventud
                                  "27-59",  # adultez
                                  "60+"),    # persona mayor
                       right = TRUE)  # right=TRUE includes upper limit

# filter data
data_2009 <- accidents_data %>% filter(ano == 2009)
data_2010 <- accidents_data %>% filter(ano == 2010)

# Spatial EDA

# Map graphic
cali_map<-readShapePoly("Mapas/mapa_km.shp") 

par(mfrow = c(1, 1))
plot(cali_map, axes=T)
points(accidents_data$coordenada_x_km, accidents_data$coordenada_y_km, pch=19, cex=0.5)

# generate border
map_border<-readShapePoly("Mapas/borde_km.shp")
#plot(map_border)
borde_owin<-as(map_border, "owin")
#class(borde_owin)

# ppp objects
# accidents_2009_ppp <- ppp(data_2009$coordenada_x_km, data_2009$coordenada_y_km, window = borde_poly)
# accidents_2010_ppp <- ppp(data_2010$coordenada_x_km, data_2010$coordenada_y_km, window = borde_poly)

accidents_2009_ppp <- ppp(
  x = data_2009$coordenada_x_km,
  y = data_2009$coordenada_y_km,
  window = borde_owin)

accidents_2010_ppp <- ppp(
  x = data_2010$coordenada_x_km,
  y = data_2010$coordenada_y_km,
  window = borde_owin)


# Quadrant analysis

quadrat_test_2009 <- quadrat.test(accidents_2009_ppp, nx = 4, ny = 5) # p-value = 2.9055e-13
quadrat_test_2009

quadrat_test_2010 <- quadrat.test(accidents_2010_ppp, nx = 4, ny = 5) # p-value = 4.9393e-07
quadrat_test_2010


# Quadrant frequency plot
par(mfrow =c(1,2))
plot(accidents_2009_ppp, cex = 0.5, pch = "+", main="2009")
plot(quadratcount(accidents_2009_ppp, nx = 4, ny = 5), add=T, col=9)
plot(cali_map, add=T, border=8)

plot(accidents_2010_ppp, cex = 0.5, pch = "+", main="2010") 
plot(quadratcount(accidents_2010_ppp, nx = 4, ny = 5), add=T, col=9)
plot(cali_map, add = TRUE, border = 8)

# variables exploratory analysis------------------------

# GENDER ANALYSIS
gender_2009_ppp <- ppp(data_2009$coordenada_x_km, data_2009$coordenada_y_km,
                       window = borde_owin, marks = data_2009$sexo)

gender_2010_ppp <- ppp(data_2010$coordenada_x_km, data_2010$coordenada_y_km,
                       window = borde_owin, marks = data_2010$sexo)

# Map per categories
par(mfrow = c(1, 2))
plot(gender_2009_ppp, main = "2009", cols = 1:3)
plot(gender_2010_ppp, main = "2010", cols = 1:3)
# caption: Homicides by Traffic Accidents per Gender

# Density estimation using Kernel

density_male_2009 <- density(gender_2009_ppp[gender_2009_ppp$marks == "M"], sigma = bw.diggle)
density_female_2009 <- density(gender_2009_ppp[gender_2009_ppp$marks == "F"], sigma = bw.diggle)

density_male_2010 <- density(gender_2010_ppp[gender_2010_ppp$marks == "M"], sigma = bw.diggle)
density_female_2010 <- density(gender_2010_ppp[gender_2010_ppp$marks == "F"], sigma = bw.diggle)


par(mfrow = c(2, 2))
plot(density_male_2009, main = "Male (2009)")
plot(map_border, add = TRUE)
plot(density_female_2009, main = "Female (2009)")
plot(map_border, add = TRUE)

plot(density_male_2010, main = "Male (2010)")
plot(map_border, add = TRUE)
plot(density_female_2010, main = "Female (2010)")
plot(map_border, add = TRUE)

#mtext("Density of Homicides by Traffic Accidents per Gender and Year", side = 3, outer = TRUE, line = -1.5, cex = 1.5)


# Inhomogeneous Ripley's K function

# Ripley's K function if used for stationary and isotropic processes, so we use instead, the Kinhom() function 
# to compute a generalization of the K function for inhomogeneous point patterns (Baddeley et al. 2000)

# 2009
K_inhom_male_2009 <- Kinhom(gender_2009_ppp[gender_2009_ppp$marks == "M"], lambda = density_male_2009)
K_inhom_female_2009 <- Kinhom(gender_2009_ppp[gender_2009_ppp$marks == "F"], lambda = density_female_2009)

# 2010
K_inhom_male_2010 <- Kinhom(gender_2010_ppp[gender_2010_ppp$marks == "M"], lambda = density_male_2010)
K_inhom_female_2010 <- Kinhom(gender_2010_ppp[gender_2010_ppp$marks == "F"], lambda = density_female_2010)


par(mfrow = c(2, 2))
plot(K_inhom_male_2009, main = "Male (2009)")
plot(K_inhom_female_2009, main = "Female (2009)", legend=FALSE)

plot(K_inhom_male_2010, main = "Male (2010)", legend=FALSE)
plot(K_inhom_female_2010, main = "Female (2010)", legend=FALSE)



# AGE
age_2009_ppp <- ppp(data_2009$coordenada_x_km, 
                    data_2009$coordenada_y_km, 
                    window = borde_owin, 
                    marks = data_2009$edad_agrupada)

age_2010_ppp <- ppp(data_2010$coordenada_x_km,
                    data_2010$coordenada_y_km, 
                    window = borde_owin, 
                    marks = data_2010$edad_agrupada)

# map per categories
par(mfrow = c(1, 2))
plot(age_2009_ppp, main = "2009", cols = 1:3)
plot(age_2010_ppp, main = "2010", cols = 1:3)
# caption: Homicides by Traffic Accidents per Age Group


# Density estimation using Kernel

# 2009 Density
density_0_11_2009 <- density(age_2009_ppp[age_2009_ppp$marks == "0-11"], sigma = bw.diggle)
density_12_18_2009 <- density(age_2009_ppp[age_2009_ppp$marks == "12-18"], sigma = bw.diggle)
density_18_26_2009 <- density(age_2009_ppp[age_2009_ppp$marks == "19-26"], sigma = bw.diggle)
density_27_59_2009 <- density(age_2009_ppp[age_2009_ppp$marks == "27-59"], sigma = bw.diggle)
density_60_plus_2009 <- density(age_2009_ppp[age_2009_ppp$marks == "60+"], sigma = bw.diggle)

# 2010 Density
#density_0_11_2010 <- density(age_2010_ppp[age_2010_ppp$marks == "0-11"], sigma = bw.diggle)
density_12_18_2010 <- density(age_2010_ppp[age_2010_ppp$marks == "12-18"], sigma = bw.diggle)
density_18_26_2010 <- density(age_2010_ppp[age_2010_ppp$marks == "19-26"], sigma = bw.diggle)
density_27_59_2010 <- density(age_2010_ppp[age_2010_ppp$marks == "27-59"], sigma = bw.diggle)
density_60_plus_2010 <- density(age_2010_ppp[age_2010_ppp$marks == "60+"], sigma = bw.diggle)


# "Densidad de accidentes de tránsito fatales"

par(mar = c(2, 2, 1, 1))
par(mfrow = c(4, 2))

plot(density_0_11_2009, main = "0-11 years, 2009")
plot(map_border, add = TRUE)


plot(density_12_18_2009, main = "12-18 years, 2009")
plot(map_border, add = TRUE)
plot(density_12_18_2010, main = "12-18 years, 2010")
plot(map_border, add = TRUE)

plot(density_18_26_2009, main = "18-26 years, 2009")
plot(map_border, add = TRUE)
plot(density_18_26_2010, main = "18-26 years, 2010")
plot(map_border, add = TRUE)

plot(density_27_59_2009, main = "27-59 years, 2009")
plot(map_border, add = TRUE)
plot(density_27_59_2010, main = "27-59 years, 2010")
plot(map_border, add = TRUE)

plot(density_60_plus_2009, main = "60+ years, 2009")
plot(map_border, add = TRUE)
plot(density_60_plus_2010, main = "60+ years, 2010")
plot(map_border, add = TRUE)


# Ripley's K function

# 2009
K_inhom_0_11_2009 <- Kinhom(age_2009_ppp[age_2009_ppp$marks == "0-11"], lambda = density_0_11_2009)
K_inhom_12_18_2009 <- Kinhom(age_2009_ppp[age_2009_ppp$marks == "12-18"], lambda = density_12_18_2009)
K_inhom_18_26_2009 <- Kinhom(age_2009_ppp[age_2009_ppp$marks == "18-26"], lambda = density_18_26_2009)
K_inhom_27_59_2009 <- Kinhom(age_2009_ppp[age_2009_ppp$marks == "27-59"], lambda = density_27_59_2009)
K_inhom_60_plus_2009 <- Kinhom(age_2009_ppp[age_2009_ppp$marks == "60+"], lambda = density_60_plus_2009)


# 2010
#K_inhom_0_11_2010 <- Kinhom(age_2009_ppp[age_2010_ppp$marks == "0-11"], lambda = density_0_11_2009)
K_inhom_12_18_2010 <- Kinhom(age_2010_ppp[age_2010_ppp$marks == "12-18"], lambda = density_12_18_2010)
K_inhom_18_26_2010 <- Kinhom(age_2010_ppp[age_2010_ppp$marks == "18-26"], lambda = density_18_26_2010)
K_inhom_27_59_2010 <- Kinhom(age_2010_ppp[age_2010_ppp$marks == "27-59"], lambda = density_27_59_2010)
K_inhom_60_plus_2010 <- Kinhom(age_2010_ppp[age_2010_ppp$marks == "60+"], lambda = density_60_plus_2010)

par(mfrow = c(2, 3))
plot(K_inhom_0_11_2009, main = "0-11 years (2009)")
plot(K_inhom_12_18_2009, main = "12-18 years (2009)", legend=FALSE)
plot(K_inhom_18_26_2009, main = "18-26 years (2009)", legend=FALSE)
plot(K_inhom_27_59_2009, main = "27-59 years (2009)", legend=FALSE)
plot(K_inhom_60_plus_2009, main = "60+ years (2009)", legend=FALSE)

par(mfrow = c(2, 2))
#plot(K_inhom_0_11_2010, main = "0-11 years", legend=FALSE) # makes no sense to graph as there's only one data point for this category
plot(K_inhom_12_18_2010, main = "12-18 years (2010)", legend=FALSE)
plot(K_inhom_18_26_2010, main = "18-26 years (2010)", legend=FALSE)
plot(K_inhom_27_59_2010, main = "27-59 years (2010)", legend=FALSE)
plot(K_inhom_60_plus_2010, main = "60+ years (2010)", legend=FALSE)



# VEHICLE
condition_2009_ppp <- ppp(data_2009$coordenada_x_km,
                          data_2009$coordenada_y_km,
                          window = borde_owin, 
                          marks = data_2009$condicion)

condition_2010_ppp <- ppp(data_2010$coordenada_x_km,
                          data_2010$coordenada_y_km,
                          window = borde_owin, 
                          marks = data_2010$condicion)

# map per categories
par(mfrow = c(1, 2))
plot(condition_2009_ppp, main = "2009", cols = 1:3)
plot(condition_2010_ppp, main = "2010", cols = 1:3)

# Density estimation using Kernel

# 2009 Density
density_moto_2009 <- density(condition_2009_ppp[condition_2009_ppp$marks == "MOTO"], sigma = bw.diggle)
density_peaton_2009 <- density(condition_2009_ppp[condition_2009_ppp$marks == "PEATON"], sigma = bw.diggle)
density_vehiculo_2009 <- density(condition_2009_ppp[condition_2009_ppp$marks == "VEHICULO"], sigma = bw.diggle)
density_ciclista_2009 <- density(condition_2009_ppp[condition_2009_ppp$marks == "CICLISTA"], sigma = bw.diggle)

# 2010 Density
density_moto_2010 <- density(condition_2010_ppp[condition_2010_ppp$marks == "MOTO"], sigma = bw.diggle)
density_peaton_2010 <- density(condition_2010_ppp[condition_2010_ppp$marks == "PEATON"], sigma = bw.diggle)
density_vehiculo_2010 <- density(condition_2010_ppp[condition_2010_ppp$marks == "VEHICULO"], sigma = bw.diggle)
density_ciclista_2010 <- density(condition_2010_ppp[condition_2010_ppp$marks == "CICLISTA"], sigma = bw.diggle)


par(mar = c(2, 2, 1, 1))
par(mfrow = c(4, 2))

plot(density_moto_2009, main = "Motorcycle (2009)")
plot(map_border, add = TRUE)
plot(density_moto_2010, main = "Motorcycle (2010)")
plot(map_border, add = TRUE)

plot(density_peaton_2009, main = "Pedestrian (2009)")
plot(map_border, add = TRUE)
plot(density_peaton_2010, main = "Pedestrian (2010)")
plot(map_border, add = TRUE)

plot(density_ciclista_2009, main = "Cyclist (2009)")
plot(map_border, add = TRUE)
plot(density_ciclista_2010, main = "Cyclist (2010)")
plot(map_border, add = TRUE)

plot(density_vehiculo_2009, main = "Other Vehicle (2009)")
plot(map_border, add = TRUE)
plot(density_vehiculo_2010, main = "Other Vehicle (2010)")
plot(map_border, add = TRUE)


# Ripley's K function

K_inhom_moto_2009 <- Kinhom(condition_2009_ppp[condition_2009_ppp$marks == "MOTO"], lambda = density_moto_2009)
K_inhom_peaton_2009 <- Kinhom(condition_2009_ppp[condition_2009_ppp$marks == "PEATON"], lambda = density_peaton_2009)
K_inhom_ciclista_2009 <- Kinhom(condition_2009_ppp[condition_2009_ppp$marks == "CICLISTA"], lambda = density_ciclista_2009)
K_inhom_vehiculo_2009 <- Kinhom(condition_2009_ppp[condition_2009_ppp$marks == "VEHICULO"], lambda = density_vehiculo_2009)

# 2010
K_inhom_moto_2010 <- Kinhom(condition_2010_ppp[condition_2010_ppp$marks == "MOTO"], lambda = density_moto_2010)
K_inhom_peaton_2010 <- Kinhom(condition_2010_ppp[condition_2010_ppp$marks == "PEATON"], lambda = density_peaton_2010)
K_inhom_ciclista_2010 <- Kinhom(condition_2010_ppp[condition_2010_ppp$marks == "CICLISTA"], lambda = density_ciclista_2010)
K_inhom_vehiculo_2010 <- Kinhom(condition_2010_ppp[condition_2010_ppp$marks == "VEHICULO"], lambda = density_vehiculo_2010)

par(mfrow = c(2, 2))
plot(K_inhom_moto_2009, main = "Motorcycle", legend=FALSE)
plot(K_inhom_peaton_2009, main = "Pedestrian", legend=FALSE)
plot(K_inhom_ciclista_2009, main = "Cyclist", legend=FALSE)
plot(K_inhom_vehiculo_2009, main = "Other Vehicle", legend=FALSE)

par(mfrow = c(2, 2))
plot(K_inhom_moto_2010, main = "Motorcycle", legend=FALSE)
plot(K_inhom_peaton_2010, main = "Pedestrian", legend=FALSE)
plot(K_inhom_ciclista_2010, main = "Cyclist", legend=FALSE)
plot(K_inhom_vehiculo_2010, main = "Other Vehicle", legend=FALSE)


# Model fit (Marked point patterns)--------------


### GENDER(Sexo) 2009###

data_2009$sexo <- factor(data_2009$sexo, levels = c("F", "M"))

# Create point patterns
accidents_2009_sexo_ppp <- ppp(x = data_2009$coordenada_x_km, 
                          y = data_2009$coordenada_y_km, 
                          window = borde_owin,
                          marks = data_2009$sexo) 
plot(accidents_2009_sexo_ppp)


# Fitting the inhomogeneous Poisson model for the marked point pattern
fit_marked <- ppm(accidents_2009_sexo_ppp, ~marks)

plot(predict(fit_marked))

# View the coefficients of the fitted model
fit_marked$coef

summary(fit_marked)


# Este es un ejemplo siguiendo el código de Ramón Giraldo -----------------
# Mostrar el patrón de puntos
plot(accidents_2009_ppp, pch=20)

# Proceso Poisson No Homogéneo con Marcas

# Definir la función de intensidad lambda
lambda <- function(x, y) {
  100 * (x + y) # La intensidad aumenta con x e y
}

# Generar un proceso Poisson inhomogéneo basado en la función de intensidad
nohomo <- rpoispp(lambda, win = borde_owin)
summary(nohomo)

# Visualizar el patrón no homogéneo
plot(nohomo, main="Patrón Poisson No Homogéneo", pch=20)
plot(density(nohomo), main="Estimación Kernel de la Intensidad No Homogénea")
contour(density(nohomo), add=TRUE)
points(nohomo, pch=20)

# Ajuste de un modelo lineal a la intensidad usando las marcas
fit_nohomo <- ppm(accidents_2009_ppp, ~marks + x + y) 
summary(fit_nohomo)

# Mostrar los coeficientes del modelo ajustado
fit_nohomo$coef

# Función para calcular la intensidad en un punto (x, y) usando los coeficientes ajustados
lambda_lineal <- function(x, y, fit) {
  lambda <- exp(fit[[1]] + fit[[2]] * x + fit[[3]] * y)
  return(lambda)
}

# Estimar la intensidad en un punto específico
x <- 0.7
y <- 0.8
lambda_estimada <- lambda_lineal(x, y, fit_nohomo$coef)
print(lambda_estimada) # Estimación de la función de intensidad en x=0.7 y y=0.8

# Comparar contra el gráfico de densidad
par(mfrow=c(1,2))
plot(density(nohomo), main="Patrón No Homogéneo - Estimación No Paramétrica")
contour(density(nohomo), add=TRUE)
points(x, y, col=2, pch=19)

plot(fit_nohomo, se=FALSE, main="Modelo Lineal Ajustado a la Intensidad")
points(x, y, col=2, pch=19)

# Ajuste de un modelo polinomial
fit_nohomo2 <- ppm(accidents_2009_ppp, ~polynom(x,2) + y)
summary(fit_nohomo2)
plot(fit_nohomo2, se=FALSE, main="Modelo Polinomial Ajustado")

fit_nohomo3 <- ppm(accidents_2009_ppp, ~polynom(x, y, 2))
summary(fit_nohomo3)
plot(fit_nohomo3, se=FALSE, main="Modelo Polinomial con Interacción")

# Comparación de modelos usando la tabla de desviación basada en la razón de verosimilitud
anova(fit_nohomo, fit_nohomo2, test="Chi")
anova(fit_nohomo2, fit_nohomo3, test="Chi")


# -------------------------------------------------------------------------


### GENDER(Sexo) 2010###
data_2010$sexo <- factor(data_2010$sexo, levels = c("F", "M"))

# Create point patterns
accidents_2010_sexo_ppp <- ppp(x = data_2010$coordenada_x_km, 
                          y = data_2010$coordenada_y_km, 
                          window = borde_owin,
                          marks = data_2010$sexo) 
plot(accidents_2010_sexo_ppp)


# Fitting the inhomogeneous Poisson model for the marked point pattern
fit_marked <- ppm(accidents_2010_sexo_ppp, ~marks)

plot(predict(fit_marked))

# View the coefficients of the fitted model
fit_marked$coef

summary(fit_marked)


### VEHICLE(Condicion) 2009###
data_2009$condicion <- factor(data_2009$condicion, levels = c("MOTO", "PEATON", "CICLISTA", "VEHICULO"))

# Create point patterns
accidents_2009_condicion_ppp <- ppp(x = data_2009$coordenada_x_km, 
                          y = data_2009$coordenada_y_km, 
                          window = borde_owin,
                          marks = data_2009$condicion) 
plot(accidents_2009_condicion_ppp)


# Fitting the inhomogeneous Poisson model for the marked point pattern
fit_marked <- ppm(accidents_2009_condicion_ppp, ~marks)

plot(predict(fit_marked))

# View the coefficients of the fitted model
fit_marked$coef

summary(fit_marked)


### VEHICLE(Condicion) 2010###
data_2010$condicion <- factor(data_2010$condicion, levels = c("MOTO", "PEATON", "CICLISTA", "VEHICULO"))

accidents_2010_condicion_ppp <- ppp(x = data_2010$coordenada_x_km, 
                          y = data_2010$coordenada_y_km, 
                          window = borde_owin,
                          marks = data_2010$condicion) 
plot(accidents_2010_condicion_ppp)

# Fitting the inhomogeneous Poisson model for the marked point pattern
fit_marked <- ppm(accidents_2010_condicion_ppp, ~marks)

plot(predict(fit_marked))

# View the coefficients of the fitted model
fit_marked$coef

summary(fit_marked)


### AGE GROUP (Edad agrupada) 2009###

data_2009$edad_agrupada <- factor(data_2009$edad_agrupada, levels = c("0-11", "12-18", "18-26", "27-59", "60+"))

# Create point patterns
accidents_2009_edad_agrupada_ppp <- ppp(x = data_2009$coordenada_x_km, 
                                    y = data_2009$coordenada_y_km, 
                                    window = borde_owin,
                                    marks = data_2009$edad_agrupada) 
plot(accidents_2009_edad_agrupada_ppp)


# Fitting the inhomogeneous Poisson model for the marked point pattern
fit_marked <- ppm(accidents_2009_edad_agrupada_ppp, ~marks)

plot(predict(fit_marked))

# View the coefficients of the fitted model
fit_marked$coef

summary(fit_marked)



### AGE GROUP (Edad agrupada) 2010###

data_2010$edad_agrupada <- factor(data_2010$edad_agrupada, levels = c("0-11", "12-18", "18-26", "27-59", "60+"))

accidents_2010_edad_agrupada_ppp <- ppp(x = data_2010$coordenada_x_km, 
                                    y = data_2010$coordenada_y_km, 
                                    window = borde_owin,
                                    marks = data_2010$edad_agrupada) 
plot(accidents_2010_edad_agrupada_ppp)

# Fitting the inhomogeneous Poisson model for the marked point pattern
fit_marked <- ppm(accidents_2010_edad_agrupada_ppp, ~marks)

plot(predict(fit_marked))

# View the coefficients of the fitted model
fit_marked$coef

summary(fit_marked)


