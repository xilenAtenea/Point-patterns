
# Loading packages --------------------------------------------------------

to_be_loaded <- c("readxl",
                  "dplyr",
                  "VIM",
                  "patchwork",
                  "ggplot2"
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

# EDA -------------------------------------------------------------------------


# Loading the data
data_2009 <- read_xlsx('AccidentesFatales_2009.xlsx')
data_2010 <- read_xlsx('AccidentesFatales_2010.xlsx')


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
data_2010 <- data_2010 %>% select(-c("FECHA INSP.", "Ciudad"))
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
colnames(accidents_data) <- gsub(" ", "_", colnames(accidents_data))
colnames(accidents_data)[colnames(accidents_data) == "com"] <- "comuna"
colnames(accidents_data)[colnames(accidents_data) == "condiccion"] <- "condicion"
colnames(accidents_data) <- gsub("\\(", "", colnames(accidents_data))
colnames(accidents_data) <- gsub("\\)", "", colnames(accidents_data))
colnames(accidents_data)



# Cleaning  ---------------------------------------------------------------

# Checking null data
colSums(is.na(accidents_data))

na_values <- c("", "NA", "N/A", "NULL", "No aplica", "Desconocido") # Checking for other notations of "NA"
accidents_data <- accidents_data %>%
  mutate_all(~ replace(., . %in% na_values, NA))
colSums(is.na(accidents_data))


accidents_data  <-  accidents_data %>% select(-c("profesion")) # We´re not gonna use this column

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
unique(accidents_data$hora_fallecimiento) # Needs cleaning (time standarization) and null handling
unique(accidents_data$hora_accidente) # Needs cleaning (time standarization) and null handling
unique(accidents_data$dia_semana_fallecimiento)
unique(accidents_data$dia_semana_accidente)  # Needs cleaning
unique(accidents_data$condiccion) # Needs cleaning (agrupation)
unique(accidents_data$vehiculos)
unique(accidents_data$ano)


# Cleaning "mes_accidente"
accidents_data$mes_accidente[accidents_data$mes_accidente == "OCTOBRE"] <- "OCTUBRE"
unique(accidents_data$mes_accidente)

# Cleaning "barrio"
table(accidents_data$barrio)
accidents_data<- accidents_data %>% select(-c("barrio"))# after extensive analysis, it was determined to not use "barrio" varaible


# Cleaning "sexo"
table(accidents_data$sexo) # F 113  M 484 
accidents_data$sexo[is.na(accidents_data$sexo)] <- "M" # Mode based imputation
table(accidents_data$sexo) # F 113  M 486

# Cleaning "edad"

accidents_data$edad <- as.numeric(accidents_data$edad)
#accidents_data$edad <- as.integer(accidents_data$edad)
summary(accidents_data$edad)

# KNN imputation (k-Nearest Neighbour Imputation)
accidents_data_imputed <- kNN(accidents_data, variable = "edad", k=10) # put results in new df for comparison purposes

# imputation result
summary(accidents_data_imputed$edad)


p1 <- ggplot(accidents_data, aes(x = edad)) + 
  geom_histogram(binwidth = 5, fill = 'blue', alpha = 0.5) +
  ggtitle("Age distribution before Imputation")

p2 <- ggplot(accidents_data_imputed, aes(x = edad)) + 
  geom_histogram(binwidth = 5, fill = 'red', alpha = 0.5) +
  ggtitle("Age distribution after Imputation")

(p1 | p2) # plot histograms side to side

# replace old dataframe
accidents_data <- accidents_data_imputed

# Cleaning "edad_agrupada"
unique(accidents_data$edad_agrupada)

accidents_data$edad_agrupada[accidents_data$edad_agrupada %in% c("80 Y +", "80Y+")] <- "80Y+"

accidents_data$edad_agrupada <- cut(accidents_data$edad, # Change intervals to 10-year intervals
                                    breaks = c(-Inf, 10, 20, 30, 40, 50, 60, 70, 80, Inf),
                                    labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80Y+"),
                                    right = FALSE)

unique(accidents_data$edad_agrupada)
table(accidents_data$edad_agrupada)

#sum(is.na(accidents_data$edad_agrupada)) # 0

# Cleaning "hora_fallecimiento"
unique(accidents_data$hora_fallecimiento)

convert_time <- function(fraction_of_day) {
  if (is.na(fraction_of_day)) return(NA)  # Check for NA values and return NA if found
  
  fraction_as_numeric <- as.numeric(fraction_of_day) # string to numeric
  
  if (is.na(fraction_as_numeric) || fraction_as_numeric > 1 || fraction_as_numeric < 0) { # Check for conversion issues and out-of-range values
    return(fraction_of_day)  # Return original value if not a valid fraction of a day
  }
  
  total_hours <- fraction_as_numeric * 24 # Multiply by 24 to get the hour in 24-hour format
  
  hours <- floor(total_hours) # Extract the hours and minutes from the total hours
  minutes <- round((total_hours - hours) * 60)
  
  minutes <- sprintf("%02d", minutes) # Ensure minutes are formatted with two digits
  
  return(paste(hours, minutes, sep = ":")) # Return the time in HH:MM format
}

accidents_data$hora_fallecimiento <- sapply(accidents_data$hora_fallecimiento, convert_time)

unique(accidents_data$hora_fallecimiento)

# Handling NA "hora_fallecimiento"

# NOTA: Imputación multiple



# Cleaning "hora_accidente"
unique(accidents_data$hora_accidente)

# NOTA: Imputación multiple



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
    return("PEATÓN")
  } else {
    return("VEHÍCULO")  # All other categories are grouped as "VEHÍCULO"
  }
}

# Apply the function to the 'condiccion' column
accidents_data$condicion <- sapply(accidents_data$condicion, clean_condicion)

# Check the unique values after cleaning
unique(accidents_data$condicion)
table(accidents_data$condicion)

