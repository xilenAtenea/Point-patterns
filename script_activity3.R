
# Loading packages --------------------------------------------------------

to_be_loaded <- c("readxl",
                  "dplyr",
                  "VIM",
                  "patchwork",
                  "ggplot2",
                  "gtsummary",
                  "gridExtra",
                  "lubridate"
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
unique(accidents_data$lugar_insp.)
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

p1 <- ggplot(accidents_data, aes(x = edad)) + 
  geom_histogram(binwidth = 5, fill = 'blue', alpha = 0.5) +
  ggtitle("Age distribution before Imputation")

p2 <- ggplot(accidents_data_imputed, aes(x = edad)) + 
  geom_histogram(binwidth = 5, fill = 'red', alpha = 0.5) +
  ggtitle("Age distribution after Imputation")

(p1 | p2) # plot histograms side to side

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

# sum(is.na(accidents_data$edad_agrupada)) # 0


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

# Convert "HH:MM" to minutes since midnight
convert_to_minutes <- function(time_str) {
  if (is.na(time_str)) return(NA)
  time_parts <- strsplit(time_str, ":")[[1]]
  hours <- as.numeric(time_parts[1])
  minutes <- as.numeric(time_parts[2])
  return(hours * 60 + minutes)
}

accidents_data$hora_fallecimiento_minutes <- sapply(accidents_data$hora_fallecimiento, convert_to_minutes)

imputed_data <- kNN(accidents_data, variable = "hora_fallecimiento_minutes", k = 5) # Perform KNN imputation

# Convert the imputed minutes back to "HH:MM"
convert_to_time <- function(minutes) {
  if (is.na(minutes)) return(NA)
  hours <- floor(minutes / 60)
  mins <- round(minutes %% 60)
  return(sprintf("%02d:%02d", hours, mins))
}

# Apply the reverse conversion to obtain the imputed values in HH:MM
imputed_data$hora_fallecimiento <- sapply(imputed_data$hora_fallecimiento_minutes, convert_to_time)

accidents_data <- imputed_data # replace old dataframe
accidents_data <- accidents_data %>% select(-c("hora_fallecimiento_minutes_imp", "hora_fallecimiento_minutes")) # Droping unnecessary column

unique(accidents_data$hora_fallecimiento)
# sum(is.na(accidents_data$hora_fallecimiento)) # 0


# Cleaning "hora_accidente"
unique(accidents_data$hora_accidente)

convert_fraction_to_hhmm <- function(fraction) {
  hours <- floor(fraction * 24)
  minutes <- round((fraction * 24 - hours) * 60)
  return(sprintf("%02d:%02d", hours, minutes))
}

is_fraction <- grepl("^[0-9]+\\.?[0-9]*$", accidents_data$hora_accidente)
accidents_data$hora_accidente[is_fraction] <- sapply(as.numeric(accidents_data$hora_accidente[is_fraction]), convert_fraction_to_hhmm)

accidents_data$hora_accidente[!is_fraction] <- NA # Convert negative timestamps to NA 

unique(accidents_data$hora_accidente)
sum(is.na(accidents_data$hora_accidente))


# Handling NA "hora_accidente"
accidents_data$hora_accidente_minutes <- sapply(accidents_data$hora_accidente, convert_to_minutes)

imputed_data_hora_accidente <- kNN(accidents_data, variable = "hora_accidente_minutes", k = 5) # Perform KNN imputation

# Apply the reverse conversion to obtain the imputed values in HH:MM
imputed_data_hora_accidente$hora_accidente <- sapply(imputed_data_hora_accidente$hora_accidente_minutes, convert_to_time)

accidents_data <- imputed_data_hora_accidente # replace old dataframe
accidents_data <- accidents_data %>% select(-c("hora_accidente_minutes_imp", "hora_accidente_minutes"))

unique(accidents_data$hora_accidente)
# sum(is.na(accidents_data$hora_accidente)) # 0
# table(accidents_data$hora_accidente)


# Cleaning "dia_semana_accidente"
unique(accidents_data$dia_semana_accidente)

accidents_data$dia_semana_accidente <- toupper(accidents_data$dia_semana_accidente)
unique(accidents_data$dia_semana_accidente)


# Cleaning "lugar_insp."
unique(accidents_data$lugar_insp.)

accidents_data$lugar_insp. <- gsub("^CL[.]?\\s*REM[EDIOS]*$", "CL. REMEDIOS", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^CL[.]?\\s*TEQUENDAMA$", "CL. TEQUENDAMA", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^CL[.]?\\s*SANTILLANA$", "CL. SANTILLANA", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^CL[.]?\\s*SANTILANA$", "CL. SANTILLANA", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^CL[.]?\\s*SANTIAGO DE CALI$", "CL. SANTIAGO", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^CL\\s*VALLE LILI$", "CL. VALLE LILI", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^VALLE LILI$", "CL. VALLE LILI", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^CL[.]?\\s*OCCIDENTE$", "CL. OCCIDENTE", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^CL[.]?\\s*FARALLONES$", "CL. FARALLONES", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^H[.]?U[.]?V[.]?$", "H.U.V.", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^H[.]?J[.]?P[.]?B[.]?$", "HOSPITAL J.P.B.", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^HOSPITAL J[.]?P[.]?B[.]?$", "HOSPITAL J.P.B.", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^H[.]?C[.]?H[.]?T[.]?$", "HOSPITAL C.H.T.", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^HOSPITAL C[.]?H[.]?T[.]?$", "HOSPITAL C.H.T.", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^H[.]? PRIMITIVO IGLESIAS$", "PRIMITIVO IGLESIAS", accidents_data$lugar_insp.)
accidents_data$lugar_insp. <- gsub("^I[.]?S[.]?S[.]?$", "I.S.S.", accidents_data$lugar_insp.)

!!!!!!!!!!!!!!!!!!!!!!!
# TODO agrupar las VIAs

unique(accidents_data$lugar_insp.)
table(accidents_data$lugar_insp.)

# Handling NA "lugar_insp."
mode <- names(sort(table(accidents_data$lugar_insp.), decreasing = TRUE))[1]
accidents_data$lugar_insp.[is.na(accidents_data$lugar_insp.)] <- mode # Impute the null value with the mode


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



# Statistical analysis ----------------------------------------------------

CV <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
}

accidents_data$comuna <- factor(accidents_data$comuna)

table1 <- accidents_data %>%
  tbl_summary(
    include = c(mes_fallecimiento, mes_accidente, comuna, sexo, edad, 
                fecha_accidente, fecha_fallecimiento, hora_fallecimiento, hora_accidente, 
                dia_semana_fallecimiento, dia_semana_accidente, lugar_insp., condicion, vehiculos, ano),
    statistic = list(
      all_continuous() ~ "{mean} ({min}, {max}) {sd} {CV}",
      all_categorical() ~ "{n} ({p}%)"
    )
  )

table1

# An error occurs because mathematical operations, such as division (/), are not defined for objects of type POSIXt,
# since these variables represent dates and are not numeric. This error does not affect the execution of the rest of the code,
# simply the Coefficient of Variation (CV) will not be calculated for these date type variables.

# Visualizations ----------------------------------------------------------

# Age Distribution
p1 <- ggplot(accidents_data, aes(x = "", y = edad)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Age Distribution (Boxplot)", x = "", y = "Age")

p2 <- ggplot(accidents_data, aes(x = edad)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "darkblue", size = 1) +
  theme_minimal() +
  labs(title = "Age Distribution (Histogram)", x = "Age", y = "Density")

(p1 | p2)

# Distribution of accidents by month with year comparison
ggplot(accidents_data, aes(x = mes_accidente, fill = factor(ano))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Accidents by Month",
       x = "Month of Accident",
       y = "Number of Accidents",
       fill = "Year") +
  scale_fill_manual(values = c("2009" = "blue", "2010" = "red"))

# Distribution of accidents by comuna with year comparison
ggplot(accidents_data, aes(x = as.factor(comuna), fill = factor(ano))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Accidents by Comuna",
       x = "Comuna",
       y = "Number of Accidents",
       fill = "Year") +
  scale_fill_manual(values = c("2009" = "darkgreen", "2010" = "lightgreen"))

# Distribution of accidents by gender with year comparison
ggplot(accidents_data, aes(x = sexo, fill = factor(ano))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Accidents by Gender",
       x = "Gender",
       y = "Number of Accidents",
       fill = "Year") +
  scale_fill_manual(values = c("2009" = "purple", "2010" = "pink"))

# TODO poner los numeros del valor de las barras en el tope

# Distribution of accidents by age group with year comparison
ggplot(accidents_data, aes(x = edad_agrupada, fill = factor(ano))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Accidents by Age Group",
       x = "Age Group",
       y = "Number of Accidents",
       fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("2009" = "orange", "2010" = "yellow"))

# Distribution of accidents by day of the week with year comparison
ggplot(accidents_data, aes(x = dia_semana_accidente, fill = factor(ano))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Accidents by Day of the Week",
       x = "Day of the Week",
       y = "Number of Accidents",
       fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("2009" = "red", "2010" = "darkred"))


# Distribution of accidents by hour of the day with year comparison

accidents_data$hora_accidente <- as.POSIXct(accidents_data$hora_accidente, format = "%H:%M")
accidents_data$hora_accidente_hora <- format(accidents_data$hora_accidente, "%H")

ggplot(accidents_data, aes(x = hora_accidente_hora, fill = factor(ano))) +
  geom_bar(position = "dodge", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Accidents by Hour of the Day",
       x = "Hour of the Day",
       y = "Number of Accidents",
       fill = "Year") +
  scale_fill_manual(values = c("2009" = "darkred", "2010" = "lightcoral"))


# Distribution of accidents by condition of the person with year comparison
ggplot(accidents_data, aes(x = condicion, fill = factor(ano))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Accidents by Condition",
       x = "Condition of the Person",
       y = "Number of Accidents",
       fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("2009" = "dodgerblue", "2010" = "lightblue"))

# Homicides by traffic accidents, broken down by month and gender with year comparison
ggplot(accidents_data, aes(x = mes_accidente, fill = sexo)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ ano) +
  theme_minimal() +
  labs(title = "Homicides by Traffic Accidents in Cali - By Month and Gender",
       x = "Month of Accident",
       y = "Number of Homicides",
       fill = "Gender") +
  scale_fill_manual(values = c("M" = "lightblue", "F" = "pink"))

# TODO ponerle división marcada entre las dos gráficas x año

# Homicides by traffic accidents, broken down by age group and condition with year comparison
ggplot(accidents_data, aes(x = edad_agrupada, fill = condicion)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ ano) +
  theme_minimal() +
  labs(title = "Homicides by Traffic Accidents in Cali - By Age Group and Condition",
       x = "Age Group",
       y = "Number of Homicides",
       fill = "Condition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Boxplot of age by condition with year comparison
ggplot(accidents_data, aes(x = condicion, y = edad, fill = factor(ano))) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Age by Condition",
       x = "Condition of the Person",
       y = "Age",
       fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("2009" = "lightblue", "2010" = "darkblue"))



!!!!!!!!!! Esta no me convence, se ve como desorganizada pero quería que tuvieramos más graficos aparte de barras y boxplots...
# Pie chart of accidents by condition with year comparison

accident_condition <- accidents_data %>%
  group_by(ano, condicion) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(ano) %>% 
  mutate(percentage = count / sum(count) * 100)

ggplot(accident_condition, aes(x = "", y = count, fill = condicion)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~ ano) +
  theme_minimal() +
  labs(title = "Pie Chart of Accidents by Condition",
       fill = "Condition") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5))




# BORRAR AL FINAL -------------------------------------------------------------------------

# Data imputation
mode_profesion <- names(sort(table(accidents_data$profesion), decreasing = TRUE))[1]
accidents_data$profesion[is.na(accidents_data$profesion)] <- mode_profesion

unique(accidents_data$profesion)
accidents_data$profesion[accidents_data$profesion %in% c("", "NA", "N/A", "NULL", "No aplica", "Desconocido")] <- NA

colSums(is.na(accidents_data)) # There is more null data

mode_profesion <- names(sort(table(accidents_data$profesion), decreasing = TRUE))[1]
accidents_data$profesion[is.na(accidents_data$profesion)] <- mode_profesion


# Data imputation
accidents_data$edad <- as.numeric(as.character(accidents_data$edad))
accidents_data$edad <- as.integer(accidents_data$edad)
accidents_data$edad[is.na(accidents_data$edad)] <- mean(accidents_data$edad, na.rm = TRUE)
colSums(is.na(accidents_data))








