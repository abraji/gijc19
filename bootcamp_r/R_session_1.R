# Aula no GIJC19 - Hamburgo
# Data Hands-On: The Power of R
# Luuk Sengers (Speaker) Co-founder, Trainer, Story-Based Inquiry Associates 
# Jonathan Stoneman (Speaker) Trainer, Freelance 
# Robert Gebeloff (Speaker) Data Projects Editor, The New York Times 

# Instalar pacotes
install.packages("tidyverse")
install.packages("writexl")

# Carregar pacotes
library(tidyverse)
library(readxl) 
library(writexl)

# Mostra o diretório atual
getwd()

# Importar dataset
Pollution <- read_csv("T4J_session_data/Pollution.csv")

names(Pollution)

Pollution %>%  count(CountryName, sort = TRUE)
