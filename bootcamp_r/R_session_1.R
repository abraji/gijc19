# Aula no GIJC19 - Hamburgo
# Data Hands-On: The Power of R
# Luuk Sengers (Speaker) Co-founder, Trainer, Story-Based Inquiry Associates 
# Jonathan Stoneman (Speaker) Trainer, Freelance 
# Robert Gebeloff (Speaker) Data Projects Editor, The New York Times 

# Os arquivos de dados estão em: http://bit.ly/Tidyverse_training 

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
Pollution <- read_csv("T4J_session_data/pollution16.csv")

names(Pollution)

Pollution %>%  count(CountryName, sort = TRUE)

# Mostrar todas as instalações no meu país
Total <- Pollution %>%
  filter(CountryName == "Germany")


# Mostrar todas as instalações no meu país em 2017
Total17 <- Total %>%
  filter(ReportingYear == "2017")


# Mostrar todas as fazendas de gado no meu país em 2017
Livestock17 <- Total17 %>%
  filter(str_detect(MainIASectorName, "livestock"))


# Mostrar apenas os nomes das fazendas de gado e as cidades em que estão localizadas no meu país
NameCity <- Livestock17 %>%
  select(4,7)


# Mostrar todas as instalações em meu país que emitem NOx (nitrogênio), com nomes e quantidades
NOX17 <- Pollution %>%
  filter(CountryName == "Germany", ReportingYear == "2017", str_detect(PollutantName, "NOx")) %>%
  select(FacilityName,TotalQuantity)


# Qual instalação emitiu mais NOx?
NOX17 <- NOX17 %>%
  arrange(desc(TotalQuantity))


# Quantas instalações por setor emitem NOx?
NOX17SectorCount <- Total17 %>%
  filter(str_detect(PollutantName, "NOx")) %>%
  group_by(MainIASectorName) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))


# Qual setor emite mais nitrogênio e quanto?
NOX17SectorSum <- Total17 %>%
  filter(str_detect(PollutantName, "NOx")) %>%
  group_by(MainIASectorName) %>%
  summarise(Total = sum(TotalQuantity, na.rm=TRUE)) %>%
  arrange(desc(Total))


# Exportar tabela para csv
write_csv(Total17, "Polluters.csv")

# Exportar tabela para xlsx
write_xlsx(Total17, "Polluters.xlsx")


# Ver tabela “Total17”
View(Total17)


# Mostrar todos os nomes de colunas (= variáveis) na tabela 'Total17'
names(Total17)


# Mostrar todos os valores únicos na coluna ‘PollutantName’
unique(Total17$PollutantName) 


# Salve os valores únicos como uma lista (para referência)
Pollutants <- unique(Total17$PollutantName)
View(Pollutants)


# Agrupe, conte e classifique os valores na coluna ‘CountryName’
Pollution %>% 
  count(CountryName, sort = TRUE)

# Mostrar o top 10 de ‘NOX17’
head(NOX17, 10)


# Mostrar os últimos 10 no final de ‘NOX17’
tail(NOX17, 10)


# Mostrar mínimo, máximo, média e mediana das quantidades na tabela ‘NOX17’
summary(NOX17$TotalQuantity)


# Quem são os outliers no topo?
Outliers1 <- NOX17 %>%
  filter(NOX17$TotalQuantity > mean(NOX17$TotalQuantity) + 3 * sd(NOX17$TotalQuantity)) 


Outliers2 <- NOX17 %>%
  filter(TotalQuantity > (quantile(NOX17$TotalQuantity, probs = 0.75)) + (1.5 * (quantile(NOX17$TotalQuantity, probs = 0.75) - quantile(NOX17$TotalQuantity, probs = 0.25))))


# Qual é a porcentagem do total de cada instalação na tabela ‘NOX17’?
Ratios <- NOX17 %>%
  mutate(PctTotal  =  (NOX17$TotalQuantity / (sum(NOX17$TotalQuantity, na.rm=TRUE)) * 100))


# Primeiro: importe a tabela ‘NOXNL.csv’ no R
NOXNL <- read_csv("T4J_session_data/NOXNL.csv")


# Qual instalação teve o maior aumento de emissões de nitrogênio entre 2016 e 2017?
NOXNL <- NOXNL %>%
  mutate(PctChange = ((Year2017 - Year2016)/Year2016)*100) %>%
  arrange(desc(PctChange))


# Primeiro: importe tabela ‘SchoolsR’ no R
Schools <- read_csv("T4J_session_data/SchoolsR.csv")


# Mostre R-squared
Correlation <- lm(Schools$ReadScore~Schools$LowIncomePct)
summary(Correlation)


# Primeiro: importe três conjuntos de dados csv para o R - eles devem estar na pasta do projeto da seção 5
Facilities <- read_csv("T4J_session_data/FACILITY16.csv")
PolRel <- read_csv("T4J_session_data/POLLUTANTRELEASE16.csv")
PolRelTrans <- read_csv("T4J_session_data/POLLUTANTRELEASEANDTRANSFER16.csv")


# Junte Facility ao PolRel para recuperar os poluentes - há uma chave comum nos datasets:  “FacilityReportID”
Joined1 <- left_join(PolRel, Facilities, by = "FacilityReportID")


# Junte Facility ao PolRelTrans para recuperar os anos de referência - há uma chave comum nos datasets:  “PollutantReleaseAndTransferReportID”
Joined2 <- left_join(Joined1, PolRelTrans, by = "PollutantReleaseAndTransferReportID")


# Remover notações científicas
options(scipen = 999)



# Quem foi o maior poluidor de CO2 do seu país em 2017?
CO2Facilities <- Joined2 %>%
  filter(PollutantName == "Carbon dioxide (CO2)", ReportingYear == "2017", CountryName.x == "Germany") %>% 
  select(FacilityName, TotalQuantity) %>%
  arrange(desc(TotalQuantity))


# Qual setor é o maior poluidor de CO2 do seu país em 2017?
CO2Sectors <- Joined2 %>%
  filter(PollutantName == "Carbon dioxide (CO2)", ReportingYear == "2017", CountryName.x == "Germany") %>%
  group_by(MainIASectorName) %>%
  summarise(TotalQuantity=sum(TotalQuantity, na.rm=TRUE)) %>%
  arrange(desc(TotalQuantity))

# Faça uma lista dos fabricantes de aço na UE com base nas suas emissões de CO2 em 2017
SteelEU <- Joined2 %>%
  filter(PollutantName == "Carbon dioxide (CO2)", ReportingYear == "2017", str_detect(MainIAActivityName, "ferrous metals")) %>%
  group_by(ParentCompanyName) %>%
  summarise(TotalQuantity=sum(TotalQuantity, na.rm=TRUE)) %>%
  arrange(desc(TotalQuantity))

# As fazendas de suínos, aves e bovinos contribuem para a poluição do ar emitindo amônia. Quais são as piores fazendas do seu país e onde elas estão exatamente?
Farms <- Joined2 %>%
  filter(ReportingYear == "2017", CountryName.x == "Germany", str_detect(MainIASectorName, "livestock"), str_detect(PollutantName, "Ammonia")) %>% 
  group_by(FacilityName, Long, Lat) %>%
  summarise(TotalQuantity=sum(TotalQuantity, na.rm=TRUE)) %>%
  arrange(desc(TotalQuantity))

# Contar o número de fazendas emissoras de amônia por país da UE
FarmsEU <- Joined2 %>%
  filter(ReportingYear == "2017", str_detect(PollutantName, "Ammonia"), str_detect(MainIASectorName, "livestock")) %>%
  group_by(CountryName.x) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) 

FarmsEU2 <- Joined2 %>%
  filter(ReportingYear == "2017", str_detect(PollutantName, "Ammonia"), str_detect(MainIASectorName, "livestock")) %>% 
  count(CountryName.x, sort = T)


# Mostre as emissões de amônia das fazendas no seu país ao longo dos anos. Qual é a tendência?
FarmsTrend <- Joined2 %>%
  filter(CountryName.x == "Germany", str_detect(PollutantName, "Ammonia"), str_detect(MainIASectorName, "livestock")) %>% 
  group_by(ReportingYear) %>%
  summarise(TotalQuantity = sum(TotalQuantity), na.rm = TRUE) %>%
  arrange(ReportingYear)



# Poluição de CO2 em MyCountry por setor
ggplot(data = CO2Sectors, aes(x = reorder(MainIASectorName, -TotalQuantity), y = TotalQuantity, fill = MainIASectorName)) + 
  geom_bar(colour = "black", fill = "blue", width = 1, stat = "identity") +
  xlab("Sectors") + ylab("CO2 pollution") +
  ggtitle("CO2 emission per sector in my country")


ggplot(data = CO2Sectors, aes(x = reorder(MainIASectorName, -TotalQuantity), y = TotalQuantity, fill = MainIASectorName)) +
  geom_bar(colour = "black", fill = "blue", width = 1, stat = "identity") +
  coord_flip() +
  xlab("Sectors") +
  ylab("CO2 pollution") +
  ggtitle("CO2 emission per sector in my country")



ggplot(FarmsTrend, aes(x = ReportingYear, y = TotalQuantity)) + 
geom_line(color="red") +
geom_point() +
xlab("Year") + 
ylab("Ammonia emission") +
ggtitle("Ammonia emission from farms")



# Criar um scatterplot
ggplot(Schools, aes(x = ReadScore, y = LowIncomePct)) +
geom_point() +
geom_smooth(method = lm, color = "red", se = FALSE) +
xlab("Pontuações de leitura") + ylab('Renda dos pais') +
ggtitle("Correlação entre escores de leitura e renda dos pais")


# Com o uso do Esquisse
install.packages("esquisse")
esquisse::esquisser(CO2Sectors)


# Verifique o tipo de dados
glimpse(Joined2)


# Alterar tipo de dados
Joined2$TotalQuantity <- as.numeric(Joined2$TotalQuantity) 

# Remover notações científicas 
options(scipen = 999)


# Arredondar para decimais
Joined2$TotalQuantity <- round(Joined2$TotalQuantity, 1)




# Primeiro: Importe a tabela ‘Journalists’ into R
library(readxl)
Journalists <- read_excel("T4J_session_data/Journalists.xlsx")
View(Journalists)


# Dividir células com valores múltiplos em colunas separadas
Journalists <- separate(Journalists, Medium, into = c("Medium1", "Medium2"), sep = ",")


# Dividir Jobs 
Journalists <- separate(Journalists, Job, into = c("FirstJob", "SecondJob", "ThirdJob"), sep = ",")


install.packages("lubridate")
library(lubridate)

# Criar coluna para data com nova formatação
Journalists <- Journalists %>%
  mutate(newdate = mdy(Date))

# Criar coluna para ano
Journalists <- Journalists %>%
  mutate(year = year(newdate))

# Unir valores de várias colunas em uma coluna
Journalists <- unite(Journalists, Medium, Medium1, Medium2, sep = ", ")


# Remova NA (not available) ou espaços em branco
Journalists <- Journalists %>%
  drop_na(Nationality)


# Remova duplicadas
Journalists <- Journalists[!duplicated(Journalists), ]


# Remova espaços à esquerda e à direita
Journalists $Name <- str_trim(Journalists$Name)

# Alterar maiúsculas e minúsculas
Journalists$Name <- str_to_title(Journalists$Name, locale = "en")


# Substituir correspondências de uma string
Journalists$Organization <- gsub("Freelancer", "Freelance", Journalists$Organization, ignore.case=TRUE)



