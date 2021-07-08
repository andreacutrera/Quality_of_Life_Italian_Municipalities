#data source: https://www.urbanindex.it
library(dplyr)
library(tidyverse)
setwd('/Users/andreacutrera/Desktop/Municipalities-Provinces')

descrizione <- read.csv("/Users/andreacutrera/Desktop/Municipalities-Provinces/data collection/DB_DIPE_TRACCIATO.csv", sep = ';')
glimpse(descrizione)
descrizione$nome
#98 rows decribing all the variables

dataset <- read.csv("/Users/andreacutrera/Desktop/Municipalities-Provinces/data collection/DB_DIPE.csv", sep = ';')
glimpse(dataset)

dim(dataset)
length(unique(dataset$NOME))
length(dataset$NOME)
#8 should be repeated because of 8 more rows than unique values in names of municipality
dataset$NOME[duplicated(dataset$NOME)]
dataset[(dataset$NOME == 'Brione'),] #2143, 2834
dataset[(dataset$NOME == 'Calliano'),] #753, 2841
dataset[(dataset$NOME == 'Livo'),] #1584, 2910
dataset[(dataset$NOME == 'Samone'),] #235, 2966
dataset[(dataset$NOME == 'Peglio'),] #1612, 4134
dataset[(dataset$NOME == 'Castro'),] #1932, 6232
dataset[(dataset$NOME == 'Valverde'),] #2489, 7047
dataset[(dataset$NOME == 'San Teodoro'),] #6892, 7910
#since all of them seems repeated with different values I decide to drop all of them
dataset <- dataset[-c(2143, 2834, 753, 2841, 1584, 2910, 235, 2966,
                      1612, 4134, 1932, 6232, 2489, 7047, 6892, 7910),]
#now no more duplicates - 8076 unique municipalities
length(unique(dataset$NOME))
length(dataset$NOME)
dataset$NOME[duplicated(dataset$NOME)]

#variables selection for the purpose of my analysis
var <- c("NOME", "a1_1", "b1_1", "b2_2", "b4", "b7", "b8",
         "c1", "c2", "c5", "c6", "d1", "d2", "d4", "d5", "d7", "d8", "e1",
         "e2_1", "e2_2", "e3", "e4", "e5", "e6", "f1", "f3_1", "f3_2", "f4",
         "f5", "f7", "f8", "g2_1", "g2_2", "g3", "g4", "g5", "g6", "g7", "g8",
         "g9", "h1", "h5", "h6", "i1", "i3", "l1", "l2", "l5", "n2_1", 
         "n2", "n3", "n4", "n5", "n6", "n7", "n8", "o2_1", "o4")
length(var)
dataset %>% select(any_of(var)) -> dataset

#a last glimpse to our preprocessed dataset
glimpse(dataset)

#make also the selection for the descriptive dataframe
filter(descrizione, nome %in% var[2:length(var)]) -> descrizione
descrizione <- descrizione[,-4]
descrizione <- descrizione %>% arrange(ambito)

#Check NAs - seems that there are no missing values - not the truth
for (v in var) {
  print(paste(sum(is.na(dataset$v)), v))
}
colSums(is.na(dataset))

#I'm missing the #N/D and na
data <- replace(dataset, dataset =="NULL", NA)
data <- replace(data, data =="#N/D", NA)
data <- replace(data, data =="na", NA)
data <- replace(data, data =="#DIV/0!", NA)

missing_values <- colSums(is.na(data))
sort(missing_values, decreasing = T)

sort(rowSums(is.na(data)), decreasing = T)
which.max(rowSums(is.na(data)))
#Gravedona ed Uniti, Ardenno drop the 2 municipalities with most NA values
data[c(1652, 1657),]
data <- data[-c(1652, 1657),]
sort(colSums(is.na(data)), decreasing = T)
which.max(rowSums(is.na(data)))
#Pian di Sco, Casole d'Elsa drop the 2 municipalities with most NA values
data[c(4487, 4501),]
data <- data[-c(4487, 4501),]
sort(colSums(is.na(data)), decreasing = T)
which.max(rowSums(is.na(data)))
#Quero, Seren del Grappa drop the 2 municipalities with most NA values
data[c(3275, 3288),]
data <- data[-c(3275, 3288),]
sort(colSums(is.na(data)), decreasing = T)
which.max(rowSums(is.na(data)))
#Vas, Casier
data[c(3295, 3310),]
data <- data[-c(3295, 3310),]
sort(colSums(is.na(data)), decreasing = T)
which.max(rowSums(is.na(data)))
#Rivigliano, Taipana
data[c(3686, 3703),]
data <- data[-c(3686, 3703),]
sort(colSums(is.na(data)), decreasing = T)
which.max(rowSums(is.na(data)))

data[c(3706, 3725),]
data <- data[-c(3706, 3725),]
sort(colSums(is.na(data)), decreasing = T)
which.max(rowSums(is.na(data)))

data[c(3836, 3857),]
data <- data[-c(3836, 3857),]
sort(colSums(is.na(data)), decreasing = T)
which.max(rowSums(is.na(data)))


str(data)
#write csv
write.csv(data, "dataset.csv")


#open the reformatted . <-- ,
df <- read.csv('dataset.csv', sep = ';')
df <- df[,-1]
df <- df %>% drop_na(any_of(var))
#no more missing values
colSums(is.na(df))
#7921 municipalities and 59 variables
dim(df)
str(df)

#make a subset of our dataset for only the municipality which is also the province
provincie <- read.csv('provincie.csv', sep = ';', header = T)
names(provincie)[5] <- 'NOME'
names(provincie)
provincie <- merge(df, provincie, by = 'NOME')
dim(provincie)
names(provincie)
provincie <- provincie[,-c(60, 61, 62, 63, 64, 68)]
prov <- provincie[,c(1:59)]
prov <- cbind(provincie$Sigla_automobilistica, prov)
names(prov)[1] <- 'codice_provincia'

names(prov)
names(prov) <- c('codice_provincia', 'provincia', 'variazione_pop_residente', 
                 'densita_umana', 'dispersione_abitazioni', 'mobilita_privata',
                 'sottoutilizzo_abitazioni', 'densita_commercio_dettaglio_sede_fissa',
                 'attrattivita_residenziale', 'vecchiaia', 'verde_urbano_pro_capite',
                 'coppie_giovani_con_figli', 'occupazione', 'ricambio_occupazionale', 
                 'presenza_universitaria', 'pendolarismo', 'mobilita_residenziale',
                 'dinamismo_economico', 'tasso_funzione_ricettiva', 'visitatori_luoghi_cultura',
                 'luoghi_cultura', 'poli_fieristici', 'stadi', 'intrattenimento', 'hub_commerciali',
                 'digital_divide', 'stazione_silver', 'stazioni_accessibili', 'posti_letto_ospedale',
                 'dinamismo_pa', 'mobilita_pubblica', 'biblioteche', 'abbandono_scolastico_1grado',
                 'abbandono_scolastico_2grado', 'uscita_precoce_istruzione_formazione', 'disoccupazione',
                 'disoccupazione_giovanile', 'giovani_fuori_mercato_lavoro_formazione', 'famiglie_rischio_disagio_economico',
                 'anziani_soli', 'suicidio', 'affollamento_abitazioni', 'edifici_residenziali_pessimo_stato',
                 'servizi_abitazione', 'popolazione_straniera', 'coppie_miste', 'gini', 'occupazione_ita_straniera',
                 'occupazione_m_f', 'pericolo_frana', 'suolo_consumato_pro_capite', 'suolo_agricolo_utilizzato',
                 'rifiuti_urbani_pro_capite', 'mobilita_lenta', 'acqua_potabile', 'raccolta_differenziata',
                 'impianti_fotovoltaici', 'auto_e5_e6', 'cnr', 'centri_eccellenza')

#NA for codice_provincia is NAPLES not NULL
colSums(is.na(prov))
str(prov)

#write csv
write.csv(prov, 'prov.csv')
prov <- read.csv('prov.csv', header=T, sep = ';')

prov <- prov %>% mutate_at(c('variazione_pop_residente', 
                     'densita_umana', 'dispersione_abitazioni', 'mobilita_privata',
                     'sottoutilizzo_abitazioni', 'densita_commercio_dettaglio_sede_fissa',
                     'attrattivita_residenziale', 'vecchiaia', 'verde_urbano_pro_capite',
                     'coppie_giovani_con_figli', 'occupazione', 'ricambio_occupazionale', 
                     'presenza_universitaria', 'pendolarismo', 'mobilita_residenziale',
                     'dinamismo_economico', 'tasso_funzione_ricettiva', 'visitatori_luoghi_cultura',
                     'luoghi_cultura', 'poli_fieristici', 'stadi', 'intrattenimento', 'hub_commerciali',
                     'digital_divide', 'stazione_silver', 'stazioni_accessibili', 'posti_letto_ospedale',
                     'dinamismo_pa', 'mobilita_pubblica', 'biblioteche', 'abbandono_scolastico_1grado',
                     'abbandono_scolastico_2grado', 'uscita_precoce_istruzione_formazione', 'disoccupazione',
                     'disoccupazione_giovanile', 'giovani_fuori_mercato_lavoro_formazione', 'famiglie_rischio_disagio_economico',
                     'anziani_soli', 'suicidio', 'affollamento_abitazioni', 'edifici_residenziali_pessimo_stato',
                     'servizi_abitazione', 'popolazione_straniera', 'coppie_miste', 'gini', 'occupazione_ita_straniera',
                     'occupazione_m_f', 'pericolo_frana', 'suolo_consumato_pro_capite', 'suolo_agricolo_utilizzato',
                     'rifiuti_urbani_pro_capite', 'mobilita_lenta', 'acqua_potabile', 'raccolta_differenziata',
                     'impianti_fotovoltaici', 'auto_e5_e6', 'cnr', 'centri_eccellenza'), as.numeric)

prov <- prov[,-1]
str(prov)
#PROVINCES DATASET IS READY

#go back to the MUNICIPALITY one
names(prov)
length(names(prov))
names(df)
length(names(df))

names(df) <- c(  'comune', 'variazione_pop_residente', 
                 'densita_umana', 'dispersione_abitazioni', 'mobilita_privata',
                 'sottoutilizzo_abitazioni', 'densita_commercio_dettaglio_sede_fissa',
                 'attrattivita_residenziale', 'vecchiaia', 'verde_urbano_pro_capite',
                 'coppie_giovani_con_figli', 'occupazione', 'ricambio_occupazionale', 
                 'presenza_universitaria', 'pendolarismo', 'mobilita_residenziale',
                 'dinamismo_economico', 'tasso_funzione_ricettiva', 'visitatori_luoghi_cultura',
                 'luoghi_cultura', 'poli_fieristici', 'stadi', 'intrattenimento', 'hub_commerciali',
                 'digital_divide', 'stazione_silver', 'stazioni_accessibili', 'posti_letto_ospedale',
                 'dinamismo_pa', 'mobilita_pubblica', 'biblioteche', 'abbandono_scolastico_1grado',
                 'abbandono_scolastico_2grado', 'uscita_precoce_istruzione_formazione', 'disoccupazione',
                 'disoccupazione_giovanile', 'giovani_fuori_mercato_lavoro_formazione', 'famiglie_rischio_disagio_economico',
                 'anziani_soli', 'suicidio', 'affollamento_abitazioni', 'edifici_residenziali_pessimo_stato',
                 'servizi_abitazione', 'popolazione_straniera', 'coppie_miste', 'gini', 'occupazione_ita_straniera',
                 'occupazione_m_f', 'pericolo_frana', 'suolo_consumato_pro_capite', 'suolo_agricolo_utilizzato',
                 'rifiuti_urbani_pro_capite', 'mobilita_lenta', 'acqua_potabile', 'raccolta_differenziata',
                 'impianti_fotovoltaici', 'auto_e5_e6', 'cnr', 'centri_eccellenza')

str(df)
df <- df %>% mutate_at(c('variazione_pop_residente', 
                             'densita_umana', 'dispersione_abitazioni', 'mobilita_privata',
                             'sottoutilizzo_abitazioni', 'densita_commercio_dettaglio_sede_fissa',
                             'attrattivita_residenziale', 'vecchiaia', 'verde_urbano_pro_capite',
                             'coppie_giovani_con_figli', 'occupazione', 'ricambio_occupazionale', 
                             'presenza_universitaria', 'pendolarismo', 'mobilita_residenziale',
                             'dinamismo_economico', 'tasso_funzione_ricettiva', 'visitatori_luoghi_cultura',
                             'luoghi_cultura', 'poli_fieristici', 'stadi', 'intrattenimento', 'hub_commerciali',
                             'digital_divide', 'stazione_silver', 'stazioni_accessibili', 'posti_letto_ospedale',
                             'dinamismo_pa', 'mobilita_pubblica', 'biblioteche', 'abbandono_scolastico_1grado',
                             'abbandono_scolastico_2grado', 'uscita_precoce_istruzione_formazione', 'disoccupazione',
                             'disoccupazione_giovanile', 'giovani_fuori_mercato_lavoro_formazione', 'famiglie_rischio_disagio_economico',
                             'anziani_soli', 'suicidio', 'affollamento_abitazioni', 'edifici_residenziali_pessimo_stato',
                             'servizi_abitazione', 'popolazione_straniera', 'coppie_miste', 'gini', 'occupazione_ita_straniera',
                             'occupazione_m_f', 'pericolo_frana', 'suolo_consumato_pro_capite', 'suolo_agricolo_utilizzato',
                             'rifiuti_urbani_pro_capite', 'mobilita_lenta', 'acqua_potabile', 'raccolta_differenziata',
                             'impianti_fotovoltaici', 'auto_e5_e6', 'cnr', 'centri_eccellenza'), as.numeric)

str(df)

#MUNICIPALITY is also ready
write.csv(prov, 'provinces.csv')
write.csv(df, "municipalities.csv")

