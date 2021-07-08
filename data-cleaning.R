#read the csv I prepared in the first preprocessing
setwd('/Users/andreacutrera/Desktop/Municipalities-Provinces')
prov <- read.csv('provinces.csv', sep = ',', header = T)
municip <- read.csv('municipalities.csv', sep = ',', header = T)
ranking <- read.csv('ranking.csv', sep = ';', header = T)

prov <- prov[,-1]
municip <- municip[,-1]
names(ranking) <- c('provincia', 'rank', 'life_quality')
#################################################################
#NA =! from missing value, but NAPLES
prov$codice_provincia[61] <- 'NAP'

prov$codice_provincia[duplicated(prov$codice_provincia)]
#Barletta Trani (BT) duplicated
prov$codice_provincia[11] <- 'BARL'
prov$codice_provincia[99] <- 'ANDR'

prov$codice_provincia[duplicated(prov$codice_provincia)]
#Carbonia-Iglesias (CI) duplicated
prov$codice_provincia[22] <- 'CAR'
prov$codice_provincia[41] <- 'IGL'

prov$codice_provincia[duplicated(prov$codice_provincia)]
#Olbia, Tempio Pausania (OT) duplicated
prov$codice_provincia[64] <- 'OLB'
prov$codice_provincia[95] <- 'TEP'

prov$codice_provincia[duplicated(prov$codice_provincia)]
#Pesaro-Urbino (PU) duplicated
prov$codice_provincia[71] <- 'PES'
prov$codice_provincia[105] <- 'URB'

prov$codice_provincia[duplicated(prov$codice_provincia)]
#Villacidro, Sanluri (VS) duplicated
prov$codice_provincia[88] <- 'SAN'
prov$codice_provincia[113] <- 'VIL'

#no more duplications
prov$codice_provincia[duplicated(prov$codice_provincia)]
rownames(prov) <- prov$codice_provincia


#now my dataset is made up by a bunch of int-num variables (58)
#114 Provinces (observations for 2011)
str(prov)
dim(prov)

write.csv(prov, 'prov.csv')
write.csv(ranking, 'rank.csv')
