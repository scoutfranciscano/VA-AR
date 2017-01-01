################################################################################
##
## EstimacionVAEspecificas2014_2015.R
##
## Descripción: This script contains the VA models of Specific Subjects
##              for Engineering.
##
##
## Project: Value Added
##
## Inputs: - 
##
## Outputs: - All the outputs and graphics for VA models
##
## Author: Edwin Cuellar
##
## Date: August 18 of 2015.
################################################################################

################################################################################
# # Libraries
################################################################################

library(RODBC)
library(plyr)
library(foreign)
library(xtable)
library(lme4)
library(ggplot2)
library(reshape)
library(stringr)
library(scales)
require(RColorBrewer)
library(car)

################################################################################
# # Declaring functions
################################################################################
mean.na <- function(x) round(mean(x,na.rm=TRUE),2)
sd.na <- function(x) sd(x,na.rm=TRUE)

CambCaractRaros <- function(Base, Variable){
  Base[, Variable] <- gsub("Á", "A",
                           Base[, Variable])
  Base[, Variable]  <- gsub("É", "E",
                            Base[, Variable])
  Base[, Variable]  <- gsub("Í", "I",
                            Base[, Variable])
  Base[, Variable] <- gsub("Ó", "O",
                           Base[, Variable])
  Base[, Variable] <- gsub("Ú", "U",
                           Base[, Variable])
  Base[, Variable] <- gsub("Ñ", "NN",
                           Base[, Variable])
}

################################################################################
# # Bases de SABER PRO 2012-2014
################################################################################

################################################################################
# # Folders
################################################################################
setwd("C:/ecuellar/2014/ek/Cruce/src")

outPath      <- "../output/"
inPath       <- "../input/"
srcPath      <- "../src/"

fileName      <- "SBPRO-20121-RGSTRO-RECLFCCN-GEN.txt"
inFile        <- paste (inPath,fileName, sep = "")
Pro20121Gen   <- read.delim (inFile, sep="|", header=TRUE, dec=",", encoding="latin1", na.strings="", quote="")

fileName      <- "SBPRO-20123-RGSTRO-RECLFCCN-GEN.txt"
inFile        <- paste (inPath,fileName, sep = "")
Pro20123Gen   <- read.delim (inFile, sep="|", header=TRUE, dec=",", encoding="latin1", na.strings="", quote="")

PRO20121y3GEN <- rbind(Pro20121Gen, Pro20123Gen)
rm(Pro20121Gen, Pro20123Gen)

PRO20121y3GEN <- subset(PRO20121y3GEN, PRO20121y3GEN[,
                        'ESTU_COD_GRUPO_REF'] %in% c(1, 2, 3, 4, 5, 6,
                                                     7, 8, 9, 10, 11,
                                                     12, 13, 14, 31, 32,
                                                     33, 34))

setwd("C:/ecuellar/2015/ek/Cruce/src")

outPath      <- "../output/"
inPath       <- "../input/"
srcPath      <- "../src/"

fileName      <- "FTPPUB_SBPRO_20131_U_GEN.txt"
inFile        <- paste (inPath,fileName, sep = "")
Pro20131Gen   <- read.delim (inFile, sep="|", header=TRUE, dec=",", encoding="latin1", na.strings="", quote="")

fileName      <- "FTPPUB_SBPRO_20133_U_GEN.txt"
inFile        <- paste (inPath,fileName, sep = "")
Pro20133Gen   <- read.delim (inFile, sep="|", header=TRUE, dec=",", encoding="latin1", na.strings="", quote="")

names(Pro20131Gen)[names(Pro20131Gen)=="ESTU_GRUPOREFPROGPER_ID"]<-"ESTU_GRUPOREFPERIODO_ID"

PRO20131y3GEN <- rbind(Pro20131Gen, Pro20133Gen)
rm(Pro20131Gen,Pro20133Gen)

PRO20131y3GEN <- subset(PRO20131y3GEN, PRO20131y3GEN[, 'ESTU_COD_GRUPO_REF'] %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 31, 32, 33, 34))

fileName      <- "sbpro20142_f_v1-0.txt"
inFile        <- paste (inPath, fileName, sep = "")
Pro20142Gen   <- read.delim (inFile, sep="|", header=TRUE, dec=",",
                             colClasses = "character", encoding="latin1",
                             na.strings="", quote="")

fileName      <- "sbpro20143_f_v1-0.txt"
inFile        <- paste (inPath, fileName, sep = "")
Pro20143Gen   <- read.delim (inFile, sep="|", header=TRUE, dec=",",
                             encoding="latin1", colClasses = "character",
                             na.strings="", quote="")
							 
# names(Pro20131Gen)[names(Pro20131Gen)=="ESTU_GRUPOREFPROGPER_ID"]<-"ESTU_GRUPOREFPERIODO_ID"

PRO20142y3GEN <- rbind(Pro20142Gen, Pro20143Gen)
rm(Pro20142Gen, Pro20143Gen)

PRO20142y3GEN <- rename(PRO20142y3GEN, c('ESTU_CONSECUTIVO'='FTP_CONSECUTIVO'))
PRO20142y3GEN <- rename(PRO20142y3GEN, c('INST_NOMBREINSTITUCION'='INST_PERTENECE_EVALUADO'))
PRO20142y3GEN <- rename(PRO20142y3GEN, c('INST_CARACTERACADEMICO'='INST_CARACTER_ACADEMICO'))
PRO20142y3GEN <- rename(PRO20142y3GEN, c('INST_PRAC_CODIGOICFES'='ESTU_PRGM_ACADEMICO_COD'))
PRO20142y3GEN <- rename(PRO20142y3GEN, c('PRAC_NOMBRE'='ESTU_PRGM_ACADEMICO'))
PRO20142y3GEN <- rename(PRO20142y3GEN, c('DIPO_NOMBREDEPARTAMENTO'='ESTU_COLEGIOTERMINO_DEPT'))
PRO20142y3GEN <- rename(PRO20142y3GEN, c('GRRE_NOMBRE'='ESTU_GRUPO_REFERENCIA'))
PRO20142y3GEN <- rename(PRO20142y3GEN, c('CAPR_PUNTAJE_COMU'='MOD_COMUNICA_ESCRITA_PUNT'))
PRO20142y3GEN <- rename(PRO20142y3GEN, c('CAPR_DESEMPENO_COMU'='MOD_COMUNICA_ESCRITA_DESEM'))
PRO20142y3GEN <- rename(PRO20142y3GEN, c('CAPR_PUNTAJE_INGL'='MOD_INGLES_PUNT'))
PRO20142y3GEN <- rename(PRO20142y3GEN, c('CAPR_DESEMPENO_INGL'='MOD_INGLES_DESEM'))
PRO20142y3GEN <- rename(PRO20142y3GEN, c('CAPR_PUNTAJE_RAZO'='MOD_RAZONA_CUANTITATIVO_PUNT'))
PRO20142y3GEN <- rename(PRO20142y3GEN, c('CAPR_PUNTAJE_LECT'='MOD_LECTURA_CRITICA'))
PRO20142y3GEN <- rename(PRO20142y3GEN, c('CAPR_PUNTAJE_COMP'='MOD_COMP_CIUDADANAS_PUNT'))

PRO20142y3GEN <- subset(PRO20142y3GEN, PRO20142y3GEN[, 'ESTU_GRUPO_REFERENCIA'] %in% 
c('ADMINISTRACIÓN Y AFINES', 'ARQUITECTURA Y URBANISMO ', 'BELLAS ARTES Y DISEÑO',
'CIENCIAS AGROPECUARIAS ', 'CIENCIAS MILITARES Y NAVALES', 'CIENCIAS NATURALES Y EXACTAS', 
'CIENCIAS SOCIALES', 'COMUNICACIÓN, PERIODISMO Y PUBLICIDAD', 'CONTADURÍA Y AFINES', 
'DERECHO', 'ECONOMIA', 'EDUCACIÓN', 'ENFERMERÍA', 'HUMANIDADES', 'INGENIERÍA',
'MEDICINA', 'PSICOLOGÍA', 'SALUD'))

ResultadosPruebas <- c('MOD_COMUNICA_ESCRITA_PUNT', 'MOD_INGLES_PUNT',
'MOD_RAZONA_CUANTITATIVO_PUNT', 'MOD_LECTURA_CRITICA', 'MOD_COMP_CIUDADANAS_PUNT')

for (RP in ResultadosPruebas){
PRO20142y3GEN[, RP] <- as.numeric(as.character(sub(',', '.', PRO20142y3GEN[, RP])))
}

PRO2012_2014 <- rbind.fill(PRO20121y3GEN, PRO20131y3GEN)
rm(PRO20121y3GEN,PRO20131y3GEN)

PRO2012_2014 <- rbind.fill(PRO2012_2014, PRO20142y3GEN)
rm(PRO20142y3GEN)

################################################################################
# # Bases de VALOR AGREGADO 2012-2014
################################################################################

################################################################################
# # Folders
################################################################################

setwd("C:/ecuellar/2014/ek/Aporte Relativo/src")

outPath      <- "../output/"
inPath       <- "../input/"
srcPath      <- "../src/"

fileName <- "pro11_12_1_to_12_2_vPUBFinal.txt"
inFile   <- paste (inPath, fileName, sep = "")
VA2012 <- read.delim(file(inFile, encoding="latin1"), sep = "|", header=TRUE, dec=".")

setwd("C:/ecuellar/2015/ek/Aporte Relativo/src")

outPath      <- "../output/"
inPath       <- "../input/"
srcPath      <- "../src/"

fileName    <- "pro11_13_1_to_13_2_v2014.txt"
inFile      <- paste(inPath, fileName, sep = "")
VA2013 <- read.delim(file(inFile, encoding="latin1"), 
                          sep = "|", header=TRUE, dec=".")

fileName    <- "pro11_14_1_to_14_2_v2014.txt"
inFile      <- paste(inPath, fileName, sep = "")
VA2014 <- read.delim(file(inFile, encoding="latin1"), 
                          sep = "|", header=TRUE, dec=".")
						  
fileName    <- "SB1106_10PuntParaVAPublic.txt"
inFile      <- paste(inPath, fileName, sep = "")
IndSB11     <- read.delim(file(inFile, encoding="latin1"), 
                          sep = "|", header=TRUE, dec=".")						  

##############################################################################
# # Databases by specific skills
################################################################################

fileName    <- "PrimeraBaseEspecificas12_14.txt"
inFile      <- paste(inPath, fileName, sep = "")
PrimeraBaseEspecificas12_14 <- read.delim(file(inFile, encoding="latin1"), 
                          sep = "|", header=TRUE, dec=".")
						  
PrimeraBaseEspecificas12_14 <- subset(PrimeraBaseEspecificas12_14, !is.na(PrimeraBaseEspecificas12_14[, 'ESTU_CONSECUTIVO']))						 

PrimeraBaseEspecificas12_14 <- subset(PrimeraBaseEspecificas12_14, PrimeraBaseEspecificas12_14[, 'ESTU_CONSECUTIVO'] != '')						 
						  
PrimeraBaseEspecificas12_14 <- PrimeraBaseEspecificas12_14[, c('ESTU_CONSECUTIVO',
'PRUE_NOMBRE', 'CAPR_PERIODO', 'CAPR_PUNTAJETOTAL')]	

head(PrimeraBaseEspecificas12_14)

table(PrimeraBaseEspecificas12_14[, 'PRUE_NOMBRE'], PrimeraBaseEspecificas12_14[, 'CAPR_PERIODO'])

PrimeraBaseEspecificas12_14 <- subset(PrimeraBaseEspecificas12_14, PrimeraBaseEspecificas12_14[, 'PRUE_NOMBRE'] == 'PRODUCCION AGRICOLA' | 
PrimeraBaseEspecificas12_14[, 'CAPR_PERIODO'] == '20143')

table(PrimeraBaseEspecificas12_14[, 'PRUE_NOMBRE'], PrimeraBaseEspecificas12_14[, 'CAPR_PERIODO'])

fileName    <- "SegundaBaseEspecificas12_14.txt"
inFile      <- paste(inPath, fileName, sep = "")
SegundaBaseEspecificas12_14 <- read.delim(file(inFile, encoding="latin1"), 
                          sep = "|", header=TRUE, dec=".")	

SegundaBaseEspecificas12_14 <- subset(SegundaBaseEspecificas12_14, !is.na(SegundaBaseEspecificas12_14[, 'ESTU_CONSECUTIVO']))						 

SegundaBaseEspecificas12_14 <- subset(SegundaBaseEspecificas12_14, SegundaBaseEspecificas12_14[, 'ESTU_CONSECUTIVO'] != '')						 
						  
SegundaBaseEspecificas12_14 <- SegundaBaseEspecificas12_14[, c('ESTU_CONSECUTIVO',
'PRUE_NOMBRE', 'CAPR_PERIODO', 'CAPR_PUNTAJETOTAL')]

head(SegundaBaseEspecificas12_14)

table(SegundaBaseEspecificas12_14[, 'PRUE_NOMBRE'], SegundaBaseEspecificas12_14[, 'CAPR_PERIODO'])

isProcPrueba <- SegundaBaseEspecificas12_14[, 'PRUE_NOMBRE'] == 'DISENNO DE PROCESOS PRODUCTIVOS Y LOGISTICOS'
SegundaBaseEspecificas12_14[isProcPrueba, 'PRUE_NOMBRE'] <- 'DISENNO DE SISTEMAS PRODUCTIVOS Y LOGISTICOS'

BaseEspecificas12_14 <- rbind(PrimeraBaseEspecificas12_14,	SegundaBaseEspecificas12_14)
rm(PrimeraBaseEspecificas12_14, SegundaBaseEspecificas12_14)

BaseEspecificas12_14[, 'PRUE_NOMBRE'] <- droplevels(BaseEspecificas12_14[, 'PRUE_NOMBRE'])

BaseEspecificas12_14[, 'CAPR_PERIODO'] <- NULL

table(BaseEspecificas12_14[, 'PRUE_NOMBRE'])

BaseEspecificas12_14tmp <- subset(BaseEspecificas12_14, BaseEspecificas12_14[, 'PRUE_NOMBRE'] == 'GESTION DE ORGANIZACIONES')

for(kk in levels(BaseEspecificas12_14[, 'PRUE_NOMBRE'])){
tmp <- subset(BaseEspecificas12_14, BaseEspecificas12_14[, 'PRUE_NOMBRE'] == kk)
names(tmp)[3] <- kk
tmp[, 'PRUE_NOMBRE'] <- NULL
BaseEspecificas12_14tmp <- merge(tmp, BaseEspecificas12_14tmp, by = 'ESTU_CONSECUTIVO', all = TRUE)
}

BaseEspecificas12_14tmp[, 'PRUE_NOMBRE'] <- NULL

BaseEspecificas12_14tmp[, 'CAPR_PUNTAJETOTAL'] <- NULL

BaseEspecificas12_14Compl <- merge(PRO2012_2014, BaseEspecificas12_14tmp, 
by.x = 'FTP_CONSECUTIVO', by.y = 'ESTU_CONSECUTIVO', all.y = TRUE)

BaseEspecificas12_14Compl <- subset(BaseEspecificas12_14Compl, 
BaseEspecificas12_14Compl[, 'ESTU_GRUPO_REFERENCIA'] == "INGENIERÍA")

NumInstituc <- vector()
NumEstudiantes <- vector()

i <- 1

for(kk in levels(BaseEspecificas12_14[, 'PRUE_NOMBRE'])){
tmp <- subset(BaseEspecificas12_14Compl, !is.na(BaseEspecificas12_14Compl[, kk]))
NumInstituc[i] <- length(unique(tmp[, 'INST_COD_INSTITUCION']))
NumEstudiantes[i] <- nrow(tmp)
i <- i +1
}

## Número de instituciones en total
PRO2012_2014Ing <- subset(PRO2012_2014, PRO2012_2014[, 'ESTU_GRUPO_REFERENCIA'] == 'INGENIERÍA')
length(unique(PRO2012_2014Ing[, 'INST_COD_INSTITUCION']))

##Número de instituciones en 2014
tmp <- subset(PRO2012_2014Ing, !(PRUEBA  %in% c('20121', '20123', '20131', '20133')))
length(unique(tmp[, 'INST_COD_INSTITUCION']))
			
## Correlaciones entre resultados de competencias genéricas y 
## competencias específicas en SABER PRO 

rm(PRO2012_2014Ing)

BaseEspecificas12_14Compl <- subset(BaseEspecificas12_14Compl, !is.na(BaseEspecificas12_14Compl[, 'MOD_LECTURA_CRITICA']))

BaseEspecificas12_14Compl <- subset(BaseEspecificas12_14Compl, !is.na(BaseEspecificas12_14Compl[, 'MOD_RAZONA_CUANTITATIVO_PUNT']))

for (Prueba in c('FORMULACION DE PROYECTOS DE INGENIERIA',
'DISENNO DE SOFTWARE', 'DISENNO DE SISTEMAS PRODUCTIVOS Y LOGISTICOS', 
'DISENNO DE SISTEMAS MECANICOS', 'DISENNO DE SISTEMAS DE CONTROL',
'DISENNO DE PROCESOS INDUSTRIALES', 'DISENNO DE OBRAS DE INFRAESTRUCTURA',
'PRODUCCION AGRICOLA', 'PENSAMIENTO CIENTIFICO QUIMICA',
'PENSAMIENTO CIENTIFICO MATEMATICAS Y ESTADISTICA', 
'PENSAMIENTO CIENTIFICO CIENCIAS FISICAS', 'PENSAMIENTO CIENTIFICO CIENCIAS DE LA TIERRA', 
'PENSAMIENTO CIENTIFICO CIENCIAS BIOLOGICAS', 'GESTION DE ORGANIZACIONES')) {
isNullPrueba <- BaseEspecificas12_14Compl[, Prueba] == 0 &
               !is.na(BaseEspecificas12_14Compl[, Prueba])
BaseEspecificas12_14Compl[isNullPrueba, Prueba] <- NA
}
	
#for(k in seq(nrow(BaseEspecificas12_14Compl))){	
#BaseEspecificas12_14Compl[k, 'NumeroEspecif'] <- sum(!is.na(BaseEspecificas12_14Compl[k, c('FORMULACION DE PROYECTOS DE INGENIERIA',
#'DISENNO DE SOFTWARE', 'DISENNO DE SISTEMAS PRODUCTIVOS Y LOGISTICOS', 
#'DISENNO DE SISTEMAS MECANICOS', 'DISENNO DE SISTEMAS DE CONTROL',
#'DISENNO DE PROCESOS INDUSTRIALES', 'DISENNO DE OBRAS DE INFRAESTRUCTURA',
#'PRODUCCION AGRICOLA', 'PENSAMIENTO CIENTIFICO QUIMICA',
#'PENSAMIENTO CIENTIFICO MATEMATICAS Y ESTADISTICA', 
#'PENSAMIENTO CIENTIFICO CIENCIAS FISICAS', 'PENSAMIENTO CIENTIFICO CIENCIAS DE LA TIERRA', 
#'PENSAMIENTO CIENTIFICO CIENCIAS BIOLOGICAS', 'GESTION DE ORGANIZACIONES')]))
#}

#table(BaseEspecificas12_14Compl[, 'NumeroEspecif'])

## Base con información sólo de 2014.

#BaseEspecificas14Compl <- subset(BaseEspecificas12_14Compl, is.na(BaseEspecificas12_14Compl[, 'PRUEBA']))

#for(k in seq(nrow(BaseEspecificas14Compl))){	
#BaseEspecificas14Compl[k, 'NumeroEspecif'] <- sum(!is.na(BaseEspecificas14Compl[k, c('FORMULACION DE PROYECTOS DE INGENIERIA',
#'DISENNO DE SOFTWARE', 'DISENNO DE SISTEMAS PRODUCTIVOS Y LOGISTICOS', 
#'DISENNO DE SISTEMAS MECANICOS', 'DISENNO DE SISTEMAS DE CONTROL',
#'DISENNO DE PROCESOS INDUSTRIALES', 'DISENNO DE OBRAS DE INFRAESTRUCTURA',
#'PRODUCCION AGRICOLA', 'PENSAMIENTO CIENTIFICO QUIMICA',
#'PENSAMIENTO CIENTIFICO MATEMATICAS Y ESTADISTICA', 
#'PENSAMIENTO CIENTIFICO CIENCIAS FISICAS', 'PENSAMIENTO CIENTIFICO CIENCIAS DE LA TIERRA', 
#'PENSAMIENTO CIENTIFICO CIENCIAS BIOLOGICAS', 'GESTION DE ORGANIZACIONES')]))
#}

#table(BaseEspecificas14Compl[, 'NumeroEspecif'])

#fileName    <- "BaseEspecificas14Compl.txt"
#outFile      <- paste(outPath, fileName, sep = "")
#write.table (BaseEspecificas14Compl, outFile, row.names =TRUE,
#             sep="|",fileEncoding = "latin1", dec=".")

#fileName    <- "EIA.txt"
#outFile      <- paste(outPath, fileName, sep = "")
#write.table (EIA, outFile, row.names =TRUE,
#             sep="|",fileEncoding = "latin1", dec=".")

i = 0		
Correlaciones <- data.frame()
for(Pruebas in c("FORMULACION DE PROYECTOS DE INGENIERIA", "DISENNO DE SOFTWARE",
				  "DISENNO DE SISTEMAS PRODUCTIVOS Y LOGISTICOS", "DISENNO DE SISTEMAS MECANICOS",
				  "DISENNO DE SISTEMAS DE CONTROL", "DISENNO DE PROCESOS INDUSTRIALES",
				  "DISENNO DE OBRAS DE INFRAESTRUCTURA", "PRODUCCION AGRICOLA", 
				  "PENSAMIENTO CIENTIFICO QUIMICA", "PENSAMIENTO CIENTIFICO MATEMATICAS Y ESTADISTICA",
				  "PENSAMIENTO CIENTIFICO CIENCIAS FISICAS", 
				  "PENSAMIENTO CIENTIFICO CIENCIAS DE LA TIERRA",
				  "PENSAMIENTO CIENTIFICO CIENCIAS BIOLOGICAS", "GESTION DE ORGANIZACIONES")){
tmp0 <- subset(BaseEspecificas12_14Compl, !is.na(BaseEspecificas12_14Compl[, Pruebas]))
i = i +1
Correlaciones[i, 'Prueba'] <- Pruebas
Correlaciones[i, 'CorrLC'] <- cor(tmp0[, c('MOD_LECTURA_CRITICA', Pruebas)])[1,2] 
Correlaciones[i, 'CorrRC'] <- cor(tmp0[, c('MOD_RAZONA_CUANTITATIVO_PUNT', Pruebas)])[1,2]
}
			
################################################################################
# # Value Added Databases
################################################################################

## Valor Agregado 2012-2014

dat <- rbind.fill(VA2012, VA2013)
rm(VA2012, VA2013)

dat <- rbind.fill(dat, VA2014)
rm(VA2014)

dat[, 'inst_by_ref'] <- paste(dat[, 'INST_COD_INSTITUCION'],
                              dat[, 'ESTU_GRUPO_REFERENCIA'],
                              sep="__")

tmp <- aggregate(dat[, 'inse'],
                 list(dat[, 'inst_by_ref']), mean, na.rm = TRUE)

names(tmp) <- c("inst_by_ref", "inse_mean")

dat <- merge(dat,tmp)

Cruce <- merge(dat, BaseEspecificas12_14tmp, by.y = 'ESTU_CONSECUTIVO', 
by.x = 'FTP_CONSECUTIVO')

Cruce <- subset(Cruce, Cruce[, 'ESTU_GRUPO_REFERENCIA'] == "INGENIERÍA")
rm(dat)
rm(BaseEspecificas12_14tmp)

i=0
Resumen <- data.frame()
for(Pruebas in c("FORMULACION DE PROYECTOS DE INGENIERIA", "DISENNO DE SOFTWARE",
				  "DISENNO DE SISTEMAS PRODUCTIVOS Y LOGISTICOS", "DISENNO DE SISTEMAS MECANICOS",
				  "DISENNO DE SISTEMAS DE CONTROL", "DISENNO DE PROCESOS INDUSTRIALES",
				  "DISENNO DE OBRAS DE INFRAESTRUCTURA", "PRODUCCION AGRICOLA", 
				  "PENSAMIENTO CIENTIFICO QUIMICA", "PENSAMIENTO CIENTIFICO MATEMATICAS Y ESTADISTICA",
				  "PENSAMIENTO CIENTIFICO CIENCIAS FISICAS", 
				  "PENSAMIENTO CIENTIFICO CIENCIAS DE LA TIERRA",
				  "PENSAMIENTO CIENTIFICO CIENCIAS BIOLOGICAS")){
i = i +1
tmp0 <- subset(Cruce, !is.na(Cruce[, Pruebas]))
tmpBaseEspecificas12_14Compl <- subset(BaseEspecificas12_14Compl, !is.na(BaseEspecificas12_14Compl[, Pruebas]))
tmp0Agreg <- aggregate(tmp0[, 'INST_COD_INSTITUCION'], by = list(tmp0[, 'INST_COD_INSTITUCION']), FUN= length)
tmpComp <- aggregate(tmpBaseEspecificas12_14Compl[, 'INST_COD_INSTITUCION'], by = list(tmpBaseEspecificas12_14Compl[, 'INST_COD_INSTITUCION']), FUN= length)
tmp0Agreg <- merge(tmp0Agreg, tmpComp, by = 'Group.1', all.x = TRUE)
tmp0Agreg[, 'Prop'] <- tmp0Agreg[, 'x.x']/tmp0Agreg[, 'x.y']
Resumen[i, 'Prueba'] <- Pruebas
Resumen[i, 'NumEstudiantes'] <- nrow(tmp0)
Resumen[i, 'NumInstituciones'] <- length(unique(tmp0[, 'INST_COD_INSTITUCION']))
Resumen[i, 'NumInsti30oMas'] <- nrow(subset(tmp0Agreg, x.x >= 30 & Prop > 0.3))
Resumen[i, 'NumInsti15oMas'] <- nrow(subset(tmp0Agreg, x.x >= 15 & Prop > 0.3))
}

dim(Cruce)

Cruce <- subset(Cruce, !is.na(Cruce[, 'LENGUAJE_PUNT_ESTAND']))
Cruce <- subset(Cruce, !is.na(Cruce[, 'MATEMATICAS_PUNT_ESTAND']))
Cruce <- subset(Cruce, !is.na(Cruce[, 'CIENCIAS_SOCIALES_PUNT_ESTAND']))
Cruce <- subset(Cruce, !is.na(Cruce[, 'QUIMICA_PUNT_ESTAND']))
Cruce <- subset(Cruce, !is.na(Cruce[, 'FISICA_PUNT_ESTAND']))
Cruce <- subset(Cruce, !is.na(Cruce[, 'BIOLOGIA_PUNT_ESTAND']))

i = 0		
CorrelacionesPROSB11 <- data.frame()
for(Pruebas in c("FORMULACION DE PROYECTOS DE INGENIERIA", "DISENNO DE SOFTWARE",
				  "DISENNO DE SISTEMAS PRODUCTIVOS Y LOGISTICOS", "DISENNO DE SISTEMAS MECANICOS",
				  "DISENNO DE SISTEMAS DE CONTROL", "DISENNO DE PROCESOS INDUSTRIALES",
				  "DISENNO DE OBRAS DE INFRAESTRUCTURA", "PRODUCCION AGRICOLA", 
				  "PENSAMIENTO CIENTIFICO QUIMICA", "PENSAMIENTO CIENTIFICO MATEMATICAS Y ESTADISTICA",
				  "PENSAMIENTO CIENTIFICO CIENCIAS FISICAS", 
				  "PENSAMIENTO CIENTIFICO CIENCIAS DE LA TIERRA",
				  "PENSAMIENTO CIENTIFICO CIENCIAS BIOLOGICAS", "GESTION DE ORGANIZACIONES")){
tmp0 <- subset(Cruce, !is.na(Cruce[, Pruebas]))
i = i +1
CorrelacionesPROSB11[i, 'Prueba'] <- Pruebas
CorrelacionesPROSB11[i, 'CorrLC'] <- cor(tmp0[, c('MOD_LECTURA_CRITICA', Pruebas)])[1,2] 
CorrelacionesPROSB11[i, 'CorrRC'] <- cor(tmp0[, c('MOD_RAZONA_CUANTITATIVO_PUNT', Pruebas)])[1,2]
CorrelacionesPROSB11[i, 'CorrLeng'] <- cor(tmp0[, c('LENGUAJE_PUNT_ESTAND', Pruebas)])[1,2] 
CorrelacionesPROSB11[i, 'CorrMate'] <- cor(tmp0[, c('MATEMATICAS_PUNT_ESTAND', Pruebas)])[1,2]
CorrelacionesPROSB11[i, 'CorrCienSoci'] <- cor(tmp0[, c('CIENCIAS_SOCIALES_PUNT_ESTAND', Pruebas)])[1,2] 
CorrelacionesPROSB11[i, 'CorrQuim'] <- cor(tmp0[, c('QUIMICA_PUNT_ESTAND', Pruebas)])[1,2]
CorrelacionesPROSB11[i, 'CorrFisica'] <- cor(tmp0[, c('FISICA_PUNT_ESTAND', Pruebas)])[1,2] 
CorrelacionesPROSB11[i, 'CorrBiolo'] <- cor(tmp0[, c('BIOLOGIA_PUNT_ESTAND', Pruebas)])[1,2]
}

rm(PRO2012_2014)

Cruce[,'ESTU_GRUPO_REFERENCIA'] <- droplevels(Cruce[,'ESTU_GRUPO_REFERENCIA'])

Cruce[, 'inst_by_ref'] <- as.factor(Cruce[, 'inst_by_ref'])

CruceTMP <- Cruce[, c('inst_by_ref', 'LENGUAJE_PUNT_ESTAND', 
'MATEMATICAS_PUNT_ESTAND', 'QUIMICA_PUNT_ESTAND', 'CIENCIAS_SOCIALES_PUNT_ESTAND',
'ESTU_GRUPO_REFERENCIA', 'inse', 'inse_mean', 'FORMULACION DE PROYECTOS DE INGENIERIA', 'DISENNO DE SOFTWARE',
				  'DISENNO DE SISTEMAS PRODUCTIVOS Y LOGISTICOS', 'DISENNO DE SISTEMAS MECANICOS',
				  'DISENNO DE SISTEMAS DE CONTROL', 'DISENNO DE PROCESOS INDUSTRIALES',
				  'DISENNO DE OBRAS DE INFRAESTRUCTURA', 'PRODUCCION AGRICOLA', 
				  'PENSAMIENTO CIENTIFICO QUIMICA', 'PENSAMIENTO CIENTIFICO MATEMATICAS Y ESTADISTICA',
				  'PENSAMIENTO CIENTIFICO CIENCIAS FISICAS', 
				  'PENSAMIENTO CIENTIFICO CIENCIAS DE LA TIERRA',
				  'PENSAMIENTO CIENTIFICO CIENCIAS BIOLOGICAS')]
				  
names(CruceTMP) <- c('inst_by_ref', 'LENGUAJE_PUNT_ESTAND', 
'MATEMATICAS_PUNT_ESTAND', 'QUIMICA_PUNT_ESTAND', 'CIENCIAS_SOCIALES_PUNT_ESTAND',
'ESTU_GRUPO_REFERENCIA', 'inse', 'inse_mean', 'FORMULACION_DE_PROYECTOS_DE_INGENIERIA', 'DISENNO_DE_SOFTWARE',
				'DISENNO_DE_SISTEMAS_PRODUCTIVOS_Y_LOGISTICOS', 'DISENNO_DE_SISTEMAS_MECANICOS',
				'DISENNO_DE_SISTEMAS_DE_CONTROL', 'DISENNO_DE_PROCESOS_INDUSTRIALES',
				'DISENNO_DE_OBRAS_DE_INFRAESTRUCTURA', 'PRODUCCION_AGRICOLA',
				'PENSAMIENTO_CIENTIFICO_QUIMICA','PENSAMIENTO_CIENTIFICO_MATEMATICAS_Y_ESTADISTICA',
			    'PENSAMIENTO_CIENTIFICO_CIENCIAS_FISICAS','PENSAMIENTO_CIENTIFICO_CIENCIAS_DE_LA_TIERRA',
				'PENSAMIENTO_CIENTIFICO_CIENCIAS_BIOLOGICAS')

memory.size(16000)

EstimacionesEspecif <- data.frame()
i=0

for (Pruebas in c('FORMULACION_DE_PROYECTOS_DE_INGENIERIA', 'DISENNO_DE_SOFTWARE',
				'DISENNO_DE_SISTEMAS_PRODUCTIVOS_Y_LOGISTICOS', 'DISENNO_DE_SISTEMAS_MECANICOS',
				'DISENNO_DE_SISTEMAS_DE_CONTROL', 'DISENNO_DE_PROCESOS_INDUSTRIALES',
				'DISENNO_DE_OBRAS_DE_INFRAESTRUCTURA', 'PRODUCCION_AGRICOLA',
				'PENSAMIENTO_CIENTIFICO_QUIMICA','PENSAMIENTO_CIENTIFICO_MATEMATICAS_Y_ESTADISTICA',
			    'PENSAMIENTO_CIENTIFICO_CIENCIAS_FISICAS','PENSAMIENTO_CIENTIFICO_CIENCIAS_DE_LA_TIERRA',
				'PENSAMIENTO_CIENTIFICO_CIENCIAS_BIOLOGICAS')){

#outcomes <- c("MOD_RAZONA_CUANTITATIVO_PUNT","MOD_LECTURA_CRITICA")

#prior <- list(MOD_RAZONA_CUANTITATIVO_PUNT="MATEMATICAS_PUNT_ESTAND",
#              MOD_LECTURA_CRITICA="LENGUAJE_PUNT_ESTAND")				  
				  
outcomes <- Pruebas				  
				  
prior <- list(FORMULACION_DE_PROYECTOS_DE_INGENIERIA='MATEMATICAS_PUNT_ESTAND',
			  DISENNO_DE_SOFTWARE='MATEMATICAS_PUNT_ESTAND',
			  DISENNO_DE_SISTEMAS_PRODUCTIVOS_Y_LOGISTICOS='MATEMATICAS_PUNT_ESTAND',
			  DISENNO_DE_SISTEMAS_MECANICOS='MATEMATICAS_PUNT_ESTAND',
			  DISENNO_DE_SISTEMAS_DE_CONTROL='MATEMATICAS_PUNT_ESTAND',
			  DISENNO_DE_PROCESOS_INDUSTRIALES='MATEMATICAS_PUNT_ESTAND',
			  DISENNO_DE_OBRAS_DE_INFRAESTRUCTURA='MATEMATICAS_PUNT_ESTAND',
			  PRODUCCION_AGRICOLA ='MATEMATICAS_PUNT_ESTAND',
			  PENSAMIENTO_CIENTIFICO_QUIMICA='MATEMATICAS_PUNT_ESTAND',
			  PENSAMIENTO_CIENTIFICO_MATEMATICAS_Y_ESTADISTICA='MATEMATICAS_PUNT_ESTAND',
			  PENSAMIENTO_CIENTIFICO_CIENCIAS_FISICAS='MATEMATICAS_PUNT_ESTAND',
			  PENSAMIENTO_CIENTIFICO_CIENCIAS_DE_LA_TIERRA='MATEMATICAS_PUNT_ESTAND',
			  PENSAMIENTO_CIENTIFICO_CIENCIAS_BIOLOGICAS='MATEMATICAS_PUNT_ESTAND')
	
dat_tmp <- subset(CruceTMP, !is.na(CruceTMP[, outcomes]))

#dat_tmp[, 'ESTU_GRUPO_REFERENCIA'] <- droplevels(dat_tmp[, 'ESTU_GRUPO_REFERENCIA'])

dat_tmp[, 'inst_by_ref'] <- droplevels(dat_tmp[, 'inst_by_ref'])

ref.grps <- split(dat_tmp, dat_tmp[,'ESTU_GRUPO_REFERENCIA'])

#ref.grps <- split(Cruce, Cruce[,'ESTU_GRUPO_REFERENCIA'])

CalculateICC <- function(mod) {
## This function compute the Intra Class Correlation (ICC) from a model
##
## Arguments:
##   mod: Adjusted model with
##
## Return:
##   ICC: The Intra Class Correlation of a model

	z <- VarCorr(mod)
	err.var <- attr(z, "sc")^2
	re.var <- z[[1]][1]
	ICC <- re.var/(re.var + err.var)
	return(ICC)
}

EstimateVA <- function(dat, outcome) {
  ## This function estimate the parameters in the Value Added Model.
  ##
  ## Arguments:
  ##  dat: Data
  ##  outcome: Outcome variable
  ##
  ## Return: Value Added estimation parameters
  ##

  context11 <- prior[[outcome]]
  tmp <- aggregate(dat[[context11]], list(dat[, 'inst_by_ref']), mean, na.rm=TRUE)
  context11.nm <- paste(context11, "mean", sep="_")
  names(tmp) <- c("inst_by_ref", context11.nm)
  dat <- merge(dat, tmp, by="inst_by_ref")
  if (length(unique(dat [, 'inst_by_ref']))>5) {
    fm_list <- list()
    fm_list$base <- formula(paste("outcome~(1|inst_by_ref) +
                                  MATEMATICAS_PUNT_ESTAND +
                                  LENGUAJE_PUNT_ESTAND +
                                  QUIMICA_PUNT_ESTAND +
                                  CIENCIAS_SOCIALES_PUNT_ESTAND"))
    fm_list$inse <- update.formula(fm_list$base, ".~.+inse_mean")
    fm_list$sb11 <- update.formula(fm_list$base, paste(".~.+", context11.nm))
    dat$outcome <- dat[[outcome]]
    icc <- bar <- list()
    for (i in 1:length(fm_list)) {
      fm <- fm_list[[i]]
      mod <- lmer(fm, data=dat)
      icc[[names(fm_list)[i] ]] <- CalculateICC(mod)
      ## Get the VA estimates
      foo <- ranef(mod)$inst_by_ref
      foo <- data.frame(inst_by_ref=rownames(foo), va=foo[,1])
      names(foo)[2] <- names(fm_list)[i]
      bar[[i]] <- foo
    }
    df <- bar[[1]]
    for (i in 2:length(bar)) df <- merge(df, bar[[i]], all=TRUE)
    tmp <- split(dat, dat[, 'inst_by_ref'])
    fun <- function(x) colMeans(x[, c("outcome", context11, "inse")], na.rm=TRUE)
    tmp <- lapply(tmp, fun)
    hold <- names(tmp)
    tmp <- data.frame(do.call("rbind",tmp))
    names(tmp) <- c("M_post", "M_pre", "M_inse")
# #     df[, 'base']  <- (df[, 'base']*10) + 50
# #     df[, 'inse']  <- (df[, 'inse']*10) + 50
# #     df[, 'sb11']  <- (df[, 'sb11']*10) + 50
    tmp[, 'inst_by_ref'] <- hold
    df <- merge(df, tmp)
    list(df, icc)
  }
  else NULL
}

icc <- va <- list()
for (outcome in outcomes) {
	tmp <- lapply(ref.grps, EstimateVA, outcome=outcome)
	index <- sapply(tmp, is.null)
	tmp <- tmp[!index]
	icc[[outcome]] <- lapply(tmp, function(tmp) tmp[[2]])
	va[[outcome]] <- lapply(tmp, function(tmp) tmp[[1]])
}

CalculateConf <- function(ValAdEst, ref.grps, n.boot) {
  ## This function compute the confidence bands of VA estimates
  ##
  ## Arguments:
  ##   va: VA estimates
  ##
  ## Return:
  ##   ICC: The Intra Class Correlation of a model

  ## This is insufficient, but it takes a while to run 100 or so bootstrap iterations.
  for (out in names(ValAdEst)) {
    for (ref.grp in names(ValAdEst[[out]])) {
      va.tmp <- ValAdEst[[out]][[ref.grp]]
      dat <- ref.grps[[ref.grp]]
      schools <- split(dat, dat[, 'inst_by_ref'])
      boot.list <- list()
      for (i in seq(n.boot)) {
        sample.fun <- function(x) x[sample(1:nrow(x), nrow(x), replace=TRUE), ]
        schools.tmp <- lapply(schools, sample.fun)
        dat.tmp <- data.frame(do.call("rbind", schools.tmp))
        boot.list[[i]] <- EstimateVA(dat.tmp, outcome=out)[[1]]
      }
      nms <- boot.list[[1]]$inst_by_ref
      for (i in 2:length(boot.list)) {
        if (!all(nms==boot.list[[i]]$inst_by_ref)) stop("names out of order")
      ## Probably redundant error checking
      for (nm in c("base", "inse", "sb11")) {
        tmp <- lapply(boot.list,function(x) x[, nm])
        tmp <- do.call("cbind",tmp)
        qu025 <- apply(tmp, 1, quantile, 0.025, na.rm = TRUE)
        qu975 <- apply(tmp, 1, quantile, 0.975, na.rm = TRUE)
        tmp <- data.frame(nms, qu025, qu975)
        names(tmp) <- c("inst_by_ref", paste0(nm, "_025"), paste0(nm, "_975"))
        va.tmp <- merge(va.tmp, tmp)
      }
      ValAdEst[[out]][[ref.grp]] <- va.tmp
      }
    }
  }

  return(ValAdEst)
}

VAEstimacionesEsp <- CalculateConf(va, ref.grps, n.boot = 100)

outFile <- file.path(outPath, paste("VAEstimacionesEsp_", outcomes, ".Rdata", sep = ''))
save(VAEstimacionesEsp, file = outFile)
i = i+1
tmpEspe <- VAEstimacionesEsp[[1]][[1]][, c('inst_by_ref', 'base', 'base_025', 'base_975')]
VAPrueba <- paste("VA", Pruebas, sep ='')
names(tmpEspe) <- c('inst_by_ref', VAPrueba, paste(VAPrueba, "_025", sep = ''), paste(VAPrueba, "_975", sep = ''))

if(i==1){
EstimacionesEspecif <- merge(tmpEspe, tmpEspe, all = TRUE)
}
EstimacionesEspecif <- merge(EstimacionesEspecif, tmpEspe, all = TRUE)

}

setwd("C:/ecuellar/2015/ek/Analisis de Aporte Relativo/src")

outPath      <- "../output/"
inPath       <- "../input/"
srcPath      <- "../src/"

load(file.path(outPath, "va_estimates14.Rdata"))

IngenieriaRC14 <- as.data.frame(VAEstimaciones14[[1]][14])
IngenieriaLC14 <- as.data.frame(VAEstimaciones14[[2]][14])

load(file.path(outPath, "va_estimates12_14.Rdata"))

IngenieriaRC12_14 <- as.data.frame(VAEstimaciones12_14[[1]][14])
IngenieriaLC12_14 <- as.data.frame(VAEstimaciones12_14[[2]][14])

IngenieriaRC14 <- IngenieriaRC14[, c('INGENIERÍA.inst_by_ref', 'INGENIERÍA.base', 'INGENIERÍA.base_025', 'INGENIERÍA.base_975')]
names(IngenieriaRC14) <- c('inst_by_ref', 'VARC', 'VARC_025', 'VARC_975')
IngenieriaLC14 <- IngenieriaLC14[, c('INGENIERÍA.inst_by_ref', 'INGENIERÍA.base', 'INGENIERÍA.base_025', 'INGENIERÍA.base_975')]
names(IngenieriaLC14) <- c('inst_by_ref', 'VALC', 'VALC_025', 'VALC_975')

IngenieriaRC12_14 <- IngenieriaRC12_14[, c('INGENIERÍA.inst_by_ref', 'INGENIERÍA.base', 'INGENIERÍA.base_025', 'INGENIERÍA.base_975')]
names(IngenieriaRC12_14) <- c('inst_by_ref', 'VARC', 'VARC_025', 'VARC_975')
IngenieriaLC12_14 <- IngenieriaLC12_14[, c('INGENIERÍA.inst_by_ref', 'INGENIERÍA.base', 'INGENIERÍA.base_025', 'INGENIERÍA.base_975')]
names(IngenieriaLC12_14) <- c('inst_by_ref', 'VALC', 'VALC_025', 'VALC_975')

Ingenieria14 <- merge(IngenieriaRC14, IngenieriaLC14)

Ingenieria12_14 <- merge(IngenieriaRC12_14, IngenieriaLC12_14)

EstimacionesEspecif_Gener <- merge(EstimacionesEspecif, Ingenieria14, all.y = TRUE)

EstimacionesEspecif_Gener12_14 <- merge(EstimacionesEspecif, Ingenieria12_14, all = TRUE)

EstimacionesEspecif_Gener12_14 <- subset(EstimacionesEspecif_Gener12_14, !is.na(EstimacionesEspecif_Gener12_14[, 'VAPRODUCCION_AGRICOLA']))

EstimacionesEspecif_Gener <- subset(EstimacionesEspecif_Gener,  !(EstimacionesEspecif_Gener[, 'inst_by_ref'] %in% EstimacionesEspecif_Gener12_14[, 'inst_by_ref']))

EstimacionesEspecif_Gener <- rbind(EstimacionesEspecif_Gener, EstimacionesEspecif_Gener12_14)

i = 0		
CorrelacionesGenEsp <- data.frame()
for(Pruebas in c('FORMULACION_DE_PROYECTOS_DE_INGENIERIA', 'DISENNO_DE_SOFTWARE',
				'DISENNO_DE_SISTEMAS_PRODUCTIVOS_Y_LOGISTICOS', 'DISENNO_DE_SISTEMAS_MECANICOS',
				'DISENNO_DE_SISTEMAS_DE_CONTROL', 'DISENNO_DE_PROCESOS_INDUSTRIALES',
				'DISENNO_DE_OBRAS_DE_INFRAESTRUCTURA', 'PRODUCCION_AGRICOLA',
				'PENSAMIENTO_CIENTIFICO_QUIMICA','PENSAMIENTO_CIENTIFICO_MATEMATICAS_Y_ESTADISTICA',
			    'PENSAMIENTO_CIENTIFICO_CIENCIAS_FISICAS','PENSAMIENTO_CIENTIFICO_CIENCIAS_DE_LA_TIERRA',
				'PENSAMIENTO_CIENTIFICO_CIENCIAS_BIOLOGICAS')){
VAPruebas <- paste("VA", Pruebas, sep = '')				  
tmp0GenEsp <- subset(EstimacionesEspecif_Gener, !is.na(EstimacionesEspecif_Gener[, VAPruebas]))
i = i +1
CorrelacionesGenEsp[i, 'Prueba'] <- Pruebas
CorrelacionesGenEsp[i, 'CorrLC'] <- cor(tmp0GenEsp[, c('VALC', VAPruebas)])[1,2] 
CorrelacionesGenEsp[i, 'CorrRC'] <- cor(tmp0GenEsp[, c('VARC', VAPruebas)])[1,2]
}

fileName    <- "EstimacionesEspecif_Gener.txt"
outFile      <- paste(outPath, fileName, sep = "")
write.table (EstimacionesEspecif_Gener, outFile, row.names =TRUE,
             sep="|",fileEncoding = "latin1", dec=".")

###############################################################################
##	Tablas y gráficos para competencias genéricas				 
###############################################################################

IndSB11 <- IndSB11[, c('ESTU_CONSECUTIVO','IndicadorPuntajes')]

Cruce <- merge(Cruce, IndSB11, by = 'ESTU_CONSECUTIVO')

Cruce[, 'inst_by_ref'] <- CambCaractRaros(Cruce, 'inst_by_ref')

names(Cruce)[94] <- "FORMULACION_DE_PROYECTOS_DE_INGENIERIA"

names(Cruce)[96] <- "DISENNO_DE_SISTEMAS_PRODUCTIVOS_Y_LOGISTICOS"

names(Cruce)[103] <- "PENSAMIENTO_CIENTIFICO_MATEMATICAS_Y_ESTADISTICA"

names(Cruce)[104] <- "PENSAMIENTO_CIENTIFICO_CIENCIAS_FISICAS"

EstimacionesACOFI <- EstimacionesEspecif_Gener[, c('inst_by_ref', 
'VAFORMULACION_DE_PROYECTOS_DE_INGENIERIA', 
'VAFORMULACION_DE_PROYECTOS_DE_INGENIERIA_025',
'VAFORMULACION_DE_PROYECTOS_DE_INGENIERIA_975',
'VADISENNO_DE_SISTEMAS_PRODUCTIVOS_Y_LOGISTICOS',
'VADISENNO_DE_SISTEMAS_PRODUCTIVOS_Y_LOGISTICOS_025',
'VADISENNO_DE_SISTEMAS_PRODUCTIVOS_Y_LOGISTICOS_975',
'VAPENSAMIENTO_CIENTIFICO_MATEMATICAS_Y_ESTADISTICA',
'VAPENSAMIENTO_CIENTIFICO_MATEMATICAS_Y_ESTADISTICA_025',
'VAPENSAMIENTO_CIENTIFICO_MATEMATICAS_Y_ESTADISTICA_975',
'VAPENSAMIENTO_CIENTIFICO_CIENCIAS_FISICAS',
'VAPENSAMIENTO_CIENTIFICO_CIENCIAS_FISICAS_025',
'VAPENSAMIENTO_CIENTIFICO_CIENCIAS_FISICAS_975')]

BaseEspecificas12_14Compl[, 'inst_by_ref'] <- paste(BaseEspecificas12_14Compl[, 'INST_COD_INSTITUCION'],
                              BaseEspecificas12_14Compl[, 'ESTU_GRUPO_REFERENCIA'],
                              sep="__")

names(BaseEspecificas12_14Compl)[109] <- "FORMULACION_DE_PROYECTOS_DE_INGENIERIA"

names(BaseEspecificas12_14Compl)[111] <- "DISENNO_DE_SISTEMAS_PRODUCTIVOS_Y_LOGISTICOS"

names(BaseEspecificas12_14Compl)[118] <- "PENSAMIENTO_CIENTIFICO_MATEMATICAS_Y_ESTADISTICA"

names(BaseEspecificas12_14Compl)[119] <- "PENSAMIENTO_CIENTIFICO_CIENCIAS_FISICAS"

BaseEspecificas12_14Compl[, 'inst_by_ref'] <- CambCaractRaros(BaseEspecificas12_14Compl, 'inst_by_ref')
							  
NumPROPrueba <- aggregate(BaseEspecificas12_14Compl[, 'inst_by_ref'],					  
list(BaseEspecificas12_14Compl[, 'inst_by_ref']), length)

NumVAPrueba <- aggregate(Cruce[, 'inst_by_ref'],					  
list(Cruce[, 'inst_by_ref']), length)

NumVAPruebaSB11 <- aggregate(Cruce[, 'IndicadorPuntajes'],					  
list(Cruce[, 'inst_by_ref']), mean, na.rm = TRUE)

TablaANEXO <- merge(NumPROPrueba, NumVAPrueba, by = 'Group.1')

TablaANEXO <- merge(TablaANEXO, NumVAPruebaSB11, by = 'Group.1')

names(TablaANEXO) <- c('INST_BY_REF', 'Tamano.x', 'Tamano.y', 'IndSB11')

## La siguiente variable permite identificar a las IGR que tienen más
## de 30 estudiantes en el cruce de Aporte Relativo 2012-2013 y
## cuyo porcentaje equivale a más del 30%.
## Esto equivale a que estas IGR tendrán resultados en las secciones de
## Aporte Relativo.

TablaANEXO[, 'hasVA'] <- TablaANEXO[, 'Tamano.y']/TablaANEXO[, 'Tamano.x']> .3 &
                         TablaANEXO[, 'Tamano.y'] > 30

## La siguiente variable permite identificar a las IGR que tienen más
## de 15 estudiantes en el cruce de Aporte Relativo 2012-2013 y
## cuyo porcentaje equivale a más del 30%.
## Esto equivale a que estas IGR tendrán vecindad para la presentación
## de los resultados de SABER PRO pero no tendrán incluidos los
## resultados de Aporte Relativo.

TablaANEXO[, 'has15Vec'] <-
  TablaANEXO[, 'Tamano.y']/TablaANEXO[, 'Tamano.x']> .3 &
                    TablaANEXO[, 'Tamano.y'] > 15

TablaANEXO[, 'INST_BY_REF'] <-
  CambCaractRaros(TablaANEXO, 'INST_BY_REF')

Desviac2014GR <- data.frame()

Desviac2014GR[1, 'Prueba'] <- 'FORMULACION_DE_PROYECTOS_DE_INGENIERIA'

Desviac2014GR[2, 'Prueba'] <- 'DISENNO_DE_SISTEMAS_PRODUCTIVOS_Y_LOGISTICOS'

Desviac2014GR[3, 'Prueba'] <- 'PENSAMIENTO_CIENTIFICO_MATEMATICAS_Y_ESTADISTICA'

Desviac2014GR[4, 'Prueba'] <- 'PENSAMIENTO_CIENTIFICO_CIENCIAS_FISICAS'

Desviac2014GR[1, 'DesvEstand'] <- sd(Cruce[, 'FORMULACION_DE_PROYECTOS_DE_INGENIERIA'], na.rm=TRUE)

Desviac2014GR[2, 'DesvEstand'] <- sd(Cruce[, 'DISENNO_DE_SISTEMAS_PRODUCTIVOS_Y_LOGISTICOS'], na.rm=TRUE)

Desviac2014GR[3, 'DesvEstand'] <- sd(Cruce[, 'PENSAMIENTO_CIENTIFICO_MATEMATICAS_Y_ESTADISTICA'], na.rm=TRUE)

Desviac2014GR[4, 'DesvEstand'] <- sd(Cruce[, 'PENSAMIENTO_CIENTIFICO_CIENCIAS_FISICAS'], na.rm=TRUE)

NombrePruebaEspec <- c('FORMULACION_DE_PROYECTOS_DE_INGENIERIA' = 'Formulación de proyectos de ingeniería',
'DISENNO_DE_SISTEMAS_PRODUCTIVOS_Y_LOGISTICOS' = 'Diseño de sistemas productivos y logísticos',
'PENSAMIENTO_CIENTIFICO_MATEMATICAS_Y_ESTADISTICA' = 'Pensamiento científico Matemáticas y Estadística',
'PENSAMIENTO_CIENTIFICO_CIENCIAS_FISICAS' = 'Pensamiento científico Ciencias Físicas')
  
setwd("C:/ecuellar/2015/ek/Aporte Relativo/src")

outPath      <- "../output"
inPath       <- "../input/"
srcPath      <- "../src/"
docPath      <- "../doc"					
					
fileName <- "InstitNombreReducidoRevisadoEC.txt"
inFile   <- paste (inPath, fileName, sep = "")
universidades <- read.table(file(inFile, encoding="latin1"), sep = "\t", header=TRUE)

universidades <- universidades[, c('INST_ID', 'UNIV_NOMBREREDUCIDO')]					

for(PruebaEsp in c("FORMULACION_DE_PROYECTOS_DE_INGENIERIA",
"DISENNO_DE_SISTEMAS_PRODUCTIVOS_Y_LOGISTICOS",
"PENSAMIENTO_CIENTIFICO_MATEMATICAS_Y_ESTADISTICA",
"PENSAMIENTO_CIENTIFICO_CIENCIAS_FISICAS")){

load(file.path(outPath, paste("VAEstimacionesEsp_",PruebaEsp,".Rdata", sep = "")))

CambiarEscala <- function (datos, variables = c("base","inse","sb11"),
                           media = 50, desviacion = 10) {
  # # description
  # #
  # # Arg:
  # #  arguments
  # #
  # # Ret:
  # #  value

  promedios    <- apply(datos[, variables], 2, mean)
  desviaciones <- apply(datos[, variables], 2, sd)

  kA <- desviacion / desviaciones
  kB <- media - desviacion * promedios / desviaciones

  variablesTransform <- c(variables,
                          paste(variables, "_025", sep = ''),
                          paste(variables, "_975", sep = ''))

  datos[, variablesTransform] <- sweep(datos[, variablesTransform],
                                     2, kA, FUN = "*")
  datos[, variablesTransform] <- sweep(datos[, variablesTransform],
                                     2, kB, FUN = "+")

# #   apply(datos[, variablesTransform], 2, mean)
# #   apply(datos[, variablesTransform], 2, sd)

  return(datos)
}

VAEstimacionesEsp <- lapply(VAEstimacionesEsp, function(x)
                              lapply(x, CambiarEscala))


Crucetmp <- subset(Cruce, !is.na(Cruce[, PruebaEsp]))

Instituciones <- unique(Crucetmp[, 'INST_COD_INSTITUCION'])

Agreg1 <- aggregate(Crucetmp[, c('MOD_COMUNICA_ESCRITA_PUNT',
                                    'MOD_INGLES_PUNT', 'MOD_LECTURA_CRITICA',
                                    'MOD_RAZONA_CUANTITATIVO_PUNT',
                                    'MOD_COMP_CIUDADANAS_PUNT',
                                    'inse', 'IndicadorPuntajes')],
                    list(Crucetmp$inst_by_ref), mean,
                    na.rm = TRUE)

Agreg2 <- aggregate(Crucetmp$inst_by_ref,
                    list(Crucetmp$inst_by_ref), length)

AgregComp <- merge(Agreg1, Agreg2, by = 'Group.1')

AgregComp <- rename(AgregComp, c("Group.1" = "INST_BY_REF",
                             "MOD_COMUNICA_ESCRITA_PUNT" =
                             "MOD_COMUNICA_ESCRITA_PUNT_VA",
                             "MOD_INGLES_PUNT" = "MOD_INGLES_PUNT_VA",
                             "MOD_LECTURA_CRITICA" =
                             "MOD_LECTURA_CRITICA_VA",
                             "MOD_RAZONA_CUANTITATIVO_PUNT" =
                             "MOD_RAZONA_CUANTITATIVO_PUNT_VA",
                             "MOD_COMP_CIUDADANAS_PUNT" =
                             "MOD_COMP_CIUDADANAS_PUNT_VA",
                             "IndicadorPuntajes" = "IndicadorPuntajes_va",
                             "inse" = "inse_va", "x" = "n_va"))

AgregComp[, 'INST_BY_REF'] <-
  CambCaractRaros(AgregComp, 'INST_BY_REF')					
					
AgregCompPunta  <- AgregComp[, c('INST_BY_REF', 'IndicadorPuntajes_va')]					

tmp <- matrix(unlist(strsplit(as.character(AgregCompPunta$INST_BY_REF),
                              '__')), ncol=2, byrow=TRUE)

tmp  <-  as.data.frame(tmp)

names(tmp) <- c('Inst', 'RefGrp')

AgregCompPunta <- cbind(AgregCompPunta, tmp)
					
# for (Instit in seq (1,length(Instituciones))){
#       Inst <- Instituciones[Instit]

##1828

for (Inst in c('2813', '2811')){
	   
NombreUniv <- subset(universidades[, 'UNIV_NOMBREREDUCIDO'],
                     universidades[, 'INST_ID'] == Inst)

refGr <- 'INGENIERIA'
					 
InstGR <- paste(Inst, refGr, sep = "__")

rnwBase <- file.path("Sweave", "ResultadosEspecificasIngenieriav01.rnw")

runPath <- file.path(docPath, str_replace_all(refGr, "[ ,Y]", ""))

  if (!file.exists(runPath)) {
    dir.create(runPath)
    file.copy("Sweave.sty",
              file.path(runPath, "Sweave.sty"))
    file.copy("icfesdocapa.cls",
              file.path(runPath, "icfesdocapa.cls"))
 }
 
 archivos <- list.files(runPath)
    noBorrar <- grep("pdf$|cls$|tex$|sty$|jpg$", archivos)
    archivos <- archivos[-noBorrar]
    lapply(file.path(runPath, archivos), file.remove)

    # # generar copia del rnw renombrando
    outFile     <- paste("Graficos__", Inst, "__", refGr, "__", PruebaEsp, sep = "")
    outFileRnw  <- file.path(runPath,
                              paste(outFile, ".Rnw", sep = ""))
    outFileTexE <- paste(outFile, ".tex", sep = "")
# #     outFileTexS <- file.path(runPath,
# #                              paste(outFile, ".tex", sep = ""))

  if (file.exists(outFileRnw)) {
    file.remove(outFileRnw)
#    lapply(file.path(runPath, filesAux),
#           file.remove)
  }
  file.copy(rnwBase, outFileRnw)
#  lapply(file.path("Sweave", filesAux),
#         file.copy, to = runPath)

# #   file.copy("entidades080910.Rnw", outFileRnw)
 auxPath <- getwd()
 auxPath2 <- runPath
 setwd(runPath)
 runPath <- "."
  Sweave(paste(outFile, ".Rnw", sep = ""), encoding = "latin1")

 runlatex   <- paste("latex --interaction=nonstopmode ", outFileTexE,
                     "\n", sep="")

 batFile <- file.path(runPath, "correr12_13.bat")
 cat(runlatex, runlatex,
     "dvips -tletter -Ppdf -o ", outFile, ".ps ", outFile,
     '.dvi', "\nps2pdf ", outFile, ".ps\n", sep = "",
     file = batFile)

 system("correr12_13.bat")
 
  estanAr <- list.files(pattern = outFile)
 sonAr <- grep("[dvi|ps]$", estanAr)
 
  setwd(auxPath)
 runPath <- auxPath2

}
}
