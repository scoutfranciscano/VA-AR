
################################################################################
# # Libraries
################################################################################

library(plyr)
library(xtable)

################################################################################
# # Construcción de la base VA2013
################################################################################

################################################################################
# # Folders
################################################################################

setwd("C:/ecuellar/2015/ek/Aporte Relativo/src")

outPath      <- "../output/"
inPath       <- "../input/"
srcPath      <- "../src/"

############################### AGREGADOS 2014

################################################################################
# # Importing the databases
################################################################################

fileName      <- "FTPPUB_SBPRO_20131_U_GEN.txt"
inFile        <- paste (inPath,fileName, sep = "")
Pro20131Gen   <- read.delim (inFile, sep="|", header=TRUE, dec=",", encoding="latin1", na.strings="", quote="")

fileName      <- "FTPPUB_SBPRO_20133_U_GEN.txt"
inFile        <- paste (inPath,fileName, sep = "")
Pro20133Gen   <- read.delim (inFile, sep="|", header=TRUE, dec=",", encoding="latin1", na.strings="", quote="")

names(Pro20131Gen)[names(Pro20131Gen)=="ESTU_GRUPOREFPROGPER_ID"]<-"ESTU_GRUPOREFPERIODO_ID"

## Construcción de la base de resultados SABER PRO 2013

PRO20131y3GEN <- rbind(Pro20131Gen, Pro20133Gen)

################################################################################
# # Para simplificar los análisis provenientes de los modelos de valor
# agregado y debido a que se cuenta con puntajes comparables entre
# aplicaciones de los estudiantes en los grupos de referencia
# universitarios, se consideran únicamente los estudiantes que
# presentaron el examen en un programa de este nivel. Se excluyen del
# análisis los estudiantes de los programas técnicos, tecnólogos y normalistas.
################################################################################

PRO20131y3GEN <- subset(PRO20131y3GEN, PRO20131y3GEN[, 'ESTU_COD_GRUPO_REF'] %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 31, 32, 33, 34))

PRO20131y3GEN[, 'INST_BY_REF'] <- paste(PRO20131y3GEN[, 'INST_COD_INSTITUCION'],
                            PRO20131y3GEN[, 'ESTU_GRUPO_REFERENCIA'], sep="__")

PRO20131y3GEN[, 'INST_PRAC'] <- paste(PRO20131y3GEN[, 'INST_COD_INSTITUCION'],
                         PRO20131y3GEN[, 'ESTU_PRGM_ACADEMICO_COD'], sep="__")

fileName      <- "base_escalamientoJUNTAStrans2013_2014.txt"
inFile        <- paste (inPath, fileName, sep = "")
INSE  <- read.delim (inFile, sep="\t", header=TRUE, dec=".", encoding="latin1", na.strings="", quote="")

INSE <- INSE[, c("X_NAME_", "Prin1")]

PRO20131y3GEN <- merge(PRO20131y3GEN, INSE,
                       by.x = "FTP_CONSECUTIVO", by.y = "X_NAME_")

dim(PRO20131y3GEN)

PRO20131y3GEN <- rename(PRO20131y3GEN, c("Prin1" = "INSE"))

Agreg1 <- aggregate(PRO20131y3GEN[, c('MOD_COMUNICA_ESCRITA_PUNT',
                                    'MOD_INGLES_PUNT', 'MOD_LECTURA_CRITICA',
                                    'MOD_RAZONA_CUANTITATIVO_PUNT',
                                    'MOD_COMP_CIUDADANAS_PUNT', 'INSE')],
                    list(PRO20131y3GEN$INST_BY_REF), mean,
                    na.rm = TRUE)

Agreg2 <- aggregate(PRO20131y3GEN$INST_BY_REF,
                    list(PRO20131y3GEN$INST_BY_REF), length)

AgregPRO <- merge(Agreg1, Agreg2, by = 'Group.1')

AgregPRO <- rename(AgregPRO, c("Group.1" = "INST_BY_REF",
                               "MOD_COMUNICA_ESCRITA_PUNT" =
                               "mod_comunica_escrita_TODOS",
                               "MOD_INGLES_PUNT" =
                               "mod_ingles_punt_TODOS",
                              "MOD_LECTURA_CRITICA" =
                              "mod_lectura_critica_TODOS",
                              "MOD_RAZONA_CUANTITATIVO_PUNT" =
                              "mod_razona_cuantitativo_TODOS",
                              "MOD_COMP_CIUDADANAS_PUNT" =
                              "mod_comp_ciudadanas_TODOS", "INSE" =
                              "INSE_TODOS", "x" =
                              "n_TODOS"))

fileName    <- "AgregPRO2013.txt"
outFile      <- paste(outPath, fileName, sep = "")
write.table (AgregPRO, outFile, row.names =FALSE,
             sep="|",fileEncoding = "latin1", dec=".")

############################### AGREGADOS 2014			 
			 
################################################################################
# # Importing the databases
################################################################################

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
							 
## Construcción de la base de resultados SABER PRO 2014

PRO20142y3GEN <- rbind(Pro20142Gen, Pro20143Gen)

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

################################################################################
# # Para simplificar los análisis provenientes de los modelos de valor
# agregado y debido a que se cuenta con puntajes comparables entre
# aplicaciones de los estudiantes en los grupos de referencia
# universitarios, se consideran únicamente los estudiantes que
# presentaron el examen en un programa de este nivel. Se excluyen del
# análisis los estudiantes de los programas técnicos, tecnólogos y normalistas.
################################################################################

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

PRO20142y3GEN[, 'INST_BY_REF'] <- paste(PRO20142y3GEN[, 'INST_COD_INSTITUCION'],
                            PRO20142y3GEN[, 'ESTU_GRUPO_REFERENCIA'], sep="__")

PRO20142y3GEN[, 'INST_PRAC'] <- paste(PRO20142y3GEN[, 'INST_COD_INSTITUCION'],
                         PRO20142y3GEN[, 'ESTU_PRGM_ACADEMICO_COD'], sep="__")

fileName      <- "base_escalamientoJUNTAStrans2013_2014.txt"
inFile        <- paste (inPath, fileName, sep = "")
INSE  <- read.delim (inFile, sep="\t", header=TRUE, dec=".", encoding="latin1", na.strings="", quote="")

INSE <- INSE[, c("X_NAME_", "Prin1")]

PRO20142y3GEN <- merge(PRO20142y3GEN, INSE,
                       by.x = "FTP_CONSECUTIVO", by.y = "X_NAME_")

dim(PRO20142y3GEN)

PRO20142y3GEN <- rename(PRO20142y3GEN, c("Prin1" = "INSE"))

Agreg1 <- aggregate(PRO20142y3GEN[, c('MOD_COMUNICA_ESCRITA_PUNT',
                                    'MOD_INGLES_PUNT', 'MOD_LECTURA_CRITICA',
                                    'MOD_RAZONA_CUANTITATIVO_PUNT',
                                    'MOD_COMP_CIUDADANAS_PUNT', 'INSE')],
                    list(PRO20142y3GEN$INST_BY_REF), mean,
                    na.rm = TRUE)

Agreg2 <- aggregate(PRO20142y3GEN$INST_BY_REF,
                    list(PRO20142y3GEN$INST_BY_REF), length)

AgregPRO <- merge(Agreg1, Agreg2, by = 'Group.1')

AgregPRO <- rename(AgregPRO, c("Group.1" = "INST_BY_REF",
                               "MOD_COMUNICA_ESCRITA_PUNT" =
                               "mod_comunica_escrita_TODOS",
                               "MOD_INGLES_PUNT" =
                               "mod_ingles_punt_TODOS",
                              "MOD_LECTURA_CRITICA" =
                              "mod_lectura_critica_TODOS",
                              "MOD_RAZONA_CUANTITATIVO_PUNT" =
                              "mod_razona_cuantitativo_TODOS",
                              "MOD_COMP_CIUDADANAS_PUNT" =
                              "mod_comp_ciudadanas_TODOS", "INSE" =
                              "INSE_TODOS", "x" =
                              "n_TODOS"))

fileName    <- "AgregPRO2014.txt"
outFile      <- paste(outPath, fileName, sep = "")
write.table (AgregPRO, outFile, row.names =FALSE,
             sep="|",fileEncoding = "latin1", dec=".")