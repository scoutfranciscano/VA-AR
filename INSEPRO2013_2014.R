################################################################################
# # filename
# # R Versions: filename
# #
# # Author(s): authors
# #
# # evaluacion
# # Description: description
# #
# # Inputs: input
# #
# # Outputs: output
# #
# # File history:
# #   date: changes
################################################################################

################################################################################
# # PROCEDIMIENTO 1
# # Arreglando las bases
# #
# #
################################################################################

################################################################################
# #  paths
################################################################################

setwd("C:/ecuellar/2015/ek/INSE/src")

outPath      <- "../output/"
inPath       <- "../input/"
srcPath      <- "../src/"

################################################################################
# # libraries
################################################################################

library(r2lh)
library(RODBC)
library(foreign)
library(ade4)
library(homals)
library(HH)
library(xtable)
library(FactoMineR)
library(plyr)

################################################################################
# # Importing
################################################################################

fileName      <- "FTPPUB_SBPRO_20131_U_GEN.txt"
inFile        <- paste (inPath, fileName, sep = "")
Pro20131Gen   <- read.delim (inFile, sep="|", header=TRUE, dec=",",
                             colClasses = "character", encoding="latin1",
                             na.strings="", quote="")

fileName      <- "FTPPUB_SBPRO_20133_U_GEN.txt"
inFile        <- paste (inPath, fileName, sep = "")
Pro20133Gen   <- read.delim (inFile, sep="|", header=TRUE, dec=",",
                             colClasses = "character", encoding="latin1",
                             na.strings="", quote="")

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

################################################################################
# # load functions
################################################################################

files <- list.files("funct_graf/src/", pattern = "\\.R$")
files <- paste("funct_graf/src/", files, sep = '')
lapply(files, source)

################################################################################
##  Se modifican los nombres de las variables para que queden iguales
## entre los diferentes períodos
################################################################################

names(Pro20131Gen)[names(Pro20131Gen)=="ESTU_GRUPOREFPROGPER_ID"]<-"ESTU_GRUPOREFPERIODO_ID"

datos2013 <- rbind(Pro20131Gen, Pro20133Gen)

datos2014 <- rbind(Pro20142Gen,Pro20143Gen)

datos2014 <- rename(datos2014, c('ESTU_CONSECUTIVO'='FTP_CONSECUTIVO'))
datos2014 <- rename(datos2014, c('INST_NOMBREINSTITUCION'='INST_PERTENECE_EVALUADO'))
datos2014 <- rename(datos2014, c('INST_CARACTERACADEMICO'='INST_CARACTER_ACADEMICO'))
datos2014 <- rename(datos2014, c('INST_PRAC_CODIGOICFES'='ESTU_PRGM_ACADEMICO_COD'))
datos2014 <- rename(datos2014, c('PRAC_NOMBRE'='ESTU_PRGM_ACADEMICO'))
datos2014 <- rename(datos2014, c('DIPO_NOMBREDEPARTAMENTO'='ESTU_COLEGIOTERMINO_DEPT'))
datos2014 <- rename(datos2014, c('GRRE_NOMBRE'='ESTU_GRUPO_REFERENCIA'))
datos2014 <- rename(datos2014, c('CAPR_PUNTAJE_COMU'='MOD_COMUNICA_ESCRITA_PUNT'))
datos2014 <- rename(datos2014, c('CAPR_DESEMPENO_COMU'='MOD_COMUNICA_ESCRITA_DESEM'))
datos2014 <- rename(datos2014, c('CAPR_PUNTAJE_INGL'='MOD_INGLES_PUNT'))
datos2014 <- rename(datos2014, c('CAPR_DESEMPENO_INGL'='MOD_INGLES_DESEM'))
datos2014 <- rename(datos2014, c('CAPR_PUNTAJE_RAZO'='MOD_RAZONA_CUANTITATIVO_PUNT'))
datos2014 <- rename(datos2014, c('CAPR_PUNTAJE_LECT'='MOD_LECTURA_CRITICA'))
datos2014 <- rename(datos2014, c('CAPR_PUNTAJE_COMP'='MOD_COMP_CIUDADANAS_PUNT'))

datos2014[, 'INPE_TIPODOCSB11'] <- NULL
datos2014[, 'INPE_DOCUMENTOSB11'] <- NULL

datos <- rbind.fill(datos2013,datos2014)

# # options(encoding="latin1")
# # fileName <- "pro11_11_2_to_12_2_v2_everyvar.dta"
# # outFile  <- paste (inPath,fileName, sep = "")
# # datos    <- read.dta (outFile, warn.missing.labels = "NA")
dim(datos)

NAs <- datos == "NA"
datos[!is.na(NAs)&NAs==TRUE ] <- NA

NAs <- datos == "."
datos[!is.na(NAs)&NAs==TRUE ] <- NA

# # Los nombres de las variables en nuestros archivos están en mayuscula
# # luego esta parte no es necesaria

# # minusc  <- colnames(datos)
# # mayusc  <- toupper(minusc)
# # colnames(datos)  <- mayusc

dic   <- file.path(inPath, "dicc2013_2014.txt")
dicci <- read.table(dic, sep = "\t", header = TRUE, colClasses = "character")

################################################################################
# #   variables for descriptive analysis
################################################################################

diccINPUT  <-  subset( dicci, type == "input")
vardes     <-  diccINPUT[,"var_name"]
datosVAR   <- datos[,vardes]
rownames(datosVAR)   <- datos[,"FTP_CONSECUTIVO" ]
names(datosVAR)  <-  diccINPUT[,"var_name2"]

## Se elminan los individuos con 3 o más valores faltantes en las variables
## de entrada para cálculo del INSE.

datosVAR <- as.matrix(datosVAR)
datosVAR <- as.data.frame(datosVAR)

# # outdoc  <-  paste(outPathDoc,"UNIVARIATE2013OFF.tex",sep="")
# # rtlu( datosVAR ,fileOut= outdoc , graphName="GRAPHUNIVARIATEOFF", graphDir="", type="postscript") #textBefore= as.character(fil[, "LABEL"]

ausentes      <- apply(datosVAR,1,function(x) sum(is.na(x))) # sum(is.na(x)))

# Una prueba para mirar si sí esta contando bien los faltantes
   prueba      <- as.numeric()
  ii <- 0
    for (xx in names(datosVAR)){
    ii          <- ii + 1
    prueba1    <- apply(datosVAR[xx],1,function(x) sum(is.na(x))) # sum(is.na(x)))
    prueba[ii] <- sum(prueba1)
    }
   names(prueba) <-  names(datosVAR)
   prueba
######################
sum(ausentes>=3)

datosVAR$naf  <-  as.numeric(ausentes>=3) # el vector naf es 1 si el individuo se debe eliminar por no respuesta
#revi  <-  subset(data2VAR, naf == 1 )
datosFINAL  <-  subset(datosVAR, naf == 0 )
dim(datosFINAL)
datosFINAL  <-  datosFINAL[,-dim(datosFINAL)[2]]
datosFINAL[,"FTP_CONSECUTIVO"]  <-  rownames(datosFINAL)
rownames(datosFINAL)  <- NULL

datos ["ECON_DORMITORIOS"] <- datos ["ESTU_DORMITORIOS"]

vars <- c("FTP_CONSECUTIVO", "INST_COD_INSTITUCION",
          "INST_PRAC_CONSECUTIVOSNIES",
          "ESTU_PRGM_ACADEMICO","FAMI_ING_FMLIAR_MENSUAL","ECON_DORMITORIOS")
datosFINALouput  <-   merge( datosFINAL,
                            datos[, vars],
                            by.x ="FTP_CONSECUTIVO",   by.y ="FTP_CONSECUTIVO")

################################################################################
# #  output
################################################################################

datosoutput <- paste(outPath, "SABERPRO_OFF_INPUT_PRINQUAL2013_2014.txt" , sep="")
write.table(datosFINALouput  , datosoutput   ,sep = "\t", quote = FALSE ,row.names = FALSE,na = ""  )

#####
## SE CORRE EL ARCHIVO "C:\ecuellar\2015\ek\INSE\src\INSE_JUNTASCompleto.SAS"
####

