################################################################################
# # ReporteIES_AR.R
# # R Versions: R.2.15.0
# #
# # Author(s): Edwin Cuéllar y Alvaro Uzaheta
# #
# # Description: Este archivo genera los reportes de Resultados en SABER PRO,
# # medidas de aporte relativo y otros indicadores de calidad de las
# # Instituciones de Educación Superior.
# #
# # Inputs:
# #
# # Outputs:
# #
################################################################################
options(encoding='UTF-8')
################################################################################
# # Folders
################################################################################

setwd("C:/ecuellar/2015/ek/Aporte Relativo/src")

outPath      <- "../output/"
inPath       <- "../input/"
srcPath      <- "../src/"
docPath      <- "../doc"

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
  # # function to built multirow
################################################################################
  MakeMultiRow <- function (variable, texto = "*", ajust = FALSE ) {
  # # introduce a multirow aspect to latex tables
  # #
  # # Arg:
  # #  variable: [vector of character class] a variable that you want to built a multirow...
  # #  texto: [character] option to fixed the width of the column in the table
  # #  ajust: [logical] if the table consider white rows for clarity, ajust the number of rows
  # #
  # # Ret:
  # #  variable
  rleVar     <- rle(variable)
  lenRleVar  <- length(rleVar$lengths)
  if (lenRleVar > 1){
    posiRleVar <- c(1, cumsum(rleVar$lengths[seq(lenRleVar - 1)]) + 1)
  } else{
    posiRleVar <- 1
  }

  if (ajust){
    noRows <- 2 * rleVar$lengths - 1
  } else {
    noRows <- rleVar$lengths
  }

  condicional <- rleVar$lengths != 1
  variable[-posiRleVar] <- ""
  variable[ posiRleVar[condicional]] <- paste("\\multirow{", noRows[condicional],
                      "}{", texto, "}{", rleVar$values[condicional], "}", sep = "")

  return(variable)
  }

################################################################################
# #  Funcion para cambiar formato
################################################################################

CambiarFormato <- function(x, mult = 1,
                           digits = 2,
                           decimal.mark = ',', big.mark = '.',
                           textPre = '', textPost = '',
                           cambiarNR = FALSE, valorNR = 'NR',
                           valorNA = 'NA') {
                    if (is.null(cambiarNR)){
                      cambiarNR <- x == valorNR
                    }
                    isValue <- !is.na(x) & !cambiarNR
                    y <- character(length(x))
                    y[isValue] <- paste(textPre,
                                        formatC(x[isValue] * mult,
                                                format = 'f',
                                                digits = digits,
                                                decimal.mark = decimal.mark,
                                                big.mark = big.mark),
                                        textPost, sep = '')
                    y[is.na(x)]  <- valorNA
                    y[cambiarNR] <- valorNR

                    return(y)

                  }


CambiarGra <- function (x, ...)
{
    format(x, ..., decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

################################################################################
# # funciones predefinidas
################################################################################
PrintThous <- function (x) {
  # # Imprime valores enteros en formato de miles con separador de miles el .
  # #
  # # Arg:
  # #  x: un vector
  # #
  # # Ret:
  # #  un vector tipo character con los valores
  if (class(x)[1] == 'numeric') {
    x <- ifelse(is.na(x), '-', formatC(x, format = "d", big.mark = '.'))
  }

  return(x)
}

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
# # Fu8nción para graficar varios ggplot en una sola ventana.
# #
# # Fuente: http://gettinggeneticsdone.blogspot.com/2010/03/arrange-multiple-ggplot2-plots-in-same.html
################################################################################


## Function for arranging ggplots. use png(); arrange(p1, p2, ncol=1); dev.off() to save.
library(grid)
require(grid)
vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
	dots <- list(...)
	n <- length(dots)
	if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
	if(is.null(nrow)) { nrow = ceiling(n/ncol)}
	if(is.null(ncol)) { ncol = ceiling(n/nrow)}
        ## NOTE see n2mfrow in grDevices for possible alternative
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
	ii.p <- 1
	for(ii.row in seq(1, nrow)){
	ii.table.row <- ii.row
	if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
		for(ii.col in seq(1, ncol)){
			ii.table <- ii.p
			if(ii.p > n) break
			print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
			ii.p <- ii.p + 1
		}
	}
}

###############################################################################
## convert list to data.frame
###############################################################################

listaToDataframe <- function(data) {
    nCol <- max(vapply(data, length, 0))
    data <- lapply(data, function(row) c(row, rep(NA, nCol-length(row))))
    data <- matrix(unlist(data), nrow=length(data), ncol=nCol, byrow=TRUE)
    data.frame(data)
  }

################################################################################
# # Global definitions
################################################################################
# # Número de estudiantes
kBig <- 30
kBig2 <- 5

################################################################################
# # Input files
################################################################################

# # El siguiente archivo contiene los nombres de las universidades
fileName <- "InstitNombreReducidoRevisadoEC.txt"
inFile   <- paste (inPath, fileName, sep = "")
universidades <- read.table(file(inFile, encoding="latin1"), sep = "\t", header=TRUE)

universidades <- universidades[, c('INST_ID', 'UNIV_NOMBREREDUCIDO')]

# # Los siguientes son los archivos que contienen las bases de Valor
# # Agregado

fileName <- "pro11_13_1_to_13_2_v2014.txt"
inFile   <- paste (inPath, fileName, sep = "")
VA2013 <- read.delim(file(inFile, encoding="latin1"), sep = "|", header=TRUE, dec=".")

fileName <- "pro11_14_1_to_14_2_v2014.txt"
inFile   <- paste (inPath, fileName, sep = "")
VA2014 <- read.delim(file(inFile, encoding="latin1"), sep = "|", header=TRUE, dec=".")

dat <- rbind.fill(VA2013, VA2014)

dat[, 'inst_by_ref'] <- paste(dat[, 'INST_COD_INSTITUCION'],
                              dat[, 'ESTU_GRUPO_REFERENCIA'],
                              sep="__")

tmp <- aggregate(dat[, 'inse'],
                 list(dat[, 'inst_by_ref']), mean, na.rm = TRUE)

names(tmp) <- c("inst_by_ref", "inse_mean")

dat <- merge(dat, tmp)

outcomes <- c("MOD_RAZONA_CUANTITATIVO_PUNT", "MOD_LECTURA_CRITICA")
prior <- list(MOD_RAZONA_CUANTITATIVO_PUNT = "MATEMATICAS_PUNT_ESTAND",
              MOD_LECTURA_CRITICA = "LENGUAJE_PUNT_ESTAND")
ref.grps <- split(dat,dat[, 'ESTU_GRUPO_REFERENCIA'])

# # A continuación se pegan los puntajes del indicador de los resultados
# # del examen SABER 11 calculado con las 8 áreas del examen.

fileName    <- "SB1106_10PuntParaVAPublic.txt"
inFile      <- paste(inPath, fileName, sep = "")
SB1106_10  <-  read.delim(file(inFile, encoding="latin1"),
                                          sep = "|", header=TRUE, dec=".")

SB1106_10 <- SB1106_10[, c('ESTU_CONSECUTIVO', 'IndicadorPuntajes')]

dat <- merge(dat, SB1106_10, by.x =
                         'ESTU_CONSECUTIVO', by.y = 'ESTU_CONSECUTIVO')

dat[, 'EsFemenino'] <- ifelse(dat[, 'ESTU_GENERO'] == 'F', 1, 0)

dat[, 'EsOficial'] <- ifelse(dat[, 'COLE_NATURALEZA'] == 'O', 1, 0)

dat[, 'EsUrbano'] <- ifelse(dat[, 'COLE_UBICACIONPLANTEL'] == 'U', 1, 0)

dat[, 'TipoZona'] <- paste(dat[, 'COLE_NATURALEZA'],
                           dat[, 'COLE_UBICACIONPLANTEL'], sep = '')

TablaTipoZona <- as.data.frame.matrix(table(dat[, 'inst_by_ref'], dat[, 'TipoZona']))

TablaTipoZona[, 'INST_BY_REF'] <- row.names(TablaTipoZona)

row.names(TablaTipoZona) <- NULL

TablaTipoZona <- TablaTipoZona[, c('INST_BY_REF', 'OR', 'OU', 'NR', 'NU')]

TablaTipoZona[, 'Inst'] <- gsub("(\\d+)__.*", "\\1",
                                     TablaTipoZona[, 'INST_BY_REF'])


TablaTipoZona[, 'INST_BY_REF'] <- CambCaractRaros(TablaTipoZona, 'INST_BY_REF')

rm(SB1106_10)

datVA <- ddply(dat,  .(inst_by_ref, INST_COD_INSTITUCION), summarize,
                      LC = mean(MOD_LECTURA_CRITICA),
                      RC = mean(MOD_RAZONA_CUANTITATIVO_PUNT),
                      Tamano = length(ESTU_CONSECUTIVO),
                      INSE = mean(inse),
                      IndSB11 = mean(IndicadorPuntajes))

datVA[, 'inst_by_ref'] <-
  CambCaractRaros(datVA, 'inst_by_ref')

# # En este segmento del código se calculan algunos consolidados de la base
# # de VA

Agreg1 <- aggregate(dat[, c('MOD_COMUNICA_ESCRITA_PUNT',
                                    'MOD_INGLES_PUNT', 'MOD_LECTURA_CRITICA',
                                    'MOD_RAZONA_CUANTITATIVO_PUNT',
                                    'MOD_COMP_CIUDADANAS_PUNT',
                                    'inse', 'IndicadorPuntajes')],
                    list(dat$inst_by_ref), mean,
                    na.rm = TRUE)

AgregInfoSocioEcon  <- aggregate(dat[, c('inse', 'IndicadorPuntajes',
                                    'ESTU_EDAD', 'EsFemenino',
                                    'EsOficial', 'EsUrbano')],
                    list(dat$inst_by_ref), mean,
                    na.rm = TRUE)

AgregInfoSocioEcon[, 'Inst'] <- gsub("(\\d+)__.*", "\\1",
                                     AgregInfoSocioEcon[, 'Group.1'])

AgregInfoSocioEcon[, 'Group.1'] <-
  CambCaractRaros(AgregInfoSocioEcon, 'Group.1')

Agreg2 <- aggregate(dat$inst_by_ref,
                    list(dat$inst_by_ref), length)

AgregVA <- merge(Agreg1, Agreg2, by = 'Group.1')

AgregVA <- rename(AgregVA, c("Group.1" = "INST_BY_REF",
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

AgregVA[, 'INST_BY_REF'] <-
  CambCaractRaros(AgregVA, 'INST_BY_REF')

## Se trae el archivo con los agregados de los resultados de SABER PRO
## 2013 que fue construido en el script AgregadosInstRef.R. (Ejecutar el
## archivo y pasarlo al input)

fileName <- "AgregPRO2013.txt"
inFile   <- paste (inPath, fileName, sep = "")
AgregPRO2013 <- read.delim(file(inFile, encoding="latin1"), sep = "|",
                           header=TRUE, dec=".")

AgregPRO2013[, 'INST_BY_REF'] <-
  CambCaractRaros(AgregPRO2013, 'INST_BY_REF')

fileName <- "AgregPRO2014.txt"
inFile   <- paste (inPath, fileName, sep = "")
AgregPRO2014 <- read.delim(file(inFile, encoding="latin1"), sep = "|",
                           header=TRUE, dec=".")

AgregPRO2014[, 'INST_BY_REF'] <-
  CambCaractRaros(AgregPRO2014, 'INST_BY_REF')

AGREGADOS2013_2014 <- rbind(AgregPRO2013, AgregPRO2014)

AGREGADOS2013_2014 <- ddply(AGREGADOS2013_2014,
                      .(INST_BY_REF), summarize,
                      LC = weighted.mean(mod_lectura_critica_TODOS, n_TODOS),
                      RC = weighted.mean(mod_razona_cuantitativo_TODOS,
                                         n_TODOS),
                      Tamano = sum(n_TODOS),
                      INSE = weighted.mean(INSE_TODOS, n_TODOS))

AgregComp  <- merge(AgregVA, AgregPRO2014, by = "INST_BY_REF", all.y =
                    TRUE)

# # Se cargan las estimaciones de Valor Agregado junto con sus errores de
# # estimación

load(file.path(outPath, "va_estimates13_14.Rdata"))

# # Cambio de escala
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

VAEstimaciones13_14 <- lapply(VAEstimaciones13_14, function(x)
                              lapply(x, CambiarEscala))

AgregCompPunta  <- AgregComp[, c('INST_BY_REF', 'IndicadorPuntajes_va')]

tmp <- matrix(unlist(strsplit(as.character(AgregCompPunta$INST_BY_REF),
                              '__')), ncol=2, byrow=TRUE)

tmp  <-  as.data.frame(tmp)

names(tmp) <- c('Inst', 'RefGrp')

AgregCompPunta <- cbind(AgregCompPunta, tmp)

fileName <- "Agregados2013.txt"
inFile   <- paste (inPath, fileName, sep = "")
Agregados2013 <- read.delim(file(inFile, encoding="latin1"), sep = "\t",
                  header=TRUE, dec=".")

Agregados2013[, 'Prueba'] <- as.character(Agregados2013[, 'Prueba'])				  
				  
Agregados2013$Prueba[Agregados2013$Prueba ==
          "COMUNICACION ESCRITA"] <- "COMUNICACIÓN ESCRITA"

Agregados2013$Prueba[Agregados2013$Prueba ==
          "LECTURA CRITICA"] <- "LECTURA CRÍTICA"

Agregados2013$Prueba[Agregados2013$Prueba ==
          "INGLES"] <- "INGLÉS"					  				  

Agregados2013[, 'Prueba'] <- as.factor(Agregados2013[, 'Prueba'])	
		  
Agregados2013[, 'INST_BY_REF'] <-
  CambCaractRaros(Agregados2013, 'INST_BY_REF')

tmp <- matrix(unlist(strsplit(as.character(Agregados2013$INST_BY_REF),
                              '__')), ncol=2, byrow=TRUE)

tmp  <-  as.data.frame(tmp)

names(tmp) <- c('Instit', 'RefGrp')

Agregados2013 <- cbind(Agregados2013, tmp)

# # Se cargan los resultados Agregados de 2013. Este archivo fue entregad
# # por Tecnología con la información de publicación.

fileName <- "Agregados2014.txt"
inFile   <- paste (inPath, fileName, sep = "")
Agregados2014 <- read.delim(file(inFile, encoding="latin1"), sep = "\t",
                  header=TRUE, dec=".")	  
				  
Agregados2014[, 'INST_BY_REF'] <-
  CambCaractRaros(Agregados2014, 'INST_BY_REF')

tmp <- matrix(unlist(strsplit(as.character(Agregados2014$INST_BY_REF),
                              '__')), ncol=2, byrow=TRUE)

tmp  <-  as.data.frame(tmp)

names(tmp) <- c('Instit', 'RefGrp')

Agregados2014 <- cbind(Agregados2014, tmp)

Agregados <- rbind(Agregados2013, Agregados2014)
names(Agregados)[3] <- 'Ano'
names(Agregados)[4] <- 'Tamano'
Agregados[, 'x2'] <- Agregados[, 'Tamano'] *
                    ((Agregados[, 'DesvEstand'] ^ 2)  +
                     Agregados[, 'Promedio'] ^ 2)

 head(sqrt( ( (Agregados[, 'x2'] /  Agregados[, 'Tamano']) -
             (Agregados[, 'Promedio'] ^ 2)) ))

######################## AGREGADOSPROGRAMAS 2013			 
			 
fileName <- "Agregados20131Programas.txt"
inFile   <- paste (inPath, fileName, sep = "")
Agregados20131Programas <- read.delim(file(inFile,
                  encoding="latin1"), sep = "\t", header=TRUE, dec=".")

# # Cuatro programas con problemas en el grupo de referencia. 
# # Se deja el del último período

isCond <- Agregados20131Programas[, 'PRAC_ID'] == '8103'
Agregados20131Programas[isCond, 'GrupoReferencia']  <- 'INGENIERIA'

isCond <- Agregados20131Programas[, 'PRAC_ID'] == '14931'
Agregados20131Programas[isCond, 'GrupoReferencia']  <- 'ADMINISTRACION Y AFINES'

isCond <- Agregados20131Programas[, 'PRAC_ID'] == '22787'
Agregados20131Programas[isCond, 'GrupoReferencia']  <- 'ADMINISTRACION Y AFINES'

isCond <- Agregados20131Programas[, 'PRAC_ID'] == '15182'
Agregados20131Programas[isCond, 'GrupoReferencia']  <- 'CIENCIAS SOCIALES'

fileName <- "Agregados20132Programas.txt"
inFile   <- paste (inPath, fileName, sep = "")
Agregados20132Programas <- read.delim(file(inFile,
                  encoding="latin1"), sep = "\t", header=TRUE, dec=".")

isCond <- Agregados20132Programas[, 'PRAC_ID'] == '15182'
Agregados20132Programas[isCond, 'GrupoReferencia']  <- 'CIENCIAS SOCIALES'
				  				  
Agregados2013Programas <- rbind(Agregados20131Programas,
                                Agregados20132Programas)

Agregados2013Programas[, 'GrupoReferencia'] <-
  CambCaractRaros(Agregados2013Programas, 'GrupoReferencia')

Agregados2013Programas[, 'INST_BY_REF'] <-
               paste(Agregados2013Programas[, 'CodInstitucion'],
               Agregados2013Programas[, 'GrupoReferencia'],
                              sep="__")

Agregados2013Programas[, 'Ano'] <- 2013

names(Agregados2013Programas)[6] <- 'Tamano'

Niveles2013 <-
  aggregate(Agregados2013Programas[, c('A.', 'A1', 'A2', 'B1',
                                     'B.', 'SINNIVEL', 'N1', 'N2', 'N3',
                                     'N4', 'N5', 'N6', 'N7', 'N8')],
          Agregados2013Programas[, c('INST_BY_REF', 'NombrePrueba', 'Ano')],
          sum, na.rm = TRUE)
	  
######################## AgregadosProgramas 2014					  
					  
fileName <- "Agregados2014Programas.txt"
inFile   <- paste (inPath, fileName, sep = "")
Agregados2014Programas <- read.delim(file(inFile,
                  encoding="latin1"), sep = "\t", header=TRUE, dec=".")

Agregados2014Programas[, 'GrupoReferencia'] <-
  CambCaractRaros(Agregados2014Programas, 'GrupoReferencia')

Agregados2014Programas[, 'INST_BY_REF'] <-
               paste(Agregados2014Programas[, 'CodInstitucion'],
               Agregados2014Programas[, 'GrupoReferencia'],
                              sep="__")

Agregados2014Programas[, 'Ano'] <- 2014

names(Agregados2014Programas)[6] <- 'Tamano'

levels(Agregados2014Programas[, 'NombrePrueba']) <- levels(Agregados2013Programas[, 'NombrePrueba'])		  

Niveles2014 <-
  aggregate(Agregados2014Programas[, c('A.', 'A1', 'A2', 'B1',
                                     'B.', 'SINNIVEL', 'N1', 'N2', 'N3',
                                     'N4', 'N5', 'N6', 'N7', 'N8')],
          Agregados2014Programas[, c('INST_BY_REF', 'NombrePrueba', 'Ano')],
          sum, na.rm = TRUE)

levels(Niveles2014[, 'NombrePrueba']) <- levels(Niveles2013[, 'NombrePrueba'])		  
		  
Niveles  <- rbind(Niveles2013, Niveles2014)

names(Niveles) <- c('INST_BY_REF', 'Prueba', 'Ano', 'A-', 'A1',
                       'A2', 'B1', 'B+', 'SINNIVEL', 'N1', 'N2',
                       'N3', 'N4', 'N5', 'N6', 'N7', 'N8')					   
					   
levels(Niveles[, 'Prueba']) <- levels(Agregados[, 'Prueba'])

Agregados <- merge(Agregados, Niveles, by = c('INST_BY_REF', 'Prueba',
                                              'Ano'))

Agregados2013Programas[, 'x2'] <- Agregados2013Programas[, 'Tamano'] *
                    ((Agregados2013Programas[, 'DesvEstand'] ^ 2)  +
                     Agregados2013Programas[, 'Promedio'] ^ 2)

AgregadosTablaProgr2013 <- ddply(Agregados2013Programas,
                      .(INST_BY_REF, PRAC_ID, NombrePrueba, Ano), summarize,
                      DesvEstand = sqrt( sum(x2, na.rm = TRUE) / sum(Tamano) -
                                        weighted.mean(Promedio, Tamano) *
                                        weighted.mean(Promedio, Tamano)),
                      Promedio = weighted.mean(Promedio, Tamano),
                      Tamano = sum(Tamano))											  
											  
Agregados2014Programas[, 'x2'] <- Agregados2014Programas[, 'Tamano'] *
                    ((Agregados2014Programas[, 'DesvEstand'] ^ 2)  +
                     Agregados2014Programas[, 'Promedio'] ^ 2)

AgregadosTablaProgr2014 <- ddply(Agregados2014Programas,
                      .(INST_BY_REF, PRAC_ID, NombrePrueba, Ano), summarize,
                      DesvEstand = sqrt( sum(x2, na.rm = TRUE) / sum(Tamano) -
                                        weighted.mean(Promedio, Tamano) *
                                        weighted.mean(Promedio, Tamano)),
                      Promedio = weighted.mean(Promedio, Tamano),
                      Tamano = sum(Tamano))
					  


fileName <- "Programas_Nombre_ReducidoTildesAct2015.txt"
inFile   <- paste (inPath, fileName, sep = "")
ProgramasNombres <- read.table(file(inFile, encoding="latin1"),
                               sep = "\t", header=TRUE, allowEscapes =
                               TRUE)

Orden <- order(ProgramasNombres[, 'PRAC_ID'])

ProgramasNombres <- ProgramasNombres[Orden,]

Logic <- c("FALSE")

Duplicados <- duplicated(ProgramasNombres[,'PRAC_ID'], fromLast =
                         TRUE) %in% Logic

ProgramasNombres <- ProgramasNombres[which(Duplicados),]

fileName <- "Programas_Nombre_ReducidoAnexoAct2015.txt"
inFile   <- paste (inPath, fileName, sep = "")
ProgramasNombresAnexo <- read.table(file(inFile, encoding="latin1"),
                               sep = "\t", header=TRUE, allowEscapes =
                               TRUE)

Orden <- order(ProgramasNombresAnexo[, 'PRAC_ID'])

ProgramasNombresAnexo <- ProgramasNombresAnexo[Orden,]

Logic <- c("FALSE")

Duplicados <- duplicated(ProgramasNombresAnexo[,'PRAC_ID'], fromLast =
                         TRUE) %in% Logic

ProgramasNombresAnexo <- ProgramasNombresAnexo[which(Duplicados),]

fileName <- "PRAC_ID__SNIESAct2015.txt"
inFile   <- paste (inPath, fileName, sep = "")
PRAC_SNIES <- read.table(file(inFile, encoding="latin1"),
                               sep = "\t", header=TRUE)

fileName <- "Programa_MetodologiaAct2015.txt"
inFile   <- paste (inPath, fileName, sep = "")
Programa_Metodologia <- read.table(file(inFile, encoding="latin1"),
                               sep = "\t", header=TRUE)
			   
## CALCULOS POR GRUPO DE REFERENCIA

PromedioPond <- ddply(Agregados,
                      .(RefGrp, Prueba, Ano), summarize,
                      DesvEstand = sqrt( sum(x2, na.rm = TRUE) / sum(Tamano) -
                                        weighted.mean(Promedio, Tamano) *
                                        weighted.mean(Promedio, Tamano)),
                      Promedio = weighted.mean(Promedio, Tamano),
                      Tamano = sum(Tamano)
                      )

Quintiles <- aggregate(Agregados[, c('Q1', 'Q2', 'Q3', 'Q4', 'Q5')],
          Agregados[, c('RefGrp', 'Prueba', 'Ano')],
          FUN = sum, na.rm = TRUE)

AgregadosPruebas  <- merge(PromedioPond, Quintiles,
                           by = c('RefGrp', 'Prueba', 'Ano'))

NivelesPruebas <- aggregate(Agregados[, c('A-', 'A1', 'A2', 'B1', 'B+', 'N1',
                                    'N2', 'N3', 'N4', 'N5', 'N6', 'N7',
                                    'N8')],
          Agregados[, c('RefGrp', 'Prueba', 'Ano')],
          FUN = sum, na.rm = TRUE)

AgregadosPruebas  <- merge(AgregadosPruebas, NivelesPruebas,
                           by = c('RefGrp', 'Prueba', 'Ano'))

Modulos <- c("LECTURA CRÍTICA", "RAZONAMIENTO CUANTITATIVO",
             "COMPETENCIAS CIUDADANAS", "INGLÉS",
             "COMUNICACIÓN ESCRITA")

pruebas <- c('LECTURA CRÍTICA', 'RAZONAMIENTO CUANTITATIVO',
              'INGLÉS', 'COMUNICACIÓN ESCRITA')

nivelPrPro <- list('LECTURA CRÍTICA' =  c('1', '2', '3'),
            'RAZONAMIENTO CUANTITATIVO' =  c('1', '2', '3'),
            'INGLÉS' = c('A-', 'A1', 'A2', 'B1', 'B+'),
            'COMUNICACIÓN ESCRITA' = c('1', '2', '3', '4', '5', '6', '7', '8'))

nivelNeg <- list('LECTURA CRÍTICA' = c('1'),
                 'RAZONAMIENTO CUANTITATIVO' = c('1'),
                 'INGLÉS'  = c('A-'),
                'COMUNICACIÓN ESCRITA' = c('1', '2', '3'))

nivelPos <- list('LECTURA CRÍTICA' = c('3'),
                 'RAZONAMIENTO CUANTITATIVO' = c('3'),
                 'INGLÉS'  = c('B1', 'B+'),
                 'COMUNICACIÓN ESCRITA' = c('6', '7', '8'))

GrupRefer <- c('ADMINISTRACION Y AFINES' = 'Administración y afines',
               'ARQUITECTURA Y URBANISMO ' = 'Arquitectura y urbanismo',
               'BELLAS ARTES Y DISENNO' = 'Bellas artes y diseño',
               'CIENCIAS AGROPECUARIAS ' = 'Ciencias agropecuarias',
               'CIENCIAS MILITARES Y NAVALES' = 'Ciencias militares y navales',
               'CIENCIAS NATURALES Y EXACTAS' = 'Ciencias naturales y exactas',
               'CIENCIAS SOCIALES' = 'Ciencias sociales',
               'COMUNICACION, PERIODISMO Y PUBLICIDAD' = 'Comunicación, periodismo y publicidad',
               'CONTADURIA Y AFINES' = 'Contaduria y afines',
               'DERECHO' = 'Derecho',
               'ECONOMIA' = 'Economía',
               'EDUCACION' = 'Educación',
               'ENFERMERIA' = 'Enfermería',
               'HUMANIDADES' = 'Humanidades',
               'INGENIERIA' = 'Ingeniería',
               'MEDICINA' = 'Medicina',
               'PSICOLOGIA' = 'Psicología',
               'SALUD' = 'Salud')

PruebasMin <- c('LECTURA CRITICA' = 'Lectura crítica',
                'RAZONAMIENTO CUANTITATIVO' = 'Razonamiento cuantitativo',
                'COMPETENCIAS CIUDADANAS' = 'Competencias ciudadanas',
                'INGLES' = 'Inglés',
                'COMUNICACION ESCRITA' = 'Comunicación escrita')

PruebasMin1 <- c('LECTURA CRÍTICA' = 'Lectura crítica',
                'RAZONAMIENTO CUANTITATIVO' = 'Razonamiento cuantitativo',
                'COMPETENCIAS CIUDADANAS' = 'Competencias ciudadanas',
                'INGLÉS' = 'Inglés',
                'COMUNICACIÓN ESCRITA' = 'Comunicación escrita')

PrueGraf1 <- c('LECTURA CRÍTICA' = 'LECTURA CRITICA',
               'RAZONAMIENTO CUANTITATIVO' = 'RAZONAMIENTO CUANTITATIVO',
               'COMPETENCIAS CIUDADANAS' = 'COMPETENCIAS CIUDADANAS',
               'INGLÉS' = 'INGLES',
               'COMUNICACIÓN ESCRITA' = 'COMUNICACION ESCRITA')

# # Estas desviaciones son incorrectas, ya que se trabaja con el grupo de
# # referencia vigente.

#fileName <- "DesviacionesGR_Prueba2012.txt"
#inFile   <- paste (inPath, fileName, sep = "")
#Desviac2012 <- read.table(file(inFile, encoding="latin1"),
#                        sep = "\t", header=TRUE, dec = ".")

#fileName <- "DesviacionesGR_Prueba2013.txt"
#inFile   <- paste (inPath, fileName, sep = "")
#Desviac2013 <- read.table(file(inFile, encoding="latin1"),
#                        sep = "\t", header=TRUE, dec = ".")

Desviaciones <- AgregadosPruebas[, c('RefGrp', 'Prueba', 'Ano', 'DesvEstand')]

Desviaciones[, 'RefGrp'] <-
  CambCaractRaros(Desviaciones, 'RefGrp')

Desviaciones[, 'Prueba'] <-
  CambCaractRaros(Desviaciones, 'Prueba')

################################################################################
# # Cargada bases del MEN para calcular otros indicadores de calidad
################################################################################

Agreg2013_2014 <- rbind.fill(Agregados2013Programas, Agregados2014Programas)

InstitProgr <- Agreg2013_2014[, c('CodInstitucion', 'PRAC_ID', 'INST_BY_REF')]

Orden <- order(InstitProgr[, 'CodInstitucion'], InstitProgr[, 'PRAC_ID'])

InstitProgr <- InstitProgr[Orden,]

Logic <- c("FALSE")

Duplicados <- duplicated(InstitProgr[,'PRAC_ID'], fromLast = TRUE) %in% Logic

InstitProgr <- InstitProgr[which(Duplicados),]

ConteoProgramas <- aggregate(InstitProgr[, 'PRAC_ID'],
                 list(InstitProgr[, 'INST_BY_REF']), length)

names(ConteoProgramas) <- c('INST_BY_REF', 'TotalProg')

InstitProgr <- merge(PRAC_SNIES, InstitProgr, by = 'PRAC_ID', all.y = TRUE)

InstitProgr[, 'Llave'] <- paste(InstitProgr[, 'CodInstitucion'],
                                InstitProgr[, 'PRAC_CONSECUTIVOSNIES'],
                                sep = "__")

fileName <- "Admitidos_Programas2013_2014.txt"
inFile   <- paste (inPath, fileName, sep = "")
Admitidos <- read.table(file(inFile, encoding="latin1"),
                        sep = "\t", header=TRUE)

Admitidos <- Admitidos[, c('CodInst', 'CodigoSNIES', 'Adm2013_1', 'Adm2013_2', 'Adm2014_1', 'Adm2014_2')]

Admitidos[, 'Llave'] <- paste(Admitidos[, 'CodInst'], Admitidos[,
                              'CodigoSNIES'], sep = "__")

Admitidos <- merge(InstitProgr, Admitidos, by = 'Llave', all.x = TRUE)

Admitidos[, 'Adm2013_1'] <- as.numeric(levels((Admitidos[,
                          'Adm2013_1'])))[Admitidos[, 'Adm2013_1']]

Admitidos[, 'Adm2013_2'] <- as.numeric(levels((Admitidos[,
                          'Adm2013_2'])))[Admitidos[, 'Adm2013_2']]
						  
Admitidos[, 'Adm2014_1'] <- as.numeric(levels((Admitidos[,
                          'Adm2014_1'])))[Admitidos[, 'Adm2014_1']]

Admitidos[, 'Adm2014_2'] <- as.numeric(levels((Admitidos[,
                          'Adm2014_2'])))[Admitidos[, 'Adm2014_2']]

Admitidos[, 'Adm2013_2014Comp'] <- rowSums(Admitidos[, c('Adm2013_1',
                    'Adm2013_2', 'Adm2014_1', 'Adm2014_2')])

Admitidos[, 'Adm2013_1'] <- NULL

Admitidos[, 'Adm2013_2'] <- NULL

Admitidos[, 'Adm2014_1'] <- NULL

Admitidos[, 'Adm2014_2'] <- NULL

Revtmp1 <- aggregate(Admitidos[, 'Adm2013_2014Comp'],
                 list(Admitidos[, 'INST_BY_REF']), function(x)
                   sum(is.na(x)))

names(Revtmp1) <- c('INST_BY_REF', 'NumNA')

Revtmp2 <- aggregate(Admitidos[, 'Adm2013_2014Comp'],
                 list(Admitidos[, 'INST_BY_REF']), length)

names(Revtmp2) <- c('INST_BY_REF', 'NumProg')

Revtmp <- merge(Revtmp1, Revtmp2, by = 'INST_BY_REF')

Revtmp[, 'PropNAAdm'] <- Revtmp[, 'NumNA']/Revtmp[, 'NumProg']

Revtmp[, 'NumNA'] <- NULL

Revtmp[, 'NumProg'] <- NULL

AdmitInst <- aggregate(Admitidos[, 'Adm2013_2014Comp'],
                 list(Admitidos[, 'INST_BY_REF']), sum, na.rm = TRUE)

names(AdmitInst) <- c('INST_BY_REF', 'Adm2013_2014')

AdmitInst <- merge(AdmitInst, Revtmp, by = 'INST_BY_REF')

# # Inscritos

fileName <- "Inscritos_Programas2013_2014.txt"
inFile   <- paste (inPath, fileName, sep = "")
Inscritos <- read.table(file(inFile, encoding="latin1"),
                        sep = "\t", header=TRUE)

Inscritos <- Inscritos[, c('CodInst', 'CodigoSNIES', 'Inscr2013_1',
                           'Inscr2013_2', 'Inscr2014_1',
                           'Inscr2014_2')]

Inscritos[, 'Llave'] <- paste(Inscritos[, 'CodInst'], Inscritos[,
                              'CodigoSNIES'], sep = "__")

Inscritos <- merge(InstitProgr, Inscritos, by = 'Llave', all.x = TRUE)

Inscritos[, 'Inscr2013_1'] <- as.numeric(levels((Inscritos[,
                          'Inscr2013_1'])))[Inscritos[, 'Inscr2013_1']]

Inscritos[, 'Inscr2013_2'] <- as.numeric(levels((Inscritos[,
                          'Inscr2013_2'])))[Inscritos[, 'Inscr2013_2']]
						  
Inscritos[, 'Inscr2014_1'] <- as.numeric(levels((Inscritos[,
                          'Inscr2014_1'])))[Inscritos[, 'Inscr2014_1']]

Inscritos[, 'Inscr2014_2'] <- as.numeric(levels((Inscritos[,
                          'Inscr2014_2'])))[Inscritos[, 'Inscr2014_2']]

Inscritos[, 'Insc2013_2014Comp'] <-
  rowSums(Inscritos[, c('Inscr2013_1', 'Inscr2013_2',
                        'Inscr2014_1', 'Inscr2014_2')])

Inscritos[, 'Inscr2013_1'] <- NULL
Inscritos[, 'Inscr2013_2'] <- NULL
Inscritos[, 'Inscr2014_1'] <- NULL
Inscritos[, 'Inscr2014_2'] <- NULL

Revtmp1 <- aggregate(Inscritos[, 'Insc2013_2014Comp'],
                 list(Inscritos[, 'INST_BY_REF']), function(x)
                   sum(is.na(x)))

names(Revtmp1) <- c('INST_BY_REF', 'NumNA')

Revtmp2 <- aggregate(Inscritos[, 'Insc2013_2014Comp'],
                 list(Inscritos[, 'INST_BY_REF']), length)

names(Revtmp2) <- c('INST_BY_REF', 'NumProg')

Revtmp <- merge(Revtmp1, Revtmp2, by = 'INST_BY_REF')

Revtmp[, 'PropNAInsc'] <- Revtmp[, 'NumNA']/Revtmp[, 'NumProg']

Revtmp[, 'NumNA'] <- NULL

Revtmp[, 'NumProg'] <- NULL

InscrInst <- aggregate(Inscritos[, 'Insc2013_2014Comp'],
                 list(Inscritos[, 'INST_BY_REF']), sum, na.rm = TRUE)

names(InscrInst) <- c('INST_BY_REF', 'Insc2013_2014')

InscrInst <- merge(InscrInst, Revtmp, by = 'INST_BY_REF')

# # Instituciones acreditadas

fileName <- "Instituciones_Acreditadas_CNA_Mayo20_2015.txt"
inFile   <- paste (inPath, fileName, sep = "")
Acreditadas <- read.table(file(inFile, encoding="latin1"),
                        sep = "\t", header=TRUE, dec = ".")

# # Desercion cohorte

fileName <- "DesercionCohorte.txt"
inFile   <- paste (inPath, fileName, sep = "")
DesercionCohorte <- read.table(file(inFile, encoding="latin1"),
                        sep = "\t", header=TRUE, dec = ".")

DesercionCohorte <- DesercionCohorte[, c('CodInst', 'CodPrograma',
                                         'X10')]

DesercionCohorte[, 'Llave'] <- paste(DesercionCohorte[, 'CodInst'],
                                    DesercionCohorte[, 'CodPrograma'],
                                    sep = "__")

DesercionCohorte <- merge(InstitProgr, DesercionCohorte, by = 'Llave', all.x = TRUE)

Revtmp1 <- aggregate(DesercionCohorte[, 'X10'],
                 list(DesercionCohorte[, 'INST_BY_REF']), function(x)
                   sum(is.na(x)))

names(Revtmp1) <- c('INST_BY_REF', 'NumNA')

Revtmp2 <- aggregate(DesercionCohorte[, 'X10'],
                 list(DesercionCohorte[, 'INST_BY_REF']), length)

names(Revtmp2) <- c('INST_BY_REF', 'NumProg')

Revtmp <- merge(Revtmp1, Revtmp2, by = 'INST_BY_REF')

Revtmp[, 'PropNADesC'] <- Revtmp[, 'NumNA']/Revtmp[, 'NumProg']

Revtmp[, 'NumNA'] <- NULL

Revtmp[, 'NumProg'] <- NULL

DesercionCohorte <- aggregate(DesercionCohorte[, 'X10'],
                 list(DesercionCohorte[, 'INST_BY_REF']), mean, na.rm = TRUE)

names(DesercionCohorte) <- c('INST_BY_REF', 'DesercionCohorte')

DesercionCohorte <- merge(DesercionCohorte, Revtmp, by = 'INST_BY_REF')

# # Desercion periodo

fileName <- "DesercionPeriodo.txt"
inFile   <- paste (inPath, fileName, sep = "")
DesercionPeriodo <- read.table(file(inFile, encoding="latin1"),
                        sep = "\t", header=TRUE, dec = ".")

DesercionPeriodo <- subset(DesercionPeriodo, DesercionPeriodo[,
                           'Periodo'] == '20141')

DesercionPeriodo[, 'Llave'] <- paste(DesercionPeriodo[, 'CodInst'],
                                    DesercionPeriodo[, 'CodPrograma'],
                                    sep = "__")

DesercionPeriodo <- merge(InstitProgr, DesercionPeriodo, by = 'Llave',
                          all.x = TRUE)

Revtmp1 <- aggregate(DesercionPeriodo[, 'TasaDesercion'],
                 list(DesercionPeriodo[, 'INST_BY_REF']), function(x)
                   sum(is.na(x)))

names(Revtmp1) <- c('INST_BY_REF', 'NumNA')

Revtmp2 <- aggregate(DesercionPeriodo[, 'TasaDesercion'],
                 list(DesercionPeriodo[, 'INST_BY_REF']), length)

names(Revtmp2) <- c('INST_BY_REF', 'NumProg')

Revtmp <- merge(Revtmp1, Revtmp2, by = 'INST_BY_REF')

Revtmp[, 'PropNADesP'] <- Revtmp[, 'NumNA']/Revtmp[, 'NumProg']

Revtmp[, 'NumNA'] <- NULL

Revtmp[, 'NumProg'] <- NULL

DesercionPeriodo <- aggregate(DesercionPeriodo[, 'TasaDesercion'],
                 list(DesercionPeriodo[, 'INST_BY_REF']), mean, na.rm = TRUE)

names(DesercionPeriodo) <- c('INST_BY_REF', 'DesercionPeriodo')

DesercionPeriodo <- merge(DesercionPeriodo, Revtmp, by = 'INST_BY_REF')

# # Acreditacion programas

fileName <- "Programas_Acreditados_CNA_Abril30_2015.txt"
inFile   <- paste (inPath, fileName, sep = "")
ProgramAcred <- read.table(file(inFile, encoding="latin1"),
                        sep = "\t", header=TRUE, dec = ".")

ProgramAcred[, 'Llave'] <- paste(ProgramAcred[, 'CodInst'], ProgramAcred[,
                              'CodSNIES'], sep = "__")

## Se hace el calculo solamente con base en los programas academicos con
## estudiantes en el examen SABER PRO en 2013 y/0 2014 (por eso el all.y = TRUE)

ProgramAcred <- merge(ProgramAcred, InstitProgr, by = 'Llave', all.y = TRUE)

ProgramAcred <- aggregate(ProgramAcred[, 'ProgramAcred'],
                 list(ProgramAcred[, c('INST_BY_REF')]),
                 sum, na.rm = TRUE)

names(ProgramAcred) <- c('INST_BY_REF', 'ProgAcred')

InstTMP <- InstitProgr[, c('CodInstitucion', 'INST_BY_REF')]

Orden <- order(InstTMP[, 'CodInstitucion'])

InstTMP <- InstTMP[Orden,]

Logic <- c("FALSE")

Duplicados <- duplicated(InstTMP[,'INST_BY_REF'], fromLast = TRUE) %in% Logic

InstTMP <- InstTMP[which(Duplicados),]

ProgramAcred <- merge(ProgramAcred, InstTMP, by = 'INST_BY_REF', all.x =
                      TRUE)

# # ProgramAcred[, 'Inst'] <- gsub(".*__.*__(.*)__.*", "\\1", ProgramAcred[,
# #                      'INST_BY_REF'])

# # Valor matricula

fileName <- "Valores_matricula_SNIES2013.txt"
inFile   <- paste (inPath, fileName, sep = "")
ValorMatricula <- read.table(file(inFile, encoding="latin1"),
                        sep = "\t", header=TRUE, dec = ".")

ValorMatricula[, 'Llave']  <- paste(ValorMatricula[, 'CodInstitucion'],
                                    ValorMatricula[, 'CodigoSNIES'], sep = "__")

ValorMatricula <- merge(InstitProgr, ValorMatricula, by = 'Llave', all.x = TRUE)

Revtmp1 <- aggregate(ValorMatricula[, 'ValorMatricula'],
                 list(ValorMatricula[, 'INST_BY_REF']), function(x)
                   sum(is.na(x)))

names(Revtmp1) <- c('INST_BY_REF', 'NumNA')

Revtmp2 <- aggregate(ValorMatricula[, 'ValorMatricula'],
                 list(ValorMatricula[, 'INST_BY_REF']), length)

names(Revtmp2) <- c('INST_BY_REF', 'NumProg')

Revtmp <- merge(Revtmp1, Revtmp2, by = 'INST_BY_REF')

Revtmp[, 'PropNAMat'] <- Revtmp[, 'NumNA']/Revtmp[, 'NumProg']

Revtmp[, 'NumNA'] <- NULL

Revtmp[, 'NumProg'] <- NULL

ValorMatricula <- aggregate(ValorMatricula[, 'ValorMatricula'],
                 list(ValorMatricula[, 'INST_BY_REF']), mean, na.rm = TRUE)

names(ValorMatricula) <- c('INST_BY_REF', 'CostoMatricula')

ValorMatricula <- merge(ValorMatricula, Revtmp, by = 'INST_BY_REF')

# # Caracter de la institucion

fileName <- "OrigenInstituc2015.txt"
inFile   <- paste (inPath, fileName, sep = "")
Origen_Inst <- read.table(file(inFile, encoding="latin1"),
                        sep = "|", header=TRUE, dec = ".")

names(Origen_Inst) <- c('Instit', 'Caracter')

# # Salario

fileName <- "OLE_SALARIO DE ENGANCHE CORTE 2013.txt"
inFile   <- paste (inPath, fileName, sep = "")
Salario <- read.table(file(inFile, encoding="latin1"),
                        sep = "\t", header=TRUE, dec = ".")

names(Salario)[1]  <- 'PRAC_CONSECUTIVOSNIES'

Salario <- merge(Salario, InstitProgr, by=c("PRAC_CONSECUTIVOSNIES"))

Revtmp1 <- aggregate(Salario[, 'SALARIO'],
                 list(Salario[, 'INST_BY_REF']), function(x)
                   sum(is.na(x)))

names(Revtmp1) <- c('INST_BY_REF', 'NumNA')

Revtmp2 <- aggregate(Salario[, 'SALARIO'],
                 list(Salario[, 'INST_BY_REF']), length)

names(Revtmp2) <- c('INST_BY_REF', 'NumProg')

Revtmp <- merge(Revtmp1, Revtmp2, by = 'INST_BY_REF')

Revtmp[, 'PropNASal'] <- Revtmp[, 'NumNA']/Revtmp[, 'NumProg']

Revtmp[, 'NumNA'] <- NULL

Revtmp[, 'NumProg'] <- NULL

Salario <- aggregate(Salario[, 'SALARIO'],
                 list(Salario[, 'INST_BY_REF']), mean, na.rm = TRUE)

names(Salario) <- c('INST_BY_REF', 'Salario')

Salario <- merge(Salario, Revtmp, by = 'INST_BY_REF')

# # Tasa de vinculación

fileName <- "OLE_TASA DE VINCULACION CORTE 2013.txt"
inFile   <- paste (inPath, fileName, sep = "")
Vinculacion <- read.table(file(inFile, encoding="latin1"),
                        sep = "\t", header=TRUE, dec = ".")

names(Vinculacion)[1] <- 'PRAC_CONSECUTIVOSNIES'

Vinculacion <- merge(Vinculacion, InstitProgr, by=c("PRAC_CONSECUTIVOSNIES"))

Revtmp1 <- aggregate(Vinculacion[, 'TASA_COTIZANTES'],
                 list(Vinculacion[, 'INST_BY_REF']), function(x)
                   sum(is.na(x)))

names(Revtmp1) <- c('INST_BY_REF', 'NumNA')

Revtmp2 <- aggregate(Vinculacion[, 'TASA_COTIZANTES'],
                 list(Vinculacion[, 'INST_BY_REF']), length)

names(Revtmp2) <- c('INST_BY_REF', 'NumProg')

Revtmp <- merge(Revtmp1, Revtmp2, by = 'INST_BY_REF')

Revtmp[, 'PropNAVin'] <- Revtmp[, 'NumNA']/Revtmp[, 'NumProg']

Revtmp[, 'NumNA'] <- NULL

Revtmp[, 'NumProg'] <- NULL

Vinculacion <- aggregate(Vinculacion[, 'TASA_COTIZANTES'],
                 list(Vinculacion[, 'INST_BY_REF']), mean, na.rm = TRUE)

names(Vinculacion) <- c('INST_BY_REF', 'Vinculacion')

Vinculacion <- merge(Vinculacion, Revtmp, by = 'INST_BY_REF')

## Cargada de bases de SABER PRO 2014 para gráficos de distribución

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

PRO20142y3GEN[, 'PRUEBA'] <- as.numeric(substr(PRO20142y3GEN$FTP_CONSECUTIVO, 6, 10)) 

PRO20142y3GEN <- PRO20142y3GEN[, c('PRUEBA', 'FTP_CONSECUTIVO',
                                   'INST_COD_INSTITUCION',
                                   'INST_PRAC_CONSECUTIVOSNIES',
                                   'ESTU_GRUPO_REFERENCIA',
                                   'MOD_COMUNICA_ESCRITA_PUNT',
                                   'MOD_INGLES_PUNT',
                                   'MOD_LECTURA_CRITICA',
                                   'MOD_RAZONA_CUANTITATIVO_PUNT',
                                   'MOD_COMP_CIUDADANAS_PUNT')]

ResultadosPruebas <- c('MOD_COMUNICA_ESCRITA_PUNT', 'MOD_INGLES_PUNT',
'MOD_RAZONA_CUANTITATIVO_PUNT', 'MOD_LECTURA_CRITICA', 'MOD_COMP_CIUDADANAS_PUNT')

for (RP in ResultadosPruebas){
PRO20142y3GEN[, RP] <- as.numeric(as.character(sub(',', '.', PRO20142y3GEN[, RP])))
}								   
								   
PRO20142y3GEN[, 'ESTU_GRUPO_REFERENCIA'] <-
  CambCaractRaros(PRO20142y3GEN, 'ESTU_GRUPO_REFERENCIA')

PRO20142y3GEN[, 'INST_BY_REF'] <- paste(PRO20142y3GEN[,
                                        'INST_COD_INSTITUCION'],
PRO20142y3GEN[, 'ESTU_GRUPO_REFERENCIA'], sep = '__')

names(PRO20142y3GEN) <- c('PRUEBA', 'FTP_CONSECUTIVO', 'INST_COD_INSTITUCION',
                          'INST_PRAC_CONSECUTIVOSNIES',
                          'ESTU_GRUPO_REFERENCIA',
                          'COMUNICACIÓN ESCRITA', 'INGLÉS', 'LECTURA CRÍTICA',
                          'RAZONAMIENTO CUANTITATIVO',
                          'COMPETENCIAS CIUDADANAS', 'INST_BY_REF')						  
						  
## Se excluyen puntajes muy bajos. Esto se realiza debido a que el
## kernel de la densidad gráficamente es más fácil de leer cuando los
## valores tienen un menor rango. Además, al hacer el cálculo de los
## puntajes atípicos se encuentra que estos resultan ser marginales por
## grupo de referencia.

# > dim(PRO20131y3GEN)
# [1] 239688     10

for(md in Modulos){
PRO20142y3GEN <- subset(PRO20142y3GEN, PRO20142y3GEN[, md] > 6)
}

# > dim(PRO20131y3GEN)
# [1] 237518     10

################################################################################
# # TABLA6
################################################################################

tabla6 <- merge(ProgramAcred, ConteoProgramas, by = 'INST_BY_REF')

tabla6 <- merge(tabla6, Acreditadas, by.x = 'CodInstitucion', by.y =
                'CodInst')

################################################################################
# # TABLA7
################################################################################

tabla7 <- merge(AdmitInst, InscrInst, by = 'INST_BY_REF', all = TRUE)

tabla7[, 'PropAdmInsc'] <- tabla7[, 'Adm2013_2014']/tabla7[, 'Insc2013_2014']

tabla7 <- merge(tabla7, ValorMatricula, by = 'INST_BY_REF', all = TRUE)

tabla7 <- merge(tabla7, InstTMP, by = 'INST_BY_REF', all.x =
                      TRUE)

tabla7 <- merge(tabla7, Origen_Inst, by.x = 'CodInstitucion', by.y = 'Instit',
                all.x = TRUE)

tabla7[, 'PropAdmInsc'] <- tabla7[, 'Adm2013_2014']/tabla7[, 'Insc2013_2014']

tabla7[, 'Adm2013_2014'] <- CambiarFormato(tabla7[, 'Adm2013_2014'], digits = 0,
                                   cambiarNR = tabla7[, 'PropNAAdm'] >
                                   0.3 | (tabla7[, 'Adm2013_2014'] >
                                          tabla7[, 'Insc2013_2014']))

tabla7[, 'Insc2013_2014'] <- CambiarFormato(tabla7[, 'Insc2013_2014'], digits = 0,
                                   cambiarNR = tabla7[, 'PropNAInsc'] >
                                   0.3 | (tabla7[, 'Adm2013_2014'] >
                                          tabla7[, 'Insc2013_2014']))

tabla7[, 'PropAdmInsc'] <- CambiarFormato(tabla7[, 'PropAdmInsc'],
                                      mult = 100, digits = 2, textPost = '\\%',
                                      cambiarNR = tabla7[, 'PropAdmInsc'] > 1
                                      | tabla7[, 'Adm2013_2014'] == 'NR' |
                                         tabla7[, 'Insc2013_2014'] == 'NR'
                                      | tabla7[, 'Adm2013_2014'] == 0)

tabla7[, 'CostoMatricula'] <- CambiarFormato(tabla7[, 'CostoMatricula'],
                                      digits = 0, textPre = '\\$',
                                      cambiarNR = tabla7[, 'PropNAMat'] > 0.3)

################################################################################
# # TABLA8
################################################################################

tabla8 <- merge(DesercionPeriodo, DesercionCohorte, by = 'INST_BY_REF',
                all = TRUE)

tabla8 <- merge(tabla8, InstTMP, by = 'INST_BY_REF', all.x =
                      TRUE)

tabla8[, 'DesercionPeriodo'] <- CambiarFormato(tabla8[,
                                               'DesercionPeriodo'],
textPos = '\\%', digits = 2)

tabla8[, 'DesercionCohorte'] <- CambiarFormato(tabla8[,
                                               'DesercionCohorte'],
textPost = '\\%', mult = 100, digits = 2)

################################################################################
# # TABLA9
################################################################################

tabla9 <- merge(Salario, Vinculacion, by = 'INST_BY_REF')

tabla9 <- merge(tabla9, InstTMP, by = 'INST_BY_REF', all.x =
                      TRUE)

tabla9[, 'Vinculacion'] <- CambiarFormato(tabla9[,
                                              'Vinculacion'], digits
= 1, textPost = '\\%', cambiarNR = tabla9[, 'PropNAVin'] > 0.3)

tabla9[, 'Salario'] <- CambiarFormato(tabla9[, 'Salario'],
                                      digits = 0, textPre = '\\$',
                                      cambiarNR = tabla9[, 'PropNASal'] > 0.3)

TablaOC <- merge(tabla6, tabla7, by = 'INST_BY_REF')

TablaOC[, 'CodInstitucion.x'] <- NULL

TablaOC[, 'CodInstitucion.y'] <- NULL

TablaOC <- merge(TablaOC, tabla8, by = 'INST_BY_REF')

TablaOC <- merge(TablaOC, tabla9, by = 'INST_BY_REF')

TablaOC[, 'CodInstitucion.y'] <- NULL

TablaOC <- merge(TablaOC, universidades, by.x = 'CodInstitucion.x',
                 by.y = 'INST_ID')

TablaOC[, 'FechaAcreditacion'] <- NULL

# # Tabla Anexo 1

TablaANEXO <- merge(AGREGADOS2013_2014, datVA, by.x =  'INST_BY_REF',
                    by.y = 'inst_by_ref', all = TRUE)

TablaANEXO[, 'INSE.x'] <- CambiarFormato(TablaANEXO[, 'INSE.x'])

TablaANEXO[, 'LC.x'] <- CambiarFormato(TablaANEXO[, 'LC.x'])

TablaANEXO[, 'RC.x'] <- CambiarFormato(TablaANEXO[, 'RC.x'])

TablaANEXO[, 'INSE.y'] <- CambiarFormato(TablaANEXO[, 'INSE.y'])

TablaANEXO[, 'LC.y'] <- CambiarFormato(TablaANEXO[, 'LC.y'])

TablaANEXO[, 'RC.y'] <- CambiarFormato(TablaANEXO[, 'RC.y'])

TablaANEXO[, 'IndSB11'] <- CambiarFormato(TablaANEXO[, 'IndSB11'])

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

## Identifica a las IGR que tienen más de 15 estudiantes en SABER PRO
## 2013 y que por lo tanto tendrán incluido los resultados de SABER PRO
## 2013 pero no necesariamente se les calcuala vecindad

## Todos los reportes incluyen los resultados de SABER PRO
## indepentientemente del número de estudiantes que tenga. Esta decisión
## se toma debido a que la única razón para no hacerlo era que los
## quintiles no tenían sentido. Como ese resultado se elimino del
## reporte no hay problema con el número de estudiantes de SABER PRO.

## Para tener en cuenta las IGR que no tienen ni siquiera un estudiante en Valor Agregado,
## se icluyo en el merge entre AGREGADOS2012_2013 y datVA la instrucción
## all = TRUE. Para estas IGR se hace el siguiente cambio para poder generar 
## banderas que definen la generación de reportes.

isIGRsinEstVA <- TablaANEXO[, 'IndSB11'] == 'NA'
TablaANEXO[isIGRsinEstVA, 'hasVA'] <- FALSE
TablaANEXO[isIGRsinEstVA, 'has15Vec'] <- FALSE

# TablaANEXO[, 'has15'] <- TablaANEXO[, 'INST_BY_REF'] %in%
#                  AgregPRO2013[AgregPRO2013[, 'n_TODOS'] > 15, 'INST_BY_REF']

# # Tabla Anexo 2

UmbDesvEst <- 1.2

TablaANEXO2 <- TablaANEXO

TablaANEXO2[, 'GrupoReferencia'] <- gsub("\\d+__(*)",
                                   "\\1",TablaANEXO2[, 'INST_BY_REF'])

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "ADMINISTRACION Y AFINES"] <- "Administración y afines"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "ARQUITECTURA Y URBANISMO "] <- "Arquitectura y Urbanismo"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "BELLAS ARTES Y DISENNO"] <- "Bellas Artes y Diseño"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "CIENCIAS AGROPECUARIAS "] <- "Ciencias Agropecuarias"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "CIENCIAS MILITARES Y NAVALES"] <- "Ciencias Militares y Navales"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "CIENCIAS NATURALES Y EXACTAS"] <- "Ciencias Naturales y exactas"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "CIENCIAS SOCIALES"] <- "Ciencias Sociales"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
"COMUNICACION, PERIODISMO Y PUBLICIDAD"] <- "Comunicación, Periodismo y Publicidad"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "CONTADURIA Y AFINES"] <- "Contaduría y afines"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "DERECHO"] <- "Derecho"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "ECONOMIA"] <- "Economía"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "EDUCACION"] <- "Educación"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "ENFERMERIA"] <- "Enfermería"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "HUMANIDADES"] <- "Humanidades"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "INGENIERIA"] <- "Ingeniería"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "MEDICINA"] <- "Medicina"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "PSICOLOGIA"] <- "Psicología"

TablaANEXO2$GrupoReferencia[TablaANEXO2$GrupoReferencia ==
          "SALUD"] <- "Salud"

TablaANEXO2[, 'PropTotAR'] <- TablaANEXO2[,'Tamano.y']/TablaANEXO2[,'Tamano.x']

TablaANEXO2[, 'PropTotAR'] <- CambiarFormato(TablaANEXO2[, 'PropTotAR'],
                                      mult = 100, digits = 1, textPost =
                                      '\\%')

TablaANEXO2$hasVA[TablaANEXO2$hasVA == "TRUE"] <- "Sí"

TablaANEXO2$hasVA[TablaANEXO2$hasVA == "FALSE"] <- "No"

## Tabla de resumen institucional

TablaResInst <- subset(Agregados, Agregados[, 'Ano'] == 2014)

TablaResInst[, 'Ano'] <- NULL

TablaResInst[, 'RefGrp'] <- as.character(TablaResInst[, 'RefGrp'])

TablaResInst$RefGrp[TablaResInst$RefGrp == "ADMINISTRACION Y AFINES"] <-
  "Administración y afines"

TablaResInst$RefGrp[TablaResInst$RefGrp == "ARQUITECTURA Y URBANISMO "] <-
  "Arquitectura y Urbanismo"

TablaResInst$RefGrp[TablaResInst$RefGrp == "BELLAS ARTES Y DISENNO"] <-
  "Bellas Artes y Diseño"

TablaResInst$RefGrp[TablaResInst$RefGrp == "CIENCIAS AGROPECUARIAS "] <-
  "Ciencias Agropecuarias"

TablaResInst$RefGrp[TablaResInst$RefGrp == "CIENCIAS MILITARES Y NAVALES"] <-
  "Ciencias Militares y Navales"

TablaResInst$RefGrp[TablaResInst$RefGrp == "CIENCIAS NATURALES Y EXACTAS"] <-
  "Ciencias Naturales y exactas"

TablaResInst$RefGrp[TablaResInst$RefGrp == "CIENCIAS SOCIALES"] <- "Ciencias Sociales"

TablaResInst$RefGrp[TablaResInst$RefGrp ==
   "COMUNICACION, PERIODISMO Y PUBLICIDAD"] <-
     "Comunicación, Periodismo y Publicidad"

TablaResInst$RefGrp[TablaResInst$RefGrp == "CONTADURIA Y AFINES"] <- "Contaduría y afines"

TablaResInst$RefGrp[TablaResInst$RefGrp ==
          "DERECHO"] <- "Derecho"

TablaResInst$RefGrp[TablaResInst$RefGrp ==
          "ECONOMIA"] <- "Economía"

TablaResInst$RefGrp[TablaResInst$RefGrp ==
          "EDUCACION"] <- "Educación"

TablaResInst$RefGrp[TablaResInst$RefGrp ==
          "ENFERMERIA"] <- "Enfermería"

TablaResInst$RefGrp[TablaResInst$RefGrp ==
          "HUMANIDADES"] <- "Humanidades"

TablaResInst$RefGrp[TablaResInst$RefGrp ==
          "INGENIERIA"] <- "Ingeniería"

TablaResInst$RefGrp[TablaResInst$RefGrp ==
          "MEDICINA"] <- "Medicina"

TablaResInst$RefGrp[TablaResInst$RefGrp ==
          "PSICOLOGIA"] <- "Psicología"

TablaResInst$RefGrp[TablaResInst$RefGrp ==
          "SALUD"] <- "Salud"

### Tabla para comparar con GR-Nac en los reportes institucionales

AgregadosPruebas2 <- AgregadosPruebas[, c('RefGrp', 'Prueba', 'Ano',
 'DesvEstand', 'Promedio', 'Tamano')]

AgregadosPruebas2 <- subset(AgregadosPruebas2, AgregadosPruebas2[, 'Ano'] == 2013)

AgregadosPruebas2[, 'Ano'] <- NULL

TabPromediosAgregGR <- cast(AgregadosPruebas2, RefGrp ~ Prueba, value = c('Promedio'))

### Siempre verificar que estos nombres se encuentren en el orden correcto.

names(TabPromediosAgregGR) <- c('RefGrp', 'COMPETENCIAS CIUDADANASProm',
                                 'COMUNICACIÓN ESCRITAProm', 
                                 'INGLÉSProm', 'LECTURA CRÍTICAProm',
                                 'RAZONAMIENTO CUANTITATIVOProm')

TabDesvEstandAgregGR <- cast(AgregadosPruebas2, RefGrp ~ Prueba, value = c('DesvEstand'))

### Siempre verificar que estos nombres se encuentren en el orden correcto.

names(TabDesvEstandAgregGR) <- c('RefGrp', 'COMPETENCIAS CIUDADANASDesE',
                                 'COMUNICACIÓN ESCRITADesE', 
                                 'INGLÉSDesE', 'LECTURA CRÍTICADesE',
                                 'RAZONAMIENTO CUANTITATIVODesE')

TabTamanoAgregGR <- cast(AgregadosPruebas2, RefGrp ~ Prueba, value = c('Tamano'))

names(TabTamanoAgregGR) <- c('RefGrp', 'COMPETENCIAS CIUDADANAS',
'COMUNICACIÓN ESCRITA', 'INGLÉS', 'LECTURA CRÍTICA', 'RAZONAMIENTO CUANTITATIVO')

for(aa in seq(nrow(TabTamanoAgregGR))){
TabTamanoAgregGR[aa, 'n'] <- max(TabTamanoAgregGR[aa, 'COMPETENCIAS CIUDADANAS'],
							   TabTamanoAgregGR[aa, 'COMUNICACIÓN ESCRITA'],
							   TabTamanoAgregGR[aa, 'INGLÉS'],
							   TabTamanoAgregGR[aa, 'LECTURA CRÍTICA'],
							   TabTamanoAgregGR[aa, 'RAZONAMIENTO CUANTITATIVO'])								 
}

TabTamanoAgregGR <- TabTamanoAgregGR[, c('RefGrp', 'n')]

TabResInstGR <- merge(TabTamanoAgregGR, TabPromediosAgregGR, by = 'RefGrp')

TabResInstGR <- merge(TabResInstGR, TabDesvEstandAgregGR, by = 'RefGrp')
								
TabResInstGR[, 'LECTURA CRÍTICAProm'] <- CambiarFormato(TabResInstGR[, 'LECTURA CRÍTICAProm'])

TabResInstGR[, 'LECTURA CRÍTICADesE'] <- CambiarFormato(TabResInstGR[, 'LECTURA CRÍTICADesE'])

TabResInstGR[, 'RAZONAMIENTO CUANTITATIVOProm'] <- CambiarFormato(TabResInstGR[, 'RAZONAMIENTO CUANTITATIVOProm'])

TabResInstGR[, 'RAZONAMIENTO CUANTITATIVODesE'] <- CambiarFormato(TabResInstGR[, 'RAZONAMIENTO CUANTITATIVODesE'])

TabResInstGR[, 'COMPETENCIAS CIUDADANASProm'] <- CambiarFormato(TabResInstGR[, 'COMPETENCIAS CIUDADANASProm'])

TabResInstGR[, 'COMPETENCIAS CIUDADANASDesE'] <- CambiarFormato(TabResInstGR[, 'COMPETENCIAS CIUDADANASDesE'])

TabResInstGR[, 'INGLÉSProm'] <- CambiarFormato(TabResInstGR[, 'INGLÉSProm'])

TabResInstGR[, 'INGLÉSDesE'] <- CambiarFormato(TabResInstGR[, 'INGLÉSDesE'])

TabResInstGR[, 'COMUNICACIÓN ESCRITAProm'] <- CambiarFormato(TabResInstGR[, 'COMUNICACIÓN ESCRITAProm'])

TabResInstGR[, 'COMUNICACIÓN ESCRITADesE'] <- CambiarFormato(TabResInstGR[, 'COMUNICACIÓN ESCRITADesE'])

TabResInstGR[, 'RefGrp0'] <- as.character(TabResInstGR[, 'RefGrp'])

TabResInstGR[, 'RefGrp'] <- as.character(TabResInstGR[, 'RefGrp'])								
								
TabResInstGR$RefGrp[TabResInstGR$RefGrp == "ADMINISTRACION Y AFINES"] <-
  "Administración y afines"

TabResInstGR$RefGrp[TabResInstGR$RefGrp == "ARQUITECTURA Y URBANISMO "] <-
  "Arquitectura y Urbanismo"

TabResInstGR$RefGrp[TabResInstGR$RefGrp == "BELLAS ARTES Y DISENNO"] <-
  "Bellas Artes y Diseño"

TabResInstGR$RefGrp[TabResInstGR$RefGrp == "CIENCIAS AGROPECUARIAS "] <-
  "Ciencias Agropecuarias"

TabResInstGR$RefGrp[TabResInstGR$RefGrp == "CIENCIAS MILITARES Y NAVALES"] <-
  "Ciencias Militares y Navales"

TabResInstGR$RefGrp[TabResInstGR$RefGrp == "CIENCIAS NATURALES Y EXACTAS"] <-
  "Ciencias Naturales y exactas"

TabResInstGR$RefGrp[TabResInstGR$RefGrp == "CIENCIAS SOCIALES"] <- "Ciencias Sociales"

TabResInstGR$RefGrp[TabResInstGR$RefGrp ==
   "COMUNICACION, PERIODISMO Y PUBLICIDAD"] <-
     "Comunicación, Periodismo y Publicidad"

TabResInstGR$RefGrp[TabResInstGR$RefGrp == "CONTADURIA Y AFINES"] <- "Contaduría y afines"

TabResInstGR$RefGrp[TabResInstGR$RefGrp ==
          "DERECHO"] <- "Derecho"

TabResInstGR$RefGrp[TabResInstGR$RefGrp ==
          "ECONOMIA"] <- "Economía"

TabResInstGR$RefGrp[TabResInstGR$RefGrp ==
          "EDUCACION"] <- "Educación"

TabResInstGR$RefGrp[TabResInstGR$RefGrp ==
          "ENFERMERIA"] <- "Enfermería"

TabResInstGR$RefGrp[TabResInstGR$RefGrp ==
          "HUMANIDADES"] <- "Humanidades"

TabResInstGR$RefGrp[TabResInstGR$RefGrp ==
          "INGENIERIA"] <- "Ingeniería"

TabResInstGR$RefGrp[TabResInstGR$RefGrp ==
          "MEDICINA"] <- "Medicina"

TabResInstGR$RefGrp[TabResInstGR$RefGrp ==
          "PSICOLOGIA"] <- "Psicología"

TabResInstGR$RefGrp[TabResInstGR$RefGrp ==
          "SALUD"] <- "Salud"
								
TabResInstGR[, 'NomRefGrp'] <- paste(TabResInstGR[, 'RefGrp'], '-Nac.', sep ="")

TabResInstGR <- TabResInstGR[, c('RefGrp0', 'RefGrp', 'NomRefGrp', 'n', 'LECTURA CRÍTICAProm', 'LECTURA CRÍTICADesE',
								'RAZONAMIENTO CUANTITATIVOProm',
								'RAZONAMIENTO CUANTITATIVODesE',
								'COMPETENCIAS CIUDADANASProm',
								'COMPETENCIAS CIUDADANASDesE',
								'INGLÉSProm', 'INGLÉSDesE',
								'COMUNICACIÓN ESCRITAProm',
								'COMUNICACIÓN ESCRITADesE')]
							   
### Tabla de promedios y desviaciones estándar

TabPromedios <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba,
                     value = 'Promedio')

names(TabPromedios) <- c('INST_BY_REF', 'Instit', 'RefGrp',
                         'COMPETENCIAS CIUDADANASProm',
                         'COMUNICACIÓN ESCRITAProm', 'INGLÉSProm',
                         'LECTURA CRÍTICAProm', 'RAZONAMIENTO CUANTITATIVOProm')

TabDesvEstand <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba,
                      value = 'DesvEstand')

names(TabDesvEstand) <- c('INST_BY_REFDesE', 'InstitDesE', 'RefGrpDesE',
                          'COMPETENCIAS CIUDADANASDesE',
                         'COMUNICACIÓN ESCRITADesE', 'INGLÉSDesE',
                         'LECTURA CRÍTICADesE', 'RAZONAMIENTO CUANTITATIVODesE')

TablaResInst1 <- cbind(TabPromedios, TabDesvEstand)

Totales14 <- ddply(AgregadosTablaProgr2014, .(INST_BY_REF, PRAC_ID),
summarize, n = Tamano[which.max(Tamano)])

Totales14 <- ddply(Totales14, .(INST_BY_REF), summarize, n = sum(n))

TablaResInst1 <- merge(TablaResInst1, Totales14, by = 'INST_BY_REF')

TablaResInst1 <- TablaResInst1[, c('Instit', 'RefGrp','n',
                                   'LECTURA CRÍTICAProm',
                                   'LECTURA CRÍTICADesE',
                                   'RAZONAMIENTO CUANTITATIVOProm',
                                   'RAZONAMIENTO CUANTITATIVODesE',
                                   'COMPETENCIAS CIUDADANASProm',
                                   'COMPETENCIAS CIUDADANASDesE',
                                   'INGLÉSProm', 'INGLÉSDesE',
                                   'COMUNICACIÓN ESCRITAProm',
                                   'COMUNICACIÓN ESCRITADesE' )]
								 
TablaResInst1[, 'LECTURA CRÍTICAProm'] <- CambiarFormato(TablaResInst1[, 'LECTURA CRÍTICAProm'])

TablaResInst1[, 'LECTURA CRÍTICADesE'] <- CambiarFormato(TablaResInst1[, 'LECTURA CRÍTICADesE'])

TablaResInst1[, 'RAZONAMIENTO CUANTITATIVOProm'] <- CambiarFormato(TablaResInst1[, 'RAZONAMIENTO CUANTITATIVOProm'])

TablaResInst1[, 'RAZONAMIENTO CUANTITATIVODesE'] <- CambiarFormato(TablaResInst1[, 'RAZONAMIENTO CUANTITATIVODesE'])

TablaResInst1[, 'COMPETENCIAS CIUDADANASProm'] <- CambiarFormato(TablaResInst1[, 'COMPETENCIAS CIUDADANASProm'])

TablaResInst1[, 'COMPETENCIAS CIUDADANASDesE'] <- CambiarFormato(TablaResInst1[, 'COMPETENCIAS CIUDADANASDesE'])

TablaResInst1[, 'INGLÉSProm'] <- CambiarFormato(TablaResInst1[, 'INGLÉSProm'])

TablaResInst1[, 'INGLÉSDesE'] <- CambiarFormato(TablaResInst1[, 'INGLÉSDesE'])

TablaResInst1[, 'COMUNICACIÓN ESCRITAProm'] <- CambiarFormato(TablaResInst1[, 'COMUNICACIÓN ESCRITAProm'])

TablaResInst1[, 'COMUNICACIÓN ESCRITADesE'] <- CambiarFormato(TablaResInst1[, 'COMUNICACIÓN ESCRITADesE'])

### Tabla de niveles de desempeño

TabN1 <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba, value =
                     'N1')

names(TabN1) <- paste(names(TabN1), 'N1', sep = '')

TabN2 <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba, value =
                     'N2')

names(TabN2) <- paste(names(TabN2), 'N2', sep = '')

TabN3 <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba, value =
                     'N3')

names(TabN3) <- paste(names(TabN3), 'N3', sep = '')

TabN4 <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba, value =
                     'N4')

names(TabN4) <- paste(names(TabN4), 'N4', sep = '')

TabN5 <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba, value =
                     'N5')

names(TabN5) <- paste(names(TabN5), 'N5', sep = '')

TabN6 <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba, value =
                     'N6')

names(TabN6) <- paste(names(TabN6), 'N6', sep = '')

TabN7 <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba, value =
                     'N7')

names(TabN7) <- paste(names(TabN7), 'N7', sep = '')

TabN8 <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba, value =
                     'N8')

names(TabN8) <- paste(names(TabN8), 'N8', sep = '')

TabA0 <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba, value =
                     'A-')

names(TabA0) <- paste(names(TabA0), 'A-', sep = '')

TabA1 <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba, value =
                     'A1')

names(TabA1) <- paste(names(TabA1), 'A1', sep = '')

TabA2 <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba, value =
                     'A2')

names(TabA2) <- paste(names(TabA2), 'A2', sep = '')

TabB1 <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba, value =
                     'B1')

names(TabB1) <- paste(names(TabB1), 'B1', sep = '')

TabB2 <- cast(TablaResInst, INST_BY_REF + Instit + RefGrp ~ Prueba, value =
                     'B+')

names(TabB2) <- paste(names(TabB2), 'B+', sep = '')

TablaResInst2 <- cbind(TabN1, TabN2, TabN3, TabN4, TabN5, TabN6, TabN7,
                       TabN8, TabA0, TabA1, TabA2, TabB1, TabB2)

TablaResInst2[, 'sumLC'] <-
  rowSums(TablaResInst2[, c('LECTURA CRÍTICAN1',
                            'LECTURA CRÍTICAN2',
                            'LECTURA CRÍTICAN3')])

TablaResInst2[, 'sumRC'] <-
  rowSums(TablaResInst2[, c('RAZONAMIENTO CUANTITATIVON1',
                            'RAZONAMIENTO CUANTITATIVON2',
                            'RAZONAMIENTO CUANTITATIVON3')])

TablaResInst2[, 'sumI'] <-
  rowSums(TablaResInst2[, c('INGLÉSA-', 'INGLÉSA1', 'INGLÉSA2',
                            'INGLÉSB1', 'INGLÉSB+')])

TablaResInst2[, 'sumCE'] <-
  rowSums(TablaResInst2[, c('COMUNICACIÓN ESCRITAN1',
                            'COMUNICACIÓN ESCRITAN2',
                            'COMUNICACIÓN ESCRITAN3',
                            'COMUNICACIÓN ESCRITAN4',
                            'COMUNICACIÓN ESCRITAN5',
                            'COMUNICACIÓN ESCRITAN6',
                            'COMUNICACIÓN ESCRITAN7',
                            'COMUNICACIÓN ESCRITAN8')])

TablaResInst2[, 'LECTURA CRÍTICAN1'] <- TablaResInst2[, 'LECTURA CRÍTICAN1']/
                                        TablaResInst2[, 'sumLC']

TablaResInst2[, 'LECTURA CRÍTICAN2'] <- TablaResInst2[, 'LECTURA CRÍTICAN2']/
                                        TablaResInst2[, 'sumLC']

TablaResInst2[, 'LECTURA CRÍTICAN3'] <- TablaResInst2[, 'LECTURA CRÍTICAN3']/
                                        TablaResInst2[, 'sumLC']

TablaResInst2[, 'RAZONAMIENTO CUANTITATIVON1'] <-
  TablaResInst2[, 'RAZONAMIENTO CUANTITATIVON1']/ TablaResInst2[, 'sumRC']

TablaResInst2[, 'RAZONAMIENTO CUANTITATIVON2'] <-
  TablaResInst2[, 'RAZONAMIENTO CUANTITATIVON2']/ TablaResInst2[, 'sumRC']

TablaResInst2[, 'RAZONAMIENTO CUANTITATIVON3'] <-
  TablaResInst2[, 'RAZONAMIENTO CUANTITATIVON3']/ TablaResInst2[, 'sumRC']

TablaResInst2[, 'INGLÉSA-'] <- TablaResInst2[, 'INGLÉSA-']/
                                        TablaResInst2[, 'sumI']

TablaResInst2[, 'INGLÉSA1'] <- TablaResInst2[, 'INGLÉSA1']/
                                        TablaResInst2[, 'sumI']

TablaResInst2[, 'INGLÉSA2'] <- TablaResInst2[, 'INGLÉSA2']/
                                        TablaResInst2[, 'sumI']

TablaResInst2[, 'INGLÉSB1'] <- TablaResInst2[, 'INGLÉSB1']/
                                        TablaResInst2[, 'sumI']

TablaResInst2[, 'INGLÉSB+'] <- TablaResInst2[, 'INGLÉSB+']/
                                        TablaResInst2[, 'sumI']
TablaResInst2[, 'COMUNICACIÓN ESCRITAN1'] <-
  TablaResInst2[, 'COMUNICACIÓN ESCRITAN1']/ TablaResInst2[, 'sumCE']

TablaResInst2[, 'COMUNICACIÓN ESCRITAN2'] <-
  TablaResInst2[, 'COMUNICACIÓN ESCRITAN2']/ TablaResInst2[, 'sumCE']

TablaResInst2[, 'COMUNICACIÓN ESCRITAN3'] <-
  TablaResInst2[, 'COMUNICACIÓN ESCRITAN3']/ TablaResInst2[, 'sumCE']

TablaResInst2[, 'COMUNICACIÓN ESCRITAN4'] <-
  TablaResInst2[, 'COMUNICACIÓN ESCRITAN4']/ TablaResInst2[, 'sumCE']

TablaResInst2[, 'COMUNICACIÓN ESCRITAN5'] <-
  TablaResInst2[, 'COMUNICACIÓN ESCRITAN5']/ TablaResInst2[, 'sumCE']

TablaResInst2[, 'COMUNICACIÓN ESCRITAN6'] <-
  TablaResInst2[, 'COMUNICACIÓN ESCRITAN6']/ TablaResInst2[, 'sumCE']

TablaResInst2[, 'COMUNICACIÓN ESCRITAN7'] <-
  TablaResInst2[, 'COMUNICACIÓN ESCRITAN7']/ TablaResInst2[, 'sumCE']

TablaResInst2[, 'COMUNICACIÓN ESCRITAN8'] <-
  TablaResInst2[, 'COMUNICACIÓN ESCRITAN8']/ TablaResInst2[, 'sumCE']

TablaResInst2[, 'LECTURA CRÍTICAN1'] <-
  CambiarFormato(TablaResInst2[, 'LECTURA CRÍTICAN1'],                                      mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'LECTURA CRÍTICAN2'] <-
  CambiarFormato(TablaResInst2[, 'LECTURA CRÍTICAN2'],                                      mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'LECTURA CRÍTICAN3'] <-
  CambiarFormato(TablaResInst2[, 'LECTURA CRÍTICAN3'],                                      mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'RAZONAMIENTO CUANTITATIVON1'] <-
  CambiarFormato(TablaResInst2[, 'RAZONAMIENTO CUANTITATIVON1'],                                      mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'RAZONAMIENTO CUANTITATIVON2'] <-
  CambiarFormato(TablaResInst2[, 'RAZONAMIENTO CUANTITATIVON2'],                                      mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'RAZONAMIENTO CUANTITATIVON3'] <-
  CambiarFormato(TablaResInst2[, 'RAZONAMIENTO CUANTITATIVON3'],                                      mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'INGLÉSA-'] <-
  CambiarFormato(TablaResInst2[, 'INGLÉSA-'],
                 mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'INGLÉSA1'] <-
  CambiarFormato(TablaResInst2[, 'INGLÉSA1'],
                 mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'INGLÉSA2'] <-
  CambiarFormato(TablaResInst2[, 'INGLÉSA2'],
                 mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'INGLÉSB1'] <-
  CambiarFormato(TablaResInst2[, 'INGLÉSB1'],
                 mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'INGLÉSB+'] <-
  CambiarFormato(TablaResInst2[, 'INGLÉSB+'],
                 mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'COMUNICACIÓN ESCRITAN1'] <-
  CambiarFormato(TablaResInst2[, 'COMUNICACIÓN ESCRITAN1'],                                      mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'COMUNICACIÓN ESCRITAN2'] <-
  CambiarFormato(TablaResInst2[, 'COMUNICACIÓN ESCRITAN2'],                                      mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'COMUNICACIÓN ESCRITAN3'] <-
  CambiarFormato(TablaResInst2[, 'COMUNICACIÓN ESCRITAN3'],                                      mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'COMUNICACIÓN ESCRITAN4'] <-
  CambiarFormato(TablaResInst2[, 'COMUNICACIÓN ESCRITAN4'],                                      mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'COMUNICACIÓN ESCRITAN5'] <-
  CambiarFormato(TablaResInst2[, 'COMUNICACIÓN ESCRITAN5'],                                      mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'COMUNICACIÓN ESCRITAN6'] <-
  CambiarFormato(TablaResInst2[, 'COMUNICACIÓN ESCRITAN6'],                                      mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'COMUNICACIÓN ESCRITAN7'] <-
  CambiarFormato(TablaResInst2[, 'COMUNICACIÓN ESCRITAN7'],                                      mult = 100, digits = 2, textPost = '\\%')

TablaResInst2[, 'COMUNICACIÓN ESCRITAN8'] <-
  CambiarFormato(TablaResInst2[, 'COMUNICACIÓN ESCRITAN8'],                                      mult = 100, digits = 2, textPost = '\\%')

TablaResInst2 <- merge(TablaResInst2, Totales14, by.x = 'INST_BY_REFN1',
                       by.y = 'INST_BY_REF')

TablaResInst2 <- TablaResInst2[, c('InstitN1', 'RefGrpN1', 'n',
                                 'LECTURA CRÍTICAN1',
                                 'LECTURA CRÍTICAN2', 'LECTURA CRÍTICAN3',
                                 'RAZONAMIENTO CUANTITATIVON1',
                                 'RAZONAMIENTO CUANTITATIVON2',
                                 'RAZONAMIENTO CUANTITATIVON3',
                                 'INGLÉSA-', 'INGLÉSA1', 'INGLÉSA2',
                                 'INGLÉSB1', 'INGLÉSB+',
                                 'COMUNICACIÓN ESCRITAN1',
                                 'COMUNICACIÓN ESCRITAN2',
                                 'COMUNICACIÓN ESCRITAN3',
                                 'COMUNICACIÓN ESCRITAN4',
                                 'COMUNICACIÓN ESCRITAN5',
                                 'COMUNICACIÓN ESCRITAN6',
                                 'COMUNICACIÓN ESCRITAN7',
                                 'COMUNICACIÓN ESCRITAN8')]
								 
################################################################################
# # Definición de la parte iterativa
################################################################################

setwd("C:/ecuellar/2015/ek/Aporte Relativo/src")

seguimiento <- NULL

filesAux <- c('ResPRO2013_2014v02.rnw', 'AR2013_2014v01.rnw',
              'OCalidad2013_2014v01.rnw', 'anexoAR2013_2014v01.rnw',
              'anexoCompAR2013_2014v01.rnw',
              'anexoDetalles2013_2014v01.rnw', 
			  'anexoGrupRef2013_2014v01.rnw')

nombres <- CambCaractRaros(data.frame(nombre = names(ref.grps)), 'nombre')
names(ref.grps) <- nombres

dat[, 'ESTU_GRUPO_REFERENCIA'] <- CambCaractRaros(dat, 'ESTU_GRUPO_REFERENCIA')
dat[, 'inst_by_ref'] <- CambCaractRaros(dat, 'inst_by_ref')

rnwBase <- file.path("Sweave", "InformesIES2013_2014v02.rnw")

# refGr = names(ref.grps)[18]
# refGr = names(ref.grps)[18]
for (ij in seq(1,length(names(ref.grps)))){
 refGr <- names(ref.grps)[ij]
# refGr = names(ref.grps)[17]
# refGr = names(ref.grps)[18]
  dattmp <- subset(dat, dat[, 'ESTU_GRUPO_REFERENCIA'] == refGr)
  Instituciones <- unique(dattmp[, 'INST_COD_INSTITUCION'])

  runPath <- file.path(docPath, str_replace_all(refGr, "[ ,Y]", ""))

  if (!file.exists(runPath)) {
    dir.create(runPath)
    file.copy("Sweave.sty",
              file.path(runPath, "Sweave.sty"))
    file.copy("icfesdocapa.cls",
              file.path(runPath, "icfesdocapa.cls"))
 }
# Inst = Instituciones[2]
# Inst = Instituciones[5]
 for (Instit in seq (1,length(Instituciones))){
        Inst <- Instituciones[Instit]
# for (Instit in seq (76,length(Instituciones))){
# Inst <- Instituciones[Instit]
    archivos <- list.files(runPath)
    noBorrar <- grep("pdf$|cls$|tex$|sty$", archivos)
    archivos <- archivos[-noBorrar]
    lapply(file.path(runPath, archivos), file.remove)

    # # generar copia del rnw renombrando
    outFile     <- paste("Inst", Inst, "_", str_replace_all(refGr, "[ ,Y]", ""), sep = "")
    outFileRnw  <- file.path(runPath,
                              paste(outFile, ".Rnw", sep = ""))
    outFileTexE <- paste(outFile, ".tex", sep = "")
# #     outFileTexS <- file.path(runPath,
# #                              paste(outFile, ".tex", sep = ""))

  if (file.exists(outFileRnw)) {
    file.remove(outFileRnw)
    lapply(file.path(runPath, filesAux),
           file.remove)
  }
  file.copy(rnwBase, outFileRnw)
  lapply(file.path("Sweave", filesAux),
         file.copy, to = runPath)

# #   file.copy("entidades080910.Rnw", outFileRnw)
 auxPath <- getwd()
 auxPath2 <- runPath
 setwd(runPath)
 runPath <- "."
  Sweave(paste(outFile, ".Rnw", sep = ""), encoding = "latin1")

 runlatex   <- paste("latex --interaction=nonstopmode ", outFileTexE,
                     "\n", sep="")

 batFile <- file.path(runPath, "correr13_14.bat")
 cat(runlatex, runlatex,
     "dvips -tletter -Ppdf -o ", outFile, ".ps ", outFile,
     '.dvi', "\nps2pdf ", outFile, ".ps\n", sep = "",
     file = batFile)

 system("correr13_14.bat")

 estanAr <- list.files(pattern = outFile)
 sonAr <- grep("[dvi|ps]$", estanAr)
 seguimiento <- rbind(seguimiento,
                     data.frame(INST_BY_REF = InstGR,
                                flagVA = flagVA,
								flagVec = flagVec,
                                nCom = nComparables,
                                generado = length(sonAr) == 2,
                                flag2 = flag2))

 setwd(auxPath)
 runPath <- auxPath2
   }
}


setwd("C:/ecuellar/2015/ek/Aporte Relativo/src")

outPath      <- "../output/"

fileName    <- "seguimiento.txt"
outFile      <- paste(outPath, fileName, sep = "")
write.table (seguimiento, outFile, row.names =TRUE,
             sep="|",fileEncoding = "latin1", dec=".")

fileName    <- "TablaANEXO13_14.txt"
outFile      <- paste(outPath, fileName, sep = "")
write.table (TablaANEXO, outFile, row.names =TRUE,
             sep="|",fileEncoding = "latin1", dec=".")


################################################################################
# # RESUMENES EJECUTIVOS
################################################################################

setwd("C:/ecuellar/2015/ek/Aporte Relativo/src")

outPath      <- "../output/"
inPath       <- "../input/"
srcPath      <- "../src/"
docPath      <- "../doc"

seguimiento <- NULL

filesAux <- c('REResPRO2013_2014v02.rnw', 'REAR2013_2014v01.rnw',
              'REOCalidad2013_2014v01.rnw')

nombres <- CambCaractRaros(data.frame(nombre = names(ref.grps)), 'nombre')
names(ref.grps) <- nombres

dat[, 'ESTU_GRUPO_REFERENCIA'] <- CambCaractRaros(dat, 'ESTU_GRUPO_REFERENCIA')
dat[, 'inst_by_ref'] <- CambCaractRaros(dat, 'inst_by_ref')

rnwBase <- file.path("Sweave", "ResuEjecIES2013_2014v02.rnw")

# refGr = names(ref.grps)[1]
for (ij in seq(1,length(names(ref.grps)))){
 refGr <- names(ref.grps)[ij]
# refGr = names(ref.grps)[17]
#refGr = names(ref.grps)[18]
  dattmp <- subset(dat, dat[, 'ESTU_GRUPO_REFERENCIA'] == refGr)
  Instituciones <- unique(dattmp[, 'INST_COD_INSTITUCION'])

  runPath <- file.path(docPath, str_replace_all(refGr, "[ ,Y]", ""))

  if (!file.exists(runPath)) {
    dir.create(runPath)
    file.copy("Sweave.sty",
              file.path(runPath, "Sweave.sty"))
    file.copy("icfesdocapa.cls",
              file.path(runPath, "icfesdocapa.cls"))
 }
# Inst = Instituciones[2]
# Inst = Instituciones[5]
# for (Instit in seq (1,length(Instituciones))){
#        Inst <- Instituciones[Instit]
 for (Instit in seq (1,length(Instituciones))){
        Inst <- Instituciones[Instit]
# for (Instit in seq (76,length(Instituciones))){
#        Inst <- Instituciones[Instit]
    archivos <- list.files(runPath)
    noBorrar <- grep("pdf$|cls$|tex$|sty$", archivos)
    archivos <- archivos[-noBorrar]
    lapply(file.path(runPath, archivos), file.remove)

    # # generar copia del rnw renombrando
    outFile     <- paste("RE", "Inst", Inst, "_", str_replace_all(refGr, "[ ,Y]", ""), sep = "")
    outFileRnw  <- file.path(runPath,
                              paste(outFile, ".Rnw", sep = ""))
    outFileTexE <- paste(outFile, ".tex", sep = "")
# #     outFileTexS <- file.path(runPath,
# #                              paste(outFile, ".tex", sep = ""))

  if (file.exists(outFileRnw)) {
    file.remove(outFileRnw)
    lapply(file.path(runPath, filesAux),
           file.remove)
  }
  file.copy(rnwBase, outFileRnw)
  lapply(file.path("Sweave", filesAux),
         file.copy, to = runPath)


# #   file.copy("entidades080910.Rnw", outFileRnw)
 auxPath <- getwd()
 auxPath2 <- runPath
 setwd(runPath)
 runPath <- "."
  Sweave(paste(outFile, ".Rnw", sep = ""), encoding = "latin1")

 runlatex   <- paste("latex --interaction=nonstopmode ", outFileTexE,
                     "\n", sep="")

 batFile <- file.path(runPath, "correr13_14.bat")
 cat(runlatex, runlatex,
     "dvips -tletter -Ppdf -o ", outFile, ".ps ", outFile,
     '.dvi', "\nps2pdf ", outFile, ".ps\n", sep = "",
     file = batFile)

 system("correr13_14.bat")

 estanAr <- list.files(pattern = outFile)
 sonAr <- grep("[dvi|ps]$", estanAr)
 seguimiento <- rbind(seguimiento,
                     data.frame(INST_BY_REF = InstGR,
                                flagVA = flagVA,
                                flagVec = flagVec,
                                nCom = nComparables,
                                generado = length(sonAr) == 2,
                                flag2 = flag2))

 setwd(auxPath)
 runPath <- auxPath2

   }
}

setwd("C:/ecuellar/2015/ek/Aporte Relativo/src")

outPath      <- "../output/"

fileName    <- "seguimiento.txt"
outFile      <- paste(outPath, fileName, sep = "")
write.table (seguimiento, outFile, row.names =TRUE,
             sep="|",fileEncoding = "latin1", dec=".")

################################################################################
# # RESUMENES POR INSTITUCIÓN
################################################################################

setwd("C:/ecuellar/2015/ek/Aporte Relativo/src")

seguimiento <- NULL

filesAux <- c('TablaReportesInst2013_2014.rnw')

rnwBase <- file.path("Sweave", "ReportesInstit2013_2014.rnw")

Instituciones <- unique(dat[, 'INST_COD_INSTITUCION'])

  runPath <- file.path(docPath, 'ReportesInstitucionales')

  if (!file.exists(runPath)) {
    dir.create(runPath)
    file.copy("Sweave.sty",
              file.path(runPath, "Sweave.sty"))
    file.copy("icfesdocapa.cls",
              file.path(runPath, "icfesdocapa.cls"))
 }
 # Quitar la 240 (Inst1222)
 for (Instit in seq (1,length(Instituciones))){
       Inst <- Instituciones[Instit]
# Inst <- 1805
    archivos <- list.files(runPath)
    noBorrar <- grep("pdf$|cls$|tex$|sty$", archivos)
    archivos <- archivos[-noBorrar]
    lapply(file.path(runPath, archivos), file.remove)

    # # generar copia del rnw renombrando
    outFile     <- paste("RepInst", Inst, sep = "")
    outFileRnw  <- file.path(runPath,
                              paste(outFile, ".Rnw", sep = ""))
    outFileTexE <- paste(outFile, ".tex", sep = "")
# #     outFileTexS <- file.path(runPath,
# #                              paste(outFile, ".tex", sep = ""))

  if (file.exists(outFileRnw)) {
    file.remove(outFileRnw)
    lapply(file.path(runPath, filesAux),
           file.remove)
  }
  file.copy(rnwBase, outFileRnw)
  lapply(file.path("Sweave", filesAux),
         file.copy, to = runPath)


# #   file.copy("entidades080910.Rnw", outFileRnw)
 auxPath <- getwd()
 auxPath2 <- runPath
 setwd(runPath)
 runPath <- "."
  Sweave(paste(outFile, ".Rnw", sep = ""), encoding = "latin1")

 runlatex   <- paste("latex --interaction=nonstopmode ", outFileTexE,
                     "\n", sep="")

 batFile <- file.path(runPath, "correr13_14.bat")
 cat(runlatex, runlatex,
     "dvips -tletter -Ppdf -o ", outFile, ".ps ", outFile,
     '.dvi', "\nps2pdf ", outFile, ".ps\n", sep = "",
     file = batFile)
	 
 system("correr13_14.bat")

 estanAr <- list.files(pattern = outFile)
 sonAr <- grep("[dvi|ps]$", estanAr)
 
 setwd(auxPath)
 runPath <- auxPath2

}

################################################################################
# # Reportes para IGR que no tienen ningun estudiantes en AR
################################################################################

setwd("C:/ecuellar/2015/ek/Aporte Relativo/src")

seguimiento <- NULL

filesAux <- c('ResPRO2013_2014v03.rnw', 'AR2013_2014v01.rnw',
              'OCalidad2013_2014v02.rnw', 'anexoAR2013_2014v01.rnw',
              'anexoCompAR2013_2014v01.rnw',
              'anexoDetalles2013_2014v01.rnw', 
			  'anexoGrupRef2013_2014v01.rnw')
			  
for(InstGR in c('1218__BELLAS ARTES Y DISENNO', '1219__CIENCIAS NATURALES Y EXACTAS',
'1220__DERECHO', '1221__CIENCIAS SOCIALES', '1221__EDUCACION', '1222__DERECHO',
'1222__PSICOLOGIA', '1704__HUMANIDADES', '1718__SALUD', '1812__ECONOMIA',
'1823__HUMANIDADES', '1831__HUMANIDADES', '2102__CIENCIAS NATURALES Y EXACTAS',
'2102__HUMANIDADES', '2715__ECONOMIA', '2725__CIENCIAS NATURALES Y EXACTAS',
'2725__CIENCIAS SOCIALES', '2727__INGENIERIA', '2733__ADMINISTRACION Y AFINES',
'2733__HUMANIDADES', '2833__COMUNICACION, PERIODISMO Y PUBLICIDAD',
'2901__CIENCIAS MILITARES Y NAVALES', '3703__INGENIERIA', '9105__ADMINISTRACION Y AFINES',
'9105__CIENCIAS NATURALES Y EXACTAS', '9116__EDUCACION', '9122__SALUD',
'9131__HUMANIDADES')){

refGr <- gsub("\\d+__(*)", "\\1",InstGR)

Inst <-  gsub("(\\d+)__.*", "\\1",InstGR)
			 			  
#nombres <- CambCaractRaros(data.frame(nombre = names(ref.grps)), 'nombre')
#names(ref.grps) <- nombres

#dat[, 'ESTU_GRUPO_REFERENCIA'] <- CambCaractRaros(dat, 'ESTU_GRUPO_REFERENCIA')
#dat[, 'inst_by_ref'] <- CambCaractRaros(dat, 'inst_by_ref')

rnwBase <- file.path("Sweave", "InformesIES2013_2014v03.rnw")

# refGr = names(ref.grps)[18]
# refGr = names(ref.grps)[18]
#for (ij in seq(1,length(names(ref.grps)))){
# refGr <- names(ref.grps)[ij]
# refGr = names(ref.grps)[17]
# refGr = names(ref.grps)[18]
#  dattmp <- subset(dat, dat[, 'ESTU_GRUPO_REFERENCIA'] == refGr)
#  Instituciones <- unique(dattmp[, 'INST_COD_INSTITUCION'])

  runPath <- file.path(docPath, str_replace_all(refGr, "[ ,Y]", ""))

  if (!file.exists(runPath)) {
    dir.create(runPath)
    file.copy("Sweave.sty",
              file.path(runPath, "Sweave.sty"))
    file.copy("icfesdocapa.cls",
              file.path(runPath, "icfesdocapa.cls"))
 }
# Inst = Instituciones[2]
# Inst = Instituciones[5]
# for (Instit in seq (1,length(Instituciones))){
#        Inst <- Instituciones[Instit]
# for (Instit in seq (76,length(Instituciones))){
# Inst <- Instituciones[Instit]
    archivos <- list.files(runPath)
    noBorrar <- grep("pdf$|cls$|tex$|sty$", archivos)
    archivos <- archivos[-noBorrar]
    lapply(file.path(runPath, archivos), file.remove)

    # # generar copia del rnw renombrando
    outFile     <- paste("Inst", Inst, "_", str_replace_all(refGr, "[ ,Y]", ""), sep = "")
    outFileRnw  <- file.path(runPath,
                              paste(outFile, ".Rnw", sep = ""))
    outFileTexE <- paste(outFile, ".tex", sep = "")
# #     outFileTexS <- file.path(runPath,
# #                              paste(outFile, ".tex", sep = ""))

  if (file.exists(outFileRnw)) {
    file.remove(outFileRnw)
    lapply(file.path(runPath, filesAux),
           file.remove)
  }
  file.copy(rnwBase, outFileRnw)
  lapply(file.path("Sweave", filesAux),
         file.copy, to = runPath)


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
 seguimiento <- rbind(seguimiento,
                     data.frame(INST_BY_REF = InstGR,
                                flagVA = flagVA,
								flagVec = flagVec,
                                nCom = nComparables,
                                generado = length(sonAr) == 2,
                                flag2 = flag2))

 setwd(auxPath)
 runPath <- auxPath2
   }

################################################################################
# # Resumenes para IGR que no tienen ningun estudiantes en AR
################################################################################

setwd("C:/ecuellar/2015/ek/Aporte Relativo/src")

seguimiento <- NULL

filesAux <- c('REResPRO2013_2014v03.rnw', 'REAR2013_2014v01.rnw',
              'REOCalidad2013_2014v02.rnw')

for(InstGR in c('1218__BELLAS ARTES Y DISENNO', '1219__CIENCIAS NATURALES Y EXACTAS',
'1220__DERECHO', '1221__CIENCIAS SOCIALES', '1221__EDUCACION', '1222__DERECHO',
'1222__PSICOLOGIA', '1704__HUMANIDADES', '1718__SALUD', '1812__ECONOMIA',
'1823__HUMANIDADES', '1831__HUMANIDADES', '2102__CIENCIAS NATURALES Y EXACTAS',
'2102__HUMANIDADES', '2715__ECONOMIA', '2725__CIENCIAS NATURALES Y EXACTAS',
'2725__CIENCIAS SOCIALES', '2727__INGENIERIA', '2733__ADMINISTRACION Y AFINES',
'2733__HUMANIDADES', '2833__COMUNICACION, PERIODISMO Y PUBLICIDAD',
'2901__CIENCIAS MILITARES Y NAVALES', '3703__INGENIERIA', '9105__ADMINISTRACION Y AFINES',
'9105__CIENCIAS NATURALES Y EXACTAS', '9116__EDUCACION', '9122__SALUD',
'9131__HUMANIDADES')){

refGr <- gsub("\\d+__(*)", "\\1",InstGR)

Inst <-  gsub("(\\d+)__.*", "\\1",InstGR)
			 			  
#nombres <- CambCaractRaros(data.frame(nombre = names(ref.grps)), 'nombre')
#names(ref.grps) <- nombres

#dat[, 'ESTU_GRUPO_REFERENCIA'] <- CambCaractRaros(dat, 'ESTU_GRUPO_REFERENCIA')
#dat[, 'inst_by_ref'] <- CambCaractRaros(dat, 'inst_by_ref')

rnwBase <- file.path("Sweave", "ResuEjecIES2013_2014v03.rnw")

# refGr = names(ref.grps)[18]
# refGr = names(ref.grps)[18]
#for (ij in seq(1,length(names(ref.grps)))){
# refGr <- names(ref.grps)[ij]
# refGr = names(ref.grps)[17]
# refGr = names(ref.grps)[18]
#  dattmp <- subset(dat, dat[, 'ESTU_GRUPO_REFERENCIA'] == refGr)
#  Instituciones <- unique(dattmp[, 'INST_COD_INSTITUCION'])

  runPath <- file.path(docPath, str_replace_all(refGr, "[ ,Y]", ""))

  if (!file.exists(runPath)) {
    dir.create(runPath)
    file.copy("Sweave.sty",
              file.path(runPath, "Sweave.sty"))
    file.copy("icfesdocapa.cls",
              file.path(runPath, "icfesdocapa.cls"))
 }
# Inst = Instituciones[2]
# Inst = Instituciones[5]
# for (Instit in seq (1,length(Instituciones))){
#        Inst <- Instituciones[Instit]
# for (Instit in seq (76,length(Instituciones))){
# Inst <- Instituciones[Instit]
    archivos <- list.files(runPath)
    noBorrar <- grep("pdf$|cls$|tex$|sty$", archivos)
    archivos <- archivos[-noBorrar]
    lapply(file.path(runPath, archivos), file.remove)

    # # generar copia del rnw renombrando
    outFile     <- paste("RE", "Inst", Inst, "_", str_replace_all(refGr, "[ ,Y]", ""), sep = "")
    outFileRnw  <- file.path(runPath,
                              paste(outFile, ".Rnw", sep = ""))
    outFileTexE <- paste(outFile, ".tex", sep = "")
# #     outFileTexS <- file.path(runPath,
# #                              paste(outFile, ".tex", sep = ""))

  if (file.exists(outFileRnw)) {
    file.remove(outFileRnw)
    lapply(file.path(runPath, filesAux),
           file.remove)
  }
  file.copy(rnwBase, outFileRnw)
  lapply(file.path("Sweave", filesAux),
         file.copy, to = runPath)


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
 seguimiento <- rbind(seguimiento,
                     data.frame(INST_BY_REF = InstGR,
                                flagVA = flagVA,
								flagVec = flagVec,
                                nCom = nComparables,
                                generado = length(sonAr) == 2,
                                flag2 = flag2))

 setwd(auxPath)
 runPath <- auxPath2
   }

################################################################################
# # RESUMENES POR INSTITUCIÓN para IGR que no tienen ningun estudiantes en AR
################################################################################

setwd("C:/ecuellar/2015/ek/Aporte Relativo/src")

seguimiento <- NULL

filesAux <- c('TablaReportesInst2013_2014.rnw')

rnwBase <- file.path("Sweave", "ReportesInstit2013_2014.rnw")

# Instituciones <- unique(dat[, 'INST_COD_INSTITUCION'])

  runPath <- file.path(docPath, 'ReportesInstitucionales')

  if (!file.exists(runPath)) {
    dir.create(runPath)
    file.copy("Sweave.sty",
              file.path(runPath, "Sweave.sty"))
    file.copy("icfesdocapa.cls",
              file.path(runPath, "icfesdocapa.cls"))
 }
 # Quitar la 240 (Inst1222)
# for (Instit in seq (241,length(Instituciones))){
 #      Inst <- Instituciones[Instit]
# Inst <- 1805
for(Inst in c('1218', '1219', '1220', '1221', '1221', '1704',
'1718', '1812', '1823', '1831', '2102', '2715', '2725', '2727', '2733',
'2833', '2901', '3703', '9105', '9116', '9122', '9131')){

    archivos <- list.files(runPath)
    noBorrar <- grep("pdf$|cls$|tex$|sty$", archivos)
    archivos <- archivos[-noBorrar]
    lapply(file.path(runPath, archivos), file.remove)

    # # generar copia del rnw renombrando
    outFile     <- paste("RepInst", Inst, sep = "")
    outFileRnw  <- file.path(runPath,
                              paste(outFile, ".Rnw", sep = ""))
    outFileTexE <- paste(outFile, ".tex", sep = "")
# #     outFileTexS <- file.path(runPath,
# #                              paste(outFile, ".tex", sep = ""))

  if (file.exists(outFileRnw)) {
    file.remove(outFileRnw)
    lapply(file.path(runPath, filesAux),
           file.remove)
  }
  file.copy(rnwBase, outFileRnw)
  lapply(file.path("Sweave", filesAux),
         file.copy, to = runPath)


# #   file.copy("entidades080910.Rnw", outFileRnw)
 auxPath <- getwd()
 auxPath2 <- runPath
 setwd(runPath)
 runPath <- "."
  Sweave(paste(outFile, ".Rnw", sep = ""), encoding = "latin1")

 runlatex   <- paste("latex --interaction=nonstopmode ", outFileTexE,
                     "\n", sep="")

 batFile <- file.path(runPath, "correr13_14.bat")
 cat(runlatex, runlatex,
     "dvips -tletter -Ppdf -o ", outFile, ".ps ", outFile,
     '.dvi', "\nps2pdf ", outFile, ".ps\n", sep = "",
     file = batFile)
	 
 system("correr13_14.bat")

 estanAr <- list.files(pattern = outFile)
 sonAr <- grep("[dvi|ps]$", estanAr)

 setwd(auxPath)
 runPath <- auxPath2

} 
   