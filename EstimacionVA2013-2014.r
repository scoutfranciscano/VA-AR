################################################################################
##
## SubjectSpecificVAModels.R
##
## Descripción: This script contains the VA models of Specific Subjects
##              for Engineering and Medicine.
##
##
## Project: Value Added
##
## Inputs: - The database built for the value added models of specific
##           subjects: pro11_11_2_to_12_2_v5grDiagTratam and
##           pro11_11_2_to_12_2_v5grFormProyIngen
##
## Outputs: - All the outputs and graphics for VA models
##
## Author: Edwin Cuellar
##
## Date: February 19 of 2014.
################################################################################

################################################################################
# # Libraries
################################################################################

library(RODBC)
library(plyr)
library(foreign)
library(xtable)
library(lme4)

################################################################################
# # Folders
################################################################################
setwd("C:/ecuellar/2015/ek/Aporte Relativo/src")

outPath      <- "../output/"
inPath       <- "../input/"
srcPath      <- "../src/"

################################################################################
# # Declaring functions
################################################################################
mean.na <- function(x) round(mean(x,na.rm=TRUE),2)
sd.na <- function(x) sd(x,na.rm=TRUE)

################################################################################
# # Bases de VALOR AGREGADO 2012 y 2013
################################################################################

fileName    <- "pro11_13_1_to_13_2_v2014.txt"
inFile      <- paste(inPath, fileName, sep = "")
VA2013 <- read.delim(file(inFile, encoding="latin1"), 
                          sep = "|", header=TRUE, dec=".")

fileName    <- "pro11_14_1_to_14_2_v2014.txt"
inFile      <- paste(inPath, fileName, sep = "")
VA2014 <- read.delim(file(inFile, encoding="latin1"), 
                          sep = "|", header=TRUE, dec=".")

################################################################################
# # Some modifications
################################################################################

## Valor Agregado 2013

VA2013[, 'inst_by_ref'] <- paste(VA2013[, 'INST_COD_INSTITUCION'],
                              VA2013[, 'ESTU_GRUPO_REFERENCIA'],
                              sep="__")

tmp <- aggregate(VA2013[, 'inse'],
                 list(VA2013[, 'inst_by_ref']), mean, na.rm = TRUE)

names(tmp) <- c("inst_by_ref", "inse_mean")

VA2013 <- merge(VA2013,tmp)

## Valor Agregado 2014

VA2014[, 'inst_by_ref'] <- paste(VA2014[, 'INST_COD_INSTITUCION'],
                              VA2014[, 'ESTU_GRUPO_REFERENCIA'],
                              sep="__")

tmp <- aggregate(VA2014[, 'inse'],
                 list(VA2014[, 'inst_by_ref']), mean, na.rm = TRUE)

names(tmp) <- c("inst_by_ref", "inse_mean")

VA2014 <- merge(VA2014,tmp)

## Valor Agregado 2013-2014

VA2013_2014 <- rbind.fill(VA2013, VA2014)

tmp <- VA2013_2014[, c("MOD_RAZONA_CUANTITATIVO_PUNT", "MOD_LECTURA_CRITICA",
                  "MATEMATICAS_PUNT_ESTAND", "LENGUAJE_PUNT_ESTAND",
                  "QUIMICA_PUNT_ESTAND", "CIENCIAS_SOCIALES_PUNT_ESTAND")]

C13_14 <- cor(tmp, use = 'pairwise.complete.obs')

C13_14[upper.tri(C13_14, diag = TRUE)] <- NA

diag(C13_14) <- apply(tmp, 2, function(x) sum(!is.na(x)))

outcomes <- c("MOD_RAZONA_CUANTITATIVO_PUNT","MOD_LECTURA_CRITICA")

prior <- list(MOD_RAZONA_CUANTITATIVO_PUNT="MATEMATICAS_PUNT_ESTAND",
              MOD_LECTURA_CRITICA="LENGUAJE_PUNT_ESTAND")

ref.grps <- split(VA2013_2014,VA2013_2014[,
                  'ESTU_GRUPO_REFERENCIA'])

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

VAEstimaciones13_14 <- CalculateConf(va, ref.grps, n.boot = 100)

outFile <- file.path(outPath, "va_estimates13_14.Rdata")
save(VAEstimaciones13_14, file = outFile)


