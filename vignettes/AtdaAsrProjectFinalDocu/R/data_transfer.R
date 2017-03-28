###
###
###
###   Purpose:   Transfer data needed for the comparison
###   started:   2017/03/14 (pvr)
###
###

op <- options()
sOldWd <- getwd()
sNewWd <- "C:/Daten/GitHub/pvrqualitasag/AtdaAsrProjectTestFinalDocu/vignettes/AtdaAsrProjectFinalDocu"
setwd(sNewWd)

### # overall constants for all transfers
sServerName <- "same.braunvieh.ch"
options(RArgusDbApi.db.inst = "AR01")
options(RArgusDbApi.user = "GESTHO")
options(RArgusDbApi.pass = "GESTHO")


### # transfer AtdaTmm data
dfGetAtdaTmm <- function(psServerName){
  ### # database connection
  dbCon <- RArgusDBAPI::getArgusCon(server = psServerName)
  
  ### # example statement
  sSqlStmt <- "select ATMM_ID,ATMM_BTR_ID,ATMM_KUH_LIFENR,ATMM_REC_DATUM,ATMM_KG_TMM from T_ATDA_TMM"
  
  ### # run statement
  dfResult <- RJDBC::dbGetQuery(dbCon, sSqlStmt)
  ### # close connection
  RJDBC::dbDisconnect(dbCon)

  return(dfResult)
}

### # run the transfer calls for atda tmm
sAtdaTmmRda <- "atda_tmm.rda"

if (!file.exists(sAtdaTmmRda)){
  cat(" * Load AtdaTmm data from database ...\n")
  dfAtdaTmm <- dfGetAtdaTmm(psServerName = sServerName)
  save(dfAtdaTmm, file = sAtdaTmmRda)
} else {
  cat(" * AtdaTmm data already available ...\n")
}


### # transfer control milk yields
dfGetControlMilkYield <- function(psServerName, pnBtrNr, psStartDate, psEndDate){
  ### # database connection
  dbCon <- RArgusDBAPI::getArgusCon(server = psServerName)
  
  ### # example statement
  sSqlStmt <- "select ani.idanimal"
  sSqlStmt <- paste0(sSqlStmt, ", ani.ANIMARQUEMETALLIQUE")
  sSqlStmt <- paste0(sSqlStmt, ", pa_ani.sTvdNrDataExport(pnAniId => ani.idanimal) TvdNr")
  sSqlStmt <- paste0(sSqlStmt, ", aep.DATEFINANIMAL")
  sSqlStmt <- paste0(sSqlStmt, ", pes.pesdate")
  sSqlStmt <- paste0(sSqlStmt, ", pes.PESKGLAITMATIN")
  sSqlStmt <- paste0(sSqlStmt, ", pes.PESKGLAITSOIR")
  sSqlStmt <- paste0(sSqlStmt, " from   aniexp                 aep")
  sSqlStmt <- paste0(sSqlStmt, " join   animal                 ani")
  sSqlStmt <- paste0(sSqlStmt, " on     aep.idanimal        =  ani.idanimal")
  sSqlStmt <- paste0(sSqlStmt, " join   lactation              lac")
  sSqlStmt <- paste0(sSqlStmt, " on     lac.idanimal        =  ani.idanimal")
  sSqlStmt <- paste0(sSqlStmt, " join   pesee                  pes")
  sSqlStmt <- paste0(sSqlStmt, " on     pes.IDLACTATION     =  lac.IDLACTATION")
  sSqlStmt <- paste0(sSqlStmt, " where  aep.IDEXPLOITATION  =  ")
  sSqlStmt <- paste0(sSqlStmt, pnBtrNr)
  sSqlStmt <- paste0(sSqlStmt, " and    (   aep.datefinanimal   is null")
  sSqlStmt <- paste0(sSqlStmt, " or aep.datefinanimal   >  pes.pesdate )")
  sSqlStmt <- paste0(sSqlStmt, " and    pes.PESDATE between to_date('")
  sSqlStmt <- paste0(sSqlStmt, psStartDate)
  sSqlStmt <- paste0(sSqlStmt, "', 'yyyymmdd') and to_date('")
  sSqlStmt <- paste0(sSqlStmt, psEndDate)
  sSqlStmt <- paste0(sSqlStmt, "', 'yyyymmdd')")
  
  ### # run statement
  dfControlMilkYield <- RJDBC::dbGetQuery(dbCon, sSqlStmt)
  ### # close connection
  RJDBC::dbDisconnect(dbCon)

  return(dfControlMilkYield)
}


### # betriebsnummer
nBtrNr <- 360830
### # Kontrollintervall 201701
sStartDate <- "20170101"
sEndDate <-  "20170131"
sControlMilkYieldRda <- paste0("con_my_",format(as.Date(sStartDate, "%Y%m%d"), "%Y%m"),".rda")

### # in case the file with the milk yield for this period does not exist, 
### #  transfer the data
if (!file.exists(sControlMilkYieldRda)){
  dfControlMilkYield201701 <- dfGetControlMilkYield(psServerName = sServerName, 
                                                    pnBtrNr      = nBtrNr, 
                                                    psStartDate  = sStartDate, 
                                                    psEndDate    = sEndDate)
  ### # save data to file
  save(dfControlMilkYield201701, file = sControlMilkYieldRda)
}

### # Kontrollintervall 201702
sStartDate <- "20170201"
sEndDate <-  "20170228"
sControlMilkYieldRda <- paste0("con_my_",format(as.Date(sStartDate, "%Y%m%d"), "%Y%m"),".rda")

### # in case the file with the milk yield for this period does not exist, 
### #  transfer the data
if (!file.exists(sControlMilkYieldRda)){
  dfControlMilkYield201702 <- dfGetControlMilkYield(psServerName = sServerName, 
                                                    pnBtrNr      = nBtrNr, 
                                                    psStartDate  = sStartDate, 
                                                    psEndDate    = sEndDate)
  ### # save data to file
  save(dfControlMilkYield201702, file = sControlMilkYieldRda)
}

### # debug sql stmt
# pnBtrNr=nBtrNr;  psStartDate=sStartDate; psEndDate=sEndDate


### # reset workdir and options
setwd(sOldWd)
options(op)