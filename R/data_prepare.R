###
###
###
###   Purpose:   Preparation of data for plotting
###   started:   2017/03/15 (pvr)
###
###

#' @title Extend transferred daily milk with cow index
#'
#' @description
#' Daily milk yield is transferred from the database as
#' a dataframe. The data frame has basically daily milk
#' yields (TMM) for each cows over a period of months.
#' The result of the function \code{tdfGetTmmPerMonth}
#' is a tbl_df{dplyr} of all milk records for the
#' given month psMonth for all cows.
#'
#' @details
#' The resulting tbl_df has an additional column with
#' the name ATMM_KUH_IDX. This column has indices that
#' represent the identities of the cow. This index is
#' only used for plotting to have more readable axis
#' labels.
#'
#' @param pdfResult original data.frame containing all data transferred from database
#' @param psMonth Month (formatted as %m) for which results should be returned
#' @return tdfTmmPerMonth tbl_df containing resulting milk records from month psMonth
#' @export tdfGetTmmPerMonth
tdfGetTmmPerMonth <- function(pdfResult, psMonth){
  ### # convert simple data.frame to tbl_df
  tdfResult <- dplyr::tbl_df(pdfResult)
  ### # extract unique TVD-Nr of cows from complete dataset into a vector
  #vecLifeNr <- distinct(tdfResult, ATMM_KUH_LIFENR)$ATMM_KUH_LIFENR
  vecLifeNr <- vecGetTmmLifeNr(pdfResult = pdfResult)
  ### # extract month out of recording date
  tdfResult <- dplyr::mutate(tdfResult, ATMM_REC_MONTH = substr(ATMM_REC_DATUM,6,7))
  ### # filter the data according to month
  tdfTmmPerMonth <- dplyr::filter(tdfResult, ATMM_REC_MONTH == psMonth)
  ### # Assign an index relating TVD-Nr to position in vecLifeNr
  ATMM_KUH_IDX <- sapply(tdfTmmPerMonth$ATMM_KUH_LIFENR,
                         function(x) which(x == vecLifeNr),
                         USE.NAMES = FALSE)
  ### # cbind the generated index to the tlb_df and return the result
  tdfTmmPerMonth <- cbind(tdfTmmPerMonth, ATMM_KUH_IDX)
  return(tdfTmmPerMonth)
}

### ######################

#' @title Convert a dataframe of control milk yield to tbl_df
#'
#' @description
#' The dataframe of control milk yields is converted to a tbl_df.
#' A mapping to cow indices of the TMM records given in pdfResult
#' is generated and added to the records of control milk yield
#'
#' @param pdfControlMilkYield original dataframe of control milk yield
#' @param pdfResult original dataframe of TMM records
#' @return tdfControlMilkYield extended tbl_df of control milk yield
#' @export tdfGetControlMilkYield
tdfGetControlMilkYield <- function(pdfControlMilkYield, pdfResult){
  ### # vector of unique TVD-Nr with spaces and "." removed
  vecCleanLifeNr <- vecCleanUpChar(pvecRemoveChar = c(" ", "."), vecGetTmmLifeNr(pdfResult = pdfResult))
  ### # construct a mapping vector from TVD-Nr in control milk yield data to Lifenumbers from TMM data
  vecTvdNrLifeNrMap <- sapply(pdfControlMilkYield$TVDNR,
                              function(x) charmatch(substr(x,1,nchar(x)-1), vecCleanLifeNr),
                              USE.NAMES = FALSE)
  ### # bind vecTvdNrLifeNrMap to tbl_df of original data.frame
  tdfControlMilkYield <- dplyr::tbl_df(pdfControlMilkYield)
  tdfControlMilkYield <- cbind(tdfControlMilkYield, vecTvdNrLifeNrMap)
  ### # compute total control milk yield per day
  tdfControlMilkYield <- dplyr::mutate(tdfControlMilkYield,
                                PESKGLAITTOTAL = PESKGLAITMATIN + PESKGLAITSOIR)
  ### # filter the records which cannot be mapped
  tdfControlMilkYield <- dplyr::filter(tdfControlMilkYield, !is.na(vecTvdNrLifeNrMap))
  return(tdfControlMilkYield)
}


### ######################

#' @title Extract unique cow lifenumbers from complete TMM dataset
#'
#' @description
#' Given a complete set of transferred milk yield records (TMM), all
#' unique cow lifenumbers are extracted and returned as a vector
#'
#' @param pdfResult original data frame of TMM records
#' @return vecTmmLifeNr vector of unique cow life numbers found in pdfResult
#' @export vecGetTmmLifeNr
vecGetTmmLifeNr <- function(pdfResult){
  vecTmmLifeNr <- dplyr::distinct(dplyr::tbl_df(pdfResult), ATMM_KUH_LIFENR)$ATMM_KUH_LIFENR
  return(vecTmmLifeNr)
}

### ######################

#' @title Remove fixed set of characters from given vector of strings
#'
#' @description
#' Given a vector of characters to be removed from a vector of strings,
#' this is done with a loop over the characters to be removed using
#' fixed version of gsub. This could be done easier with regular
#' expressions
#'
#' @param pvecRemoveChar vector of characters to be removed
#' @param pvecSrc vector of strings from which characters should be removed
#' @return vecCleanResult vector of strings w/out removed characters
vecCleanUpChar <- function(pvecRemoveChar, pvecSrc){
  vecCleanResult <- pvecSrc
  for (rc in pvecRemoveChar){
    vecCleanResult <- gsub(rc, "", vecCleanResult, fixed = TRUE)
  }
  return(vecCleanResult)
}
