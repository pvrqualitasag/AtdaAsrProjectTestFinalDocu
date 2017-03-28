###
###
###
###   Old stuff: collection of commands


tdfControlMilkYield[which(tdfControlMilkYield$vecTvdNrLifeNrMap == "5"),]
tdfResult01[tdfResult01$ATMM_KUH_IDX == 5,]

tdfControlMilkYield[which(tdfControlMilkYield$vecTvdNrLifeNrMap == "6"),]
tdfResult01[tdfResult01$ATMM_KUH_IDX == 6,]


ggp2bpResult <- ggplot(ptdfResult, aes(ATMM_KUH_IDX, ATMM_KG_TMM)) +
  geom_boxplot(aes(group = cut_width(ATMM_KUH_IDX,1))) +
  geom_point(mapping = aes(vecTvdNrLifeNrMap,PESKGLAITTOTAL), data = tdfControlMilkYield, color = "red") +
  scale_x_discrete(limits=c(1:nrow(tdfTvdNrCow)))+
  labs(title = paste0("Tagesmilchmenge im Monat 2017-",psMonthNr),
       x     = "Kuh-Nummer",
       y     = "Tagesmilchmenge (in kg)") +
  coord_flip()





testRecSummary <- function(psLifeNr, ptdfResult){
  tdfTestRec <- filter(ptdfResult, ATMM_KUH_LIFENR == psLifeNr)
  return(summary(tdfTestRec$ATMM_KG_TMM))
}

(testRecSummary(psLifeNr = "CH 12011540208", ptdfResult = tdfResult01))
(testRecSummary(psLifeNr = "CH 12011540211", ptdfResult = tdfResult01))
(testRecSummary(psLifeNr = "CH 120087773641", ptdfResult = tdfResult01))

### # select tmm for given cow and month
tdfResult %>%
  filter(ATMM_KUH_LIFENR == as.character(tdfTvdNrCow[1,1]) & ATMM_REC_MONTH == "01") %>%
  nrow

### # for a given cow x, tbd_df with records and a month
getNrRec <- function(x, ptdfRec, psMonth) {
  return(ptdfRec %>%
           filter(ATMM_KUH_LIFENR == as.character(x) & ATMM_REC_MONTH == psMonth) %>%
           nrow)
}

### # run sapply to get max nr of records
sMonth <- "01"
(vecLen <- sapply(as.vector(unlist(tdfTvdNrCow)),getNrRec, tdfResult, sMonth , USE.NAMES = FALSE))
nMaxNrRec <- max(vecLen)


(tdfResult %>% filter(ATMM_KUH_LIFENR == as.character(tdfTvdNrCow[1,1]) & ATMM_REC_MONTH == "01"))


### # get the vector of tmm
getTmmRec <- function(x, ptdfRec, psMonth, pnMaxNrRec){
  tdfTmmRecResult <- ptdfRec %>%
    filter(ATMM_KUH_LIFENR == as.character(x) & ATMM_REC_MONTH == psMonth) %>%
    select(ATMM_KG_TMM)
  nNrRecResult <- nrow(tdfTmmRecResult)
  if (pnMaxNrRec > nNrRecResult){
    tdfNaAppend <- tbl_df(data.frame(rep(NA,(pnMaxNrRec-nNrRecResult))))
    colnames(tdfNaAppend) <- colnames(tdfTmmRecResult)
    tdfTmmRecResult <- rbind(tdfTmmRecResult,tdfNaAppend)
  }

  return(tdfTmmRecResult)
}
### # data matrix
#lapply(as.vector(unlist(tdfTvdNrCow)),getTmmRec, tdfResult, sMonth)

### # Returns list of values for each cow
lMilRec <- sapply(as.vector(unlist(tdfTvdNrCow)),getTmmRec, tdfResult, sMonth, nMaxNrRec)

### # convert the list to a data frame
dfMilkRec <- as.data.frame(lMilRec)

### # adapt the colnames
vecTvdNrRecCow <- gsub(".ATMM_KG_TMM","", names(lMilRec), fixed = TRUE)
colnames(dfMilkRec) <- vecTvdNrRecCow

### # plot the whole stuff
library(ggplot2)
nNrCows <- 2
boxpMilkRec <- ggplot(dfMilkRec[,1:nNrCows], aes(vecTvdNrRecCow[1:nNrCows])) + geom_boxplot()

