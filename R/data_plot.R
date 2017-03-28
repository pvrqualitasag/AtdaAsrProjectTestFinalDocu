###
###
###
###   Purpose:   Plotting transferred milk yields vs control milk yield
###   started:   2017/03/15 (pvr)
###

#' @title Boxplot of TMM data overlayed with points of control milk yields
#'
#' @description
#' Given a tbl_df with transferred TMM data and a tbl_df of control milk yields,
#' a boxplot is generated using the TMM data and the control milk yields are
#' overlayed as red-colored points.
#'
#' @param ptdfResult tbl_df of transferred TMM data
#' @param psMonthNr Month formatted as '%m'
#' @param ptdfControlMilkYield tbl_df of control milk yields
#' @return ggp2bpResult ggplot object containing overlayed plots
#' @export ggp2bpGetBoxPlotAtmm
ggp2bpGetBoxPlotAtmm <- function(ptdfResult, psMonthNr, ptdfControlMilkYield){
  ggp2bpResult <- ggplot2::ggplot(ptdfResult, ggplot2::aes(ATMM_KUH_IDX, ATMM_KG_TMM)) +
    ggplot2::geom_boxplot(ggplot2::aes(group = ggplot2::cut_width(ATMM_KUH_IDX,1)), outlier.shape = 1) +
    ggplot2::geom_point(ggplot2::aes(vecTvdNrLifeNrMap,PESKGLAITTOTAL),
                        data = ptdfControlMilkYield, color = "red") +
    ggplot2::scale_x_discrete(limits=c(1:nNrCowsTmm))+
    ggplot2::labs(title = paste0("Tagesmilchmenge im Monat 2017-",psMonthNr),
         x     = "Kuh-Nummer",
         y     = "Tagesmilchmenge (in kg)") +
    ggplot2::coord_flip()
  return(ggp2bpResult)
}
