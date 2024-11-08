##'CH_FEVT_C_ADV Children with fever for whom advice or treatment was sought
##' ml_fev_care in github
##' KR
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
CH_FEVT_C_ADV<- function(Rdata){


# Fever and care-seeking
# Fever
KRdata <- Rdata %>%
  mutate(ml_fever = case_when(
    h22==1  ~ 1,
    h22!=1  ~ 0,
    b5==0  ~ 99),
    ml_fever = set_label(ml_fever, label = "Fever symptoms in the 2 weeks before the surveyvs"))%>%
  replace_with_na(replace = list(ml_fever = c(99)))

# this is country specific and the footnote for the final table needs to be checked to see what sources are included.
# the code below only excludes traditional practitioner (h32t). Some surveys also exclude pharmacies (h32k), shop (h32s) or other sources.
# In some surveys traditional practitioner is h32w. Please check the data file using h32*
KRdata <- KRdata %>%
  mutate(CH_FEVT_C_ADV = case_when(
    ml_fever==1 & !(h32a==1|h32b==1|h32c==1|h32d==1|h32e==1|h32f==1|h32g==1|h32h==1|h32i==1|h32j==1|
                      h32k==1|h32l==1|h32m==1|h32n==1|h32o==1|h32p==1|h32q==1|h32r==1|h32s==1|h32u==1|h32v==1|h32w==1|h32x==1) ~ 0,
    ml_fever==1 & (h32a==1|h32b==1|h32c==1|h32d==1|h32e==1|h32f==1|h32g==1|h32h==1|h32i==1|h32j==1|
                     h32k==1|h32l==1|h32m==1|h32n==1|h32o==1|h32p==1|h32q==1|h32r==1|h32s==1|h32u==1|h32v==1|h32w==1|h32x==1) ~ 1,
    b5==0  ~ 99),
    CH_FEVT_C_ADV = set_label(CH_FEVT_C_ADV, label = "Advice or treatment sought for fever symptoms"))%>%
  replace_with_na(replace = list(CH_FEVT_C_ADV = c(99)))

colnames(KRdata)[colnames(KRdata) == 'CH_FEVT_C_ADV'] <- 'value'


return(KRdata)
}
