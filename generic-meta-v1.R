## GENERIC META-ANALYSIS ##

library(tidyverse)
library(meta)

ipv_data <- botor::s3_read("s3://alpha-modaccreditedprogs/power/dv-data-v2.csv", read.csv)
vio_data <- botor::s3_read("s3://alpha-modaccreditedprogs/meta/vio-meta-v3.csv", read.csv) # create a dataframe
sex_data

# stardardise the column names
colnames(vio_data) <- c("code", "study", "year", "n.comp", "neg.comp", "pos.comp", "cneg.pc", "n.treat", "neg.treat", "pos.treat", "cpos.pc", "cohort", "outcome", "quality", "fup.months") 
vio_data <- vio_data %>% dplyr::select(code, study, year, n.comp, neg.comp, pos.comp, n.treat, neg.treat, pos.treat, fup.months, cohort, outcome, quality)

colnames(ipv_data) <- c("code", "study", "year", "multiple", "is.cbt", "disposal", "fup.months", "match", "quality", "outcome", "n.treat", "n.comp", "pos.treat", "neg.treat", "pos.comp", "neg.comp", "tpos.pc", "cpos.pc")
ipv_data$cohort <- "IPV"
ipv_data$quality <- dplyr::recode(ipv_data$quality, LQ = "Low", HQ = "High")
ipv_data <- ipv_data %>% 
  #filter(disposal == "Reconviction") %>%
  dplyr::select(code, study, year, n.comp, neg.comp, pos.comp, n.treat, neg.treat, pos.treat, fup.months, cohort, outcome, quality)

# bind the data
all_data <- rbind(vio_data, ipv_data)

# check data is in the correct format
sapply(all_data, class)

# format the data (numbers into numbers, factors into factors)
all_data$code <- factor(all_data$code)
all_data$study <- factor(all_data$study)
all_data$year <- factor(all_data$year)
all_data$cohort <- factor(all_data$cohort)
all_data$outcome <- factor(all_data$outcome)
all_data$quality <- factor(all_data$quality, levels = c("Low", "High"))

# select the outcome
# **THIS NEEDS TO BE SELECTED FOR EACH RUN!**
select_outcome <- "General" # "General", "Violent", "DV/IPV", "Sexual"

# specify the cohort
# **THIS NEEDS TO BE SELECTED FOR EACH RUN!**
meta_violence <- all_data %>% dplyr::filter(cohort == "Violence") 
meta_ipv <- all_data %>% dplyr::filter(cohort == "IPV")
#meta_sexual <- all_data %>% dplyr::filter(cohort == "Sexual") # not available yet

# run the meta analysis
print(meta <- metabin(pos.comp, pos.comp + neg.comp, pos.treat, pos.treat + neg.treat, 
                data = meta_violence, subset = (outcome == select_outcome), method.tau = "PM", sm = "OR",
                studlab = paste0(study, " ", year)))

# The classical measure of heterogeneity is Cochran’s Q, which is calculated as the weighted sum of squared differences between 
# individual study effects and the pooled effect across studies, with the weights being those used in the pooling method. 
# Q is distributed as a chi-square statistic with k (numer of studies) minus 1 degrees of freedom. 
# Q has low power as a comprehensive test of heterogeneity (Gavaghan et al, 2000), especially when the number of studies is small.
# Conversely, Q has too much power as a test of heterogeneity if the number of studies is large (Higgins et al. 2003):
# The I² statistic describes the percentage of variation across studies that is due to heterogeneity rather than chance (Higgins and Thompson, 2002; Higgins et al., 2003). 
# I² = 100% x (Q-df)/Q. I² is an intuitive and simple expression of the inconsistency of studies’ results.
# Unlike Q it does not inherently depend upon the number of studies considered. 
# A confidence interval for I² is constructed using either i) the iterative non-central chi-squared distribution method of Hedges and Piggott (2001); or
# ii) the test-based method of Higgins and Thompson (2002). 
# The non-central chi-square method is currently the method of choice (Higgins, personal communication, 2006) – it is computed if the 'exact' option is selected.

# generate a forest plot of the meta-analysis outcomes
forest(meta)

# generate quality assurance plots 
funnel(meta)
radial(meta)
qqnorm(exp(meta$TE))

# run meta analysis and produce a forest plot by methodological quality
print(meta_by_quality <- update(meta, subgroup = quality, print.subgroup.name = T))
forest(meta_by_quality)

# END #