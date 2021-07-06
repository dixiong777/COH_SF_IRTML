#################### Rescale   ##################################################
# 1. the higher the value the worse the case
# 2. rescale as 0 - 5 except to the outcomes.
# pat <- read.csv("CHL_scaledC.csv", header = T)
pat <- read.csv("PAT_scaledC.csv", header = T)
item_info <- read.csv('Variable_list.csv')

Name <- 'parent'
pat <- pat[,-1]

pat_item <- pat[, -match(c('E_D', 'E_refer2'), colnames(pat))]

# ensure every variable starts from 0
pat_item <- t(t(pat_item) - apply(pat_item, 2, min, na.rm=TRUE))

# rescale items to 0 - 5 and the higher the worse.
pat_item <- 5 - apply(pat_item, 2, 
                        function(x) round(scales::rescale(x, to = c(0, 5))))

E_refer2_12_34 <- ifelse(pat$E_refer2 <= 1, 1, 0)

E_refer2_1_234 <- ifelse(pat$E_refer2 == 0, 1, 0)
E_refer2_123_4 <- ifelse(pat$E_refer2 <= 2, 1, 0)
E_refer2_threelevel <- 2 - pat$E_refer2
E_refer2_threelevel[E_refer2_threelevel < 0] <- 0

# Update the pat dataset. referral rescaled as the higher the worse the case.
pat <- data.frame(pat_item, E_refer2 = E_refer2_12_34, E_D = pat$E_D)

library(tidyverse)
# select variables with na
indx <- apply(pat, 2, function(x) any(is.na(x)))

# Total number of items.
length(colnames(pat[,!indx])) - 2
write.csv(pat, 'data_PAR_IRT.csv')
####################Correlation##################################################

# Spearman
cor_pat <- dplyr::select(as.data.frame(cor(pat[,colnames(pat)[!indx]], 
                                           method = "spearman")), "E_D", "E_refer2")
cor_pat <- tibble::rownames_to_column(cor_pat, "Variable")[1:(ncol(pat[,colnames(pat)[!indx]])-2),]

#chisq test for D
test_pvalue <- apply(pat[,colnames(pat)[!indx]][,1:(ncol(pat[,colnames(pat)[!indx]])-2)], 2, 
                     function(x) chisq.test(table(x, pat[,colnames(pat)[!indx]][,ncol(pat[,colnames(pat)[!indx]])]))$p.value)
#chisq test for refer2
test_pvalue_r <- apply(pat[,colnames(pat)[!indx]][,1:(ncol(pat[,colnames(pat)[!indx]])-2)], 2, 
                       function(x) chisq.test(table(x, pat[,colnames(pat)[!indx]][,ncol(pat[,colnames(pat)[!indx]])-1]))$p.value)

library(Hmisc)
cor_pat = mutate(cor_pat, refer2_pvalue = rcorr(as.matrix(pat[,colnames(pat)[!indx]]), type = "spearman")[["P"]][,ncol(pat[,colnames(pat)[!indx]])-1][1:(ncol(pat[,colnames(pat)[!indx]])-2)], 
                 D_pvalue = rcorr(as.matrix(pat[,colnames(pat)[!indx]]), type = "spearman")[["P"]][,ncol(pat[,colnames(pat)[!indx]])][1:(ncol(pat[,colnames(pat)[!indx]])-2)],
                 D_chisq_pvalue = test_pvalue, refer2_chisq_pvalue = test_pvalue_r)

library(polycor)
# Polychoric for D
poly_cor <- apply(pat[,colnames(pat)[!indx]][,1:(ncol(pat[,colnames(pat)[!indx]])-2)], 2, 
                  function(x) polychor(x,pat[,colnames(pat)[!indx]][,ncol(pat[,colnames(pat)[!indx]])],std.err = TRUE)$rho)
poly_var <- apply(pat[,colnames(pat)[!indx]][,1:(ncol(pat[,colnames(pat)[!indx]])-2)], 2, 
                  function(x) polychor(x,pat[,colnames(pat)[!indx]][,ncol(pat[,colnames(pat)[!indx]])],std.err = TRUE)$var)
statistic <- poly_cor / sqrt(c(poly_var))
p.value = 2 * (1 - pnorm(abs(statistic)))
cor_pat = mutate(cor_pat, D_poly_cor = poly_cor, D_poly_pvalue = p.value)

# Polychoric for refer2
poly_cor <- apply(pat[,colnames(pat)[!indx]][,1:(ncol(pat[,colnames(pat)[!indx]])-2)], 2, 
                  function(x) polychor(x,pat[,colnames(pat)[!indx]][,ncol(pat[,colnames(pat)[!indx]])-1],std.err = TRUE)$rho)
poly_var <- apply(pat[,colnames(pat)[!indx]][,1:(ncol(pat[,colnames(pat)[!indx]])-2)], 2, 
                  function(x) polychor(x,pat[,colnames(pat)[!indx]][,ncol(pat[,colnames(pat)[!indx]])-1],std.err = TRUE)$var)
statistic <- poly_cor / sqrt(c(poly_var))
p.value = 2 * (1 - pnorm(abs(statistic)))
cor_pat = mutate(cor_pat, refer2_poly_cor = poly_cor, refer2_poly_pvalue = p.value)
rownames(cor_pat) <- cor_pat$Variable

#View(cor_pat)
cor_pat <- merge(item_info, cor_pat, 'Variable', all.y = TRUE)
cor_pat$Variable <- as.character(cor_pat$Variable)
# cor_pat$item_total <- item.total(pat[, 1:nrow(cor_pat)])[,2]
# cor_pat$item_total_check <- ifelse(cor_pat$item_total >= 0.2, 1, 0)
# View(cor_pat[colnames(pat2),c(3,4,7,10,11)])
# View(cor_pat[colnames(pat2),c(2,5,6,8,9)])
# write.csv(cor_pat, "CHL_stats.csv")

#p-Threshold = 0.05 and Cor > 0, Cor >= 0.2
#E_refer2
association_ref2_check <- with(cor_pat, 
                            {
                              cor_check = (refer2_pvalue <= 0.05) * (E_refer2 > 0) #+ 
                                                   #(E_refer2 >= 0.2)
                              poly_check = (refer2_poly_pvalue <= 0.05) * (refer2_poly_cor > 0) + 
                                                       (refer2_poly_cor >= 0.2)
                              chi2_check = (refer2_chisq_pvalue <= 0.05) * (refer2_poly_cor > 0)
                              poly_check | chi2_check
                            })
selected_pat_refer2 <- cor_pat$Variable[association_ref2_check]
#E_D
association_D_check <- with(cor_pat, 
                           {
                             cor_check = (D_pvalue <= 0.05) * (E_D > 0) #+ 
                                                  #(E_D >= 0.2)
                             poly_check = (D_poly_pvalue <= 0.05) * (D_poly_cor > 0) + 
                                                 (D_poly_cor >= 0.2)
                             chi2_check = (D_chisq_pvalue <= 0.05)  * (D_poly_cor > 0)
                             poly_check | chi2_check
                           })
selected_pat_D <- cor_pat$Variable[association_D_check]
colnames(cor_pat)[colnames(cor_pat) == 'E_D'] = 'Corr_E_D'
colnames(cor_pat)[colnames(cor_pat) == 'E_refer2'] = 'Corr_E_refer2'

res_D <- mutate(cor_pat[, c('Variable',
                            "Component",          
                            "Subcomponent", 
                            "Domain",
                            "Subdomain",
                            "Main_Question",
                            "Orig_Selection",
                            'Corr_E_D',
                            'D_pvalue', 
                            'D_poly_pvalue', 
                            'D_poly_cor',
                            'D_chisq_pvalue')], 
                association_D_check)
res_D$Variable <- as.character(res_D$Variable)
res_refer2 <- mutate(cor_pat[, c('Variable',
                                 "Component",          
                                 "Subcomponent", 
                                 "Domain",
                                 "Subdomain",
                                 "Main_Question",
                                 "Orig_Selection",
                                  'Corr_E_refer2',
                              'refer2_pvalue', 
                              'refer2_poly_pvalue', 
                              'refer2_poly_cor',
                              'refer2_chisq_pvalue')], 
                      association_ref2_check)
res_refer2$Variable <- as.character(res_refer2$Variable)
#################### Skewness ##################################################
# 1. Skewness (With the data the lower the better)
library(e1071)
# # E_refer2
# skewness_value <- apply(pat[,selected_pat], 2, 
#                                skewness, na.rm = TRUE)
# skewness_indicator <- (skewness_value >= 0)
# pat2<-pat[,selected_pat]
# pat2<-pat2[,skewness_indicator]
# View(colnames(pat2))

# No extremely negative observed for Child
# ACCESS (9.78924819), P_CHOPJ (5.84231243)
res_D$skewness <- NA
res_D$skewness <- apply(pat[,res_D$Variable], 2, skewness, na.rm = TRUE)
hist(res_D$skewness[res_D$association_D_check == TRUE])
res_D$skewness_check <- (abs(res_D$skewness) <= 6)
res_D$corr_skewness_check <- res_D$skewness_check & res_D$association_D_check 

res_refer2$skewness <- NA
res_refer2$skewness <- apply(pat[,res_refer2$Variable], 2, skewness, na.rm = TRUE)
hist(res_refer2$skewness[res_refer2$association_ref2_check == TRUE])
res_refer2$skewness_check <- (abs(res_refer2$skewness) <= 6)
res_refer2$corr_skewness_check <- res_refer2$skewness_check & 
                                      res_refer2$association_ref2_check 
#################### Selected Items ###########################################
ref_item <- res_refer2$Variable[res_refer2$corr_skewness_check == TRUE]
D_item <- res_D$Variable[res_D$corr_skewness_check == TRUE]

Component_refer2 <- as.character(res_refer2$Component[res_refer2$corr_skewness_check == TRUE])
Component_D <- as.character(res_D$Component[res_D$corr_skewness_check == TRUE])
pat_refer2 <- pat[, ref_item]
pat_D <- pat[, D_item]
#################### CFA ##################################################
# 3. CFA
library(lavaan)

# (1) Full Model 
select_ref <- res_refer2$Variable[res_refer2$corr_skewness_check == TRUE]
select_D <- res_D$Variable[res_D$corr_skewness_check == TRUE]

# Referral
CFA_ref_model_full <- paste0('F = ~',
                             paste0(select_ref,
                                    collapse = ' + '))
cfa_ref_result_full <- cfa(CFA_ref_model_full, data=pat,
                  estimator = 'WLSMV', std.lv=TRUE,
                  ordered = select_ref)
out_ref_full <- summary(cfa_ref_result_full, 
                        standardized=T, 
                        fit.measures=T, 
                        rsquare=T)
res_cor_ref <- cbind(Variable = select_ref, 
                     as.data.frame(lavResiduals(cfa_ref_result_full)$cov))
out_ref_full$FIT[c('cfi.scaled', 'rmsea.scaled', 'srmr')]


## Decay
CFA_D_model_full <- paste0('F = ~',
                             paste0(select_D,
                                    collapse = ' + '))
cfa_D_result_full <- cfa(CFA_D_model_full, data=pat,
                       estimator = 'WLSMV', std.lv=TRUE,
                       ordered = select_D)
out_D_full <- summary(cfa_D_result_full, standardized=T, fit.measures=T, rsquare=T)
res_cor_D <- cbind(Variable = select_D, 
                   as.data.frame(lavResiduals(cfa_D_result_full)$cov))
out_D_full$FIT[c('cfi.scaled', 'rmsea.scaled', 'srmr')]

# Output
res_refer2 <- Reduce(function(...) merge(..., all = TRUE, by = "Variable"),
                                    list(res_refer2,
                                         res_cor_ref))
res_refer2[is.na(res_refer2)] <- '.'
res_refer2$Component <- as.character( res_refer2$Component)
write.csv(res_refer2, paste0(Name, 'refer2.csv', collapse = '_'))

res_D <- Reduce(function(...) merge(..., all = TRUE, by = "Variable"),
                     list(res_D,
                          res_cor_D))
res_D[is.na(res_D)] <- '.'
res_D$Component <- as.character( res_D$Component)
write.csv(res_D, paste0(Name, 'D.csv', collapse = '_'))

c(n = length(select_ref),out_ref_full$FIT[c('cfi.scaled', 'tli.scaled', 'rmsea.scaled', 'srmr')])
c(n = length(select_D),out_D_full$FIT[c('cfi.scaled', 'tli.scaled', 'rmsea.scaled', 'srmr')])

# CHL with one-factor CFA (no need to follow)
# > c(n = length(select_ref),out_ref_full$FIT[c('cfi.scaled', 'tli.scaled', 'rmsea.scaled')])
# n   cfi.scaled   tli.scaled rmsea.scaled 
# 18.00000000   0.89406242   0.87993741   0.06520969 
# > c(n = length(select_D),out_D_full$FIT[c('cfi.scaled', 'tli.scaled', 'rmsea.scaled')])
# n   cfi.scaled   tli.scaled rmsea.scaled 
# 26.00000000   0.85196901   0.83909675   0.06956734 

# After checking the residual correlation
# Referral - Remove: P_ANXIETY2 (0.336921294249934), P_BELIEF, P_CLDHEALT  
# D – Remove:  P_ACCESS6, P_ANXIETY2, P_BELIEF, P_CLDHEALT, P_DEVELOP, P_DEVELOP1, 
# P_SELFEFF1, P_SWALLOW, P_SOCNET2C

# Referral
CFA_ref_model_full <- 'F = ~ P_ACCESS6 + 
P_AFRAID + 
P_CHOPA +
P_CHOPC + 
P_CHOPE +
P_CHOPF + 
P_COLOR_BAD +
P_OHEFFECT +
P_SCHMISS +
P_SELFEFF1 +
P_SELFEFF7 +
P_SH1 +
P_SOCCOM2 +
P_SOCNET1B +
P_WORRY'
cfa_ref_result_full <- cfa(CFA_ref_model_full, data=pat,
                           estimator = 'WLSMV', std.lv=TRUE,
                           ordered = select_ref)
out_ref_full <- summary(cfa_ref_result_full, 
                        standardized=T, 
                        fit.measures=T, 
                        rsquare=T)

## Decay
CFA_D_model_full <- 'F = ~ P_ACCESS1 +
P_AFRAID +
P_CHOPA +
P_CHOPB +
P_CHOPC +
P_CHOPD +
P_CHOPE +
P_CHOPF +
P_CHOPG +
P_OHEFFECT + 
P_SCHMISS +
P_SELFEFF +
P_SELFEFF2 +
P_SH1 +
P_SOCNET1B +
P_SOCNET1E +
P_WORRY'
cfa_D_result_full <- cfa(CFA_D_model_full, data=pat,
                         estimator = 'WLSMV', std.lv=TRUE,
                         ordered = select_D)
out_D_full <- summary(cfa_D_result_full, standardized=T, fit.measures=T, rsquare=T)



c(n = 15,out_ref_full$FIT[c('cfi.scaled', 'tli.scaled', 'rmsea.scaled', 'srmr')])
c(n = 17,out_D_full$FIT[c('cfi.scaled', 'tli.scaled', 'rmsea.scaled', 'srmr')])

select_data <- pat[, c("P_WORRY",
                       "P_SOCNET1E",
                       "P_SOCNET1B",
                       "P_SOCCOM2",
                       "P_SH1",
                       "P_SELFEFF7",
                       "P_SELFEFF2",
                       "P_SELFEFF1",
                       "P_SELFEFF",
                       "P_SCHMISS",
                       "P_OHEFFECT",
                       "P_COLOR_BAD",
                       "P_CHOPG",
                       "P_CHOPF",
                       "P_CHOPE",
                       "P_CHOPD",
                       "P_CHOPC",
                       "P_CHOPB",
                       "P_CHOPA",
                       "P_AFRAID",
                       "P_ACCESS6",
                       "P_ACCESS1")]
write.csv(select_data, 'data_PAR_IRT_head.csv')



save(list = c('out_ref_full', 'out_D_full'), file = "PAR_CFA.RData")

rm(list = ls())
load('PAR_CFA.RData')
ls()
out_D_full
