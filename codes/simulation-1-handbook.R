library(simex) ## only confusion matrix is provided
library(nonprobsvy)
library(data.table)
library(vcd)
library(VIM)
library(ggplot2)
library(knitr)

set.seed(2024)
N <- 100000
pop_data <- data.table(age = sample(c("Young", "Middle-aged", "Old"), size = N, c(0.40, 0.35, 0.25), replace = T),
                       ethnic = sample(c("Native", "Nonnative"), size = N, prob = c(0.85, 0.15), replace = T))

pop_data[ethnic == "Native" & age == "Young", internet := rbinom(.N, 1, 0.90)]
pop_data[ethnic == "Native" & age == "Middle-aged", internet := rbinom(.N, 1, 0.70)]
pop_data[ethnic == "Native" & age == "Old", internet := rbinom(.N, 1, 0.50)]

pop_data[ethnic == "Nonnative" & age == "Young", internet := rbinom(.N, 1, 0.25)]
pop_data[ethnic == "Nonnative" & age == "Middle-aged", internet := rbinom(.N, 1, 0.15)]
pop_data[ethnic == "Nonnative" & age == "Old", internet := rbinom(.N, 1, 0.10)]

pop_data[age == "Young", NEP := rbinom(.N, 1, 0.05)]
pop_data[age == "Middle-aged", NEP := rbinom(.N, 1, 0.40)]
pop_data[age == "Old", NEP := rbinom(.N, 1, 0.60)]

pop_data[age == "Young" & internet == 1, NIP := rbinom(.N, 1, 0.80)]
pop_data[age == "Middle-aged" & internet == 1, NIP := rbinom(.N, 1, 0.40)]
pop_data[age == "Old" & internet == 1, NIP := rbinom(.N, 1, 0.20)]
pop_data[internet == 0, NIP := rbinom(.N, 1, 0.10)]

pop_data[, age := factor(age, levels = c("Young", "Middle-aged", "Old"))]
pop_data[, ethnic := factor(ethnic, levels = c("Native", "Nonnative"))]
pop_data[, pi_A := exp(-2 + 2*internet) / (1 + exp(-2 + 2*internet))]

gamma_ethnic <- matrix(c(0.7, 0.3, 0.05, 0.95), ncol = 2)
gamma_age <- matrix(c(0.85, 0.15, 0.05, 
                      0.20, 0.70, 0.10, 
                      0.05, 0.15, 0.80), ncol = 3)

colnames(gamma_age) <- levels(pop_data$age)
colnames(gamma_ethnic) <- levels(pop_data$ethnic)

dd <- simex::misclass(pop_data[, .(age, ethnic)], 
                      list(age = gamma_age, ethnic = gamma_ethnic),
                      k = 1)
pop_data[, age_m := dd$age]
pop_data[, ethnic_m := dd$ethnic]


n_sims <- 100 
n_B <- 1000 
n_valid <- 100 ## size of validation sample
stratified <- TRUE
B_simex <- 50
results  <- list() 

for (r in 1:n_sims) {
  
  if (r %% 10 == 0) print(r)
  
  S_A <- copy(pop_data[rbinom(.N, 1, pi_A) == 1])
  S_A[, id:=1:.N]
  
  S_B <- copy(pop_data[sample(1:.N, n_B)])
  S_B[, weights := N/n_B]
  S_B_svy <- svydesign(ids = ~1, data = S_B, weights = ~weights)
  
  est_sb <- svymean(~NEP + NIP, design = S_B_svy)
  est_sa_naive <- colMeans(S_A[, .(NEP, NIP)])
  
  est_sa_mi <- nonprob(
    outcome = NEP + NIP ~ age + ethnic,
    data = S_A,
    svydesign = S_B_svy,
    method_outcome = "glm",
    family_outcome = "binomial"
  )
  
  ## with errors (estimated from S_A only)
  S_A_err <- copy(S_A)
  
  if (stratified) {
    S_A_err[, w:=.N/n_valid, .(age_m, ethnic_m)]
    S_A_err_valid_sample <- copy(S_A_err[,.SD[sample(.N, n_valid)], by = .(age_m, ethnic_m)])  
  } else {
    S_A_err[, w:=.N/(n_valid*6)]
    S_A_err_valid_sample <- copy(S_A_err[sample(1:.N, n_valid*6)])
  }
  
  sampled_ids <- S_A_err_valid_sample$id
  error_age <- as.matrix(prop.table(xtabs(w~ age_m + age, data = S_A_err_valid_sample), margin = 2))
  error_eth <- as.matrix(prop.table(xtabs(w~ ethnic_m + ethnic, data = S_A_err_valid_sample), margin = 2))
  S_A_err[, age := NULL]
  S_A_err[, ethnic := NULL]
  S_A_err[, age := age_m]
  S_A_err[, ethnic := ethnic_m]
  
  est_sa_mi_errors <- nonprob(
    outcome = NEP + NIP ~ age + ethnic,
    data = S_A_err,
    svydesign = S_B_svy,
    method_outcome = "glm",
    family_outcome = "binomial"
  )
  
  est_sa_mi_err <- glm(NEP ~ age + ethnic, family = binomial, x = TRUE, y = TRUE,
                       data = S_A_err)
  
  ## model parameters corrected by MC-SIMEX approach
  res_mi_li <- tryCatch(simex::mcsimex(model = est_sa_mi_err,
                                       mc.matrix = list(age = error_age, ethnic = error_eth),
                                       SIMEXvariable = c("age", "ethnic"),
                                       fitting.method	= "linear",
                                       asymptotic = FALSE,
                                       jackknife.estimation	= FALSE,
                                       B = B_simex), error = function(e) NULL)
  
  if (is.null(res_mi_li)) next
  
  res_mi_qu <- refit(object = res_mi_li,
                     fitting.method = "quadratic",
                     asymptotic = FALSE,
                     jackknife.estimation	= FALSE)
  
  res_mi_ll <- refit(object = res_mi_qu,
                     fitting.method = "loglinear",
                     asymptotic = FALSE,
                     jackknife.estimation	= FALSE)
  
  S_B_svy <- update(S_B_svy, 
                    NEP_li = predict(res_mi_li, S_B_svy$variables, type = "response"),
                    NEP_qu = predict(res_mi_qu, S_B_svy$variables, type = "response"),
                    NEP_ll = predict(res_mi_ll, S_B_svy$variables, type = "response"))
  
  res <- svymean(~NEP_li + NEP_qu + NEP_ll, design = S_B_svy)
  res_ci <- confint(res)
  
  ## assume that we sample subsample and have only partial information
  S_A_imputation <- copy(S_A)
  S_A_imputation[!id %in% sampled_ids, ":="(age = NA, ethnic = NA)]
  
  S_A_imputation_kk <- rangerImpute(formula = age + ethnic ~ age_m + ethnic_m ,
                                    data = S_A_imputation)
  
  S_A_imputation[, age:= S_A_imputation_kk$age]
  S_A_imputation[, ethnic:= S_A_imputation_kk$ethnic]
  
  est_sa_mi_imp <- nonprob(
    outcome = NEP + NIP ~ age + ethnic,
    data = S_A_imputation,
    svydesign = S_B_svy,
    method_outcome = "glm",
    family_outcome = "binomial"
  )
  
  
  results[[r]] <- data.frame(
    nep_srs = est_sb[1],
    nep_naive = est_sa_naive[1],
    nep_mi = est_sa_mi$output$mean[1],
    nep_err = est_sa_mi_errors$output$mean[1],
    nep_li = res[1],
    nep_qu = res[2],
    nep_ll = res[3],
    nep_imp = est_sa_mi_imp$output$mean[1],
    cr_cor = est_sa_mi$confidence_interval[1, 1] < mean(pop_data$NEP) & est_sa_mi$confidence_interval[1, 2] > mean(pop_data$NEP), 
    cr_err = est_sa_mi_errors$confidence_interval[1, 1] < mean(pop_data$NEP) & est_sa_mi_errors$confidence_interval[1, 2] > mean(pop_data$NEP), 
    cr_li = res_ci[1, 1] < mean(pop_data$NEP) & res_ci[1, 2] > mean(pop_data$NEP),
    cr_qu = res_ci[2, 1] < mean(pop_data$NEP) & res_ci[2, 2] > mean(pop_data$NEP),
    cr_ll = res_ci[3, 1] < mean(pop_data$NEP) & res_ci[3, 2] > mean(pop_data$NEP),
    cr_imp = est_sa_mi_imp$confidence_interval[1, 1] < mean(pop_data$NEP) & est_sa_mi_imp$confidence_interval[1, 2] > mean(pop_data$NEP)
  )
}

results_df <- rbindlist(results, idcol = "rep")
