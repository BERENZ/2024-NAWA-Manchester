library(nonprobsvy)
library(survey)
library(ggplot2)
library(data.table)
library(sampling)

data(diamonds)
set.seed(123)

n_b <- 1000 ## probability sample
N <- nrow(diamonds)
true <- mean(diamonds$price)

diamonds$carat_q <- cut(diamonds$carat, 
                        c(0, quantile(diamonds$carat, probs=c(0.25,0.5,0.75)), Inf),
                        include.lowest = T, 
                        right = T)

diamonds$carat_d <- cut(diamonds$carat, 
                        c(0, quantile(diamonds$carat, probs=seq(0.1,0.9,0.1)), Inf),
                        include.lowest = T, 
                        right = T)

diamonds$carat_nr <- as.numeric(diamonds$carat_q)
n_boots <- 50
n_resp <- 50
results <- list()

for (b in 1:n_resp) {
  set.seed(b)
  print(b)
  data_prob <- diamonds[sample(1:N, n_b), ]
  data_prob$w <- N/n_b
  data_prob_svy <- svydesign(ids = ~1, weights = ~ w, data = data_prob)
  
  pr <- plogis(-3.5 + 0.5*diamonds$carat_nr)
  data_nonprob <- setDT(diamonds[rbinom(N, 1, pr) == 1, ])
  
  nonprob_est <- nonprob(selection = ~ carat_q,
                         target = ~ price,
                         data = data_nonprob,
                         svydesign = data_prob_svy,
                         control_selection = controlSel(h=1, est_method_sel = "gee"))
  
  
  w_cal <- calib(Xs = model.matrix(~ carat_d, data_nonprob), 
                 d = nonprob_est$weights,
                 total = c(`(Intercept)`=sum(weights(data_prob_svy)),
                           svytotal(~carat_d, data_prob_svy)[-1]),
                 method = "linear")
  
  data_nonprob[, w:= nonprob_est$weights]
  data_nonprob[, cal_w:= w_cal*nonprob_est$weights]
  
  nonprob_est_boot <- nonprob(selection = ~ carat_q,
                              target = ~ price,
                              data = data_nonprob,
                              svydesign = data_prob_svy,
                              control_selection = controlSel(h = 1, est_method_sel = "gee"),
                              control_inference = controlInf(var_method = "bootstrap", num_boot = n_boots))
  
  data_prob_svy_boot <- as.svrepdesign(data_prob_svy, type = "subbootstrap", replicates = n_boots)
  
  price_boot <- numeric(n_boots)
  
  for (r in 1:n_boots) {
    
    data_nonprob_rr <- data_nonprob[sample(1:nrow(data_nonprob), nrow(data_nonprob), replace = T), ]
    
    totals_rr <- model.matrix(~ carat_q,  data=data_prob)*data_prob_svy_boot$repweights$weights[, r]*data_prob$w
    totals_q_rr <- model.matrix(~ carat_d,  data=data_prob)*data_prob_svy_boot$repweights$weights[, r]*data_prob$w
    
    nonprob_est_rr <- nonprob(selection = ~ carat_q,
                              target = ~ price,
                              data = data_nonprob_rr,
                              pop_totals = colSums(totals_rr),
                              pop_size = N,
                              control_selection = controlSel(h=1, est_method_sel = "gee"))
    
    w_cal <- calib(Xs = model.matrix(~ carat_d, data_nonprob), 
                   d = nonprob_est_rr$weights,
                   total = colSums(totals_q_rr),
                   method = "linear")
    
    price_boot[r] <- weighted.mean(data_nonprob_rr$price, nonprob_est_rr$weights*w_cal)
    
  }
  
  results[[b]] <- data_nonprob[, .(true = true, naive=mean(price),
                                   naive=mean(price),
                                   ipw=weighted.mean(price,w), 
                                   ipw_l = nonprob_est_boot$confidence_interval[1,1],
                                   ipw_u = nonprob_est_boot$confidence_interval[1,2],
                                   ipw_cal=weighted.mean(price, cal_w),
                                   ipw_cal_l=weighted.mean(price,cal_w)-1.96*sd(price_boot),
                                   ipw_cal_u=weighted.mean(price,cal_w)+1.96*sd(price_boot))]
}

results_df <- rbindlist(results)
results_df[, .(m1=sd(ipw),m2=sd(ipw_cal))]
results_df[, .(ipw_cr = mean(ipw_l < true & ipw_u > true), 
               ipw_cal_cr = mean(ipw_cal_l < true & ipw_cal_u > true))]

