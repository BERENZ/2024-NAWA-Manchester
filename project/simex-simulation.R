library(simex)
library(nonprobsvy)

set.seed(123)
N <- 100000
X1 <- sample(c("S", "M", "L"), N, prob = c(0.7, 0.2, 0.1), replace = T)
X1 <- factor(X1, c("M", "S", "D"))
X2 <- rbinom(N, 1, prob = 0.8)
X2 <- factor(X2, 0:1, c("Priv", "Pub"))
X3 <- rbinom(N, 1, prob = 0.5)
Y1 <- rbinom(N, 1, plogis(-1 + 0.5*(X1 == "D") + 0.5*(X2=="Pub") + -0.3*X3))
pi_A <- plogis(-2 - 2*(X2 == "Priv"))

pop_data <- data.frame(X1, X2, X3, Y1, pi_A)
n_b <- 1000 ## probability sample

results <- list()

for (b in 1:50) {
  set.seed(b)
  print(b)
  ## probability sample
  sample_b <- pop_data[sample(1:N, n_b), ]
  sample_b$sample_w <- N/n_b
  sample_b_svy <- svydesign(ids=~1, weights = ~ sample_w, data=sample_b)
  
  ## nonprobability sample
  sample_a <- pop_data[rbinom(N, 1, pop_data$pi_A) == 1, ]
  
  # without error -----------------------------------------------------------
  
  est_ipw <- nonprob(selection = ~ X1 + X2 + X3,
                     data = sample_a,
                     svydesign = sample_b_svy,
                     target = ~ Y1, 
                     control_selection = controlSel(h = 1, est_method_sel = "gee"))
  
  est_mi <- nonprob(outcome = Y1 ~ X1 + X2 + X3,
                    data = sample_a,
                    svydesign = sample_b_svy, 
                    family_outcome = "binomial")
  
  ## with errors
  sample_a_with_error <- sample_a
  
  X1_conf <- matrix(c(0.8, 0.1, 0.1, 
                      0.1, 0.7, 0.2, 
                      0.05, 0.15, 0.8), nrow = 3, byrow = T)
  
  X2_conf <- matrix(c(0.8, 0.2, 
                      0.15, 0.85), byrow = T, ncol = 2)
  
  rownames(X1_conf) <- colnames(X1_conf) <- c("M", "S", "D")
  rownames(X2_conf) <- colnames(X2_conf) <- c("Pub", "Priv")
  
  class_erros <- misclass(data.frame(X1 = sample_a_with_error$X1,
                                     X2 = sample_a_with_error$X2), 
                          list(X1=X1_conf, X2=X2_conf), k = 1)
  
  sample_a_with_error$X1  <- class_erros$X1
  sample_a_with_error$X2  <- class_erros$X2
  
  est_ipw_err <- nonprob(selection = ~ X1 + X2 + X3,
                         data = sample_a_with_error,
                         svydesign = sample_b_svy,
                         target = ~ Y1, 
                         control_selection = controlSel(h = 1, est_method_sel = "gee"))
  
  est_mi_err <- nonprob(outcome = Y1 ~ X1 + X2 + X3,
                        data = sample_a_with_error,
                        svydesign = sample_b_svy, 
                        family_outcome = "binomial")
  
  est_mi_err_naive <- glm(Y1 ~ X1 + X2 + X3, family = binomial, x = TRUE, y = TRUE,
                          data = sample_a_with_error)
  
  res_mi <- simex::mcsimex(model = est_mi_err_naive,
                           mc.matrix = list(X1 = X1_conf, X2 = X2_conf),
                           SIMEXvariable = c("X1", "X2"),
                           asymptotic = FALSE,
                           fitting.method	= "quadratic",
                           B = 100)
  
  
  results[[b]] <- data.frame(b, naive = mean(sample_a$Y1),
                             ipw = est_ipw$output$mean[1], mi = est_mi$output$mean[1],
                             ipw_err = est_ipw_err$output$mean[1], mi_err = est_mi_err$output$mean[1],
                             mi_err_cor = weighted.mean(predict(res_mi, sample_b_svy$variables, type = "response"), 
                                                        weights(sample_b_svy)))
  

}

results_df <- rbindlist(results)

sqrt((colMeans(results_df[,-1]) - mean(Y1))^2 + apply(results_df[,-1], 2, var))

boxplot(results_df[,-1]-mean(Y1))
abline(a=0,b=0,h=1,col='red')
