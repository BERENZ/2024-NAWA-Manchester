library(simex) ## only confusion matrix is provided
library(nonprobsvy)
library(data.table)
library(misclassGLM)
library(rootSolve)

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
gamma_age <- matrix(c(0.85, 0.10, 0.05, 
                      0.20, 0.70, 0.10, 
                      0.05, 0.15, 0.80), ncol = 3)

colnames(gamma_age) <- levels(pop_data$age)
colnames(gamma_ethnic) <- levels(pop_data$ethnic)

dd <- simex::misclass(pop_data[, .(age, ethnic)], 
                      list(age = gamma_age, ethnic = gamma_ethnic),
                      k = 1)
pop_data[, age_m := dd$age]
pop_data[, ethnic_m := dd$ethnic]


pop_data[age_m == "Young", young:=0.85]
pop_data[age_m == "Young", middle:=0.10]
pop_data[age_m == "Young", old:=0.05]

pop_data[age_m == "Middle-aged", young:=0.2]
pop_data[age_m == "Middle-aged", middle:=0.7]
pop_data[age_m == "Middle-aged", old:=0.1]

pop_data[age_m == "Old", young:=0.05]
pop_data[age_m == "Old", middle:=0.15]
pop_data[age_m == "Old", old:=0.80]

pop_data[ethnic_m == "Native", native:=0.7]
pop_data[ethnic_m == "Native", nonnative:=0.3]
pop_data[ethnic_m == "Nonnative", native:=0.05]
pop_data[ethnic_m == "Nonnative", nonnative:=0.95]


reglog <- function(par, y, x) {
  pr <- as.numeric(plogis(x %*% par))
  ll <- dbinom(y, 1, pr, log = T)
  return(-sum(ll))
}

optim(par = rep(0, 4), fn = reglog, y = pop_data$NEP, x = model.matrix(~age + ethnic, data = pop_data))$par


# two levels -----------------------------------------------------------

glm(NEP ~ ethnic + age, data = pop_data, family = binomial)$coef
start <- glm(NEP ~ age_m + ethnic, data = pop_data, family = binomial)$coef




glm(NEP ~ age + ethnic, data = pop_data, family = binomial)$coef

start <- glm(NEP ~ age + ethnic_m, data = pop_data, family = binomial)$coef

optim(par = start, 
      fn = reglog_m, 
      y = pop_data$NEP, 
      x = model.matrix(~age, data = pop_data), 
      probs = as.matrix(pop_data[, c("native", "nonnative")]),
      m = matrix(c(0, 1), nrow = 2),
      method = "BFGS") -> res

## check gradient

numDeriv::grad(func = function(par) reglog_m(par, 
                                             y = pop_data$NEP,
                                             x = model.matrix(~age, data = pop_data), 
                                             probs = as.matrix(pop_data[, c("native", "nonnative")]),
                                             m = matrix(c(0, 1), nrow = 2)), 
               res$par)


glm(NEP ~ ethnic + age, data = pop_data, family = binomial)$coef
start <- glm(NEP ~ ethnic + age_m, data = pop_data, family = binomial)$coef

dd <- nnet::multinom(age ~ age_m + ethnic, data = pop_data)
probs <- predict(dd, newdata = pop_data, type = "probs")

optim(par = start, 
      fn = reglog_m, 
      y = pop_data$NEP, 
      x = model.matrix(~native, data = pop_data), 
      #probs = as.matrix(pop_data[, c("young", "middle", "old")]),
      probs = probs,
      m = matrix(c(0, 1, 0, 0, 0, 1), nrow = 3),
      #m = matrix(c(0, 1), nrow = 2),
      method = "BFGS", 
      control = list(trace = 1)) -> res

numDeriv::grad(func = function(par) reglog_m(par, 
                                             y = pop_data$NEP,
                                             x = model.matrix(~native, data = pop_data),
                                             probs = probs,
                                             m = matrix(c(0, 1, 0, 0, 0, 1), nrow = 3)), 
               res$par)
## gradient


