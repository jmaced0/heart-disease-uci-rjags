#install.packages("BBmisc")
#install.packages("rjags")
#install.packages("MLmetrics"
#install.packages("mccr")
library("BBmisc")
library("rjags")
library("MLmetrics")
library("mccr")

n_iter = 1e5
set.seed(48)

#model 1

dat <- read.csv("~/heart.csv")
names(dat)[names(dat) == 'ï..age'] <- "age"

#normalize continuous variables
dat$age = normalize(
  dat$age,
  method = "standardize",
  margin = 1L,
  on.constant = "quiet"
)
dat$trestbps = normalize(
  dat$trestbps,
  method = "standardize",
  margin = 1L,
  on.constant = "quiet"
)
dat$chol = normalize(
  dat$chol,
  method = "standardize",
  margin = 1L,
  on.constant = "quiet"
)
dat$thalach = normalize(
  dat$thalach,
  method = "standardize",
  margin = 1L,
  on.constant = "quiet"
)

#shuffle rows
rows <- sample(nrow(dat))
dat <- dat[rows, ]

#split data
smp_size <- floor(0.7 * nrow(dat))
train_ind <- sample(seq_len(nrow(dat)), size = smp_size)

test <- dat[-train_ind, ]
dat <- dat[train_ind, ]

mod1_string = " model {
for (i in 1:length(target)) {
target[i] ~ dbern(p[i])
logit(p[i]) = int + b[1]*age[i] + b[2]*sex[i] + b[3]*cp[i] + b[4]*trestbps[i] + b[5]*chol[i] + b[6]*fbs[i] + b[7]*restecg[i] + b[8]*thalach[i] + b[9]*exang[i] + b[10]*oldpeak[i] + b[11]*slope[i] + b[12]*ca[i] + b[13]*thal[i]
}
int ~ dnorm(0.0, 1.0/25.0)
for (j in 1:13) {
b[j] ~ ddexp(0.0, sqrt(2.0)) # has variance 1.0
}
} "


data_jags = as.list(dat)

params = c("int", "b")

mod1 = jags.model(textConnection(mod1_string),
                  data = data_jags,
                  n.chains = 3)
update(mod1, 1e4)

mod1_sim = coda.samples(model = mod1,
                        variable.names = params,
                        n.iter = n_iter)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))

## convergence diagnostics
#plot(mod1_sim, ask=FALSE, trace = FALSE)

#gelman.diag(mod1_sim)
#autocorr.diag(mod1_sim)
#autocorr.plot(mod1_sim)
#effectiveSize(mod1_sim)

# calculate DIC
dic1 = dic.samples(mod1, n.iter = n_iter / 5)

# prediction

pm1_coef = colMeans(mod1_csim)
pm1_Xb = pm1_coef["int"] + as.matrix(test[, c(1, 2, 3, 4, 5, 6,  7, 8, 9, 10, 11, 12, 13)]) %*% pm1_coef[1:13]
phat1 = 1.0 / (1.0 + exp(-pm1_Xb))

th_inicial = min(phat1)+0.01
th_final = max(phat1)-0.01

y_pred1 = ifelse(phat1 > 0.67, 1, 0)
F1_model1 = F1_Score(test$target,y_pred1)
mccr1 = mccr(test$target,y_pred1) #Mathews correlation coefficient
final_th1 = 0.8

for(th in seq(th_inicial, th_final, 0.01)){
  y_pred = ifelse(phat1 > th, 1, 0)
  F1 = F1_Score(y_pred = y_pred,y_true =  test$target)
  Mccr = mccr(y_pred,test$target)
  if(Mccr > Mccr1){
    mccr1 = Mccr
    final_th1 = th
    F1_model1 = F1
    y_pred1 = ypred
  }
}

dic1
final_th1
(matrix1 = ConfusionMatrix(y_pred = y_pred1,y_true =  test$target))
(prec1 = Precision(y_pred = y_pred1,y_true =  test$target))
(acc1 = Accuracy(y_pred = y_pred1,y_true =  test$target))
(Rec1 = Recall(y_pred = y_pred1,y_true =  test$target))
F1_model1
mccr1
