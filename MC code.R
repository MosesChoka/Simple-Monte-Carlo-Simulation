sigma_0=5
sigma_1=0
sigma_2=0
sigma_3=-1
library(pacman)
install.packages("pacman")
install.packages("dplyr")
install.packages("fixest")
install.packages("tidyverse")
pacman::p_load(data.table,fixest,stargazer,dplyr,magrittr)
set.seed(1)
n=601
N=1000
slope1=rep(0,N)
slope2=rep(0,N)
slope3=rep(0,N)
intercept_DT=rep(0,N)
h= for (i in 1:N){
  Ui=rnorm(n, mean = 0, sd = 1)
  X1=rnorm(n, mean = 0, sd = 1)
  X2=rnorm(n, mean = 0, sd = 1)
  X3=rnorm(n, mean = 0, sd = 1)
  y=sigma_0 + sigma_1*X1 + sigma_2*X2 + sigma_3*X3 + Ui
  data_i= data.table(Y=y, X1=X1, X2=X2, X3=X3)
  ols_i=fixest::feols(data=data_i,Y~X1+X2+X3)
  slope1[i]= ols_i$coefficients[2]
  slope2[i]=ols_i$coefficients[3]
  slope3[i]=ols_i$coefficients[4]
  intercept_DT[i]=ols_i$coefficient[1]
}

summaryParam(h,detail = TRUE, alpha=0.05)
estimates_DT <- data.table(sigma_1 = slope1, sigma_2=slope2 ,sigma_3=slope3,sigma_0= intercept_DT)
stargazer(estimates_DT[, c("sigma_1","sigma_2", "sigma_3", "sigma_0")], type = "text")



install.packages("lavaan")
install.packages("simsem")
pacman::p_load(lavaan,simsem)
library(simsem)
library(lavaan)
analysis.model=lm(y~ X1 + X2 + X3)
pop.model=(y =5 + 0*X1 + 0*X2 -1*X3)
analysis3=sim(nRep = 1000,
              model=analysis.model,n=601,
              generate = pop.model, lavaanfun="sem",
              set.seed(1))
