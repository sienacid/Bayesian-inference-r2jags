#Clear the global environment
mypackages <- c("tidyverse","rjags","posterior","bayesplot","R2jags","MCMCvis",
                "mcmcplots","standardize","jagsUI","rstanarm","performance",
                "loo","rstantools","CalvinBayes","bayesrules","ggplot2",
                "ggstance","ggformula","superdiag",
                "dplyr","plyr","skimr","corrplot","Hmisc","xlsx","openxlsx",
                "naniar","knitr","kableExtra","finalfit")
for (p in mypackages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

####Total cases - ABUSE #########
fulldata<-read.csv('fulldata_recode.csv')

fulldata<-fulldata%>%
  dplyr::select(-id_nspn,-nssi_cat_type,-ethnic_final)

#scale data
fulldata[,c(4:6,8:9,39)]<-scale(fulldata[,c(4:6,8:9,39)],center=TRUE, scale=TRUE)
fulldata[,c(7,10:38,40:45)]<-0.5*scale(fulldata[,c(7,10:38,40:45)],center=TRUE, scale=TRUE)

model1 <- tempfile()

writeLines("

model {

for (i in 1:n) {

  # Model of interest
  nssi_binary_t2[i] ~ dbern(p[i])
  
    # posterior predictive
  res[i]<-nssi_binary_t2[i] - logit(p[i])
  
  nssi_binary_t2_rep[i] ~ dbern(p[i])
  
  res.new[i] <- nssi_binary_t2_rep[i] - logit(p[i])
  
  logit(p[i]) <- alpha + beta_abutot*abuse_total[i]+beta_le*leq_total_45yes[i]+ 
  beta_sex*sex[i]+beta_agehqpdone*agehqpdone[i]+
  beta_beh*behtot[i]+beta_wemwbs*wemwbs_total[i]+
  beta_bis*bistotal[i]+
  inter1 * abuse_total[i] * leq_total_45yes[i]
  
  # Model for missing covariates
  abuse_total[i] ~ dnorm (abumu[i], jprec)
  abumu[i]<- aj+bj*leq_total_45yes[i]+cj*sex[i]+
  dj*agehqpdone[i]+ej*behtot[i]+fj*wemwbs_total[i]+
  gj*bistotal[i]
  
  leq_total_45yes[i] ~ dnorm (leqmu[i], lprec)
  leqmu[i]<- al+bl*sex[i]+
  cl*agehqpdone[i]+dl*behtot[i]+el*wemwbs_total[i]+
  fl*bistotal[i]
  
  behtot[i] ~ dnorm (behmu[i], bprec)
  behmu[i]<- ab+bb*sex[i]+
  cb*agehqpdone[i]+db*wemwbs_total[i]+eb*bistotal[i]

  wemwbs_total[i] ~ dnorm (wemwbsmu[i], sprec)
  wemwbsmu[i]<- as+bs*sex[i]+
  cs*agehqpdone[i]+ds*bistotal[i]
  
  bistotal[i] ~ dnorm (bismu[i], fprec)
  bismu[i]<- af+bf*sex[i]+cf*agehqpdone[i]
  
}
 
 # Priors
 alpha ~ dt(0, 1/10^2, 1)
 beta_abutot~ dt(0, 1/2.5^2, 1)
 beta_le ~ dt(0, 1/2.5^2, 1)
 beta_agehqpdone ~ dt(0, 1/2.5^2, 1)
 beta_sex  ~ dt(0, 1/2.5^2, 1)
 beta_beh ~ dt(0, 1/2.5^2, 1)
 beta_bis ~ dt(0, 1/2.5^2, 1)
 beta_wemwbs ~ dt(0, 1/2.5^2, 1)

 inter1 ~ dt(0, 1/2.5^2, 1)

 or_abutot<- exp(beta_abutot)
 or_le<-exp(beta_le)
 or_agehqpdone <- exp(beta_agehqpdone)
 or_sex <- exp(beta_sex)
 or_beh <- exp(beta_beh)
 or_bis<-exp(beta_bis)
 or_wemwbs<-exp(beta_wemwbs)

 or_inter1<- exp(inter1)

 aj ~ dt(0, 1/2.5^2, 1)
 bj ~ dt(0, 1/2.5^2, 1)
 cj ~ dt(0, 1/2.5^2, 1)
 dj ~ dt(0, 1/2.5^2, 1)
 ej ~ dt(0, 1/2.5^2, 1)
 fj ~ dt(0, 1/2.5^2, 1)
 gj ~ dt(0, 1/2.5^2, 1)
 hj ~ dt(0, 1/2.5^2, 1)

 al ~ dt(0, 1/2.5^2, 1)
 bl ~ dt(0, 1/2.5^2, 1)
 cl ~ dt(0, 1/2.5^2, 1)
 dl ~ dt(0, 1/2.5^2, 1)
 el ~ dt(0, 1/2.5^2, 1)
 fl ~ dt(0, 1/2.5^2, 1)
 gl ~ dt(0, 1/2.5^2, 1)

 ab ~ dt(0, 1/2.5^2, 1)
 bb ~ dt(0, 1/2.5^2, 1)
 cb ~ dt(0, 1/2.5^2, 1)
 db ~ dt(0, 1/2.5^2, 1)
 eb ~ dt(0, 1/2.5^2, 1)
 fb ~ dt(0, 1/2.5^2, 1)


 as ~ dt(0, 1/2.5^2, 1)
 bs ~ dt(0, 1/2.5^2, 1)
 cs ~ dt(0, 1/2.5^2, 1)
 ds ~ dt(0, 1/2.5^2, 1)
 es ~ dt(0, 1/2.5^2, 1)
 
 af ~ dt(0, 1/2.5^2, 1)
 bf ~ dt(0, 1/2.5^2, 1)
 cf ~ dt(0, 1/2.5^2, 1)
 df ~ dt(0, 1/2.5^2, 1)
 
 aa ~ dt(0, 1/2.5^2, 1)
 ba ~ dt(0, 1/2.5^2, 1)
 ca ~ dt(0, 1/2.5^2, 1)
 

 jprec  ~ dgamma(0.001,0.001)
 lprec  ~ dgamma(0.001,0.001)
 bprec  ~ dgamma(0.001,0.001)
 sprec  ~ dgamma(0.001,0.001)
 fprec  ~ dgamma(0.001,0.001)
 aprec  ~ dgamma(0.001,0.001)
 
 #Derived parameters
 fit <- sum(res[])
 fit.new <- sum(res.new[])

}", con=model1)

dat<-list(nssi_binary_t2=pred_merge$nssi_binary_t2,
          abuse_total=pred_merge$abuse_total,
          leq_total_45yes=pred_merge$leq_total_45yes,
          sex=pred_merge$sex,
          agehqpdone = pred_merge$agehqpdone,
          behtot = pred_merge$behtot,
          wemwbs_total = pred_merge$wemwbs_total,
          bistotal = pred_merge$bistotal,
          n = nrow(pred_merge))

ini <- list(list(alpha=0, beta_abutot=0, beta_agehqpdone=0, beta_sex=0,
                 beta_wemwbs=0, beta_le=0,beta_beh=0,
                 beta_bis=0),
            list(alpha=1, beta_abutot=1, beta_agehqpdone=1, beta_sex=1,
                 beta_wemwbs=1, beta_le=1,beta_beh=1,
                 beta_bis=1))

params <- c("alpha","or_abutot","or_le","or_agehqpdone", "or_sex",
            "or_wemwbs","or_beh",
            "or_bis","or_inter1","fit","fit.new")
#Run analysis
fit1 <- jags(data = dat, parameters.to.save=params, n.chains = 2, 
             n.iter = 15000, n.adapt = 500,
             n.burnin = 1000, model.file = model1, n.thin = 1)

fit1

#Posterior predictive check plot
pp.check(fit1,observed='fit',simulated='fit.new',
         main="Posterior Predictive Check for Abuse(total) - NSSI(total)",cex.main=0.9,cex.lab=1.0)

traceplot(fit1)


####New onset - abuse #######

t1.t2.onset<- read.csv("t1.t2.onset.csv")

count(t1.t2.onset$nssi_binary_newt2==1)
#78 cases

t1.t2.onset<- dplyr::select(t1.t2.onset,-event)
t1.t2.onset<- dplyr::select(t1.t2.onset,-id_nspn)

t1.t2.onset[,c(4:6,8:9,39)]<-scale(t1.t2.onset[,c(4:6,8:9,39)],center=TRUE, scale=TRUE)
t1.t2.onset[,c(7,10:38)]<-0.5*scale(t1.t2.onset[,c(7,10:38)],center=TRUE, scale=TRUE)

model2 <- tempfile()

writeLines("model {
for (i in 1:n) {

  # Model of interest
  nssi_binary_newt2[i] ~ dbern(p[i])
  
      # posterior predictive
  res[i]<-nssi_binary_newt2[i] - logit(p[i])
  
  nssi_binary_newt2_rep[i] ~ dbern(p[i])
  
  res.new[i] <- nssi_binary_newt2_rep[i] - logit(p[i])
  
  logit(p[i]) <- alpha + beta_abutot*abuse_total[i]+beta_le*leq_total_45yes[i]+ 
  beta_sex*sex[i]+beta_agehqpdone*agehqpdone[i]+
  beta_beh*behtot[i]+beta_wemwbs*wemwbs_total[i]+ 
  beta_bis*bistotal[i]+
  inter1 * abuse_total[i] * leq_total_45yes[i]
  
  # Model for missing covariates
  abuse_total[i] ~ dnorm (abumu[i], jprec)
  abumu[i]<- aj+bj*leq_total_45yes[i]+cj*sex[i]+
  dj*agehqpdone[i]+ej*behtot[i]+fj*wemwbs_total[i]+gj*bistotal[i]
  
  leq_total_45yes[i] ~ dnorm (leqmu[i], lprec)
  leqmu[i]<- al+bl*sex[i]+
  cl*agehqpdone[i]+dl*behtot[i]+el*wemwbs_total[i]+fl*bistotal[i]
  
  behtot[i] ~ dnorm (behmu[i], bprec)
  behmu[i]<- ab+bb*sex[i]+cb*agehqpdone[i]+db*wemwbs_total[i]+
  eb*bistotal[i]

  wemwbs_total[i] ~ dnorm (wemwbsmu[i], sprec)
  wemwbsmu[i]<- as+bs*sex[i]+cs*agehqpdone[i]+
  ds*bistotal[i]
  
  bistotal[i] ~ dnorm (bismu[i], iprec)
  bismu[i]<- ai+bi*sex[i]+ci*agehqpdone[i]

 
}
 
 # Priors
 alpha ~ dt(0, 1/10^2, 1)
 beta_abutot~ dt(0, 1/2.5^2, 1)
 beta_le ~ dt(0, 1/2.5^2, 1)
 beta_agehqpdone ~ dt(0, 1/2.5^2, 1)
 beta_sex  ~ dt(0, 1/2.5^2, 1)
 beta_beh ~ dt(0, 1/2.5^2, 1)
 beta_bis ~ dt(0, 1/2.5^2, 1)
 beta_wemwbs ~ dt(0, 1/2.5^2, 1)

 inter1 ~ dt(0, 1/2.5^2, 1)

 or_abutot<- exp(beta_abutot)
 or_le<-exp(beta_le)
 or_agehqpdone <- exp(beta_agehqpdone)
 or_sex <- exp(beta_sex)
 or_beh <- exp(beta_beh)
 or_bis<-exp(beta_bis)
 or_wemwbs<-exp(beta_wemwbs)
 or_inter1<- exp(inter1)

 aj ~ dt(0, 1/2.5^2, 1)
 bj ~ dt(0, 1/2.5^2, 1)
 cj ~ dt(0, 1/2.5^2, 1)
 dj ~ dt(0, 1/2.5^2, 1)
 ej ~ dt(0, 1/2.5^2, 1) 
 fj ~ dt(0, 1/2.5^2, 1)
 gj ~ dt(0, 1/2.5^2, 1)


 al ~ dt(0, 1/2.5^2, 1)
 bl ~ dt(0, 1/2.5^2, 1)
 cl ~ dt(0, 1/2.5^2, 1)
 dl ~ dt(0, 1/2.5^2, 1)
 el ~ dt(0, 1/2.5^2, 1)
 fl ~ dt(0, 1/2.5^2, 1)

 ab ~ dt(0, 1/2.5^2, 1)
 bb ~ dt(0, 1/2.5^2, 1)
 cb ~ dt(0, 1/2.5^2, 1)
 db ~ dt(0, 1/2.5^2, 1)
 eb ~ dt(0, 1/2.5^2, 1)

 as ~ dt(0, 1/2.5^2, 1)
 bs ~ dt(0, 1/2.5^2, 1)
 cs ~ dt(0, 1/2.5^2, 1)
 ds ~ dt(0, 1/2.5^2, 1)

 ai ~ dt(0, 1/2.5^2, 1)
 bi ~ dt(0, 1/2.5^2, 1)
 ci ~ dt(0, 1/2.5^2, 1)
 

 jprec  ~ dgamma(0.01,0.01)
 lprec  ~ dgamma(0.01,0.01)
 bprec  ~ dgamma(0.01,0.01)
 sprec  ~ dgamma(0.01,0.01)
 iprec  ~ dgamma(0.01,0.01)
 
 fit <- sum(res[])
 fit.new <- sum(res.new[])
 
}", con=model2)

dat_new<-list(nssi_binary_newt2=t1.t2.onset$nssi_binary_newt2,
          abuse_total=t1.t2.onset$abuse_total,
          leq_total_45yes=t1.t2.onset$leq_total_45yes,
          sex=t1.t2.onset$sex,
          agehqpdone = t1.t2.onset$agehqpdone,
          behtot = t1.t2.onset$behtot,
          wemwbs_total = t1.t2.onset$wemwbs_total,
          bistotal = t1.t2.onset$bistotal,
          n = nrow(t1.t2.onset))

ini_new <- list(list(alpha=0, beta_abutot=0, beta_agehqpdone=0, beta_sex=0, 
                 beta_wemwbs=0, beta_le=0,beta_beh=0,beta_bis=0),
            list(alpha=1, beta_abutot=1, beta_agehqpdone=1, beta_sex=1, 
                 beta_wemwbs=1, beta_le=1,beta_beh=1,beta_bis=1))

params <- c("alpha","or_abutot","or_le","or_agehqpdone", "or_sex",
            "or_wemwbs","or_beh",
            "or_bis","or_inter1","fit","fit.new")

#Run analysis
fit2 <- jags(data = dat_new, inits=ini_new,parameters.to.save=params, n.chains = 2, 
             n.iter = 20000, n.adapt = 500,
             n.burnin = 1000, model.file = model2, n.thin = 1)

fit2

#Posterior predictive check plot
pp.check(fit2,observed='fit',simulated='fit.new',
         main="Posterior Predictive Check for Abuse(total) - NSSI (new onset)",cex.main=0.9,cex.lab=1.0)

traceplot(fit2)
