
### Mediation - Adjusted - Abuse-NSSI (total) #######

fulldata<-read.csv('fulldata_recode.csv')

fulldata<-fulldata%>%
  dplyr::select(-id_nspn,-nssi_cat_type,-ethnic_final)

fulldata[,c(4:6,8:9,39)]<-scale(fulldata[,c(4:6,8:9,39)],center=TRUE, scale=TRUE)
fulldata[,c(7,10:38,40:45)]<-0.5*scale(fulldata[,c(7,10:38,40:45)],center=TRUE, scale=TRUE)

fulldata$nssi_binary_t2<-as.numeric(fulldata$nssi_binary_t2)

modfile <- tempfile()
writeLines("
model{
# Prior distributions
intm ~ dt(0, 1/10^2, 1) # prior for the intercept M
inty ~ dnorm(0,0.16)  # prior for the intercept Y

a ~ dnorm(0,0.16)  # prior for for a
b ~ dnorm(0,0.16) # prior for b
c ~ dnorm(0,0.16) # prior for c

beta_age ~ dnorm(0,0.16)
beta_eth ~ dnorm(0,0.16)
beta_loi ~ dnorm(0,0.16)
beta_beh ~ dnorm(0,0.16)
beta_apsd ~ dnorm(0,0.16)
beta_prosoc ~ dnorm(0,0.16)
beta_negemot ~ dnorm(0,0.16)
beta_wemwbs ~ dnorm(0,0.16)
beta_bis ~ dnorm(0,0.16)
beta_fad ~ dnorm(0,0.16)
beta_cfq ~ dnorm(0,0.16)

ya ~ dnorm(0,0.16)
yb ~ dnorm(0,0.16)
yc ~ dnorm(0,0.16)
yd ~ dnorm(0,0.16)
ye ~ dnorm(0,0.16)
yf ~ dnorm(0,0.16)
yg ~ dnorm(0,0.16)
yh ~ dnorm(0,0.16)
yi ~ dnorm(0,0.16)

tau.em ~ dgamma(.01, .01); # prior for the error precision M
var_m <- 1/tau.em # residual variance of M
tau.ey ~ dgamma(.01, .01); # prior for the error precision Y
var_y <- 1/tau.ey # residual variance of Y

# Potential outcomes estimators
indirect_eff <- a*b
total<- c + (a*b)

  fit <- sum(res[])
  fit.new <- sum(res.new[])

for(i in 1:n){

      # posterior predictive
  res[i]<-nssi_binary_t2[i] - logit(y.prime[i])
  
  nssi_binary_t2_rep[i] ~ dbern(y.prime[i])
  
  res.new[i] <- nssi_binary_t2_rep[i] - logit(y.prime[i])

m.prime[i] <- intm + a*abuse_total[i] +beta_age*agehqpdone[i]+
beta_loi*loitot_3cat[i]+beta_beh*behtot[i]+
beta_negemot*cads_negemot[i]+beta_wemwbs*wemwbs_total[i]+beta_bis*bistotal[i]+beta_fad*fadtotal[i]+
beta_cfq*cfqtotal[i]

logit(y.prime[i]) <- inty + c*abuse_total[i] +  b*leq_total_45yes[i] + ya*agehqpdone[i]+
yc*loitot_3cat[i]+yd*behtot[i]+ye*cads_negemot[i]+yf*wemwbs_total[i]+yg*bistotal[i]+
yh*fadtotal[i]+yi*cfqtotal[i]

abuse_total[i] ~ dnorm(0,0.16)
ethnic_new[i] ~ dnorm(0,0.16)
loitot_3cat[i] ~ dnorm(0,0.16)
behtot[i] ~ dnorm(0,0.16)
apsdtotal[i] ~ dnorm(0,0.16)
cads_prosoc[i] ~ dnorm(0,0.16)
cads_negemot[i] ~ dnorm(0,0.16)
wemwbs_total[i] ~ dnorm(0,0.16)
bistotal[i] ~ dnorm(0,0.16)
fadtotal[i] ~ dnorm(0,0.16)
cfqtotal[i] ~ dnorm(0,0.16)

# conditional distributions of m and y
leq_total_45yes[i] ~ dnorm(m.prime[i], tau.em)
nssi_binary_t2[i] ~ dbern(y.prime[i])

} }", con=modfile)

data<-list(nssi_binary_t2=fulldata$nssi_binary_t2,
           abuse_total=fulldata$abuse_total,
           agehqpdone=fulldata$agehqpdone,
          leq_total_45yes=fulldata$leq_total_45yes,
          loitot_3cat=fulldata$loitot_3cat,
          behtot=fulldata$behtot,
          cads_negemot=fulldata$cads_negemot,
          wemwbs_total=fulldata$wemwbs_total,
          bistotal=fulldata$bistotal,
          fadtotal=fulldata$fadtotal,
          cfqtotal=fulldata$cfqtotal,
          n = nrow(fulldata))

inits <- function(){
  list("a" = c(0.5), "b" = c(0.5), "c" = c(0.5),
       "beta_eth" = c(1),"beta_age" = c(0.5),"beta_loi" = c(0.5),"beta_beh" = c(0.5),
       "beta_apsd" = c(0.5),"beta_prosoc" = c(0.5),"beta_negemot" = c(0.5),"beta_wemwbs" = c(0.5),
       "beta_bis" = c(0.5),"beta_fad" = c(0.5),"beta_cfq" = c(0.5))  ## Note that we're giving inits to theta, not theta1
}

parameters <-c("a","b","c","indirect_eff","total","fit","fit.new")

fit <- jags(data = data, inits=inits,parameters.to.save=parameters, n.chains = 2, n.iter = 20000, 
            n.burnin = 1000, model.file = modfile)
fit

pp.check(fit,observed='fit',simulated='fit.new',
         main="Posterior Predictive Check for Abuse (total)-NSSI (total)",cex.main=0.9,cex.lab=1.0)



