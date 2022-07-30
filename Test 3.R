#Clear the global environment
rm(list = ls())
library(devtools)
install_github("sienacid/MPhil-PHS-Dissertation")

setwd("/Users/siennapan/OneDrive - University of Cambridge/Dissertation/NSPN")

######### Total cases Analysis - indiff and control################

pred_merge<-read.csv("ctq_merge_data.csv")

#mops<-read.csv('Sienna LE, MOPS, APQ.csv')
#mops<-mops%>%
#dplyr::select(id_nspn, event, mops_mat_abuse)%>%
#filter(event=='Home Questionnaire Pack 1')%>%
#dplyr::select(-event)

pred_merge<-pred_merge%>%
  dplyr::select(-id_nspn,-ethnic_final)

pred_merge<-as.data.frame(pred_merge)

pred_merge[,c(5:46)] <- scale(pred_merge[,c(5:46)],center=TRUE, scale=TRUE)

colMeans(pred_merge[,c(5:41)])
apply(pred_merge, 2, sd)

model1 <- tempfile()

writeLines("model {
for (i in 1:n) {

  # Model of interest
  nssi_binary_t2[i] ~ dbern(p[i])
  
  logit(p[i]) <- alpha + beta_contot*control_total[i]+beta_le*leq_total_45yes[i]+ 
  beta_sex*sex[i]+beta_agehqpdone*agehqpdone[i]+
  beta_beh*behtot[i]+beta_wemwbs*wemwbs_total[i]+ beta_fad*fadtotal[i]+
  beta_bis*bistotal[i]+beta_apsd*apsdtotal[i]+
  beta_prosoc*cads_prosoc[i]+beta_loi*loitot_3cat[i]+beta_negem*cads_negemot[i]+
  beta_cfq*cfqtotal[i]+
  inter1 * indiff_total[i] * leq_total_45yes[i]
  
  # Model for missing covariates
  indiff_total[i] ~ dnorm (indiffmu[i], jprec)
  indiffmu[i]<- aj+bj*leq_total_45yes[i]+cj*sex[i]+
  dj*imd[i]+ej*agehqpdone[i]+fj*behtot[i]+gj*wemwbs_total[i]+ hj*fadtotal[i]+
  ij*bistotal[i]+jj*apsdtotal[i]+kj*cads_daring[i]+
  lj*cads_prosoc[i]+mj*loitot_3cat[i]+nj*cads_negemot[i]+
  oj*cfqtotal[i]
  
  leq_total_45yes[i] ~ dnorm (leqmu[i], lprec)
  leqmu[i]<- al+bl*sex[i]+cl*imd[i]+
  dl*agehqpdone[i]+el*behtot[i]+fl*wemwbs_total[i]+gl*fadtotal[i]+
  hl*bistotal[i]+il*apsdtotal[i]+jl*cads_daring[i]+
  kl*cads_prosoc[i]+ll*loitot_3cat[i]+ml*cads_negemot[i]+
  nl*cfqtotal[i]
  
  imd[i] ~ dnorm (imdmu[i], cprec)
  imdmu[i]<- ac+bc*sex[i]+cc*agehqpdone[i]+dc*behtot[i]+
  ec*wemwbs_total[i]+ fc*fadtotal[i]+
  gc*bistotal[i]+hc*apsdtotal[i]+ic*cads_daring[i]+
  jc*cads_prosoc[i]+kc*loitot_3cat[i]+lc*cads_negemot[i]+
  mc*cfqtotal[i]
  
  behtot[i] ~ dnorm (behmu[i], bprec)
  behmu[i]<- ab+bb*sex[i]+cb*agehqpdone[i]+db*wemwbs_total[i]+ eb*fadtotal[i]+
  fb*bistotal[i]+gb*apsdtotal[i]+hb*cads_daring[i]+
  ib*cads_prosoc[i]+jb*loitot_3cat[i]+kb*cads_negemot[i]+
  lb*cfqtotal[i]

  wemwbs_total[i] ~ dnorm (selfmu[i], sprec)
  selfmu[i]<- as+bs*sex[i]+cs*agehqpdone[i]+
  ds*fadtotal[i]+es*bistotal[i]+
  fs*apsdtotal[i]+gs*cads_daring[i]+
  hs*cads_prosoc[i]+is*loitot_3cat[i]+js*cads_negemot[i]+
  ks*cfqtotal[i]
  
  fadtotal[i] ~ dnorm (fadmu[i], fprec)
  fadmu[i]<- af+bf*sex[i]+cf*agehqpdone[i]+
  df*bistotal[i]+
  ef*apsdtotal[i]+gf*cads_daring[i]+
  hf*cads_prosoc[i]+if*loitot_3cat[i]+jf*cads_negemot[i]+
  kf*cfqtotal[i]
  
  bistotal[i] ~ dnorm (bismu[i], iprec)
  bismu[i]<- ai+bi*sex[i]+ci*agehqpdone[i]+
  di*apsdtotal[i]+
  ei*cads_daring[i]+
  fi*cads_prosoc[i]+gi*loitot_3cat[i]+hi*cads_negemot[i]+
  ii*cfqtotal[i]
  
  cads_daring[i] ~ dnorm (daringmu[i], xprec)
  daringmu[i]<- ax+bx*sex[i]+cx*agehqpdone[i]+
  dx*apsdtotal[i]+
  ex*cads_prosoc[i]+fx*loitot_3cat[i]+gx*cads_negemot[i]+
  hx*cfqtotal[i]
  
  apsdtotal[i] ~ dnorm (apsdmu[i], wprec)
  apsdmu[i]<- aw+bw*sex[i]+cw*agehqpdone[i]+
  dw*cads_prosoc[i]+ew*loitot_3cat[i]+fw*cads_negemot[i]+
  gw*cfqtotal[i]
  
  cads_prosoc[i] ~ dnorm (prosocmu[i], tprec)
  prosocmu[i]<- at+bt*sex[i]+ct*agehqpdone[i]+dt*loitot_3cat[i]+et*cads_negemot[i]+
  ft*cfqtotal[i]
  
  loitot_3cat[i] ~ dnorm (loimu[i], aprec)
  loimu[i]<- aa+ba*sex[i]+ca*agehqpdone[i]+da*cads_negemot[i]+
  ea*cfqtotal[i]
  
  cads_negemot[i] ~ dnorm (negemmu[i], pprec)
  negemmu[i]<- ap+bp*sex[i]+cp*agehqpdone[i]+dp*cfqtotal[i]
  
  
  cfqtotal[i] ~ dnorm (cfqmu[i], oprec)
  cfqmu[i]<- ao+bo*sex[i]+co*agehqpdone[i]
  
}
 
 # Priors
 alpha ~ dlogis(0, 0.01) 
 beta_indifftot~ dnorm(0, 0.16)
 beta_le ~ dnorm(0, 0.16)
 beta_agehqpdone ~ dnorm(0, 0.16)
 beta_sex  ~ dnorm(0, 0.16)
 beta_imd ~ dnorm(0, 0.16)
 beta_beh ~ dnorm(0, 0.16)
 beta_mfq ~ dnorm(0, 0.16)
 beta_self ~ dnorm(0, 0.16)
 beta_apsd ~ dnorm(0, 0.16)
 beta_prosoc ~ dnorm(0, 0.16)
 beta_daring ~ dnorm(0, 0.16)
 beta_negem ~ dnorm(0, 0.16)
 beta_fad ~ dnorm(0, 0.16)
 beta_bis ~ dnorm(0, 0.16)
 beta_icu ~ dnorm(0, 0.16)
 beta_wemwbs ~ dnorm(0, 0.16)
 beta_centre ~ dnorm(0, 0.16)
 beta_nssi1 ~ dnorm(0, 0.16)
 beta_loi ~ dnorm(0, 0.16)
 beta_cfq ~ dnorm(0, 0.16)
 beta_ethnic ~ dnorm(0, 0.16)

 inter1 ~ dnorm(0, 0.16)

 or_indifftot<- exp(beta_indifftot)
 or_le<-exp(beta_le)
 or_agehqpdone <- exp(beta_agehqpdone)
 or_sex <- exp(beta_sex)
 or_imd <- exp(beta_imd)
 or_beh <- exp(beta_beh)
 or_mfq<-exp(beta_mfq)
 or_self<-exp(beta_self)
 or_apsd<-exp(beta_apsd)
 or_prosoc<-exp(beta_prosoc)
 or_daring<-exp(beta_daring)
 or_negem<-exp(beta_negem)
 or_fad<-exp(beta_fad)
 or_bis<-exp(beta_bis)
 or_icu<-exp(beta_icu)
 or_wemwbs<-exp(beta_wemwbs)
 or_centre<-exp(beta_centre)
 or_nssi1<-exp(beta_nssi1)
 or_loi<-exp(beta_loi)
 or_cfq<-exp(beta_cfq)
 or_eth<-exp(beta_ethnic)

 or_inter1<- exp(inter1)

 aj ~ dnorm(0, 0.01) 
 bj ~ dnorm(0, 0.16)
 cj ~ dnorm(0, 0.16)
 dj ~ dnorm(0, 0.16)
 ej ~ dnorm(0, 0.16) 
 fj ~ dnorm(0, 0.16)
 gj ~ dnorm(0, 0.16)
 hj ~ dnorm(0, 0.16)
 ij ~ dnorm(0, 0.16)
 jj ~ dnorm(0, 0.16)
 kj~ dnorm(0, 0.16)
 lj~ dnorm(0, 0.16)
 mj~ dnorm(0, 0.16)
 nj ~ dnorm(0, 0.16)
 oj~ dnorm(0, 0.16)
 pj~ dnorm(0, 0.16)
 qj~ dnorm(0, 0.16)
 rj~ dnorm(0, 0.16)
 sj~ dnorm(0, 0.16)

 al ~ dnorm(0, 0.01) 
 bl ~ dnorm(0, 0.16)
 cl ~ dnorm(0, 0.16)
 dl ~ dnorm(0, 0.16)
 el ~ dnorm(0, 0.16)
 fl ~ dnorm(0, 0.16)
 gl ~ dnorm(0, 0.16)
 hl ~ dnorm(0, 0.16)
 il ~ dnorm(0, 0.16)
 jl ~ dnorm(0, 0.16)
 kl ~ dnorm(0, 0.16)
 ll ~ dnorm(0, 0.16)
 ml~ dnorm(0, 0.16)
 nl ~ dnorm(0, 0.16)
 ol~ dnorm(0, 0.16)
 pl~ dnorm(0, 0.16)
 ql~ dnorm(0, 0.16)
 rl~ dnorm(0, 0.16)

 ac ~ dnorm(0, 0.01) 
 bc ~ dnorm(0, 0.16)
 cc ~ dnorm(0, 0.16)
 dc ~ dnorm(0, 0.16)
 ec ~ dnorm(0, 0.16)
 fc ~ dnorm(0, 0.16)
 gc ~ dnorm(0, 0.16)
 hc ~ dnorm(0, 0.16)
 ic ~ dnorm(0, 0.16)
 jc ~ dnorm(0, 0.16)
 kc ~ dnorm(0, 0.16)
 lc ~ dnorm(0, 0.16)
 mc~ dnorm(0, 0.16)
 nc ~ dnorm(0, 0.16)
 oc~ dnorm(0, 0.16)
 pc~ dnorm(0, 0.16)
 qc~ dnorm(0, 0.16)

 ab ~ dnorm(0, 0.01) 
 bb ~ dnorm(0, 0.16)
 cb ~ dnorm(0, 0.16)
 db ~ dnorm(0, 0.16)
 eb ~ dnorm(0, 0.16)
 fb ~ dnorm(0, 0.16)
 gb ~ dnorm(0, 0.16)
 hb ~ dnorm(0, 0.16)
 ib ~ dnorm(0, 0.16)
 jb ~ dnorm(0, 0.16)
 kb ~ dnorm(0, 0.16)
 lb ~ dnorm(0, 0.16)
 mb~ dnorm(0, 0.16)
 nb ~ dnorm(0, 0.16)
 ob~ dnorm(0, 0.16)
 pb~ dnorm(0, 0.16)


 as ~ dnorm(0, 0.01) 
 bs ~ dnorm(0, 0.16)
 cs ~ dnorm(0, 0.16)
 ds ~ dnorm(0, 0.16)
 es ~ dnorm(0, 0.16)
 fs ~ dnorm(0, 0.16)
 gs ~ dnorm(0, 0.16)
 hs ~ dnorm(0, 0.16)
 is ~ dnorm(0, 0.16)
 js ~ dnorm(0, 0.16)
 ks ~ dnorm(0, 0.16)
 ls ~ dnorm(0, 0.16)
 ms~ dnorm(0, 0.16)
 ns ~ dnorm(0, 0.16)
 os~ dnorm(0, 0.16)

 aa ~ dnorm(0, 0.01) 
 ba ~ dnorm(0, 0.16)
 ca ~ dnorm(0, 0.16)
 da ~ dnorm(0, 0.16)
 ea ~ dnorm(0, 0.16)
 fa ~ dnorm(0, 0.16)
 ga ~ dnorm(0, 0.16)
 ha ~ dnorm(0, 0.16)
 ia ~ dnorm(0, 0.16)
 ja ~ dnorm(0, 0.16)
 ka ~ dnorm(0, 0.16)
 la ~ dnorm(0, 0.16)
 ma~ dnorm(0, 0.16)
 na ~ dnorm(0, 0.16)

 ap ~ dnorm(0, 0.01) 
 bp ~ dnorm(0, 0.16)
 cp ~ dnorm(0, 0.16)
 dp ~ dnorm(0, 0.16)
 ep ~ dnorm(0, 0.16)
 fp ~ dnorm(0, 0.16)
 gp ~ dnorm(0, 0.16)
 hp ~ dnorm(0, 0.16)
 ip ~ dnorm(0, 0.16)
 jp ~ dnorm(0, 0.16)
 kp ~ dnorm(0, 0.16)
 lp ~ dnorm(0, 0.16)
 mp~ dnorm(0, 0.16)
 
 ao ~ dnorm(0, 0.01) 
 bo ~ dnorm(0, 0.16) 
 co ~ dnorm(0, 0.16) 
 do ~ dnorm(0, 0.16)
 eo ~ dnorm(0, 0.16)
 fo ~ dnorm(0, 0.16)
 go ~ dnorm(0, 0.16)
 ho ~ dnorm(0, 0.16)
 io ~ dnorm(0, 0.16)
 jo ~ dnorm(0, 0.16)
 ko ~ dnorm(0, 0.16)
 lo ~ dnorm(0, 0.16)
 
 af ~ dnorm(0, 0.01) 
 bf ~ dnorm(0, 0.16) 
 cf ~ dnorm(0, 0.16) 
 df ~ dnorm(0, 0.16)
 ef ~ dnorm(0, 0.16)
 ff ~ dnorm(0, 0.16)
 gf ~ dnorm(0, 0.16)
 hf ~ dnorm(0, 0.16)
 if ~ dnorm(0, 0.16)
 jf ~ dnorm(0, 0.16)
 kf ~ dnorm(0, 0.16)
 
 ai ~ dnorm(0, 0.01) 
 bi ~ dnorm(0, 0.16) 
 ci ~ dnorm(0, 0.16) 
 di ~ dnorm(0, 0.16)
 ei ~ dnorm(0, 0.16)
 fi ~ dnorm(0, 0.16)
 gi ~ dnorm(0, 0.16)
 hi ~ dnorm(0, 0.16)
 ii ~ dnorm(0, 0.16)
 ji ~ dnorm(0, 0.16)
 
 ax ~ dnorm(0, 0.01) 
 bx ~ dnorm(0, 0.16) 
 cx ~ dnorm(0, 0.16) 
 dx ~ dnorm(0, 0.16)
 ex ~ dnorm(0, 0.16)
 fx ~ dnorm(0, 0.16)
 gx ~ dnorm(0, 0.16)
 hx ~ dnorm(0, 0.16)
 ix ~ dnorm(0, 0.16)
  
 av ~ dnorm(0, 0.01) 
 bv ~ dnorm(0, 0.16) 
 cv ~ dnorm(0, 0.16) 
 dv ~ dnorm(0, 0.16)
 ev ~ dnorm(0, 0.16)
 fv ~ dnorm(0, 0.16)
 gv ~ dnorm(0, 0.16)
 hv ~ dnorm(0, 0.16)
 
 at ~ dnorm(0, 0.01) 
 bt ~ dnorm(0, 0.16) 
 ct ~ dnorm(0, 0.16) 
 dt ~ dnorm(0, 0.16)
 et ~ dnorm(0, 0.16)
 ft ~ dnorm(0, 0.16)
 gt ~ dnorm(0, 0.16)
 
 aw ~ dnorm(0, 0.01) 
 bw ~ dnorm(0, 0.16) 
 cw ~ dnorm(0, 0.16) 
 dw ~ dnorm(0, 0.16)
 ew ~ dnorm(0, 0.16)
 fw ~ dnorm(0, 0.16)
 gw ~ dnorm(0, 0.16)
 
 ay ~ dnorm(0, 0.01) 
 by ~ dnorm(0, 0.16) 
 cy ~ dnorm(0, 0.16) 
 dy ~ dnorm(0, 0.16)
 ey ~ dnorm(0, 0.16)
 

 jprec  ~ dgamma(0.01,0.01)
 lprec  ~ dgamma(0.01,0.01)
 cprec ~ dgamma(0.01,0.01)
 bprec  ~ dgamma(0.01,0.01)
 sprec  ~ dgamma(0.01,0.01)
 aprec  ~ dgamma(0.01,0.01)
 pprec  ~ dgamma(0.01,0.01)
 oprec  ~ dgamma(0.01,0.01)
 fprec  ~ dgamma(0.01,0.01)
 iprec  ~ dgamma(0.01,0.01)
 xprec  ~ dgamma(0.01,0.01)
 vprec  ~ dgamma(0.01,0.01)
 tprec  ~ dgamma(0.01,0.01)
 wprec  ~ dgamma(0.01,0.01)
 yprec  ~ dgamma(0.01,0.01)

}", con=modfile)

dat<-list(nssi_binary_t2=pred_merge$nssi_binary_t2,
          indiff_total=pred_merge$indiff_total,
          leq_total_45yes=pred_merge$leq_total_45yes,
          sex=pred_merge$sex,
          imd = pred_merge$imd, 
          agehqpdone = pred_merge$agehqpdone,
          behtot = pred_merge$behtot,
          wemwbs_total = pred_merge$wemwbs_total,
          fadtotal = pred_merge$fadtotal,
          bistotal = pred_merge$bistotal,
          apsdtotal=pred_merge$apsdtotal,
          cads_daring=pred_merge$cads_daring,
          cads_prosoc=pred_merge$cads_prosoc,
          cads_negemot=pred_merge$cads_negemot,
          loitot_3cat=pred_merge$loitot_3cat,
          cfqtotal=pred_merge$cfqtotal,
          n = nrow(pred_merge))

ini <- list(list(alpha=0, beta_indifftot=0, beta_agehqpdone=0, beta_sex=0, beta_imd=0,
                 beta_wemwbs=0, beta_le=0,beta_beh=0,
                 beta_fad=0,beta_bis=0, beta_apsd=0,
                 beta_daring=0, beta_prosoc=0, beta_loi=1, beta_negem=0,beta_cfq=0),
            list(alpha=1, beta_indifftot=1, beta_agehqpdone=1, beta_sex=1, beta_imd=1,
                 beta_wemwbs=1, beta_le=1,beta_beh=1,
                 beta_fad=1, beta_bis=1, beta_apsd=1,
                 beta_daring=1, beta_prosoc=1, beta_loi=1,beta_negem=1,beta_cfq=1))

diab_jag <- jags.model(textConnection(model1), 
                       data=dat,inits=ini,n.chains = 2)

update(diab_jag, 1000)

sam <- coda.samples(diab_jag,
                        c("alpha","or_indifftot","or_le","or_agehqpdone", "or_sex","or_imd",
                          "or_wemwbs","or_beh",
                          "or_fad","or_bis","or_negem",
                          "or_apsd","or_daring","or_prosoc", "or_loi","or_cfq",
                          "or_inter1"),
                        n.iter=5000)

MCMCsummary(sam, round=4)

######## Total cases - control ########
pred_merge<-read.csv("fulldata_recode.csv")

#mops<-read.csv('Sienna LE, MOPS, APQ.csv')
#mops<-mops%>%
#dplyr::select(id_nspn, event, mops_mat_abuse)%>%
#filter(event=='Home Questionnaire Pack 1')%>%
#dplyr::select(-event)

pred_merge<-pred_merge%>%
  dplyr::select(-id_nspn,-ethnic_final,-nssi_cat_type)


colMeans(pred_merge1[,c(4:45)])
apply(pred_merge1, 2, sd)

str(pred_merge1)
pred_merge[,c(4:6,8:9,39)]<-scale(pred_merge[,c(4:6,8:9,39)],center=TRUE, scale=TRUE)
pred_merge[,c(7,10:38,40:45)]<-0.5*scale(pred_merge[,c(7,10:38,40:45)],center=TRUE, scale=TRUE)

nrec<-length(pred_merge1[,1])
y<-pred_merge1$nssi_t2_cat
ncat<-max(y, na.rm=T)
ncat1<-ncat-1

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
  
  logit(p[i]) <- alpha + beta_contot*control_total[i]+beta_le*leq_total_45yes[i]+ 
  beta_sex*sex[i]+beta_agehqpdone*agehqpdone[i]+
  beta_beh*behtot[i]+beta_wemwbs*wemwbs_total[i]+
  beta_bis*bistotal[i]+beta_loi*loitot_3cat[i]+beta_negemot*cads_negemot[i]+
  beta_fad*fadtotal[i]+beta_cfq*cfqtotal[i]+
  inter1 * control_total[i] * leq_total_45yes[i]
  
  # Model for missing covariates
  control_total[i] ~ dnorm (conmu[i], jprec)
  conmu[i]<- aj+bj*leq_total_45yes[i]+cj*sex[i]+
  dj*agehqpdone[i]+ej*behtot[i]+fj*wemwbs_total[i]+
  gj*bistotal[i]+hj*loitot_3cat[i]+ij*cads_negemot[i]+
  jj*fadtotal[i]+kj*cfqtotal[i]
  
  leq_total_45yes[i] ~ dnorm (leqmu[i], lprec)
  leqmu[i]<- al+bl*sex[i]+
  cl*agehqpdone[i]+dl*behtot[i]+el*wemwbs_total[i]+
  fl*bistotal[i]+gl*loitot_3cat[i]+hl*cads_negemot[i]+
  il*fadtotal[i]+jl*cfqtotal[i]
  
  behtot[i] ~ dnorm (behmu[i], bprec)
  behmu[i]<- ab+bb*sex[i]+
  cb*agehqpdone[i]+db*wemwbs_total[i]+eb*bistotal[i]+
  fb*loitot_3cat[i]+gb*cads_negemot[i]+
  hb*fadtotal[i]+ib*cfqtotal[i]

  wemwbs_total[i] ~ dnorm (wemwbsmu[i], sprec)
  wemwbsmu[i]<- as+bs*sex[i]+
  cs*agehqpdone[i]+ds*bistotal[i]+
  es*loitot_3cat[i]+fs*cads_negemot[i]+
  gs*fadtotal[i]+hs*cfqtotal[i]
  
  bistotal[i] ~ dnorm (bismu[i], fprec)
  bismu[i]<- af+bf*sex[i]+cf*agehqpdone[i]+
  df*loitot_3cat[i]+ef*cads_negemot[i]+ff*fadtotal[i]+gf*cfqtotal[i]
  
  loitot_3cat[i] ~ dnorm (loimu[i], aprec)
  loimu[i]<- aa+ba*sex[i]+ca*agehqpdone[i]+
  da*cads_negemot[i]+ea*fadtotal[i]+fa*cfqtotal[i]
  
  cads_negemot[i] ~ dnorm (negmu[i], tprec)
  negmu[i]<- at+bt*sex[i]+ct*agehqpdone[i]+
  dt*fadtotal[i]+et*cfqtotal[i]
  
  fadtotal[i] ~ dnorm (fadmu[i], oprec)
  fadmu[i]<- ao+bo*sex[i]+co*agehqpdone[i]+do*cfqtotal[i]
  
  cfqtotal[i] ~ dnorm (cfqmu[i], xprec)
  cfqmu[i]<- ax+bx*sex[i]+cx*agehqpdone[i]
}
 
 # Priors
 alpha ~ dt(0, 1/10^2, 1)
 beta_contot~ dt(0, 1/2.5^2, 1)
 beta_le ~ dt(0, 1/2.5^2, 1)
 beta_agehqpdone ~ dt(0, 1/2.5^2, 1)
 beta_sex  ~ dt(0, 1/2.5^2, 1)
 beta_beh ~ dt(0, 1/2.5^2, 1)
 beta_bis ~ dt(0, 1/2.5^2, 1)
 beta_wemwbs ~ dt(0, 1/2.5^2, 1)
 beta_loi ~ dt(0, 1/2.5^2, 1)
 beta_negemot ~ dt(0, 1/2.5^2, 1)
 beta_fad ~ dt(0, 1/2.5^2, 1)
 beta_cfq ~ dt(0, 1/2.5^2, 1)

 inter1 ~ dt(0, 1/2.5^2, 1)

 or_contot<- exp(beta_contot)
 or_le<-exp(beta_le)
 or_agehqpdone <- exp(beta_agehqpdone)
 or_sex <- exp(beta_sex)
 or_beh <- exp(beta_beh)
 or_bis<-exp(beta_bis)
 or_wemwbs<-exp(beta_wemwbs)
 or_loi<-exp(beta_loi)
 or_negemot<-exp(beta_negemot)
 or_fad<-exp(beta_fad)
 or_cfq<-exp(beta_cfq)

 or_inter1<- exp(inter1)

 aj ~ dt(0, 1/2.5^2, 1)
 bj ~ dt(0, 1/2.5^2, 1)
 cj ~ dt(0, 1/2.5^2, 1)
 dj ~ dt(0, 1/2.5^2, 1)
 ej ~ dt(0, 1/2.5^2, 1)
 fj ~ dt(0, 1/2.5^2, 1)
 gj ~ dt(0, 1/2.5^2, 1)
 hj ~ dt(0, 1/2.5^2, 1)
 ij ~ dt(0, 1/2.5^2, 1)
 jj ~ dt(0, 1/2.5^2, 1)
 kj ~ dt(0, 1/2.5^2, 1)

 al ~ dt(0, 1/2.5^2, 1)
 bl ~ dt(0, 1/2.5^2, 1)
 cl ~ dt(0, 1/2.5^2, 1)
 dl ~ dt(0, 1/2.5^2, 1)
 el ~ dt(0, 1/2.5^2, 1)
 fl ~ dt(0, 1/2.5^2, 1)
 gl ~ dt(0, 1/2.5^2, 1)
 hl ~ dt(0, 1/2.5^2, 1)
 il ~ dt(0, 1/2.5^2, 1)
 jl ~ dt(0, 1/2.5^2, 1)

 ab ~ dt(0, 1/2.5^2, 1)
 bb ~ dt(0, 1/2.5^2, 1)
 cb ~ dt(0, 1/2.5^2, 1)
 db ~ dt(0, 1/2.5^2, 1)
 eb ~ dt(0, 1/2.5^2, 1)
 fb ~ dt(0, 1/2.5^2, 1)
 gb ~ dt(0, 1/2.5^2, 1)
 hb ~ dt(0, 1/2.5^2, 1)
 ib ~ dt(0, 1/2.5^2, 1)


 as ~ dt(0, 1/2.5^2, 1)
 bs ~ dt(0, 1/2.5^2, 1)
 cs ~ dt(0, 1/2.5^2, 1)
 ds ~ dt(0, 1/2.5^2, 1)
 es ~ dt(0, 1/2.5^2, 1)
 fs ~ dt(0, 1/2.5^2, 1)
 gs ~ dt(0, 1/2.5^2, 1)
 hs ~ dt(0, 1/2.5^2, 1)
 
 af ~ dt(0, 1/2.5^2, 1)
 bf ~ dt(0, 1/2.5^2, 1)
 cf ~ dt(0, 1/2.5^2, 1)
 df ~ dt(0, 1/2.5^2, 1)
 ef ~ dt(0, 1/2.5^2, 1)
 ff ~ dt(0, 1/2.5^2, 1)
 gf ~ dt(0, 1/2.5^2, 1)
 
 aa ~ dt(0, 1/2.5^2, 1)
 ba ~ dt(0, 1/2.5^2, 1)
 ca ~ dt(0, 1/2.5^2, 1)
 da ~ dt(0, 1/2.5^2, 1)
 ea ~ dt(0, 1/2.5^2, 1)
 fa ~ dt(0, 1/2.5^2, 1)
 
 at ~ dt(0, 1/2.5^2, 1)
 bt ~ dt(0, 1/2.5^2, 1)
 ct ~ dt(0, 1/2.5^2, 1)
 dt ~ dt(0, 1/2.5^2, 1)
 et ~ dt(0, 1/2.5^2, 1)
 
  ao ~ dt(0, 1/2.5^2, 1)
 bo ~ dt(0, 1/2.5^2, 1)
 co ~ dt(0, 1/2.5^2, 1)
 do ~ dt(0, 1/2.5^2, 1)
 
 ax ~ dt(0, 1/2.5^2, 1)
 bx ~ dt(0, 1/2.5^2, 1)
 cx ~ dt(0, 1/2.5^2, 1)


 jprec  ~ dgamma(0.001,0.001)
 lprec  ~ dgamma(0.001,0.001)
 bprec  ~ dgamma(0.001,0.001)
 sprec  ~ dgamma(0.001,0.001)
 fprec  ~ dgamma(0.001,0.001)
 aprec  ~ dgamma(0.001,0.001)
 tprec  ~ dgamma(0.001,0.001)
 oprec  ~ dgamma(0.001,0.001)
 xprec  ~ dgamma(0.001,0.001)
 
 fit <- sum(res[])
 fit.new <- sum(res.new[])

}", con=model1)

dat<-list(nssi_binary_t2=pred_merge$nssi_binary_t2,
          control_total=pred_merge$control_total,
          leq_total_45yes=pred_merge$leq_total_45yes,
          sex=pred_merge$sex,
          agehqpdone = pred_merge$agehqpdone,
          behtot = pred_merge$behtot,
          wemwbs_total = pred_merge$wemwbs_total,
          bistotal = pred_merge$bistotal,
          loitot_3cat = pred_merge$loitot_3cat,
          cads_negemot=pred_merge$cads_negemot,
          fadtotal=pred_merge$fadtotal,
          cfqtotal=pred_merge$cfqtotal,
          n = nrow(pred_merge))

ini <- list(list(alpha=0, beta_contot=0, beta_agehqpdone=0, beta_sex=0,
                 beta_wemwbs=0, beta_le=0,beta_beh=0,
                 beta_bis=0, beta_loi=0, beta_negemot=0,
                 beta_fad=0, beta_cfq=0),
            list(alpha=1, beta_contot=1, beta_agehqpdone=1, beta_sex=1,
                 beta_wemwbs=1, beta_le=1,beta_beh=1,
                 beta_bis=1,beta_loi=1, beta_negemot=1,
                 beta_fad=1, beta_cfq=1))

params <- c("alpha","or_contot","or_le","or_agehqpdone", "or_sex",
            "or_wemwbs","or_beh","or_loi","or_negemot","or_fad","or_cfq",
            "or_bis","or_inter1","fit","fit.new")

fit_control <- jags(data = dat, parameters.to.save=params, n.chains = 2, 
            n.iter = 15000, n.adapt = 500,
            n.burnin = 1000, model.file = model1, n.thin = 1)
fit_control

pp.check(fit_control,observed='fit',simulated='fit.new',
         main="Posterior Predictive Check for Control(total) - NSSI(total)",cex.main=0.9,cex.lab=1.0)



diab_jag <- jags.model(textConnection(model1), 
                       data=dat,inits=ini,n.chains = 2)

update(diab_jag, 2000)

sam <- coda.samples(diab_jag,
                    c("alpha","or_abutot","or_le","or_agehqpdone", "or_sex","or_imd",
                      "or_wemwbs","or_beh",
                      "or_fad","or_bis",
                      "or_apsd","or_daring","or_prosoc","or_nssi1",
                      "or_inter1"),
                    n.iter=10000)

MCMCsummary(sam, round=4)

gelman.diag(sam)
gelman.plot(sam)
raftery.diag(fit)
superdiag(sam, burnin = 1000)

#####Total cases - indifference ####
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
  
  logit(p[i]) <- alpha + beta_indifftot*indiff_total[i]+beta_le*leq_total_45yes[i]+ 
  beta_agehqpdone*agehqpdone[i]+
  beta_beh*behtot[i]+beta_wemwbs*wemwbs_total[i]+
  beta_bis*bistotal[i]+beta_loi*loitot_3cat[i]+beta_negemot*cads_negemot[i]+
  beta_fad*fadtotal[i]+beta_cfq*cfqtotal[i]+
  inter1 * indiff_total[i] * leq_total_45yes[i]
  
  # Model for missing covariates
  indiff_total[i] ~ dnorm (indiffmu[i], jprec)
  indiffmu[i]<- aj+bj*leq_total_45yes[i]+
  dj*agehqpdone[i]+ej*behtot[i]+fj*wemwbs_total[i]+
  gj*bistotal[i]+hj*loitot_3cat[i]+ij*cads_negemot[i]+
  jj*fadtotal[i]+kj*cfqtotal[i]
  
  leq_total_45yes[i] ~ dnorm (leqmu[i], lprec)
  leqmu[i]<- al+
  cl*agehqpdone[i]+dl*behtot[i]+el*wemwbs_total[i]+
  fl*bistotal[i]+gl*loitot_3cat[i]+hl*cads_negemot[i]+
  il*fadtotal[i]+jl*cfqtotal[i]
  
  behtot[i] ~ dnorm (behmu[i], bprec)
  behmu[i]<- ab+
  cb*agehqpdone[i]+db*wemwbs_total[i]+eb*bistotal[i]+
  fb*loitot_3cat[i]+gb*cads_negemot[i]+
  hb*fadtotal[i]+ib*cfqtotal[i]

  wemwbs_total[i] ~ dnorm (wemwbsmu[i], sprec)
  wemwbsmu[i]<- as+
  cs*agehqpdone[i]+ds*bistotal[i]+
  es*loitot_3cat[i]+fs*cads_negemot[i]+
  gs*fadtotal[i]+hs*cfqtotal[i]
  
  bistotal[i] ~ dnorm (bismu[i], fprec)
  bismu[i]<- af+cf*agehqpdone[i]+
  df*loitot_3cat[i]+ef*cads_negemot[i]+ff*fadtotal[i]+gf*cfqtotal[i]
  
  loitot_3cat[i] ~ dnorm (loimu[i], aprec)
  loimu[i]<- aa+ca*agehqpdone[i]+
  da*cads_negemot[i]+ea*fadtotal[i]+fa*cfqtotal[i]
  
  cads_negemot[i] ~ dnorm (negmu[i], tprec)
  negmu[i]<- at+ct*agehqpdone[i]+
  dt*fadtotal[i]+et*cfqtotal[i]
  
  fadtotal[i] ~ dnorm (fadmu[i], oprec)
  fadmu[i]<- ao+co*agehqpdone[i]+do*cfqtotal[i]
  
  cfqtotal[i] ~ dnorm (cfqmu[i], xprec)
  cfqmu[i]<- ax+cx*agehqpdone[i]
}
 
 # Priors
 alpha ~ dt(0, 1/10^2, 1)
 beta_indifftot~ dt(0, 1/2.5^2, 1)
 beta_le ~ dt(0, 1/2.5^2, 1)
 beta_agehqpdone ~ dt(0, 1/2.5^2, 1)
 beta_beh ~ dt(0, 1/2.5^2, 1)
 beta_bis ~ dt(0, 1/2.5^2, 1)
 beta_wemwbs ~ dt(0, 1/2.5^2, 1)
 beta_loi ~ dt(0, 1/2.5^2, 1)
 beta_negemot ~ dt(0, 1/2.5^2, 1)
 beta_fad ~ dt(0, 1/2.5^2, 1)
 beta_cfq ~ dt(0, 1/2.5^2, 1)

 inter1 ~ dt(0, 1/2.5^2, 1)

 or_indifftot<- exp(beta_indifftot)
 or_le<-exp(beta_le)
 or_agehqpdone <- exp(beta_agehqpdone)
 or_beh <- exp(beta_beh)
 or_bis<-exp(beta_bis)
 or_wemwbs<-exp(beta_wemwbs)
 or_loi<-exp(beta_loi)
 or_negemot<-exp(beta_negemot)
 or_fad<-exp(beta_fad)
 or_cfq<-exp(beta_cfq)

 or_inter1<- exp(inter1)

 aj ~ dt(0, 1/2.5^2, 1)
 bj ~ dt(0, 1/2.5^2, 1)
 cj ~ dt(0, 1/2.5^2, 1)
 dj ~ dt(0, 1/2.5^2, 1)
 ej ~ dt(0, 1/2.5^2, 1)
 fj ~ dt(0, 1/2.5^2, 1)
 gj ~ dt(0, 1/2.5^2, 1)
 hj ~ dt(0, 1/2.5^2, 1)
 ij ~ dt(0, 1/2.5^2, 1)
 jj ~ dt(0, 1/2.5^2, 1)
 kj ~ dt(0, 1/2.5^2, 1)

 al ~ dt(0, 1/2.5^2, 1)
 bl ~ dt(0, 1/2.5^2, 1)
 cl ~ dt(0, 1/2.5^2, 1)
 dl ~ dt(0, 1/2.5^2, 1)
 el ~ dt(0, 1/2.5^2, 1)
 fl ~ dt(0, 1/2.5^2, 1)
 gl ~ dt(0, 1/2.5^2, 1)
 hl ~ dt(0, 1/2.5^2, 1)
 il ~ dt(0, 1/2.5^2, 1)
 jl ~ dt(0, 1/2.5^2, 1)

 ab ~ dt(0, 1/2.5^2, 1)
 bb ~ dt(0, 1/2.5^2, 1)
 cb ~ dt(0, 1/2.5^2, 1)
 db ~ dt(0, 1/2.5^2, 1)
 eb ~ dt(0, 1/2.5^2, 1)
 fb ~ dt(0, 1/2.5^2, 1)
 gb ~ dt(0, 1/2.5^2, 1)
 hb ~ dt(0, 1/2.5^2, 1)
 ib ~ dt(0, 1/2.5^2, 1)


 as ~ dt(0, 1/2.5^2, 1)
 bs ~ dt(0, 1/2.5^2, 1)
 cs ~ dt(0, 1/2.5^2, 1)
 ds ~ dt(0, 1/2.5^2, 1)
 es ~ dt(0, 1/2.5^2, 1)
 fs ~ dt(0, 1/2.5^2, 1)
 gs ~ dt(0, 1/2.5^2, 1)
 hs ~ dt(0, 1/2.5^2, 1)
 
 af ~ dt(0, 1/2.5^2, 1)
 bf ~ dt(0, 1/2.5^2, 1)
 cf ~ dt(0, 1/2.5^2, 1)
 df ~ dt(0, 1/2.5^2, 1)
 ef ~ dt(0, 1/2.5^2, 1)
 ff ~ dt(0, 1/2.5^2, 1)
 gf ~ dt(0, 1/2.5^2, 1)
 
 aa ~ dt(0, 1/2.5^2, 1)
 ba ~ dt(0, 1/2.5^2, 1)
 ca ~ dt(0, 1/2.5^2, 1)
 da ~ dt(0, 1/2.5^2, 1)
 ea ~ dt(0, 1/2.5^2, 1)
 fa ~ dt(0, 1/2.5^2, 1)
 
 at ~ dt(0, 1/2.5^2, 1)
 bt ~ dt(0, 1/2.5^2, 1)
 ct ~ dt(0, 1/2.5^2, 1)
 dt ~ dt(0, 1/2.5^2, 1)
 et ~ dt(0, 1/2.5^2, 1)
 
  ao ~ dt(0, 1/2.5^2, 1)
 bo ~ dt(0, 1/2.5^2, 1)
 co ~ dt(0, 1/2.5^2, 1)
 do ~ dt(0, 1/2.5^2, 1)
 
 ax ~ dt(0, 1/2.5^2, 1)
 bx ~ dt(0, 1/2.5^2, 1)
 cx ~ dt(0, 1/2.5^2, 1)


 jprec  ~ dgamma(0.001,0.001)
 lprec  ~ dgamma(0.001,0.001)
 bprec  ~ dgamma(0.001,0.001)
 sprec  ~ dgamma(0.001,0.001)
 fprec  ~ dgamma(0.001,0.001)
 aprec  ~ dgamma(0.001,0.001)
 tprec  ~ dgamma(0.001,0.001)
 oprec  ~ dgamma(0.001,0.001)
 xprec  ~ dgamma(0.001,0.001)
 
 fit <- sum(res[])
 fit.new <- sum(res.new[])

}", con=model1)

dat<-list(nssi_binary_t2=pred_merge$nssi_binary_t2,
          indiff_total=pred_merge$indiff_total,
          leq_total_45yes=pred_merge$leq_total_45yes,
          agehqpdone = pred_merge$agehqpdone,
          behtot = pred_merge$behtot,
          wemwbs_total = pred_merge$wemwbs_total,
          bistotal = pred_merge$bistotal,
          loitot_3cat = pred_merge$loitot_3cat,
          cads_negemot=pred_merge$cads_negemot,
          fadtotal=pred_merge$fadtotal,
          cfqtotal=pred_merge$cfqtotal,
          n = nrow(pred_merge))

ini <- list(list(alpha=0, beta_indifftot=0, beta_agehqpdone=0, 
                 beta_wemwbs=0, beta_le=0,beta_beh=0,
                 beta_bis=0, beta_loi=0, beta_negemot=0,
                 beta_fad=0, beta_cfq=0),
            list(alpha=1, beta_indifftot=1, beta_agehqpdone=1, 
                 beta_wemwbs=1, beta_le=1,beta_beh=1,
                 beta_bis=1,beta_loi=1, beta_negemot=1,
                 beta_fad=1, beta_cfq=1))

params <- c("alpha","or_indifftot","or_le","or_agehqpdone",
            "or_wemwbs","or_beh","or_loi","or_negemot","or_fad","or_cfq",
            "or_bis","or_inter1","fit","fit.new")

fit_indiff <- jags(data = dat, parameters.to.save=params, n.chains = 2, 
                   n.iter = 15000, n.adapt = 500,
                   n.burnin = 1000, model.file = model1, n.thin = 1)
fit_indiff

pp.check(fit_indiff,observed='fit',simulated='fit.new',
         main="Posterior Predictive Check for Indifference(total) - NSSI(total)",cex.main=0.9,cex.lab=1.0)



  diab_jag <- jags.model(textConnection(model1), 
                       data=dat,inits=ini,n.chains = 2)

update(diab_jag, 2000)

sam <- coda.samples(diab_jag,
                    c("alpha","or_abutot","or_le","or_agehqpdone", "or_imd",
                      "or_wemwbs","or_beh",
                      "or_fad","or_bis",
                      "or_apsd","or_daring","or_prosoc","or_nssi1",
                      "or_inter1"),
                    n.iter=10000)

MCMCsummary(sam, round=4)

gelman.diag(sam)
gelman.plot(sam)
raftery.diag(fit)
superdiag(sam, burnin = 1000)

####Total cases - ABUSE FINAL #########
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

fit1 <- jags(data = dat, parameters.to.save=params, n.chains = 2, 
            n.iter = 15000, n.adapt = 500,
            n.burnin = 1000, model.file = model1, n.thin = 1)

coefs <- fit1$BUGSoutput

fit1
gelman.diag(as.mcmc(fit1))

fit1_mcmc<-as.mcmc.list(fit1$samples)
fit1_mat<-as.matrix(fit1_mcmc)
mymodel.dat <- as.data.frame(fit1_mat)
probs <- mymodel.dat[, grep("fix[", colnames(mymodel.dat), fixed = T)]

pp.check(fit1,observed='fit',simulated='fit.new',
         main="Posterior Predictive Check for Abuse(total) - NSSI(total)",cex.main=0.9,cex.lab=1.0)

traceplot(fit1)
######New onset analysis ########
t1.t2.onset<- read.csv("t1.t2.onset.csv")
#t1.t2.onset$nssi_type_newt2<-as.factor(t1.t2.onset$nssi_type_newt2)

ctq<-read.csv("CTQ_24May18_final.csv")
#Drop depression cohort in CTQ
ctq_nodep<-ctq%>%
  filter(study_primary=="2K_Cohort")%>%
  dplyr::select(id_nspn, ctq_total,CTQemotionalabuse,CTQphysicalabuse, CTQsexualabuse,
                CTQemotionalneglect,CTQphysicalneglect,event)%>%
  filter(event=="iua_baseline_arm_1")%>%
  dplyr::select(-event)

t1.t2.onset<-t1.t2.onset%>%
  left_join(ctq_nodep)

count(t1.t2.onset$nssi_binary_newt2==1)
#78 cases

t1.t2.onset<- dplyr::select(t1.t2.onset,-event)
t1.t2.onset<- dplyr::select(t1.t2.onset,-id_nspn)

t1.t2.onset[,c(4:45)] <- scale(t1.t2.onset[,c(4:45)])

model2 <- "model {
for (i in 1:n) {

  # Model of interest
  nssi_binary_newt2[i] ~ dbern(p[i])
  
  logit(p[i]) <- alpha + beta_indifftot*indiff_total[i]+beta_le*leq_total_45yes[i]+ 
  beta_sex*sex[i]+beta_imd*imd[i]+beta_agehqpdone*agehqpdone[i]+
  beta_beh*behtot[i]+beta_mfq*mfqtot[i]+
  beta_fad*fadtotal[i]+
  beta_bis*bistotal[i]+beta_daring*cads_daring[i]+
  beta_icu*icutotal[i]+
  beta_cfq*cfqtotal[i]+beta_negem*cads_negemot[i]+
  inter1 * indiff_total[i] * leq_total_45yes[i]
  
  # Model for missing covariates
  indiff_total[i] ~ dnorm (indifftotmu[i], jprec)
  indifftotmu[i]<- aj+bj*leq_total_45yes[i]+cj*sex[i]+
  dj*imd[i]+ej*agehqpdone[i]+fj*behtot[i]+gj*mfqtot[i]+
  ij*fadtotal[i]+
  jj*bistotal[i]+kj*icutotal[i]+
  lj*cads_daring[i]+mj*cfqtotal[i]+nj*cads_negemot[i]
  
  leq_total_45yes[i] ~ dnorm (leqmu[i], lprec)
  leqmu[i]<- al+bl*sex[i]+cl*imd[i]+
  dl*agehqpdone[i]+el*behtot[i]+fl*mfqtot[i]+
  hl*fadtotal[i]+
  il*bistotal[i]+
  jl*icutotal[i]+
  kl*cads_daring[i]+ll*cfqtotal[i]+ml*cads_negemot[i]
  
  imd[i] ~ dnorm (imdmu[i], cprec)
  imdmu[i]<- ac+bc*sex[i]+cc*agehqpdone[i]+dc*behtot[i]+
  ec*mfqtot[i]+ gc*fadtotal[i]+
  hc*bistotal[i]+ic*icutotal[i]+
  jc*cads_daring[i]+kc*cfqtotal[i]+lc*cads_negemot[i]
  
  behtot[i] ~ dnorm (behmu[i], bprec)
  behmu[i]<- ab+bb*sex[i]+cb*agehqpdone[i]+db*mfqtot[i]+ 
  fb*fadtotal[i]+
  gb*bistotal[i]+hb*icutotal[i]+
  ib*cads_daring[i]+jb*cfqtotal[i]+kb*cads_negemot[i]
  
  mfqtot[i] ~ dnorm (mfqmu[i], sprec)
  mfqmu[i]<- as+bs*sex[i]+cs*agehqpdone[i]+
  es*fadtotal[i]+fs*bistotal[i]+
  gs*icutotal[i]+
  hs*cads_daring[i]+is*cfqtotal[i]+js*cads_negemot[i]
  
  fadtotal[i] ~ dnorm (fadmu[i], fprec)
  fadmu[i]<- af+bf*sex[i]+cf*agehqpdone[i]+
  df*bistotal[i]+
  ef*icutotal[i]+
  ff*cads_daring[i]+gf*cfqtotal[i]+hf*cads_negemot[i]
  
  bistotal[i] ~ dnorm (bismu[i], iprec)
  bismu[i]<- ai+bi*sex[i]+ci*agehqpdone[i]+
  di*icutotal[i]+
  ei*cads_daring[i]+fi*cfqtotal[i]+gi*cads_negemot[i]
  
  icutotal[i] ~ dnorm (icumu[i], xprec)
  icumu[i]<- ax+bx*sex[i]+cx*agehqpdone[i]+
  dx*cads_daring[i]+ex*cfqtotal[i]+fx*cads_negemot[i]
  
  cfqtotal[i] ~ dnorm (cfqmu[i], wprec)
  cfqmu[i]<- aw+bw*sex[i]+cw*agehqpdone[i]+dw*cads_daring[i]+ew*cads_negemot[i]
  
  cads_daring[i] ~ dnorm (daringmu[i], yprec)
  daringmu[i]<- ay+by*sex[i]+cy*agehqpdone[i]+
  dy*cads_negemot[i]
  
  cads_negemot[i] ~ dnorm (negmu[i], rprec)
  negmu[i]<- ar+br*sex[i]+cr*agehqpdone[i]
  
}
 
 # Priors
 alpha ~ dlogis(0, 0.01) 
 beta_indifftot ~ dnorm(0, 0.16)
 beta_le ~ dnorm(0, 0.16)
 beta_agehqpdone ~ dnorm(0, 0.16)
 beta_sex  ~ dnorm(0, 0.16)
 beta_imd ~ dnorm(0, 0.16)
 beta_beh ~ dnorm(0, 0.16)
 beta_mfq ~ dnorm(0, 0.16)
 beta_fad ~ dnorm(0, 0.16)
 beta_bis ~ dnorm(0, 0.16)
 beta_icu ~ dnorm(0, 0.16)
 beta_cfq ~ dnorm(0, 0.16)
 beta_negem ~ dnorm(0, 0.16)
 beta_daring ~ dnorm(0,0.16)

 inter1 ~ dnorm(0, 0.16)

 or_indifftot<- exp(beta_indifftot)
 or_le<-exp(beta_le)
 or_agehqpdone <- exp(beta_agehqpdone)
 or_sex <- exp(beta_sex)
 or_imd <- exp(beta_imd)
 or_beh <- exp(beta_beh)
 or_mfq<-exp(beta_mfq)
 or_fad<-exp(beta_fad)
 or_bis<-exp(beta_bis)
 or_icu<-exp(beta_icu)
 or_cfq<-exp(beta_cfq)
 or_negem<-exp(beta_negem)
 or_daring<-exp(beta_daring)

 or_inter1<- exp(inter1)

 aj ~ dnorm(0, 0.01) 
 bj ~ dnorm(0, 0.16)
 cj ~ dnorm(0, 0.16)
 dj ~ dnorm(0, 0.16)
 ej ~ dnorm(0, 0.16) 
 fj ~ dnorm(0, 0.16)
 gj ~ dnorm(0, 0.16)
 hj ~ dnorm(0, 0.16)
 ij ~ dnorm(0, 0.16)
 jj ~ dnorm(0, 0.16)
 kj~ dnorm(0, 0.16)
 lj~ dnorm(0, 0.16)
 mj~ dnorm(0, 0.16)
 nj ~ dnorm(0, 0.16)

 al ~ dnorm(0, 0.01) 
 bl ~ dnorm(0, 0.16)
 cl ~ dnorm(0, 0.16)
 dl ~ dnorm(0, 0.16)
 el ~ dnorm(0, 0.16)
 fl ~ dnorm(0, 0.16)
 gl ~ dnorm(0, 0.16)
 hl ~ dnorm(0, 0.16)
 il ~ dnorm(0, 0.16)
 jl ~ dnorm(0, 0.16)
 kl ~ dnorm(0, 0.16)
 ll ~ dnorm(0, 0.16)
 ml~ dnorm(0, 0.16)


 ac ~ dnorm(0, 0.01) 
 bc ~ dnorm(0, 0.16)
 cc ~ dnorm(0, 0.16)
 dc ~ dnorm(0, 0.16)
 ec ~ dnorm(0, 0.16)
 fc ~ dnorm(0, 0.16)
 gc ~ dnorm(0, 0.16)
 hc ~ dnorm(0, 0.16)
 ic ~ dnorm(0, 0.16)
 jc ~ dnorm(0, 0.16)
 kc ~ dnorm(0, 0.16)
 lc ~ dnorm(0, 0.16)

 ab ~ dnorm(0, 0.01) 
 bb ~ dnorm(0, 0.16)
 cb ~ dnorm(0, 0.16)
 db ~ dnorm(0, 0.16)
 eb ~ dnorm(0, 0.16)
 fb ~ dnorm(0, 0.16)
 gb ~ dnorm(0, 0.16)
 hb ~ dnorm(0, 0.16)
 ib ~ dnorm(0, 0.16)
 jb ~ dnorm(0, 0.16)
 kb ~ dnorm(0, 0.16)


 as ~ dnorm(0, 0.01) 
 bs ~ dnorm(0, 0.16)
 cs ~ dnorm(0, 0.16)
 ds ~ dnorm(0, 0.16)
 es ~ dnorm(0, 0.16)
 fs ~ dnorm(0, 0.16)
 gs ~ dnorm(0, 0.16)
 hs ~ dnorm(0, 0.16)
 is ~ dnorm(0, 0.16)
 js ~ dnorm(0, 0.16)

 aa ~ dnorm(0, 0.01) 
 ba ~ dnorm(0, 0.16)
 ca ~ dnorm(0, 0.16)
 da ~ dnorm(0, 0.16)
 ea ~ dnorm(0, 0.16)
 fa ~ dnorm(0, 0.16)
 ga ~ dnorm(0, 0.16)
 ha ~ dnorm(0, 0.16)
 ia ~ dnorm(0, 0.16)

 
 af ~ dnorm(0, 0.01) 
 bf ~ dnorm(0, 0.16) 
 cf ~ dnorm(0, 0.16) 
 df ~ dnorm(0, 0.16)
 ef ~ dnorm(0, 0.16)
 ff ~ dnorm(0, 0.16)
 gf ~ dnorm(0, 0.16)
 hf ~ dnorm(0, 0.16)
 
 ai ~ dnorm(0, 0.01) 
 bi ~ dnorm(0, 0.16) 
 ci ~ dnorm(0, 0.16) 
 di ~ dnorm(0, 0.16)
 ei ~ dnorm(0, 0.16)
 fi ~ dnorm(0, 0.16)
 gi ~ dnorm(0, 0.16)
 
 ax ~ dnorm(0, 0.01) 
 bx ~ dnorm(0, 0.16) 
 cx ~ dnorm(0, 0.16) 
 dx ~ dnorm(0, 0.16)
 ex ~ dnorm(0, 0.16)
 fx ~ dnorm(0, 0.16)
  
 aw ~ dnorm(0, 0.01) 
 bw ~ dnorm(0, 0.16) 
 cw ~ dnorm(0, 0.16) 
 dw ~ dnorm(0, 0.16)
 ew ~ dnorm(0, 0.16)

 
 ay ~ dnorm(0, 0.01) 
 by ~ dnorm(0, 0.16) 
 cy ~ dnorm(0, 0.16) 
 dy ~ dnorm(0, 0.16)
  
 ar ~ dnorm(0, 0.01) 
 br ~ dnorm(0, 0.16) 
 cr ~ dnorm(0, 0.16) 

 jprec  ~ dgamma(0.01,0.01)
 lprec  ~ dgamma(0.01,0.01)
 cprec ~ dgamma(0.01,0.01)
 bprec  ~ dgamma(0.01,0.01)
 sprec  ~ dgamma(0.01,0.01)
 aprec  ~ dgamma(0.01,0.01)
 fprec  ~ dgamma(0.01,0.01)
 iprec  ~ dgamma(0.01,0.01)
 xprec  ~ dgamma(0.01,0.01)
 wprec  ~ dgamma(0.01,0.01)
 yprec  ~ dgamma(0.01,0.01)
 rprec  ~ dgamma(0.01,0.01)

}"

dat_new<-list(nssi_binary_newt2=t1.t2.onset$nssi_binary_newt2,
              indiff_total=t1.t2.onset$indiff_total,
              leq_total_45yes=t1.t2.onset$leq_total_45yes,
              sex=t1.t2.onset$sex,
              imd = t1.t2.onset$imd, 
              agehqpdone = t1.t2.onset$agehqpdone,
              behtot = t1.t2.onset$behtot,
              mfqtot = t1.t2.onset$mfqtot,
              fadtotal = t1.t2.onset$fadtotal,
              bistotal = t1.t2.onset$bistotal,
              icutotal = t1.t2.onset$icutotal,
              cads_daring=pred_merge$cads_daring,
              cads_negemot=pred_merge$cads_negemot,
              cfqtotal=t1.t2.onset$cfqtotal,
              n = nrow(t1.t2.onset))

ini <- list(list(alpha=0, beta_indifftot=0, beta_agehqpdone=0, beta_sex=0, beta_imd=0,
                 beta_mfq=0, beta_le=0,beta_beh=0,
                 beta_fad=0,beta_bis=0, beta_icu=0, 
                 beta_daring=0,beta_cfq=0, beta_negem=0 ),
            list(alpha=1, beta_indifftot=1, beta_agehqpdone=1, beta_sex=1, beta_imd=1,
                 beta_mfq=1, beta_le=1,beta_beh=1,
                 beta_fad=1, beta_bis=1, beta_icu=1, 
                 beta_daring=1,beta_cfq=1, beta_negem=1))

diab_jag_new <- jags.model(textConnection(model2), 
                       data=dat_new,inits=ini,n.chains = 2)

update(diab_jag_new, 1000)

sam_new <- coda.samples(diab_jag_new,
                        c("alpha","or_le","or_agehqpdone", "or_sex","or_imd",
                          "or_mfq","or_beh",
                          "or_indifftot","or_fad","or_bis","or_icu",
                          "or_daring","or_cfq","or_negem",
                          "or_inter1"),
                        n.iter=15000)

MCMCsummary(sam_new, round=4)

count(is.na(t1.t2.onset$nssi_binary_newt2))

####New onset - abuse #######

t1.t2.onset<- read.csv("t1.t2.onset.csv")
#t1.t2.onset$nssi_type_newt2<-as.factor(t1.t2.onset$nssi_type_newt2)

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

fit2 <- jags(data = dat_new, inits=ini_new,parameters.to.save=params, n.chains = 2, 
             n.iter = 20000, n.adapt = 500,
             n.burnin = 1000, model.file = model2, n.thin = 1)

coefs <- fit1$BUGSoutput

library(brms)
ppc_intervals(fit2, yrep = y2rep)

mmp_brm(fit2, x = "abuse_total", prob = .95)

fit2

pp.check(fit2,observed='fit',simulated='fit.new',
         main="Posterior Predictive Check for Abuse(total) - NSSI (new onset)",cex.main=0.9,cex.lab=1.0)

pp_check(fit2,type="overlaid")

posterior_summary(fit2) %>% round(digits = 2)

diab_jag <- jags.model(textConnection(model2), 
                       data=dat_new,inits=ini_new,n.chains = 2)

update(diab_jag, 1000)

sam <- coda.samples(diab_jag,
                    c("alpha","or_abutot","or_le","or_agehqpdone", "or_sex",
                      "or_wemwbs","or_beh",
                      "or_fad","or_bis",
                      "or_icu","or_daring",
                      "or_inter1"),
                    n.iter=5000)

MCMCsummary(sam, round=4)

gelman.diag(sam)
gelman.plot(sam)
raftery.diag(sam)
superdiag(sam, burnin = 1000)

###### New Onset- control and indifference ########
model2 <- "model {
for (i in 1:n) {

  # Model of interest
  nssi_binary_newt2[i] ~ dbern(p[i])
  
  logit(p[i]) <- alpha + beta_abutot*abuse_total[i]+beta_le*leq_total_45yes[i]+ 
  beta_sex*sex[i]+beta_agehqpdone*agehqpdone[i]+
  beta_beh*behtot[i]+beta_wemwbs*wemwbs_total[i]+ beta_fad*fadtotal[i]+
  beta_bis*bistotal[i]+beta_icu*icutotal[i]+beta_daring*cads_daring[i]+
  beta_loi*loitot_3cat[i]+beta_negem*cads_negemot[i]+
  beta_cfq*cfqtotal[i]+
  inter1 * abuse_total[i] * leq_total_45yes[i]
  
  # Model for missing covariates
  abuse_total[i] ~ dnorm (abumu[i], jprec)
  abumu[i]<- aj+bj*leq_total_45yes[i]+cj*sex[i]+
  ej*agehqpdone[i]+fj*behtot[i]+gj*wemwbs_total[i]+ hj*fadtotal[i]+
  ij*bistotal[i]+jj*icutotal[i]+kj*cads_daring[i]+
  mj*loitot_3cat[i]+nj*cads_negemot[i]+
  oj*cfqtotal[i]
  
  leq_total_45yes[i] ~ dnorm (leqmu[i], lprec)
  leqmu[i]<- al+bl*sex[i]+
  dl*agehqpdone[i]+el*behtot[i]+fl*wemwbs_total[i]+gl*fadtotal[i]+
  hl*bistotal[i]+il*icutotal[i]+jl*cads_daring[i]+
  ll*loitot_3cat[i]+ml*cads_negemot[i]+
  nl*cfqtotal[i]
  
  behtot[i] ~ dnorm (behmu[i], bprec)
  behmu[i]<- ab+bb*sex[i]+cb*agehqpdone[i]+db*wemwbs_total[i]+ eb*fadtotal[i]+
  fb*bistotal[i]+gb*icutotal[i]+hb*cads_daring[i]+
  jb*loitot_3cat[i]+kb*cads_negemot[i]+
  lb*cfqtotal[i]

  wemwbs_total[i] ~ dnorm (wemmu[i], sprec)
  wemmu[i]<- as+bs*sex[i]+cs*agehqpdone[i]+
  ds*fadtotal[i]+es*bistotal[i]+
  fs*icutotal[i]+gs*cads_daring[i]+
  is*loitot_3cat[i]+js*cads_negemot[i]+
  ks*cfqtotal[i]
  
  fadtotal[i] ~ dnorm (fadmu[i], fprec)
  fadmu[i]<- af+bf*sex[i]+cf*agehqpdone[i]+
  df*bistotal[i]+
  ef*icutotal[i]+gf*cads_daring[i]+
  if*loitot_3cat[i]+jf*cads_negemot[i]+
  kf*cfqtotal[i]
  
  bistotal[i] ~ dnorm (bismu[i], iprec)
  bismu[i]<- ai+bi*sex[i]+ci*agehqpdone[i]+
  di*icutotal[i]+
  ei*cads_daring[i]+
  gi*loitot_3cat[i]+hi*cads_negemot[i]+
  ii*cfqtotal[i]
  
  cads_daring[i] ~ dnorm (daringmu[i], xprec)
  daringmu[i]<- ax+bx*sex[i]+cx*agehqpdone[i]+
  dx*icutotal[i]+
  fx*loitot_3cat[i]+gx*cads_negemot[i]+
  hx*cfqtotal[i]
  
  icutotal[i] ~ dnorm (icumu[i], wprec)
  icumu[i]<- aw+bw*sex[i]+cw*agehqpdone[i]+
  ew*loitot_3cat[i]+fw*cads_negemot[i]+
  gw*cfqtotal[i]
  
  loitot_3cat[i] ~ dnorm (loimu[i], aprec)
  loimu[i]<- aa+ba*sex[i]+ca*agehqpdone[i]+da*cads_negemot[i]+
  ea*cfqtotal[i]
  
  cads_negemot[i] ~ dnorm (negemmu[i], pprec)
  negemmu[i]<- ap+bp*sex[i]+cp*agehqpdone[i]+dp*cfqtotal[i]
  
  cfqtotal[i] ~ dnorm (cfqmu[i], oprec)
  cfqmu[i]<- ao+bo*sex[i]+co*agehqpdone[i]
  
}
 
 # Priors
 alpha ~ dlogis(0, 0.01) 
 beta_abutot~ dnorm(0, 0.16)
 beta_le ~ dnorm(0, 0.16)
 beta_agehqpdone ~ dnorm(0, 0.16)
 beta_sex  ~ dnorm(0, 0.16)
 beta_imd ~ dnorm(0, 0.16)
 beta_beh ~ dnorm(0, 0.16)
 beta_mfq ~ dnorm(0, 0.16)
 beta_self ~ dnorm(0, 0.16)
 beta_apsd ~ dnorm(0, 0.16)
 beta_prosoc ~ dnorm(0, 0.16)
 beta_daring ~ dnorm(0, 0.16)
 beta_negem ~ dnorm(0, 0.16)
 beta_fad ~ dnorm(0, 0.16)
 beta_bis ~ dnorm(0, 0.16)
 beta_icu ~ dnorm(0, 0.16)
 beta_wemwbs ~ dnorm(0, 0.16)
 beta_centre ~ dnorm(0, 0.16)
 beta_nssi1 ~ dnorm(0, 0.16)
 beta_loi ~ dnorm(0, 0.16)
 beta_cfq ~ dnorm(0, 0.16)
 beta_ethnic ~ dnorm(0, 0.16)

 inter1 ~ dnorm(0, 0.16)

 or_abutot<- exp(beta_abutot)
 or_le<-exp(beta_le)
 or_agehqpdone <- exp(beta_agehqpdone)
 or_sex <- exp(beta_sex)
 or_imd <- exp(beta_imd)
 or_beh <- exp(beta_beh)
 or_mfq<-exp(beta_mfq)
 or_self<-exp(beta_self)
 or_apsd<-exp(beta_apsd)
 or_prosoc<-exp(beta_prosoc)
 or_daring<-exp(beta_daring)
 or_negem<-exp(beta_negem)
 or_fad<-exp(beta_fad)
 or_bis<-exp(beta_bis)
 or_icu<-exp(beta_icu)
 or_wemwbs<-exp(beta_wemwbs)
 or_centre<-exp(beta_centre)
 or_nssi1<-exp(beta_nssi1)
 or_loi<-exp(beta_loi)
 or_cfq<-exp(beta_cfq)
 or_eth<-exp(beta_ethnic)

 or_inter1<- exp(inter1)

 aj ~ dnorm(0, 0.01) 
 bj ~ dnorm(0, 0.16)
 cj ~ dnorm(0, 0.16)
 dj ~ dnorm(0, 0.16)
 ej ~ dnorm(0, 0.16) 
 fj ~ dnorm(0, 0.16)
 gj ~ dnorm(0, 0.16)
 hj ~ dnorm(0, 0.16)
 ij ~ dnorm(0, 0.16)
 jj ~ dnorm(0, 0.16)
 kj~ dnorm(0, 0.16)
 lj~ dnorm(0, 0.16)
 mj~ dnorm(0, 0.16)
 nj ~ dnorm(0, 0.16)
 oj~ dnorm(0, 0.16)
 pj~ dnorm(0, 0.16)
 qj~ dnorm(0, 0.16)
 rj~ dnorm(0, 0.16)
 sj~ dnorm(0, 0.16)

 al ~ dnorm(0, 0.01) 
 bl ~ dnorm(0, 0.16)
 cl ~ dnorm(0, 0.16)
 dl ~ dnorm(0, 0.16)
 el ~ dnorm(0, 0.16)
 fl ~ dnorm(0, 0.16)
 gl ~ dnorm(0, 0.16)
 hl ~ dnorm(0, 0.16)
 il ~ dnorm(0, 0.16)
 jl ~ dnorm(0, 0.16)
 kl ~ dnorm(0, 0.16)
 ll ~ dnorm(0, 0.16)
 ml~ dnorm(0, 0.16)
 nl ~ dnorm(0, 0.16)
 ol~ dnorm(0, 0.16)
 pl~ dnorm(0, 0.16)
 ql~ dnorm(0, 0.16)
 rl~ dnorm(0, 0.16)

 ac ~ dnorm(0, 0.01) 
 bc ~ dnorm(0, 0.16)
 cc ~ dnorm(0, 0.16)
 dc ~ dnorm(0, 0.16)
 ec ~ dnorm(0, 0.16)
 fc ~ dnorm(0, 0.16)
 gc ~ dnorm(0, 0.16)
 hc ~ dnorm(0, 0.16)
 ic ~ dnorm(0, 0.16)
 jc ~ dnorm(0, 0.16)
 kc ~ dnorm(0, 0.16)
 lc ~ dnorm(0, 0.16)
 mc~ dnorm(0, 0.16)
 nc ~ dnorm(0, 0.16)
 oc~ dnorm(0, 0.16)
 pc~ dnorm(0, 0.16)
 qc~ dnorm(0, 0.16)

 ab ~ dnorm(0, 0.01) 
 bb ~ dnorm(0, 0.16)
 cb ~ dnorm(0, 0.16)
 db ~ dnorm(0, 0.16)
 eb ~ dnorm(0, 0.16)
 fb ~ dnorm(0, 0.16)
 gb ~ dnorm(0, 0.16)
 hb ~ dnorm(0, 0.16)
 ib ~ dnorm(0, 0.16)
 jb ~ dnorm(0, 0.16)
 kb ~ dnorm(0, 0.16)
 lb ~ dnorm(0, 0.16)
 mb~ dnorm(0, 0.16)
 nb ~ dnorm(0, 0.16)
 ob~ dnorm(0, 0.16)
 pb~ dnorm(0, 0.16)


 as ~ dnorm(0, 0.01) 
 bs ~ dnorm(0, 0.16)
 cs ~ dnorm(0, 0.16)
 ds ~ dnorm(0, 0.16)
 es ~ dnorm(0, 0.16)
 fs ~ dnorm(0, 0.16)
 gs ~ dnorm(0, 0.16)
 hs ~ dnorm(0, 0.16)
 is ~ dnorm(0, 0.16)
 js ~ dnorm(0, 0.16)
 ks ~ dnorm(0, 0.16)
 ls ~ dnorm(0, 0.16)
 ms~ dnorm(0, 0.16)
 ns ~ dnorm(0, 0.16)
 os~ dnorm(0, 0.16)

 aa ~ dnorm(0, 0.01) 
 ba ~ dnorm(0, 0.16)
 ca ~ dnorm(0, 0.16)
 da ~ dnorm(0, 0.16)
 ea ~ dnorm(0, 0.16)
 fa ~ dnorm(0, 0.16)
 ga ~ dnorm(0, 0.16)
 ha ~ dnorm(0, 0.16)
 ia ~ dnorm(0, 0.16)
 ja ~ dnorm(0, 0.16)
 ka ~ dnorm(0, 0.16)
 la ~ dnorm(0, 0.16)
 ma~ dnorm(0, 0.16)
 na ~ dnorm(0, 0.16)

 ap ~ dnorm(0, 0.01) 
 bp ~ dnorm(0, 0.16)
 cp ~ dnorm(0, 0.16)
 dp ~ dnorm(0, 0.16)
 ep ~ dnorm(0, 0.16)
 fp ~ dnorm(0, 0.16)
 gp ~ dnorm(0, 0.16)
 hp ~ dnorm(0, 0.16)
 ip ~ dnorm(0, 0.16)
 jp ~ dnorm(0, 0.16)
 kp ~ dnorm(0, 0.16)
 lp ~ dnorm(0, 0.16)
 mp~ dnorm(0, 0.16)
 
 ao ~ dnorm(0, 0.01) 
 bo ~ dnorm(0, 0.16) 
 co ~ dnorm(0, 0.16) 
 do ~ dnorm(0, 0.16)
 eo ~ dnorm(0, 0.16)
 fo ~ dnorm(0, 0.16)
 go ~ dnorm(0, 0.16)
 ho ~ dnorm(0, 0.16)
 io ~ dnorm(0, 0.16)
 jo ~ dnorm(0, 0.16)
 ko ~ dnorm(0, 0.16)
 lo ~ dnorm(0, 0.16)
 
 af ~ dnorm(0, 0.01) 
 bf ~ dnorm(0, 0.16) 
 cf ~ dnorm(0, 0.16) 
 df ~ dnorm(0, 0.16)
 ef ~ dnorm(0, 0.16)
 ff ~ dnorm(0, 0.16)
 gf ~ dnorm(0, 0.16)
 hf ~ dnorm(0, 0.16)
 if ~ dnorm(0, 0.16)
 jf ~ dnorm(0, 0.16)
 kf ~ dnorm(0, 0.16)
 
 ai ~ dnorm(0, 0.01) 
 bi ~ dnorm(0, 0.16) 
 ci ~ dnorm(0, 0.16) 
 di ~ dnorm(0, 0.16)
 ei ~ dnorm(0, 0.16)
 fi ~ dnorm(0, 0.16)
 gi ~ dnorm(0, 0.16)
 hi ~ dnorm(0, 0.16)
 ii ~ dnorm(0, 0.16)
 ji ~ dnorm(0, 0.16)
 
 ax ~ dnorm(0, 0.01) 
 bx ~ dnorm(0, 0.16) 
 cx ~ dnorm(0, 0.16) 
 dx ~ dnorm(0, 0.16)
 ex ~ dnorm(0, 0.16)
 fx ~ dnorm(0, 0.16)
 gx ~ dnorm(0, 0.16)
 hx ~ dnorm(0, 0.16)
 ix ~ dnorm(0, 0.16)
  
 av ~ dnorm(0, 0.01) 
 bv ~ dnorm(0, 0.16) 
 cv ~ dnorm(0, 0.16) 
 dv ~ dnorm(0, 0.16)
 ev ~ dnorm(0, 0.16)
 fv ~ dnorm(0, 0.16)
 gv ~ dnorm(0, 0.16)
 hv ~ dnorm(0, 0.16)
 
 at ~ dnorm(0, 0.01) 
 bt ~ dnorm(0, 0.16) 
 ct ~ dnorm(0, 0.16) 
 dt ~ dnorm(0, 0.16)
 et ~ dnorm(0, 0.16)
 ft ~ dnorm(0, 0.16)
 gt ~ dnorm(0, 0.16)
 
 aw ~ dnorm(0, 0.01) 
 bw ~ dnorm(0, 0.16) 
 cw ~ dnorm(0, 0.16) 
 dw ~ dnorm(0, 0.16)
 ew ~ dnorm(0, 0.16)
 fw ~ dnorm(0, 0.16)
 gw ~ dnorm(0, 0.16)
 
 ay ~ dnorm(0, 0.01) 
 by ~ dnorm(0, 0.16) 
 cy ~ dnorm(0, 0.16) 
 dy ~ dnorm(0, 0.16)
 ey ~ dnorm(0, 0.16)
 

 jprec  ~ dgamma(0.01,0.01)
 lprec  ~ dgamma(0.01,0.01)
 cprec ~ dgamma(0.01,0.01)
 bprec  ~ dgamma(0.01,0.01)
 sprec  ~ dgamma(0.01,0.01)
 aprec  ~ dgamma(0.01,0.01)
 pprec  ~ dgamma(0.01,0.01)
 oprec  ~ dgamma(0.01,0.01)
 fprec  ~ dgamma(0.01,0.01)
 iprec  ~ dgamma(0.01,0.01)
 xprec  ~ dgamma(0.01,0.01)
 vprec  ~ dgamma(0.01,0.01)
 tprec  ~ dgamma(0.01,0.01)
 wprec  ~ dgamma(0.01,0.01)
 yprec  ~ dgamma(0.01,0.01)

}"

dat_new<-list(nssi_binary_newt2=t1.t2.onset$nssi_binary_newt2,
          abuse_total=t1.t2.onset$abuse_total,
          leq_total_45yes=t1.t2.onset$leq_total_45yes,
          sex=t1.t2.onset$sex,
          agehqpdone = t1.t2.onset$agehqpdone,
          behtot = t1.t2.onset$behtot,
          wemwbs_total = t1.t2.onset$wemwbs_total,
          fadtotal = t1.t2.onset$fadtotal,
          bistotal = t1.t2.onset$bistotal,
          icutotal=t1.t2.onset$icutotal,
          cads_daring=t1.t2.onset$cads_daring,
          cads_negemot=t1.t2.onset$cads_negemot,
          loitot_3cat=t1.t2.onset$loitot_3cat,
          cfqtotal=t1.t2.onset$cfqtotal,
          n = nrow(t1.t2.onset))

ini_new <- list(list(alpha=0, beta_abutot=0, beta_agehqpdone=0, beta_sex=0, 
                 beta_wemwbs=0, beta_le=0,beta_beh=0,
                 beta_fad=0,beta_bis=0, beta_icu=0,
                 beta_daring=0, beta_loi=1, beta_negem=0,beta_cfq=0),
            list(alpha=1, beta_abutot=1, beta_agehqpdone=1, beta_sex=1,
                 beta_wemwbs=1, beta_le=1,beta_beh=1,
                 beta_fad=1, beta_bis=1, beta_icu=1,
                 beta_daring=1, beta_loi=1,beta_negem=1,beta_cfq=1))

diab_jag_new <- jags.model(textConnection(model2), 
                       data=dat_new,inits=ini_new,n.chains = 2)

update(diab_jag_new, 1000)

sam_new <- coda.samples(diab_jag_new,
                    c("alpha","or_abutot","or_le","or_agehqpdone", "or_sex",
                      "or_wemwbs","or_beh",
                      "or_fad","or_bis","or_negem",
                      "or_icu","or_daring", "or_loi","or_cfq",
                      "or_inter1"),
                    n.iter=5000)

MCMCsummary(sam_new, round=4)

##### Reduced Model -Control ###########
model2 <- tempfile()

writeLines("model {
for (i in 1:n) {

  # Model of interest
  nssi_binary_newt2[i] ~ dbern(p[i])
  
      # posterior predictive
  res[i]<-nssi_binary_newt2[i] - logit(p[i])
  
  nssi_binary_newt2_rep[i] ~ dbern(p[i])
  
  res.new[i] <- nssi_binary_newt2_rep[i] - logit(p[i])
  
  logit(p[i]) <- alpha + beta_contot*control_total[i]+beta_le*leq_total_45yes[i]+ 
  beta_sex*sex[i]+beta_agehqpdone*agehqpdone[i]+
  beta_beh*behtot[i]+beta_wemwbs*wemwbs_total[i]+
  beta_bis*bistotal[i]+beta_fad*fadtotal[i]+
  inter1 * control_total[i] * leq_total_45yes[i]
  
  # Model for missing covariates
  control_total[i] ~ dnorm (conmu[i], jprec)
  conmu[i]<- aj+bj*leq_total_45yes[i]+cj*sex[i]+
  dj*agehqpdone[i]+ej*behtot[i]+fj*wemwbs_total[i]+
  gj*bistotal[i]+hj*fadtotal[i]
  
  leq_total_45yes[i] ~ dnorm (leqmu[i], lprec)
  leqmu[i]<- al+bl*sex[i]+cl*agehqpdone[i]+dl*behtot[i]+el*wemwbs_total[i]+
  fl*bistotal[i]+gl*fadtotal[i]
  
  behtot[i] ~ dnorm (behmu[i], bprec)
  behmu[i]<- ab+bb*sex[i]+cb*agehqpdone[i]+db*wemwbs_total[i]+
  eb*bistotal[i]+fb*fadtotal[i]

  wemwbs_total[i] ~ dnorm (wemmu[i], sprec)
  wemmu[i]<- as+bs*sex[i]+cs*agehqpdone[i]+ds*bistotal[i]+es*fadtotal[i]
  
  bistotal[i] ~ dnorm (bismu[i], iprec)
  bismu[i]<- ai+bi*sex[i]+ci*agehqpdone[i]+di*fadtotal[i]
  
  fadtotal[i] ~ dnorm (fadmu[i], aprec)
  fadmu[i]<- aa+ba*sex[i]+ca*agehqpdone[i]
  
}
 
 # Priors
 alpha ~ dt(0, 1/10^2, 1)
 beta_contot~ dt(0, 1/2.5^2, 1)
 beta_le ~ dt(0, 1/2.5^2, 1)
 beta_agehqpdone ~ dt(0, 1/2.5^2, 1)
 beta_sex  ~ dt(0, 1/2.5^2, 1)
 beta_beh ~ dt(0, 1/2.5^2, 1)
 beta_bis ~ dt(0, 1/2.5^2, 1)
 beta_wemwbs ~ dt(0, 1/2.5^2, 1)
 beta_fad ~ dt(0, 1/2.5^2, 1)
 inter1 ~ dt(0, 1/2.5^2, 1)

 or_contot<- exp(beta_contot)
 or_le<-exp(beta_le)
 or_agehqpdone <- exp(beta_agehqpdone)
 or_sex <- exp(beta_sex)
 or_beh <- exp(beta_beh)
 or_bis<-exp(beta_bis)
 or_wemwbs<-exp(beta_wemwbs)
 or_fad<-exp(beta_fad)
 or_inter1<- exp(inter1)

 aj ~ dt(0, 1/2.5^2, 1)
 bj ~ dt(0, 1/2.5^2, 1)
 cj ~ dt(0, 1/2.5^2, 1)
 dj ~ dt(0, 1/2.5^2, 1)
 ej ~ dt(0, 1/2.5^2, 1) 
 fj ~ dt(0, 1/2.5^2, 1)
 gj ~ dt(0, 1/2.5^2, 1)
 hj~ dt(0, 1/2.5^2, 1)


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

 ai ~ dt(0, 1/2.5^2, 1)
 bi ~ dt(0, 1/2.5^2, 1)
 ci ~ dt(0, 1/2.5^2, 1)
 di ~ dt(0, 1/2.5^2, 1)
 
 aa ~ dt(0, 1/2.5^2, 1)
 ba ~ dt(0, 1/2.5^2, 1)
 ca ~ dt(0, 1/2.5^2, 1)

 jprec  ~ dgamma(0.01,0.01)
 lprec  ~ dgamma(0.01,0.01)

 bprec  ~ dgamma(0.01,0.01)
 sprec  ~ dgamma(0.01,0.01)

 iprec  ~ dgamma(0.01,0.01)
 
 aprec  ~ dgamma(0.01,0.01)
 
  fit <- sum(res[])
  fit.new <- sum(res.new[])

}", con=model2)

dat_new<-list(nssi_binary_newt2=t1.t2.onset$nssi_binary_newt2,
              control_total=t1.t2.onset$control_total,
              leq_total_45yes=t1.t2.onset$leq_total_45yes,
              sex=t1.t2.onset$sex,
              agehqpdone = t1.t2.onset$agehqpdone,
              behtot = t1.t2.onset$behtot,
              wemwbs_total = t1.t2.onset$wemwbs_total,
              bistotal = t1.t2.onset$bistotal,
              fadtotal = t1.t2.onset$fadtotal,
              n = nrow(t1.t2.onset))

ini_new <- list(list(alpha=0, beta_contot=0, beta_agehqpdone=0, beta_sex=0, 
                     beta_wemwbs=0, beta_le=0,beta_beh=0,beta_fad=0,
                     beta_bis=0),
                list(alpha=1, beta_contot=1, beta_agehqpdone=1, beta_sex=1, 
                     beta_wemwbs=1, beta_le=1,beta_beh=1,beta_fad=1,
                     beta_bis=1))


params <- c("alpha","or_contot","or_le","or_agehqpdone", "or_sex",
            "or_wemwbs","or_beh",'or_fad',
            "or_bis","or_inter1","fit","fit.new")

fit_contot_new <- jags(data = dat_new, inits=ini_new,parameters.to.save=params, n.chains = 2, 
             n.iter = 20000, n.adapt = 500,
             n.burnin = 1000, model.file = model2, n.thin = 1)

fit_contot_new

pp.check(fit_contot_new,observed='fit',simulated='fit.new',
         main="Posterior Predictive Check for Control(total) - NSSI(new onset)",
         cex.main=0.9,cex.lab=1.0)




diab_jag_new <- jags.model(textConnection(model2), 
                           data=dat_new,inits=ini_new, n.chains = 2)
#If scale, add inits= ini_new

update(diab_jag_new, 1000)

sam_new <- coda.samples(diab_jag_new,
                        c("alpha","or_indifftot","or_le","or_agehqpdone", "or_sex",
                          "or_wemwbs","or_beh","or_bis","or_fad",
                          "or_inter1"),
                        n.iter=10000)

MCMCsummary(sam_new, round=4)

#####New Onset - INDIFF - Final #####
model2 <- tempfile()

writeLines("model {
for (i in 1:n) {

  # Model of interest
  nssi_binary_newt2[i] ~ dbern(p[i])
  
      # posterior predictive
  res[i]<-nssi_binary_newt2[i] - logit(p[i])
  
  nssi_binary_newt2_rep[i] ~ dbern(p[i])
  
  res.new[i] <- nssi_binary_newt2_rep[i] - logit(p[i])
  
  logit(p[i]) <- alpha + beta_indifftot*indiff_total[i]+beta_le*leq_total_45yes[i]+ 
  beta_agehqpdone*agehqpdone[i]+
  beta_beh*behtot[i]+beta_wemwbs*wemwbs_total[i]+
  beta_bis*bistotal[i]+beta_fad*fadtotal[i]+
  inter1 * indiff_total[i] * leq_total_45yes[i]
  
  # Model for missing covariates
  indiff_total[i] ~ dnorm (indiffmu[i], jprec)
  indiffmu[i]<- aj+bj*leq_total_45yes[i]+
  dj*agehqpdone[i]+ej*behtot[i]+fj*wemwbs_total[i]+
  gj*bistotal[i]+hj*fadtotal[i]
  
  leq_total_45yes[i] ~ dnorm (leqmu[i], lprec)
  leqmu[i]<- al+cl*agehqpdone[i]+dl*behtot[i]+el*wemwbs_total[i]+
  fl*bistotal[i]+gl*fadtotal[i]
  
  behtot[i] ~ dnorm (behmu[i], bprec)
  behmu[i]<- ab+cb*agehqpdone[i]+db*wemwbs_total[i]+
  eb*bistotal[i]+fb*fadtotal[i]

  wemwbs_total[i] ~ dnorm (wemmu[i], sprec)
  wemmu[i]<- as+cs*agehqpdone[i]+ds*bistotal[i]+es*fadtotal[i]
  
  bistotal[i] ~ dnorm (bismu[i], iprec)
  bismu[i]<- ai+ci*agehqpdone[i]+di*fadtotal[i]
  
  fadtotal[i] ~ dnorm (fadmu[i], aprec)
  fadmu[i]<- aa+ca*agehqpdone[i]
  
}
 
 # Priors
 alpha ~ dt(0, 1/10^2, 1)
 beta_indifftot~ dt(0, 1/2.5^2, 1)
 beta_le ~ dt(0, 1/2.5^2, 1)
 beta_agehqpdone ~ dt(0, 1/2.5^2, 1)
 beta_sex  ~ dt(0, 1/2.5^2, 1)
 beta_beh ~ dt(0, 1/2.5^2, 1)
 beta_bis ~ dt(0, 1/2.5^2, 1)
 beta_wemwbs ~ dt(0, 1/2.5^2, 1)
 beta_fad ~ dt(0, 1/2.5^2, 1)
 inter1 ~ dt(0, 1/2.5^2, 1)

 or_indifftot<- exp(beta_indifftot)
 or_le<-exp(beta_le)
 or_agehqpdone <- exp(beta_agehqpdone)
 or_sex <- exp(beta_sex)
 or_beh <- exp(beta_beh)
 or_bis<-exp(beta_bis)
 or_wemwbs<-exp(beta_wemwbs)
 or_fad<-exp(beta_fad)
 or_inter1<- exp(inter1)

 aj ~ dt(0, 1/2.5^2, 1)
 bj ~ dt(0, 1/2.5^2, 1)
 cj ~ dt(0, 1/2.5^2, 1)
 dj ~ dt(0, 1/2.5^2, 1)
 ej ~ dt(0, 1/2.5^2, 1) 
 fj ~ dt(0, 1/2.5^2, 1)
 gj ~ dt(0, 1/2.5^2, 1)
 hj~ dt(0, 1/2.5^2, 1)


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

 ai ~ dt(0, 1/2.5^2, 1)
 bi ~ dt(0, 1/2.5^2, 1)
 ci ~ dt(0, 1/2.5^2, 1)
 di ~ dt(0, 1/2.5^2, 1)
 
 aa ~ dt(0, 1/2.5^2, 1)
 ba ~ dt(0, 1/2.5^2, 1)
 ca ~ dt(0, 1/2.5^2, 1)

 jprec  ~ dgamma(0.01,0.01)
 lprec  ~ dgamma(0.01,0.01)

 bprec  ~ dgamma(0.01,0.01)
 sprec  ~ dgamma(0.01,0.01)

 iprec  ~ dgamma(0.01,0.01)
 
 aprec  ~ dgamma(0.01,0.01)
 
  fit <- sum(res[])
  fit.new <- sum(res.new[])

}", con=model2)

dat_new<-list(nssi_binary_newt2=t1.t2.onset$nssi_binary_newt2,
              indiff_total=t1.t2.onset$indiff_total,
              leq_total_45yes=t1.t2.onset$leq_total_45yes,
              agehqpdone = t1.t2.onset$agehqpdone,
              behtot = t1.t2.onset$behtot,
              wemwbs_total = t1.t2.onset$wemwbs_total,
              bistotal = t1.t2.onset$bistotal,
              fadtotal = t1.t2.onset$fadtotal,
              n = nrow(t1.t2.onset))

ini_new <- list(list(alpha=0, beta_indifftot=0, beta_agehqpdone=0, 
                     beta_wemwbs=0, beta_le=0,beta_beh=0,beta_fad=0,
                     beta_bis=0),
                list(alpha=1, beta_indifftot=1, beta_agehqpdone=1,
                     beta_wemwbs=1, beta_le=1,beta_beh=1,beta_fad=1,
                     beta_bis=1))


params <- c("alpha","or_indifftot","or_le","or_agehqpdone",
            "or_wemwbs","or_beh",'or_fad',
            "or_bis","or_inter1","fit","fit.new")

fit_indifftot_new <- jags(data = dat_new, inits=ini_new,parameters.to.save=params, n.chains = 2, 
                       n.iter = 20000, n.adapt = 500,
                       n.burnin = 1000, model.file = model2, n.thin = 1)

fit_indifftot_new

pp.check(fit_indifftot_new,observed='fit',simulated='fit.new',
         main="Posterior Predictive Check for Indifference(total) - NSSI(new onset)",
         cex.main=0.9,cex.lab=1.0)


####Ordinal logistic regression #########
library(brms)
pred_merge1<- pred_merge%>%
  mutate(nssi_t2_cat= as.ordered(case_when(nssi_binary_t2 == 0 ~ "0",
                                       nssi_cat_type == "sporadic" ~ "1",
                                       nssi_cat_type == "recurrent" ~ "2")))
str(pred_merge1$nssi_t2_cat)

model3<- brm(nssi_t2_cat ~ abuse_total+leq_total_45yes+sex+agehqpdone+behtot+imd+
               selftot+cads_daring+cads_prosoc+apsdtotal+bistotal+fadtotal+abuse_total*leq_total_45yes,
             data=pred_merge1,
             family=cumulative(link="logit",threshold='flexible'),
             prior = c(set_prior(prior = "normal(0,10)", class = "Intercept"),
                       set_prior(prior = "normal(0,2.5)", coef = c("abuse_total",
 "leq_total_45yes","sex","agehqpdone","behtot","imd","selftot","cads_daring","cads_prosoc",
 "apsdtotal","bistotal","fadtotal"))),
             warmup = 1000, iter = 10000, chains = 2,
             control = list(adapt_delta = 0.95))
summary(model3)
