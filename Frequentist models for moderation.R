#### New onset - outliers - abuse ######
t1.t2.onset<- read.csv("t1.t2.onset.csv")

t1.t2.onset<- dplyr::select(t1.t2.onset,-event)
t1.t2.onset<- dplyr::select(t1.t2.onset,-id_nspn)

t1.t2.onset[,c(4:6,8:9,39)]<-scale(t1.t2.onset[,c(4:6,8:9,39)],center=TRUE, scale=TRUE)
t1.t2.onset[,c(7,10:38)]<-0.5*scale(t1.t2.onset[,c(7,10:38)],center=TRUE, scale=TRUE)

model1_abu_new <- glm(nssi_binary_newt2 ~ abuse_total+leq_total_45yes+
                        abuse_total:leq_total_45yes + sex + agehqpdone+behtot+wemwbs_total+
                        bistotal, data = t1.t2.onset,
                      family=binomial)

summary(model1_abu_new)
require(MASS)
exp(cbind(coef(model1_abu_new),confint(model1_abu_new)))

model_drop<-update(model1_abu_new, subset=c(-1655,-1383,-1987,-1351))

compareCoefs(model1_abu_new,model_drop)

exp(cbind(coef(model_drop),confint(model_drop)))

t1.t2.onset<- read.csv("t1.t2.onset.csv")
model1_abu_new <- glm(nssi_binary_newt2 ~ abuse_total+leq_total_45yes+
                        abuse_total:leq_total_45yes + sex + agehqpdone+behtot+wemwbs_total+
                        bistotal, data = t1.t2.onset,
                      family=binomial)

marginalModelPlots(model1_abu_new)

outlierTest(model1_abu_new)
influenceIndexPlot(model1_abu_new, main="Diagnostic plots of abuseXleq and NSSI (new onset)")
influencePlot(model1_abu_new,col='red', main="Influence plot of abuseXleq and NSSI (new onset)")

