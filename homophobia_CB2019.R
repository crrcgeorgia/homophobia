#Homophobia Blog Code
getwd()
setwd("")
library(foreign)
library(haven)
library(Matching)
library(rgenoud)
library(survey)
library(psych)
library(haven)
library(descr)
library(ggeffects)

cb19<-read_dta("CB_2019_Geo_public_09.01.20.dta")
dim(cb19)
names(cb19)


freq(as_factor(cb19$NEIGHBOR), cb19$INDWT)
freq(cb19$NEIGHBOR, cb19$INDWT)

freq(as_factor(cb19$BUSINGA), cb19$INDWT)
cb19[cb19==-3]<-NA
freq(cb19$BUSINUSA, cb19$INDWT)    
freq(cb19$BUSINARM, cb19$INDWT)
freq(cb19$BUSINAZE, cb19$INDWT)
freq(cb19$BUSINITA, cb19$INDWT)
freq(cb19$BUSINARA, cb19$INDWT)
freq(cb19$BUSINGEO, cb19$INDWT)   
freq(cb19$BUSINIRA, cb19$INDWT)
freq(cb19$BUSINJEW, cb19$INDWT)
freq(cb19$BUSINKUR, cb19$INDWT)
freq(cb19$BUSINRUS, cb19$INDWT)
freq(cb19$BUSINTUR, cb19$INDWT)
freq(cb19$BUSINUKR, cb19$INDWT)   
freq(cb19$BUSININD, cb19$INDWT)
freq(cb19$BUSINABK, cb19$INDWT)
freq(cb19$BUSINOSS, cb19$INDWT)
freq(cb19$BUSINARG, cb19$INDWT)
freq(cb19$BUSINAZG, cb19$INDWT)
freq(cb19$BUSINJW, cb19$INDWT)
freq(cb19$BUSINGA, cb19$INDWT)

cb19$BUSINGA_r<-cb19$BUSINGA
cb19$BUSINGA_r[cb19$BUSINGA_r<=-1]<-NA


designgcb19 <- svydesign(id=~PSU,weights=~INDWT, strat=~SUBSTRATUM, data=cb19)

designgcb19$variables$STRATUM<-as_factor(designgcb19$variables$STRATUM)
designgcb19$variables$SEX<-as_factor(designgcb19$variables$SEX)
designgcb19$variables$INT_DATE_r<-as_factor(designgcb19$variables$INT_DATE_r)
designgcb19$variables$OWNAIRC_r<-designgcb19$variables$OWNAIRC
designgcb19$variables$OWNAIRC_r[designgcb19$variables$OWNAIRC_r<=-1]<-NA

designgcb19$variables$OWNCOTV_r<-designgcb19$variables$OWNCOTV
designgcb19$variables$OWNCOTV_r[designgcb19$variables$OWNCOTV_r<=-1]<-NA

designgcb19$variables$OWNDIGC_r<-designgcb19$variables$OWNDIGC
designgcb19$variables$OWNDIGC_r[designgcb19$variables$OWNDIGC_r<=-1]<-NA

designgcb19$variables$OWNWASH_r<-designgcb19$variables$OWNWASH
designgcb19$variables$OWNWASH_r[designgcb19$variables$OWNWASH_r<=-1]<-NA

designgcb19$variables$OWNFRDG_r<-designgcb19$variables$OWNFRDG
designgcb19$variables$OWNFRDG_r[designgcb19$variables$OWNFRDG_r<=-1]<-NA

designgcb19$variables$OWNCARS_r<-designgcb19$variables$OWNCARS
designgcb19$variables$OWNCARS_r[designgcb19$variables$OWNCARS_r<=-1]<-NA

designgcb19$variables$OWNLNDP_r<-designgcb19$variables$OWNLNDP
designgcb19$variables$OWNLNDP_r[designgcb19$variables$OWNLNDP_r<=-1]<-NA

designgcb19$variables$OWNCELL_r<-designgcb19$variables$OWNCELL
designgcb19$variables$OWNCELL_r[designgcb19$variables$OWNCELL_r<=-1]<-NA

designgcb19$variables$OWNPCELL_r<-designgcb19$variables$OWNPCELL
designgcb19$variables$OWNPCELL_r[designgcb19$variables$OWNPCELL_r<=-1]<-NA

designgcb19$variables$OWNCOMP_r<-designgcb19$variables$OWNCOMP
designgcb19$variables$OWNCOMP_r[designgcb19$variables$OWNCOMP_r<=-1]<-NA
designgcb19$variables$wealth<-(designgcb19$variables$OWNAIRC_r+
                                 designgcb19$variables$OWNCOTV_r+
                                 designgcb19$variables$OWNDIGC_r+
                                 designgcb19$variables$OWNWASH_r+
                                 designgcb19$variables$OWNFRDG_r+
                                 designgcb19$variables$OWNCARS_r+
                                 designgcb19$variables$OWNLNDP_r+
                                 designgcb19$variables$OWNCELL_r+
                                 designgcb19$variables$OWNPCELL_r+
                                 designgcb19$variables$OWNCOMP_r)

table(as_factor(designgcb19$variables$EMPLSIT))
designgcb19$variables$EMPLSIT_r<-designgcb19$variables$EMPLSIT
designgcb19$variables$EMPLSIT_r[designgcb19$variables$EMPLSIT_r<=-1]<-NA
designgcb19$variables$EMPLSIT_r[designgcb19$variables$EMPLSIT_r<=4]<-0
designgcb19$variables$EMPLSIT_r[designgcb19$variables$EMPLSIT_r>=7]<-0
designgcb19$variables$EMPLSIT_r[designgcb19$variables$EMPLSIT_r>=2]<-1
designgcb19$variables$EMPLSIT_r<-as_factor(designgcb19$variables$EMPLSIT_r)

table(designgcb19$variables$EDUYRS)
designgcb19$variables$EDUYRS_r<-designgcb19$variables$EDUYRS
designgcb19$variables$EDUYRS_r[designgcb19$variables$EDUYRS_r<=-1]<-NA

table(as_factor(designgcb19$variables$NEIGHBOR))
table(designgcb19$variables$NEIGHBOR)

designgcb19$variables$NEIGHBOR_r<-designgcb19$variables$NEIGHBOR
designgcb19$variables$NEIGHBOR_r[designgcb19$variables$NEIGHBOR_r!=6]<-0
designgcb19$variables$NEIGHBOR_r[designgcb19$variables$NEIGHBOR_r==6]<-1

table(as_factor(designgcb19$variables$FRQINTR))
table(designgcb19$variables$FRQINTR)
designgcb19$variables$FRQINTR_r<-designgcb19$variables$FRQINTR
designgcb19$variables$FRQINTR_r[designgcb19$variables$FRQINTR_r<=-1]<-NA
designgcb19$variables$FRQINTR_r[designgcb19$variables$FRQINTR_r!=1]<-0
designgcb19$variables$FRQINTR_r<-as_factor(designgcb19$variables$FRQINTR_r)
table(designgcb19$variables$FRQINTR_r)

table(as_factor(designgcb19$variables$ETHNIC))
designgcb19$variables$ETHNIC_r<-designgcb19$variables$ETHNIC
designgcb19$variables$ETHNIC_r[designgcb19$variables$ETHNIC_r<=0]<-NA
designgcb19$variables$ETHNIC_r[designgcb19$variables$ETHNIC_r!=3]<-1
designgcb19$variables$ETHNIC_r[designgcb19$variables$ETHNIC_r==3]<-0
designgcb19$variables$ETHNIC_r<-as_factor(designgcb19$variables$ETHNIC_r)
table(as_factor(designgcb19$variables$ETHNIC_r))



model1<-svyglm(NEIGHBOR_r~ETHNIC_r+FRQINTR_r+
                 AGE + 
                 SEX + 
                 STRATUM+
                 EMPLSIT_r+
                 wealth+
                 EDUYRS_r, design = designgcb19, family = "binomial")
summary(model1)
crosstab(designgcb19$variables$NEIGHBOR_r, designgcb19$variables$SEX, prop.c = TRUE, w=  designgcb19$variables$INDWT)
freq(designgcb19$variables$NEIGHBOR_r, w=  designgcb19$variables$INDWT)
plot(ggemmeans(model1, terms = "STRATUM", data = designgcb19$variables))
plot(ggemmeans(model1, terms = c( "SEX"), data = designgcb19$variables))
plot(ggemmeans(model1, terms = c( "ETHNIC_r"), data = designgcb19$variables))



model2<-svyglm(BUSINGA_r~ETHNIC_r+FRQINTR_r+
                 AGE +
                 SEX + 
                 STRATUM+
                 EMPLSIT_r+
                 wealth+
                 EDUYRS_r, design = designgcb19, family = "binomial")
summary(model2)

plot(ggemmeans(model2, terms = "AGE[20, 40, 60, 80]", data = designgcb19$variables))
plot(ggemmeans(model2, terms = "SEX", data = designgcb19$variables))
plot(ggemmeans(model2, terms = "STRATUM", data = designgcb19$variables))
plot(ggemmeans(model2, terms = "wealth[1,2,3,4,5,6,7,8,9,10]", data = designgcb19$variables))
plot(ggemmeans(model2, terms = "EDUYRS_r[5, 10, 15, 20, 25]", data = designgcb19$variables))
plot(ggemmeans(model2, terms = "ETHNIC_r", data = designgcb19$variables))

table(designgcb19$variables$AGE)
x<-plot(ggemmeans(model2, terms = c("AGE[20, 40, 60, 80]"), data = designgcb19$variables))
x$predicted
x<-ggemmeans(model2, terms = "wealth[1:10]", data = designgcb19$variables)
x$predicted
x<-ggemmeans(model2, terms = "EDUYRS_r[1:30]", data = designgcb19$variables)
x$predicted