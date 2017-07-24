
rm(list = ls())
setwd("/home/anshul/Documents/BA/project")
file <- file.choose()
data <- read.csv(file,header = T)
str(data)

var <- sapply(data, is.numeric)
Othervar= !sapply(data,is.numeric)

mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

stats <- data.frame(t(apply(data[var],2,mystats)))
names(data)

o <- subset(data, select = c(region,townsize,gender,agecat,birthmonth,edcat,jobcat,union,employ,
                             empcat,retire,inccat,default,jobsat,marital,spousedcat,homeown,hometype,
                             address,addresscat,cars,carown,cartype,carcatvalue,carbought,carbuy,commute,
                             commutecat,commutecar,commutemotorcycle,commutecarpool,commutebus,commuterail,
                             commutepublic,commutebike,commutewalk,commutenonmotor,telecommute,reason,
                             polview,polparty,polcontrib,vote,card,cardtype,cardbenefit,cardfee,cardtenure,
                             cardtenurecat,card2,card2type,card2benefit,card2fee,card2tenure,card2tenurecat,
                             active,bfast,churn,tollfree,equip,callcard,wireless,multline,voice,pager,internet,
                             callid,callwait,forward,confer,ebill,owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,
                             owngame,ownfax,news,response_01,response_02,response_03))
v <- subset(data, select = -c(custid,region,townsize,gender,agecat,birthmonth,edcat,jobcat,union,employ,
                              empcat,retire,inccat,default,jobsat,marital,spousedcat,homeown,hometype,
                              address,addresscat,cars,carown,cartype,carcatvalue,carbought,carbuy,
                              commute,commutecat,commutecar,commutemotorcycle,commutecarpool,commutebus,
                              commuterail,commutepublic,commutebike,commutewalk,commutenonmotor,
                              telecommute,reason,polview,polparty,polcontrib,vote,card,cardtype,
                              cardbenefit,cardfee,cardtenure,cardtenurecat,card2,card2type,
                              card2benefit,card2fee,card2tenure,card2tenurecat,active,bfast,churn,tollfree,
                              equip,callcard,wireless,multline,voice,pager,internet,callid,callwait,forward,
                              confer,ebill,owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,owngame,ownfax,
                              news,response_01,response_02,response_03))

str(v)


as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

v[,c("lncreddebt","lnothdebt","commutetime","longten","lnlongten","lntollmon","lntollten","lnequipmon","lnequipten",
     "lncardmon","cardten","lncardten","lnwiremon","lnwireten")] <- 
  data.frame(sapply(v[,c("lncreddebt","lnothdebt","commutetime","longten","lnlongten","lntollmon","lntollten","lnequipmon","lnequipten",
                        "lncardmon","cardten","lncardten","lnwiremon","lnwireten")], as.numeric.factor))
str(v)

s1 <- data.frame(t(apply(v,2,mystats)))


require(Hmisc)
v[,c("commutetime","cardten","longten")] <-data.frame(apply(v[,c("commutetime","cardten","longten")], 2, function(x) impute(x, mean)))

v[,c("lncreddebt","lnothdebt","lnlongten","lntollmon","lntollten","lnequipmon","lnequipten","lncardmon","lncardten","lnwiremon","lnwireten")] <- 
  apply(v[,c("creddebt","othdebt","longten","tollmon","tollten","equipmon","equipten","cardmon","cardten","wiremon","wireten")],2,function(x) log(x+0.1))

s2 <- data.frame(t(apply(v, 2, mystats)))

M1_fun <- function(x){
  quantiles <- quantile( x, c(.01, .99 ),na.rm=TRUE )
  x[ x < quantiles[1] ] <- quantiles[1]
  x
  x[ x > quantiles[2] ] <- quantiles[2]
}
names(v)

v[] <-  lapply(v[], M1_fun)
ns <- data.frame(t(apply(v,2,mystats)))
v$totalspent <- v$cardspent + v$card2spent
hist(v$totalspent)
hist(log(v$totalspent))
v$lntotalspent <- log(v$totalspent)

vv1 <- v[v$carvalue==-1,]
vv2 <- v[v$carvalue!=-1,]
fit <- lm(carvalue ~., data=vv2)

carvalue ~ age + income + lninc + debtinc + creddebt + lncreddebt + 
  othdebt + pets_saltfish + cardspent + card2spent + tenure + 
  equipmon + cardmon + lncardten + lnwiremon + lnwireten + 
  hourstv + lntotalspent

summary(fit)
vif(fit)
require(car)
require(MASS)
step3<- stepAIC(fit,direction="both")
summary(step3)
vv3<-cbind(vv2, pred_carvalue = predict(step3))
vv4 <- cbind(vv1, pred_carvalue = predict(step3,vv1))
vv5 <- rbind(vv4,vv3)

v1 <- subset(vv5, select = -c(cardspent,card2spent,totalspent,lntotalspent, carvalue))
corrm1 <- cor(vv5)

## FACTOR ANALYSIS 
corrm<- cor(v1)                                 ### CORRELATION MATRIX

require(psych)
require(GPArotation)

### DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT

eigen(corrm)$values                                                     ### EIGEN VALUES

require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 

write.csv(eigen_values, "P4EigenValues.csv")  ### EXPORTING EIGEN VALUE SUMMARY


FA<-fa(r=corrm, 14, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
print(FA)                                                    ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
#FA_SORT$e.values                                            ### FINDING EIGEN VALUES FROM THE RESULTS
Loadings<-data.frame(FA_SORT$loadings[1:ncol(v1),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME

v2 <- subset(vv5, select = c(lnlongmon,longmon,tenure,age,lninc,income,pred_carvalue,lnothdebt,othdebt,lncreddebt,creddebt,
               equipmon,ed,tollmon,wiremon,cardmon,lncardmon,debtinc,pets,pets_cats,
               pets_dogs,pets_birds,lntotalspent,totalspent,card2items,carditems))

o1 <- o[,c("region","townsize","gender","birthmonth","edcat","jobcat","empcat","retire","union","inccat",
           "default","jobsat","marital","spousedcat","homeown","hometype","addresscat","cars",
           "carown","cartype","carcatvalue","carbought","carbuy","commute","commutecar","commutemotorcycle",
           "commutecarpool","commutebus","commuterail","commutepublic","commutebike","commutewalk",      
           "commutenonmotor","telecommute","reason","polview","polparty","polcontrib","vote","card",
           "cardtype","cardbenefit","cardfee","cardtenurecat","card2","card2type","card2benefit","card2fee",
           "card2tenurecat","active","bfast","churn","tollfree","equip","callcard","wireless","multline",
           "voice","pager","internet","callid","callwait","forward","confer","ebill","owntv","ownvcr",
           "owndvd","owncd","ownpda","ownpc","ownipod","owngame","ownfax","news","response_01",
           "response_02","response_03")]





o1 <- data.frame(lapply(o1, factor))
o2 <- cbind(totalspent = v$totalspent,o1)

tab <- xtabs(~totalspent+card2fee, data = o2)
chisq.test(tab)
chisq.test(tab, simulate.p.value = TRUE)

tab <- xtabs(~active +confer, data = o1)
chisq.test(tab)
chisq.test(tab, simulate.p.value = TRUE)


o2 <- subset(o1,select = c(region,gender,edcat,birthmonth,union,hometype,cars,carbuy,reason,polparty,
                           cardtype,cardbenefit,cardfee,card2type,card2benefit,card2fee,active,confer ,
                          telecommute,commutemotorcycle,commutecarpool))         

"lnlongmon"         ""           ""            "age"               "lninc"             ""           
[7] ""     "lnothdebt"         ""           "lncreddebt"        ""          ""         
[13] "ed"                "tollmon"           ""           ""           "lncardmon"         "debtinc"          
[19] "pets"              "pets_cats"         "pets_dogs"         "pets_birds"        "lntotalspent"      "totalspent"       
[25] "card2items"        "carditems"      


nd <- cbind(v2,o2)
hist(nd$totalspent)
lnlongmon,tenure,age,lninc,carvalue,lnothdebt,lncreddebt,
equipmon,ed,tollmon,wiremon,lncardmon,debtinc,pets,pets_cats,
pets_dogs,pets_birds,lntotalspent,card2items,carditems

sq <- function(x) x^2


nd$explongmon <- exp(nd$longmon)
nd$sqrtlongmon <- sqrt(nd$longmon)
nd$sqlongmon <- sq(nd$longmon)
nd$reclongmon <- 1/nd$longmon




nd$lntenure <- log(nd$tenure)
nd$exptenure <- exp(nd$tenure)
nd$sqrttenure <- sqrt(nd$tenure)
nd$sqtenure <- sq(nd$tenure)
nd$rectenure <- 1/nd$tenure



nd$expincome <- exp(nd$income)
nd$sqrtincome <- sqrt(nd$income)
nd$sqincome <- sq(nd$income)
nd$recincome <- 1/nd$income

nd$lnpred_carvalue <- log(nd$pred_carvalue)
nd$exppred_carvalue <- exp(nd$pred_carvalue)
nd$sqrtpred_carvalue <- sqrt(nd$pred_carvalue)
nd$sqpred_carvalue <- sq(nd$pred_carvalue)
nd$recpred_carvalue <- 1/nd$pred_carvalue


nd$expothdebt <- exp(nd$othdebt)
nd$sqrtothdebt <- sqrt(nd$othdebt)
nd$sqothdebt <- sq(nd$othdebt)
nd$recothdebt <- 1/nd$othdebt


nd$expcreddebt <- exp(nd$creddebt)
nd$sqrtcreddebt <- sqrt(nd$creddebt)
nd$sqcreddebt <- sq(nd$creddebt)
nd$reccreddebt <- 1/nd$creddebt



nd$exptollmon <- exp(nd$tollmon)
nd$sqrttollmon <- sqrt(nd$tollmon)
nd$sqtollmon <- sq(nd$tollmon)
nd$rectollmon <- 1/nd$tollmon


nd$expequipmon <- exp(nd$equipmon)
nd$sqrtequipmon <- sqrt(nd$equipmon)
nd$sqequipmon <- sq(nd$equipmon)
nd$recequipmon <- 1/nd$equipmon


nd$expcardmon <- exp(nd$cardmon)
nd$sqrtcardmon <- sqrt(nd$cardmon)
nd$sqcardmon <- sq(nd$cardmon)
nd$reccardmon <- 1/nd$cardmon

nd$expdebtinc <- exp(nd$debtinc)
nd$sqrtdebtinc <- sqrt(nd$debtinc)
nd$sqdebtinc <- sq(nd$debtinc)
nd$recdebtinc <- 1/nd$debtinc


vars2 <- sapply(nd, is.numeric)
corrm2 <- cor(nd[vars2])


nd1<- subset(nd, select = c(lnlongmon, tenure, age, lninc, pred_carvalue, sqrtothdebt, lncreddebt,
                            sqequipmon, ed, tollmon, wiremon, sqdebtinc,pets,pets_cats,pets_dogs,
                            pets_birds,lntotalspent,card2items,carditems, region,gender,edcat,birthmonth,
                            union,hometype,cars,carbuy,reason,polparty,cardtype,cardbenefit,cardfee,
                            card2type,card2benefit,card2fee,active,confer,telecommute,commutemotorcycle,
                            commutecarpool))




set.seed(123)
#Splitting data into Training, Validaton and Testing Dataset
t1 <- sample(1:nrow(nd), size = floor(0.80 * nrow(nd)))

train <- nd[t1,]
pred <- nd[-t1,]

require(car)
scatterplotMatrix(train)

# Multiple Linear Regression Example 
fit <- lm(lntotalspent ~., data=train)
summary(fit)

require(MASS)
step3<- stepAIC(fit,direction="both")
summary(step3)


fit1 <- lm(lntotalspent ~ lnlongmon+age+lninc+carvalue+         
             lnothdebt+lncreddebt+equipmon+tollmon+          
             wiremon+lncardmon+debtinc+pets+pets_cats+        
             pets_dogs+pets_birds+card2items+carditems+        
             region+gender+edcat+union+            
             hometype+cars+carbuy+reason+polparty+         
             cardtype+cardbenefit+cardfee+card2type+card2benefit+     
             card2fee+confer+commutemotorcycle+commutecarpool, data= train)
summary(fit1)


vif(fit1)



lnlongmon+tenure+age+lninc+carvalue+         
lnothdebt+lncreddebt+equipmon+ed+tollmon+          
wiremon+lncardmon+debtinc+pets+pets_cats+        
pets_dogs+pets_birds+card2items+carditems+        
region+gender+edcat+birthmonth+union+            
hometype+cars+carbuy+reason+polparty+         
cardtype+cardbenefit+cardfee+card2type+card2benefit+     
card2fee+active+confer+telecommute+commutemotorcycle+commutecarpool  
