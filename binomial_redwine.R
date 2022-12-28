
# binomial model
db = winequality.red

db$quality<-as.factor(db$quality)
levels(db$quality)<-c("poor","poor","poor","good", "good", "good")

db$good<-ifelse(db$quality=="good",1,0)
# create response variable which takes value 0 if quality is bad and 1 if it's good

db<-subset(db, select = -quality)
db$bound.sulfur.dioxide = db$total.sulfur.dioxide - db$free.sulfur.dioxide
db = subset(db, select = -total.sulfur.dioxide)
db = db[,c(1,2,3,4,5,10,7,8,9,6,12,11)]
View(db)
# we order the columns of the dbset

#train and test
set.seed(420)
train <- sample (nrow(db), nrow(db)*0.7) 
test <- db[-train,]
train = db[train,]
table(train$good)
table(test$good)


my_col = hcl.colors(n = 3, palette = "viridis")
Tab_prog = table(train$good)
x11();barplot(Tab_prog, main = "Quality of wine", col = my_col)
# both levels are well represented



####################### MODELS ###########################

#binomial model
glmod=glm(good~.,train, family = binomial)
summary(glmod)
# hosmer-lemershow test
hoslem.test(train$good,predict(glmod,type="response"))
# we can't reject H0: model suits the db


# standard residual check
par(mfrow=c(2,2))
plot(glmod)
# observations number 653 and 1430 may be outliers (?)

stepmod<-step(glmod,trace = F) # -->fixed.acidity, volatile.acidity, chlorides, alcohol, sulphates, bound.sulfur.dioxide
summary(stepmod)
plot(stepmod)
anova(stepmod, glmod, test="Chisq")# p-value = 0.6345
# slightly different from the step function in the multinomial model


############################################# REMOVE 653 #####################################################################
summary(db$fixed.acidity)
which(rownames(train)==653) # observation in position 456 corresponds to 653
train[456, ]
# 653 was a potential outlier and we can see that it has the max value 
# for fixed.acidity





glmod_653<-glm(good~.,train[-456,], family = "binomial")
summary(glmod_653)# --> chlorides becomes significant 
summary(glmod)
plot(glmod_653)

pval_glmod<-summary(glmod)$coefficients[,4]
pval_glmod_653<-summary(glmod_653)$coefficients[,4]
cbind(pval_glmod, pval_glmod_653)
# we compare the p-values of the full model and the model without the outlier 653
cbind(pval_glmod, pval_glmod_653)>0.05
# there is no difference in the significance of the parameters

#STEP MODEL WITHOUT 653
stepmod_653=step(glmod_653, trace = F)
summary(stepmod_653)
summary(stepmod)
# in the step model without 653 we introduce back citric acid

anova(stepmod_653, glmod_653, test = "Chisq") #p-value 0.7463 --> we use step_653


#Cooks distance
head(sort(cooks.distance(glmod), decreasing = T)) #tell us about influential observations

# we compare the models with and without the outlier 
# using the deviance explained and R2_N
n=nrow(train)
R2_d=1-glmod$deviance/glmod$null.deviance
R2_N=(1-exp((glmod$deviance-glmod$null.deviance)/n))/(1-exp((-glmod$null.deviance)/n))

m=n-1
R2_d_653=1-glmod_653$deviance/glmod_653$null.deviance
R2_N_653=(1-exp((glmod_653$deviance-glmod_653$null.deviance)/m))/(1-exp((-glmod_653$null.deviance)/m))
# both R2_d and R2_N increase in the model without 653
# so without observation 653 we increase the explained deviance
# R2_d = 1 - (D_res/D_null)
# R2_N uses the likelihood 


#######################################  DROP1 #################################################
poltr <- glm(good~.,train,family=binomial)
drop1(poltr,test="Chisq")
poltr2 <- glm(good~.-residual.sugar,train,family=binomial)
drop1(poltr2,test="Chisq")
poltr3 <- glm(good~.-residual.sugar-density,train,family=binomial)
drop1(poltr3,test="Chisq")
poltr4 <- glm(good~.-residual.sugar-density-pH,train,family = binomial)
drop1(poltr4,test="Chisq")
poltr5 <- glm(good~.-residual.sugar-pH-density-free.sulfur.dioxide,train,family=binomial)
drop1(poltr5,test="Chisq")
poltr6 <- glm(good~.-residual.sugar-pH-density-free.sulfur.dioxide-citric.acid,train,family=binomial)
drop1(poltr6,test="Chisq")
dropmod <- glm(good~.-residual.sugar-pH-density-free.sulfur.dioxide-citric.acid-fixed.acidity,train,family=binomial)
drop1(dropmod,test="Chisq")

anova(dropmod, stepmod, test="Chisq")
# we fail to reject H1 so the drop model and step model are not significantly different
# therefore we choose the smaller model 

# drop1 senza 653
train_653=train[-456,]

poltr_653 <- glm(good~.,train_653,family=binomial)
drop1(poltr_653,test="Chisq")
poltr2_653 <- glm(good~.-residual.sugar,train_653,family=binomial)
drop1(poltr2_653,test="Chisq")
poltr3_653 <- glm(good~.-residual.sugar-density,train_653,family=binomial)
drop1(poltr3_653,test="Chisq")
poltr4_653 <- glm(good~.-residual.sugar-density-pH,train_653,family = binomial)
drop1(poltr4_653,test="Chisq")
poltr5_653 <- glm(good~.-residual.sugar-pH-density-free.sulfur.dioxide,train_653,family=binomial)
drop1(poltr5_653,test="Chisq")
poltr6_653 <- glm(good~.-residual.sugar-pH-density-free.sulfur.dioxide-citric.acid,train_653,family=binomial)
drop1(poltr6_653,test="Chisq")
dropmod_653 <- glm(good~.-residual.sugar-pH-density-free.sulfur.dioxide-citric.acid-fixed.acidity,train_653,family=binomial)
drop1(dropmod_653,test="Chisq")
# comparison with stepmod_653: we remove fixed acidity and citric acid
# non cambia nulla con drop1 con o senza 653

anova(dropmod_653, stepmod_653, test = "Chisq")
# p-value: 0.05422
# we fail to reject H0 so we prefer the drop model (without 653) --> 
# dropmod_653 (valatile.acidiy, chlorides, alcohol, sulphates, bound.sulfur.dioxide)
#     

####################################### PREDICTION ########################################################
##### Full model 
predicted=predict(glmod,test,type = "response")
alpha=0.5
yhat=ifelse(predicted>=alpha,1,0)
tb=table(test$good,yhat); tb   
 
overall_acc=sum(diag(tb))/sum(tb);overall_acc #0.7520 
# our miscalculation rate is around 25%
specificity=tb[1,1]/(tb[1,1]+tb[1,2]); specificity #0.7743
# there is a 77% probability that bad wine is classified as bad
sensitivity=tb[2,2]/(tb[2,1]+tb[2,2]);sensitivity#0.7322


# let's try with a different alpha
thresh<-seq(0.04,0.9,0.1)
Sensitivity<-numeric(length(thresh));Sensitivity
Specificity<-numeric(length(thresh));Specificity
##yhat=numeric(length(test))

for (j in seq(along=thresh)){ #fare seq partendo da 0.04 in quanto min = 0.039
  yhat1=ifelse(predicted>=thresh[j],1,0)
  xx=table(test$good, yhat1)
  Specificity[j]=xx[1,1]/(xx[1,1]+xx[1,2])
  Sensitivity[j]=xx[2,2]/(xx[2,1]+xx[2,2])
}
#Grafico che ci fa vedere come variano sensitivity e specificity
matplot(thresh,cbind(Sensitivity,Specificity),type="l",
        xlab="Threshold",ylab="Proportion",lty=1:2)
#Roc curve
plot(1-Specificity,Sensitivity,type="l", ylim = c(0,1), xlim = c(0,1))
abline(0,1,lty=2)
# Specificity and Sensitivity converge for an increasing alpha
# up until alpha=
# a good test pulls the curve in the upper left corner

J=which.max(Specificity+Sensitivity-1)
thresh[J]
#see what change with j as treshold
yhat_y=ifelse(predicted>=thresh[J],1,0)
tab=table(test$good,yhat_y)
tab
Sensitivity[J]#0.6929134
Specificity[J]#0.800885
# Sensitivity increases while Specificity decreases

# prediction on dropmod_653
predicted=predict(dropmod_653,test,type = "response")
alpha=0.5
yhat=ifelse(predicted>=alpha,1,0)
tb=table(test$good,yhat); tb   

overall_acc=sum(diag(tb))/sum(tb);overall_acc #0.7479 
# our miscalculation rate is around 25%
specificity=tb[1,1]/(tb[1,1]+tb[1,2]); specificity #0.7743
# there is a 77% probability that bad wine is classified as bad
sensitivity=tb[2,2]/(tb[2,1]+tb[2,2]);sensitivity #0.7244


# let's try with a different alpha
thresh<-seq(0.04,0.9,0.1)
Sensitivity<-numeric(length(thresh));Sensitivity
Specificity<-numeric(length(thresh));Specificity
##yhat=numeric(length(test))

for (j in seq(along=thresh)){ #fare seq partendo da 0.04 in quanto min = 0.039
  yhat1=ifelse(predicted>=thresh[j],1,0)
  xx=table(test$good, yhat1)
  Specificity[j]=xx[1,1]/(xx[1,1]+xx[1,2])
  Sensitivity[j]=xx[2,2]/(xx[2,1]+xx[2,2])
}
#Grafico che ci fa vedere come variano sensitivity e specificity
# par(mfrow=c(1,1))
# matplot(thresh,cbind(Sensitivity,Specificity),type="l",
#        xlab="Threshold",ylab="Proportion",lty=1:2)

#Roc curve
plot(1-Specificity,Sensitivity,type="l", ylim = c(0,1), xlim = c(0,1))
abline(0,1,lty=2)
install.packages("ROCit")
library(ROCit)
rocitb<-rocit(predicted, test$good); plot(rocitb)

# Youden's index
J=which.max(Specificity+Sensitivity-1)
thresh[J]
#see what change with j as treshold
yhat_y=ifelse(predicted>=thresh[J],1,0)
tab=table(test$good,yhat_y)
tab
Sensitivity[J] #0.7047
Specificity[J] #0.800885
# specificity increases while sensitivity decreases

# DISPERSION PARAMETERS
r=residuals(glmod, type="pearson")
x=sum(r^2)
sigma2=x/glmod$df.residual
# sigma2=1.15 : no significant overdispersion on total model


#### on stepmod_653
r2=residuals(dropmod_653, type="pearson")
x=sum(r2^2)
sigma2step=x/dropmod_653$df.residual
# sigma2step=0.9887 : no significant underdispersion on step model
#QUESTA COSA è CONFERMATA DAL FATTO è I DF SONO SIMILI ALLA DEVIANCE --> CONTROLLARE ANCHE NEGLI ALTRI MODELLI,
#MA IN NESSUONO DOVREBBERO ESSERCI PROBLEMI


hoslem.test(train_653$good,predict(dropmod_653,type="response"))
# Still fail to reject H0 --> model suits


#Levarages 
lev<-sort(hatvalues(dropmod_653), decreasing = T)
x<-2*length(coef(dropmod_653))/nrow(train)
which(lev > x)# many observations are higher than 2p/n
#
cook<-head(sort(cooks.distance(dropmod_653), decreasing = T))
# nulla di che

exp(coef(stepmod_653))
# it shows how much the odds change by increasing each variable by 1

with653<-glm(good~.-fixed.acidity -citric.acid -residual.sugar -density 
             -pH -free.sulfur.dioxide , data=train, family="binomial")


predicted1=predict(with653,test,type = "response")

yhat_y1=ifelse(predicted1>=thresh[J],1,0)
tab1=table(test$good,yhat_y1)
tab1

pred2 <- predict(stepmod,test,type="response")
yhat_2=ifelse(pred2>=thresh[J],1,0)
tab2=table(test$good,yhat_2)
tab2




# rifare senza 653
# comparare drop e step models
# drop model è meglio
# rifare con drop model

# ANOVA
# accuracy ecc
# MSE


