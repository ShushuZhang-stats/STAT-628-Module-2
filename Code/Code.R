# Import Data
data=read.csv("../Data/BodyFat.csv")
head(data)
summary(data)

# Data Cleaning

## Detect Abnormal Data
### Outliers in Regression
#### Method 1: Cooks Distance (31  39  41  42  48  86  96 106 175)
out_ind=c()
name=colnames(data)
par(mfrow=c(4,4),mar=c(1,1,1,1))
for (i in c(2,4:length(data)))
{
  mod=lm(as.formula(paste(name[i], "~.-IDNO")),data=data)
  cooksd <- cooks.distance(mod)
  out_ind=as.numeric(which(cooksd>6*mean(cooksd, na.rm=T)))
  plot(cooksd, pch="*", cex=2, main=name[i])  # plot cook's distance
  abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
  text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>6*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
}
unique(out_ind)

#### Method 2: Residual Plot (42  31  86  175)
out_ind=c()
name=colnames(data)
par(mfrow=c(4,4),mar=c(1,1,1,1))
for (i in 4:length(data))
{
  mod=lm(as.formula(paste(name[i], "~.-IDNO"
                          , collapse = "+")),data=data)
  res=scale(mod$residuals)
  out_ind=c(out_ind,as.numeric(which(abs(res)>6)))
  plot(res,main=name[i])
  text(x=1:length(cooksd)+1, y=res, labels=ifelse(abs(res)>6,names(mod$residuals),""), col="red")  # add labels
}
unique(out_ind)
######Check if density has outliers
mod=lm(DENSITY~.-IDNO-BODYFAT,data=data)
res=scale(mod$residuals)
out_ind=c(out_ind,as.numeric(which(abs(res)>8)))
plot(res,main=name[3])
text(x=1:length(cooksd)+1, y=res, labels=ifelse(abs(res)>8,names(mod$residuals),""), col="red")  # add labels
######Check if BodyFat has outliers Based on Density (48 96)
mod=lm(BODYFAT~DENSITY,data=data)
res=scale(mod$residuals)
out_ind=c(out_ind,as.numeric(which(abs(res)>7)))
plot(res,main=name[2])
text(x=1:length(cooksd)+1, y=res, labels=ifelse(abs(res)>7,names(mod$residuals),""), col="red")  # add labels
unique(out_ind)


### Unreasonable Extreme Values (182 216)
out <- boxplot.stats(data$BODYFAT)$out
boxplot(data[,i],
        ylab = "BodyFat",
        main = "Boxplot of BodyFat"
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
out_ind <- which(data$BODYFAT %in% c(out))
data[out_ind,]  #216
sort(data$BODYFAT)
data[data$BODYFAT==0,]   #182

## Imputation of Misplaced Outliers

###Relation between "ADIPOSITY" with "WEIGHT" and "HEIGHT" (42)
BMI=data$WEIGHT*0.453592/(data$HEIGHT*0.0254)^2  #BODY WEIGHT in kg : BODY HEIGHT in m²   =  BMI
diff=round(BMI,2)-data$ADIPOSITY
BMI_out_ind=which(abs(diff)>1)  #Mismatches
diff[BMI_out_ind]
data[BMI_out_ind,]
data[42,"HEIGHT"]=round(sqrt(data[42,"WEIGHT"]*0.453592/data[42,"ADIPOSITY"])/0.0254,2)


### Ankle Outlier (31,86)
mod_ankle=lm(as.formula(paste(name[i], "~.-IDNO"
                        , collapse = "+")),data=data)
data$ANKLE[c(31,86)]=round(mod_ankle$fitted.values[c(31,86)],2)
#  data$ANKLE[which(mod_ankle$residuals>5)]=round(mod_ankle$fitted.values[which(mod_ankle$residuals>5)],2)

### Forearm Outlier (175)
mod_forearm=lm(FOREARM~.-IDNO,data=data)
data$FOREARM[175]=round(mod_forearm$fitted.values[175],2)

### BodyFat (48 96 182 219)
mod_BodyFat=lm(BODYFAT~DENSITY,data=data)
data$BODYFAT[c(48,76,96)]=round(mod_BodyFat$fitted.values[c(48,76,96)],2)
data=data[-c(182,219),] #Remove 182,216

## Write Cleaned Data Out
write.csv(data,"../Data/Data_Cleaned.csv")


# Modeling
mod=lm(BODYFAT~.-IDNO-DENSITY,data=data)
summary(mod)
library(olsrr)
ols_step_best_subset(mod)
mod_final=lm(BODYFAT~WEIGHT+NECK+ABDOMEN+FOREARM+WRIST,data=data)
summary(mod_final) 
## Final Model: -24.42-0.12*WEIGHT-0.34*NECK+0.95*ABDOMEN+0.58*FOREARM-1.42*WRIST

# Diagnostics

## Residual Plot
par(mfrow = c(1,1))
plot(predict(mod_final),resid(mod_final),pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
     xlab="Predicted Body Fat %", ylab="Standardized Residuals",main="Standardized Residual Plot")
abline(a=0,b=0,col="black",lwd=3)

## QQ Plot
qqnorm(rstandard(mod_final),pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)

## 
pii = hatvalues(mod_final)
cooki = cooks.distance(mod_final)

par(mfrow = c(2,1),mar=c(2,2,2,2))
n = dim(data)[1]
plot(1:n,pii,type="p",pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
     xlab="Index (Each Observation)",ylab="Pii",main="Leverage Values (Pii)")
plot(1:n,cooki,type="p",pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
     xlab="Index (Each Observation)",ylab="Cook's Distance",main="Influence Values (Cook's Distance)")