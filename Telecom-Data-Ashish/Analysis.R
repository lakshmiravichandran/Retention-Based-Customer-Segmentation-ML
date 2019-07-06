#update.packages(ask = FALSE)
require(tidyverse)
require(forcats)
require(caret)
require(e1071)
require(devtools)
require(onehot)
require(gridExtra)
source('multiplotFn.R')
require(fastDummies)
require(caTools)
require(ROCR)
options(warn=-1) # to disable warning
# options(warn=0) # - to enable warning
data =  read.csv("data\\mobileSubscriber_data_old.csv")
dat = as.tibble(data)
#View(dat)
names(dat)
#getting summary of each variables
summary(dat)
str(dat)

# Getting percentage of NaN in each column
print(100*colSums(is.na(dat))/nrow(dat))

nrow(dat)
ncol(dat)
naCol = colSums(is.na(dat)) > 0
table(naCol)

#Removing Row with NA more than 10%
NaRemover = colSums(is.na(dat))/nrow(dat) < 0.1
dat2 = dat[NaRemover]

table(NaRemover)
ncol(dat2)

#printing Data after NA removal
naPercent = 100*colSums(is.na(dat2))/nrow(dat2)
print(sort(naPercent,decreasing = T))

# getting list of columns which needs data imputations
ColWithna = names(naPercent[naPercent!=0])
print(head(ColWithna))

# Making customer id as string
dat2 = dat2 %>% mutate(Customer_ID = as.character(Customer_ID))

str(dat2)

table(dat2$churn)
# Imputing NA with MICE
# imputed_Data <- mice(dat2, m=1, maxit = 5, method = 'pmm', seed = 500)
# insufficient memory for mice

# imputing data with mean

dat2$avg6mou[is.na(dat2$avg6mou)]<-mean(dat2$avg6mou,na.rm = T)
dat2$avg6qty[is.na(dat2$avg6qty)]<-mean(dat2$avg6qty,na.rm = T)
dat2$hnd_price[is.na(dat2$hnd_price)]<-mean(dat2$hnd_price,na.rm = T)

naPercent = 100*colSums(is.na(dat2))/nrow(dat2)
print(sort(naPercent,decreasing = T))

# getting list of columns which needs data imputations
ColWithna = names(head(naPercent)[naPercent!=0])
print(ColWithna)

# Without data imputations
# Building Logistic Regression Model after excluding var "Customer_ID" 
#mod<-glm(churn~.,data=dat2[,-65],family="binomial")
#summary(mod)

dtypes = sapply(dat2, class)
unique(dtypes)
num_dtypes = dtypes[dtypes!="factor"]
name_num_type = names(num_dtypes)
#View(name_num_type)
dat3 = dat2
#dat2$churn <- as.factor(dat2$churn)

colNames_withNA = names(dat2[colSums(is.na(dat2))>0])

#ggplot(dat2[c(nam,'churn')],aes_string(x='churn',y=nam))+geom_boxplot()

num_cols <- dat2 %>% select_if(is.numeric) %>% colnames()
#rm(gglist)


# 
# gglist
# gglist[1] <- ggplot(dat2)+geom_boxplot(aes_string(x='churn',y=num_cols[1]))
# gglist[2] <- ggplot(dat2)+geom_boxplot(aes_string(x='churn',y=num_cols[2]))
# gglist[3] <- ggplot(dat2)+geom_boxplot(aes_string(x='churn',y=num_cols[3]))
# 
# multiplot(gglist[1],gglist[2],gglist[3], cols=2)
# class(gglist)

for (nam in colNames_withNA) {
  
  if (class(dat2[[nam]]) =='factor'||class(dat2[[nam]]) =='character') {
   
  }else{
    col_avg = mean(dat2[[nam]],na.rm = T)
    print(col_avg)
    dat2[is.na(dat2[[nam]]),nam] = col_avg
  }

}





tot_rows = nrow(dat2)
head(names(dat2[colSums(is.na(dat2))>0]))
head(unique(dat2['area']))
#imputing values for area


unique(dat2['marital'])
maritalImpact = dat2 %>% group_by(marital) %>% summarise(churnPercent = sum(churn)/n())
ggplot(maritalImpact,aes(x = marital,y = churnPercent)) + geom_bar(stat = 'identity')

#As we can see the Marital have similar churn percent thus we can drop marital from the analysis
dat2 <- dat2 %>% select(-marital)


#imputing na is ethinic
unique(dat2['ethnic'])
Impact = dat2 %>% group_by(ethnic) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = ethnic,y = churnPercent)) + geom_bar(stat = 'identity')
#imputing ethinic with Unknown
dat2['ethnic'] = fct_explicit_na(dat2[['ethnic']],na_level = "Unknown")
names(dat2[colSums(is.na(dat2))>0])


#dropping CSA
dat2 <- dat2 %>% select(-csa)
names(dat2[colSums(is.na(dat2))>0])

#Imputing car_buy
unique(dat2$car_buy)
dat2['car_buy'] = fct_explicit_na(dat2[['car_buy']],na_level = "UNKNOWN")
Impact = dat2 %>% group_by(car_buy) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = car_buy,y = churnPercent)) + geom_bar(stat = 'identity')

#similar churn percent thus removing car_buy
dat2 <- dat2 %>% select(-car_buy)
names(dat2[colSums(is.na(dat2))>0])

#Imputing hnd_webcap
unique(dat2$hnd_webcap)
dat2['hnd_webcap'] = fct_explicit_na(dat2[['hnd_webcap']],na_level = "UNKNOWN")
Impact = dat2 %>% group_by(hnd_webcap) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = hnd_webcap,y = churnPercent)) + geom_bar(stat = 'identity')
names(dat2[colSums(is.na(dat2))>0])

#imputing refurb_new
unique(dat2$refurb_new)
dat2['refurb_new'] = fct_explicit_na(dat2[['refurb_new']],na_level = "UNKNOWN")
Impact = dat2 %>% group_by(refurb_new) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = refurb_new,y = churnPercent)) + geom_bar(stat = 'identity')
#Simiar level of churn percent thus removing refurb_new
dat2 <- dat2 %>% select(-refurb_new)
names(dat2[colSums(is.na(dat2))>0])

#imputing prizm_social_one
unique(dat2$prizm_social_one)
Impact = dat2 %>% group_by(prizm_social_one) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = prizm_social_one,y = churnPercent)) + geom_bar(stat = 'identity')
#Simiar level of churn percent thus removing prizm_social_one
dat2 <- dat2 %>% select(-prizm_social_one)
names(dat2[colSums(is.na(dat2))>0])


names(Filter(is.factor,dat2))
#merging column with similar churn percentage

#staring with crclscod
unique(dat2$crclscod)
Impact = dat2 %>% group_by(crclscod) %>% summarise(churnPercent = sum(churn)/n())
#View(Impact %>% arrange(-churnPercent))

Impact = dat2 %>% group_by(crclscod) %>% summarise(churnPercent = sum(churn)/n())
#View(Impact %>% arrange(-churnPercent))

#unique(dat2[is.na(dat2[['area']]),'area'] "Unknown")
dat2['area'] = fct_explicit_na(dat2[['area']],na_level = "Unknown")
Impact <- dat2[c('area','churn')] %>% group_by(area) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = area,y = churnPercent)) + geom_bar(stat = 'identity')+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
#View(Impact %>% arrange(-churnPercent))


dat2$area_mod <- dat2$area %>% fct_collapse(
  high = c("NORTHWEST/ROCKY MOUNTAIN AREA",	"SOUTH FLORIDA AREA",	"NEW ENGLAND AREA",	
           "CALIFORNIA NORTH AREA",	"NORTH FLORIDA AREA"),
  medium = c("NORTH FLORIDA AREA",	"PHILADELPHIA AREA",	"SOUTHWEST AREA",	
             "NEW YORK CITY AREA",	"CHICAGO AREA",	"LOS ANGELES AREA",
             "DALLAS AREA",	"ATLANTIC SOUTH AREA"),
  low = c("GREAT LAKES AREA",	"DC/MARYLAND/VIRGINIA AREA",	"Unknown",	"OHIO AREA",	
          "HOUSTON AREA",	"CENTRAL/SOUTH TEXAS AREA",	"MIDWEST AREA",	"TENNESSEE AREA")
)
Impact <- dat2[c('area_mod','churn')] %>% group_by(area_mod) %>% 
  summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = area_mod,y = churnPercent)) + 
  geom_bar(stat = 'identity')+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

dat2$area_mod
dat2<-dat2 %>% select(-area)

#Binning data into 3 bins 0-20,20-27,28+
#TP_A3_A2_EF_P1_EM_IF_GY
#group A  = TP_A3_A2_EF_P1_EM_IF_GY_Z1 GA	K	B	M	G	Z	BA	A	D2
#group B = Z2	AA	JF	ZA	ZY	CC	B2	O	C	D	I	CA	U	DA	J	U1	E
#group C = L	EA	C2	Z4	CY	W	D4	Y	C5	V1	E4	Z5	D5	E2	EC	H	S	ZF
#group D = Missing, NA
unique(dat$crclscod)
Impact = dat %>% group_by(crclscod) %>% summarise(churnPercent = sum(churn)/n())
#View(Impact %>% arrange(-churnPercent))
#View(dat2)
dat2$crclscod_mod <- dat2$crclscod %>% fct_collapse(
  a = c("TP",	"A3",	"A2",	"EF",	"P1",	"EM",	"IF",	"GY",	"Z1",	
        "GA",	"K",	"B",	"M",	"G",	"Z",	"BA",	"A"),
  b = c("D2",	"Z2",	"AA",	"JF",	"ZA",	"ZY",	"CC",	"B2",	"O",	
        "C",	"D",	"I",	"CA",	"U",	"DA",	"J",	"U1",	"E"),
  c = c("L",	"EA",	"C2",	"Z4",	"CY",	"W",	"D4",	"Y",	"C5",	
        "V1",	"E4",	"Z5",	"D5",	"E2",	"EC",	"H",	"S",	"ZF")
)
dat2$crclscod_mod <- dat2$crclscod_mod %>% fct_explicit_na("M")

Impact = dat2 %>% group_by(crclscod_mod) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = crclscod_mod,y = churnPercent)) + geom_bar(stat = 'identity')
dat2<-dat2 %>% select(-crclscod)

# removed all na :)
colnames(dat2)

#Asl_flag
Impact = dat2 %>% group_by(asl_flag) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = asl_flag,y = churnPercent)) + geom_bar(stat = 'identity')

#hnd_webcap
Impact = dat2 %>% group_by(hnd_webcap) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = hnd_webcap,y = churnPercent)) + geom_bar(stat = 'identity')

#models %>% models_mod
unique(dat2$models)
Impact = dat2 %>% group_by(models) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = models,y = churnPercent)) + geom_bar(stat = 'identity')
#View(Impact)

dat2$models_fac = as.factor(dat2$models)

dat2$models_mod <- dat2$models_fac %>% fct_collapse(
  high = c('1','2','3','4','5',"1.56784723060215"),
  med = c("6","7","8","9","10"),
  low = c("15","14","11","16")
)

unique(dat2$models_mod)
Impact = dat2 %>% group_by(models_mod) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = models_mod,y = churnPercent)) + geom_bar(stat = 'identity')

dat2<-dat2 %>% select(-models_fac)
dat2<-dat2 %>% select(-models)

#MTRCYCLE
unique(dat2$mtrcycle)
unique(as.factor(dat2$mtrcycle))
dat2$mtrcycle_fac = as.factor(dat2$mtrcycle)
Impact = dat2 %>% group_by(mtrcycle_fac) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = mtrcycle_fac,y = churnPercent)) + geom_bar(stat = 'identity')
# dat2 <- dat2 %>% select(-mtrcycle_mod)
# dat2$mtrcycle_mod <- dat2$mtrcycle_fac %>% fct_collapse(
#   no = c('0','0.0134315757157111'),
#   yes = c('1')
# )

# similar churn percentage thus can be removed
dat2 <- dat2 %>% select(-mtrcycle)
dat2 <- dat2 %>% select(-mtrcycle_fac)


# uniqsubs
unique(dat2$uniqsubs)
dat2$uniqsubs_fac = as.factor(dat2$uniqsubs)
Impact = dat2 %>% group_by(uniqsubs_fac) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = uniqsubs_fac,y = churnPercent)) + geom_bar(stat = 'identity')

unique(dat2$uniqsubs_fac)
dat2$uniqsubs_mod <- dat2$uniqsubs_fac %>% fct_collapse(
  low = c('1','2','3','4'),
  medium = c('5',"6","7",'8'),
  high = c("9","10","12","13","11")
)
Impact = dat2 %>% group_by(uniqsubs_mod) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = uniqsubs_mod,y = churnPercent)) + geom_bar(stat = 'identity')
dat2<-dat2 %>% select(-uniqsubs)
dat2<-dat2 %>% select(-uniqsubs_fac)


#ethnic
unique(dat2$ethnic)
dat2$ethnic_fac = as.factor(dat2$ethnic)
Impact = dat2 %>% group_by(ethnic_fac) %>% summarise(churnPercent = sum(churn)/n())
Impact <- arrange(Impact,churnPercent)
ggplot(Impact,aes(x = reorder(ethnic_fac,churnPercent),y = churnPercent)) + geom_bar(stat = 'identity')

unique(dat2$ethnic_fac)
dat2$ethnic_mod <- dat2$ethnic_fac %>% fct_collapse(
  low = c("C", "Z", "P", "X","Unknown"),
  medium = c("M","N","S","U","H","G","R","I","F"),
  high = c("J", "D", "B", "O")
)
Impact = dat2 %>% group_by(ethnic_mod) %>% summarise(churnPercent = sum(churn)/n())
ggplot(Impact,aes(x = ethnic_mod,y = churnPercent)) + geom_bar(stat = 'identity')

dat2<-dat2 %>% select(-ethnic)
dat2<-dat2 %>% select(-ethnic_fac)

# truck
dat2$truck_fac = as.factor(dat2$truck)
Impact = dat2 %>% group_by(truck_fac) %>% summarise(churnPercent = sum(churn)/n())
Impact <- arrange(Impact,churnPercent)

dat2<-dat2 %>% select(-truck_fac)
dat2<-dat2 %>% select(-truck)

# age1

dat2$age1_fac = as.factor(dat2$age1)
Impact = dat2 %>% group_by(age1_fac) %>% summarise(churnPercent = sum(churn)/n())
Impact <- arrange(Impact,churnPercent)
#View(Impact)
ggplot(Impact,aes(x = age1_fac,y = churnPercent)) + geom_bar(stat = 'identity')+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

unique(dat2$age1)
# dividing age into multiple facets
# youth = 18,20,22
# youth,late20,age30_54,age57_70,age72more
dat2 <- dat2 %>% mutate(age1_mod = case_when(
  .$age1 == 0 ~ 'age30_54',
  .$age1 > 70 ~ 'age70more',
  .$age1 > 54 ~ 'age54_70',
  .$age1 > 29 ~ 'age30_54',
  .$age1 > 24 ~ 'late20',
  .$age1 > 17 ~ 'youth',
  TRUE ~ 'InvalidAge'
  
))
# %>% select(age1_mod) %>% unique()

Impact = dat2 %>% group_by(age1_mod) %>% summarise(churnPercent = sum(churn)/n())
Impact <- arrange(Impact,churnPercent)
#View(Impact)
ggplot(Impact,aes(x = age1_mod,y = churnPercent)) + geom_bar(stat = 'identity')+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
dat2$age1_mod <- as.factor(dat2$age1_mod)
dat2<-dat2 %>% select(-age1)
dat2<-dat2 %>% select(-age1_fac)




unique(dat2$age2)
dat2$age2_fac = as.factor(dat2$age2)
Impact = dat2 %>% group_by(age2_fac) %>% summarise(churnPercent = sum(churn)/n())
Impact <- arrange(Impact,churnPercent)
ggplot(Impact,aes(x = age2_fac,y = churnPercent)) + geom_bar(stat = 'identity')+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
#There is no visible impact of age2 on the churn, thus can be safely removved
dat2 <- dat2 %>% select(-age2)
dat2 <- dat2 %>% select(-age2_fac)

colnames(dat2) %>% sort()

dat2 %>% write_csv("Cleaned_mobileSubscriber.csv")

set.seed(1) 
sample = sample.split(dat2$churn, SplitRatio = .80)
train = subset(dat2, sample == TRUE)
test  = subset(dat2, sample == FALSE)


#traindata <- dat2
traindata1 <- train %>% select(-Customer_ID)
testdata1 <- test %>% select(-Customer_ID)
mod <- glm(formula = churn~.,data = traindata1,family = 'gaussian')
summary(mod)
predVal2 = predict(mod,newdata = testdata1, type = 'response')
predVal = predict(mod,newdata = testdata1)

prob_grp = ntile(predVal2,3)

df_predGrp <- data.frame(prob_grp,predVal2)
head(df_predGrp)

rev_grp = data.frame(ntile(test$totrev,3))
rev_grp<-data.frame(rev_grp)
colnames(rev_grp) <- 'rev_grp'
dat5 <- bind_cols(test,df_predGrp)
dat5 <- bind_cols(dat5,rev_grp)
dat5 %>% select(rev_grp,totrev,prob_grp,predVal2)
colnames(dat5)

dat5 <- dat5 %>% mutate(assetCust = case_when(
  prob_grp==3 & rev_grp == 3 ~ 1,
  prob_grp==2 & rev_grp == 3 ~ 1,
  prob_grp==3 & rev_grp == 2 ~ 1,
  TRUE ~ 0
))
mean(dat5$assetCust)

dat5 <- dat5 %>% mutate(assetCustConservative = case_when(
  prob_grp==3 & rev_grp == 3 ~ 1,
  prob_grp==2 & rev_grp == 3 ~ 0,
  prob_grp==3 & rev_grp == 2 ~ 0,
  TRUE ~ 0
))

mean(dat5$assetCustConservative)

dat5 %>% filter(assetCustConservative == 1) %>% select(Customer_ID)

pred <- prediction(predVal,test$churn %>% as.numeric())
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc


predVal = ifelse(predVal > 0.5,1,0)
#predVal
cm<-confusionMatrix(data=predVal %>% as.factor(), 
                    reference=test$churn %>% as.factor())
cm

#exporting customer_id to target
dat5 %>% select(Customer_ID,assetCust,assetCustConservative,churn) %>% 
  filter(assetCust > 0) %>% write_csv('finalTargetCustTarget.csv')

dat5 %>% select(Customer_ID,assetCust,assetCustConservative,churn) %>% 
    summarise(asset = sum(assetCust),
              C_asset = sum(assetCustConservative))


#end here
#Further for analysis in python
#exporting data to csv

dtypes = sapply(dat2 %>% select(-Customer_ID), class)
unique(dtypes)
num_dtypes = dtypes[dtypes !="factor"]
dtypes[dtypes =="character"]
cat_cols <- dtypes[dtypes =="factor"] %>% names()

#cat_cols <- (dat2 %>% select(dtypes[dtypes =="factor"]) %>% names())
#View(dat2[cat_cols])
dummyEncoded <- dummy_cols(cat_cols)
#View(dummyEncoded)

onlydummyEncoded <- dummyEncoded %>% select(-c(names(cat_cols)))

dat_col_numeric <- dat2 %>% select(-c(names(cat_cols)))


finalDat <- bind_cols(dat_col_numeric,onlydummyEncoded)
encodedNewName = colnames(onlydummyEncoded)
encodedNewName
str(finalDat)
summary(finalDat)

finalDat %>%  write_csv('mobile_data_clean_encoded.csv')

dat_col_numeric_scaled <- dat_col_numeric #%>% select(-Customer_ID)

for (nam in colnames(dat_col_numeric)) {
  if (class(dat_col_numeric[nam]) == 'numeric') {
    dat_col_numeric_scaled[nam] <-scale(dat_col_numeric[nam])
  }else if(class(dat_col_numeric[nam]) == 'integer'){
    dat_col_numeric_scaled[nam] <-scale(as.numeric(dat_col_numeric[[nam]]))
  }else{
    
  }
}

summary(dat_col_numeric_scaled)

finalDat_scaled <- bind_cols(dat_col_numeric_scaled,onlydummyEncoded)

finalDat_scaled %>% write_csv('mobile_data_clean_encoded_scaled.csv')
nrow(finalDat_scaled)
traindata1 <- finalDat_scaled %>% select(-Customer_ID)

mod <- glm(formula = churn~.,data = traindata1,family = 'gaussian')
#step_mod <- step(mod,direction = 'both')
summary(mod)
pred = predict(mod)
predVal = ifelse(pred > 0.5,1,0)
predVal
pred
prob_group <- ntile(pred,3)

cm<-confusionMatrix(data=predVal %>% as.factor(),
                    reference=traindata1$churn %>% as.factor())

cm$overall


# Svm is too slow
#svmMod <- svm(churn~., traindata1)


#data.frame(summary(mod)$coef[summary(mod)$coef[,4] <= .05, 4])
obj <- summary(mod)

class(obj$coefficients)
coefDf <- obj$coefficients %>% as_tibble()
coefDf$C_Name <- rownames(obj$coefficients)
coefDf
coefDf %>% filter(.$`Pr(>|t|)`<0.05) %>% select(C_Name) -> importantColName
vecImportantColName <- importantColName[["C_Name"]]

imp_finalDat_scaled <- finalDat_scaled %>% select(vecImportantColName)
imp_finalDat_scaled$churn <- finalDat$churn
imp_finalDat_scaled$Customer_ID <- finalDat$Customer_ID

colnames(imp_finalDat_scaled)
print('exporting scaled clean data with only important features')
imp_finalDat_scaled %>% write_csv('imp_finalDat_scaled.csv')

imp_finalDat <- finalDat %>% select(vecImportantColName)
imp_finalDat$churn <- finalDat$churn
imp_finalDat$Customer_ID <- finalDat$Customer_ID

colnames(imp_finalDat)
print('exporting cleaned data with only important features')
imp_finalDat %>% write_csv('imp_finalDat.csv')

select(finalDat)
