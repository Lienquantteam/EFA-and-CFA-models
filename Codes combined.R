
library(dplyr)
library(tidyverse)
#load data
df2016 <- read.csv('2016-aps-employee-census-5-point-dataset.csv',stringsAsFactors = TRUE, na.strings=c(""," ","NA"))
View(df2016)

#1.Re-code levels
recode_gender <- function(ls){
  return(recode(ls, 'Male' = "M",'Female' = "F", 'X (Indeterminate/Intersex/Unspecified)' = "U"))
}
df2016[,c(2)] <- sapply(df2016[,c(2)], recode_gender)
recode_agegrp <- function(ls){
  return(recode(ls, 'Under 40 years' = "1",'40 to 54 years' = "2", '55 years or older' = "3"))
}
df2016[,c(3)] <- sapply(df2016[,c(3)], recode_agegrp)
recode_jobtitle <- function(ls){
  return(recode(ls, 'Trainee/Graduate/APS' = "1",'EL' = "2", 'SES' = "3"))
}
df2016[,c(4)] <- sapply(df2016[,c(4)], recode_jobtitle)
recode_agree <- function(ls){ 
  return(recode(ls, 'Strongly agree' = "1", 'Agree' = "2", 'Neither agree nor disagree' = "3",'Disagree' = "4",'Strongly disagree' = "5", 
                'Unsure'="6"))
}

df2016[,c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,4,
          8,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,65,66,86,87,88,89,99,100,101,102,103,104,105,106,107,108,109,110,129,130,131,132,133,
          134,135,136,137,142,144,145,147,148,149,150,151,152,153,155,156,157,158,159,160,161,162,163,182,183,184,185,186,187,228,229,230,
          231,232)] <- sapply(df2016[,c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,
                   46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,65,66,86,87,88,89,99,100,101,102,103,104,105,106,107,108,109,110,129,130,131,1,
                   32,133,134,135,136,137,142,144,145,147,148,149,150,151,152,153,155,156,157,158,159,160,161,162,163,182,183,184,185,186,187,228,2,
                   29,230,231,232)], recode_agree)

recode_somewhat <- function(ls){
  return(recode(ls, 'To a very great extent' = "1",'Quite a lot' = "2", 'Somewhat' = "3",'Hardly at all' = "4",'Not at all' = "5"))
}
df2016[,c(67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78,
          79, 80, 81, 82)] <- sapply(df2016[,c(67, 68, 69, 70, 71, 72, 73,
                                               74, 75, 76, 77, 78, 79, 80, 81, 82)], recode_somewhat)
recode_satisfied <- function(ls){ 
  return(recode(ls, 'Very satisfied' = "1",'Satisfied' = "2", 'Neither satisfied or dissatisfied' = "3",'Dissatisfied' = "4",'Very dissatisfied' = "5"))
}
df2016[,c(84,90,91,166,167,168,169,170,171,172,173,174,175)] <- sapply(df2016[,c(84,90,91,166,167,168,169,170,171,172,173,174,175)], 
                                                                       recode_satisfied)
recode_always <- function(ls){
  return(recode(ls, 'Always' = "1",'Often' = "2", 'Sometimes' = "3",'Rarely' = "4",'Never' = "5", 'Not sure' = "6"))
}
df2016[,c(92, 93, 94, 95, 96, 97, 98, 188,189,190)] <- sapply(df2016[,c(92, 93,
                                                                        94, 95, 96, 97, 98, 188,189,190)], recode_always)
recode_everyday <- function(ls){
  return(recode(ls, 'Every day' = "1",'Most days' = "2", 'Once or twice' = "3",'Not this fortnight' = "4",'Would not normally do this' = "5"))
}
df2016[,c(111, 112, 113, 114, 115, 116, 117, 118)] <- sapply(df2016[,c(111, 112, 113,
                                                                       114, 115, 116, 117, 118)], recode_everyday)
recode_positively <- function(ls){
  return(recode(ls, 'Very positively' = "1",'Positively' = "2", 'Unlikely to have an impact' = "3",'Negatively' = "4",'Very negatively' = "5"))
}
df2016[,c(85)] <- sapply(df2016[,c(85)], recode_positively)
recode_yesno <- function(ls){
  return(recode(ls, 'Yes' = "1",'No' = "2", 'Not sure' = "3",'Would prefer to not answer' = "4"))
}
df2016[,c(141,176,191,213,215)] <- sapply(df2016[,c(141,176,191,213,215)], recode_yesno)
recode_yes <- function(ls){
  return(recode(ls, 'Yes, irregularly' = "2",'Yes, regularly' = "1", 'Not sure' = "4",'No' = "3"))
}
23
df2016[,c(143)] <- sapply(df2016[,c(143)], recode_yes)
recode_zero <- function(ls){
  return(recode(ls, "Don't know" = "0",'Not applicable (e.g. on leave for whole fortnight, on graduated return to work)' = "-1"))
}
df2016[,c(119,120,121,146)] <- sapply(df2016[,c(119,120,121,146)], recode_zero)
recode_1 <- function(ls){
  return (recode(ls, 'APS 1-2 (or equivalent)' = "3", 'APS 3-4 (or equivalent)'="4", 'APS 5-6 (or equivalent)'="5", 'Executive Level 
1 (or equivalent)'="6",'Executive Level 2 (or equivalent)'="7", 'Graduate APS (including Cadets)'="2", 'Senior Executive Service 
Band 1 (or equivalent)'="8", 'Senior Executive Service Band 2 or 3 (or equivalent)'="9", 'Trainee/Apprentice'="1"))
}
df2016[,c(138,139)] <- sapply(df2016[,c(138,139)], recode_1)
recode_2 <- function(ls){
  return(recode(ls, 'None' = "0",'One or more' = "1"))
}
df2016[,c(154)] <- sapply(df2016[,c(154)], recode_2)
recode_3 <- function(ls){
  return(recode(ls,'Very high'="1", 'High'="2",'Moderate'="3", 'Low'="4", 'Very low' = "5", 'Not applicable'="6"))
}
df2016[,c(164)] <- sapply(df2016[,c(164)], recode_3)
recode_4 <- function(ls){
  return(recode(ls,'1 to 2 days (include part day)'="2", '3 to 5 days'="3", '6 to 10 days'="4", 'More than 10 days'="5", 'No 
time'="1", 'Not sure'="6"))
}
df2016[,c(165)] <- sapply(df2016[,c(165)], recode_4)
recode_5 <- function(ls){
  return(recode(ls, 'An APS employee' = "1",'An employee of your agency' = "2"))
}
df2016[,c(62)] <- sapply(df2016[,c(62)], recode_5)
recode_6 <- function(ls){
  return(recode(ls, 'Above my classification level' = "1",'Appropriate for my classification level' = "2", 'Below my classification level'="3"))
}
24
df2016[,c(122,123)] <- sapply(df2016[,c(122,123)], recode_6)
recode_7 <- function(ls){
  return(recode(ls, 'I want to leave my agency as soon as possible' = "1",
                'I want to leave my agency within the next 12 months' = "2", 
                'I want to leave my agency within the next 12 months but feel it will be unlikely in the current environment'="3",
                'I want to stay working for my agency for the next one to two years'="4",
                'I want to stay working for my agency for at least the next three years'="5"))
}
df2016[,c(128)] <- sapply(df2016[,c(128)], recode_7)
recode_8 <- function(ls){
  return(recode(ls, 'Lack of promotion opportunities' = "1",
                'Lack of opportunity to work on innovative or cutting edge projects'= "2",
                'Poor visibility of opportunities'="3",
                'Unfair recruitment practices'="4",
                'I have achieved all I can in the agency'="5",
                'I am a member of a diversity group (e.g. Indigenous, gender, disability, non-English speaking background)'="6",
                'There are no major barriers to my career progression'="7",
                'Other'="8"))
}
df2016[,c(140)] <- sapply(df2016[,c(140)], recode_8)
View(df2016)
cols <- c(124,125,126,127,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,216,217,218,219,220,221,222,22,
    3,224,225,226)

recode_ticks <- function(ls){
  return(recode(ls, 'Tick' = "1"))
}

df2016[,c(124,125,126,127,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,216,217,218,219,220,22,
          1,222,223,224,225,226)] <- sapply(df2016[,c(124,125,126,127,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,216,217,218,219,
                   220,221,222,223,224,225,226)], recode_ticks)

replaceNAto0 <- function(x) { replace(x, is.na(x), "0") }

df2016[,c(124,125,126,127,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,216,217,218,219,220,22,
          1,222,223,224,225,226)] <- sapply(df2016[,c(124,125,126,127,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,216,217,218,219,
                   220,221,222,223,224,225,226)], replaceNAto0)
df2016[sapply(df2016, is.character)] <- lapply(df2016[sapply(df2016, is.character)], as.factor)
25
#2.checking column wise null percentage
Null_Cnt <- sapply(df2016, function(x){ sum(is.na(x))})
Null_percnt <- sapply(df2016, function(x){ round((sum(is.na(x))/length(x))*100,2) })
Null_Smry <- cbind(Null_Cnt,Null_percnt)
Null_Smry
#drop columns having more than 60 #q29 - ignored for time being - so dropped
df2016<- select(df2016,-
                  c("q89","q86","q82","q74","q75","q76","q77","q78","q66a","q66b","q66c","q66d","q66e","q66f","q66g","q66h","q66i","q29"))
View(df2016) 
############Export data set############################
write.csv(df2016, "df2016.csv")
############Export data set############################
#drop rows with no data - if col no 5 onwards everything is empty drop row
#df2016<- df2016[which(round(rowSums(is.na(census_2016))/dim(census_2016)[2]*100)<10),]
for(i in 1:nrow(df2016))
{
  if (is.na(df2016[i,5:15])==T)
    df2016 <- df2016[-c(i),]
}
#impute via mode method
# replacing missing data with mode value
getmode <- function(df,i) {
  uniqv <- unique(df[!is.na(df[[i]]), i, drop = T])
  res = uniqv[which.max(tabulate(match(df[[i]], uniqv)))]
  return(res)
}
for (i in (1:ncol(df2016))){
  
  df2016[is.na(df2016[[i]]), i] = getmode(df2016,i)
}
26
############Export data set############################
write.csv(df2016, "data2016_Clean_v2.csv")
############Export data set############################
#understand count value within data - group wise
count(df2016, df2016$q57, sort = TRUE)


#####################################################################################################
data_2016 = read.csv('data2016_Clean.csv')
################################################################################################
#EFA 
#q66a,q66b,q66c,q66d,q66e,q66f: missing
data_2016_new = data_2016 %>% select(q7.,q23b,q24g,q24h,q24j,q25l,q25m,q25p,q28e,q28g,q28h,q28m,q64d,q71a,q71b,
                                     q71c,q71d,q71e,q71g,q71h,q71i,q71j,q79e,q24c,q24d,q24e,q23g,q80c,q23h,q71f,
                                     q23b,q28c,q28f,q28n,q71f,q24c,q24d,q24e,q24k,q24l,q24m,q23g,q23d,
                                     q23f,q25j,q28o,q42a,q42c,q42e,q64c,q79f,q80c,q23h,q42d,
                                     q23i,q25e,q25s,q64e,q64f,q24f,q79c,q79d)
##Check outliers
data_2016_remove = data_2016_new %>% filter(!q7.%in%c(2,3))
data_2016_remove_q7 = data_2016_remove[,-1] # remove 'EL' & 'SES'
dim(data_2016_remove_q7) # 66500 50
mahal = mahalanobis(data_2016_new,
                    colMeans(data_2016_new),
                    cov(data_2016_new))
dim(data_2016_new) #[1] 95889 50
summary(data_2016_new)
##correlation adequacy Bartlett's test
##Bartlett's test assume that variances are equal across groups/samples
## the null hypothesis is that the samples have equal variance
#you want significance to be able to say that at least
##one sample has a significantly diferent variance.
27
correlations <- cor(data_2016_remove_q7)
cortest.bartlett(correlations, n = nrow(data_2016_remove_q7))
##sampling adequacy KMO test
KMO(correlations)
#Overall MSA = 0.98
# Check correlation (No high correlated variables)
library(corrplot)
M=cor(data_2016_remove_q7)
print(M)
corrplot(M,method = "circle", type = "lower", tl.cex = 0.75, tl.srt = 45, tl.col = "blue")
# other option
corrplot(cor(data_2016_remove_q7), method='number')
correl = cor(data_2016_remove_q7)
symnum(correl)#highly correlated (>0.8) and input is a matrix
##how many factors?
nofactors = fa.parallel(data_2016_remove_q7, fm="ml", fa="fa")
nofactors$fa.values#eigenvalues
sum(nofactors$fa.values > 1.0) ##4 old kaiser criterion
sum(nofactors$fa.values > 0.7) ##4 new kaiser criterion
##simple structure with a two factor model
fa1= fa(data_2016_remove_q7, nfactors=2, rotate = "oblimin", fm = "ml")
fa1$values # Eigenvalues
fa1$loadings # Loadings
##get cfi
finalmodel = fa(data_2016_remove_q7, nfactors=2, rotate = "oblimin", fm = "ml")
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))
#0.7702323
fa.diagram(finalmodel)#create a diagram
summary(finalmodel$scores)
##simple structure with a three factor model
fa2 = fa(data_2016_remove_q7, nfactors=3, rotate = "oblimin", fm = "ml")
fa2$values # Eigenvalues
fa2$loadings # Loadings
fa.diagram(fa2)
28
##get cfi
finalmodel = fa(data_2016_remove_q7, nfactors=3, rotate = "oblimin", fm = "ml")
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))
#0.8154212
fa.diagram(finalmodel)#create a diagram
summary(finalmodel$scores)
######################################################################
#FACTANAL PACKAGE
fit.1 <- factanal(data_2016_remove_q7,factors=2,rotation="varimax") 
print(fit.1) 
fit.1$loadings
fa.diagram(fit.1)
fit.2 <- factanal(data_2016_remove_q7,factors=3,rotation="varimax") 
print(fit.2) 
fit.2$loadings
fit.3.promax <- update(fit.2,rotation="promax")
print(fit.3.promax)
########################
fit.4 <- factanal(data_2016_new,factors=2,rotation="varimax") 
print(fit.4) 
fit.4$loadings
fit.5 <- factanal(data_2016_new,factors=3,rotation="varimax") 
print(fit.5) 
fit.5$loadings
# full data EFA (with all levels) with oblimin
fa_full2= fa(data_2016_new, nfactors=2, rotate = "oblimin", fm = "ml")
fa_full2$loadings 
fa_full3= fa(data_2016_new, nfactors=3, rotate = "oblimin", fm = "ml")
fa_full3$loadings 
#######################################################################
# FACTOR ANALYSIS
library(haven)
library(readr)
library(expss)
# transformational
data_1 = data_2016 %>% select(q23b,q24g,q24h,q24j,q25l,q25m,q25p,q28e,q28g,q28h,q28m,q64d,q71a,q71b,
                              q71c,q71d,q71e,q71g,q71h,q71i,q71j,q79e,q24c,q24d,q24e,q23g,q80c,q23h,q71f)
29
cortest.bartlett(data_1)
#$chisq
#[1] 2510224
raqMatrix <- cor(data_1)
KMO(raqMatrix) #Overall MSA = 0.97
alpha(data_1)
data_2 = data_2016 %>% select(q23b,q28c,q28f,q28n,q71f,q24c,q24d,q24e,q24k,q24l,q24m,q23g,q23d,
                              q23f,q25j,q28o,q42a,q42c,q42e,q64c,q79f,q80c,q23h,q42d)
cortest.bartlett(data_2)
raqMatrix <- cor(data_2)
KMO(raqMatrix) #Overall MSA = 0.95
RC1 <- pca1$scores
pca2 <- principal(data_2, nfactors = 1, rotate = "varimax")
# In ra cac gia tri loading voi cut-off 
print(loadings(pca2), digits = 2, cutoff = 0.5)
RC2 <- pca2$scores
pca3 <- principal(data_3, nfactors = 1, rotate = "varimax")
# In ra cac gia tri loading voi cut-off 
print(loadings(pca3), digits = 2, cutoff = 0.5
#q79d 0.72
RC3 <- pca3$scores

# logistic regresion
unique(data_final$q7.) 

#"Trainee/Graduate/APS" -> '0' and "EL"&"SES" -> 1 
concat <- cbind(data.frame(data_2016$q7_new),data.frame(RC1),data.frame(RC2),data.frame(RC3))
colnames(concat) <- c('supervisor_innovation','RC1','RC2','RC3')
lm1 <- glm(data= concat,data_2016$q7_new~RC1, family = binomial)
lm2 <- glm(data= concat,data_2016$q7_new~RC2, family = binomial)
summary(lm2)
lm3 <- glm(data= concat,data_2016$q7_new~RC3, family = binomial)
summary(lm3)
#2. Can leadership styles be used to predict employees that strive for creativity and innovation. EFA model 

lm4 <- glm(data= concat,data_2016$q71h_new ~ RC1, family = binomial)
lm5 <- glm(data= concat,data_2016$q71h_new ~ RC2, family = binomial)
lm6 <- glm(data= concat,data_2016$q71h_new ~ RC3, family = binomial)
lm_all <- glm(data= concat,data_2016$q71h_new ~ RC1 + RC2 + RC3, family = binomial)
summary(lm_all)
#######################################################################
#4. What are characters associated with transformational and/or consideration leaders
pcal1 <-prcomp(data_2016_new, scale = TRUE)

names(pcal1)#properties of principal component results
print(pcal1)
summary(pcal1)
screeplot(pcal1, main ="Scree Plot", xlab="Components")
screeplot(pcal1, main="Scree Plot", type="line")
biplot(pcal1, col=c("white","red"), cex=c(1, 0.7))
################################################################################
data_more = data_2016 %>% select(q20a,q20b,q20c,q20d,q20e,q20f,q20g,q28a,q28c,q28d,q28f,
                                 q32b,q32d,q35,q36,q44a,q44b,q44c,q44d,q54a,q54b)
fa8= fa(data_more, nfactors=2, rotate = "oblimin", fm = "ml")
fa8$values # Eigenvalues
fa8$loadings # Loadings,
fa9= fa(data_more, nfactors=3, rotate = "oblimin", fm = "ml")
fa9$loadings # Loadings
fa10= fa(data_more, nfactors=4, rotate = "oblimin", fm = "ml")
fa10$loadings # Loadings
##get cfi
finalmodel5 = fa(data_more, nfactors=2, rotate = "oblimin", fm = "ml")
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))
#0.916138
fa.diagram(finalmodel5)