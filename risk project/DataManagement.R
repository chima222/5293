MERGED2012_13_PP <- read.csv("~/Desktop/CollegeScorecard_Raw_Data/MERGED2012_13_PP.csv",as.is = T)
MERGED2013_14_PP <- read.csv("~/Desktop/CollegeScorecard_Raw_Data/MERGED2013_14_PP.csv",as.is = T)
MERGED2014_15_PP <- read.csv("~/Desktop/CollegeScorecard_Raw_Data/MERGED2014_15_PP.csv",as.is = T)
MERGED2015_16_PP <- read.csv("~/Desktop/CollegeScorecard_Raw_Data/MERGED2015_16_PP.csv",as.is = T)

#combine four year data
year12_16<-rbind(MERGED2012_13_PP,MERGED2013_14_PP,MERGED2014_15_PP,MERGED2015_16_PP)


# remove na for four years
null<-c()
library(dplyr)
for(i in 1:1805){
  if(is.character(year12_16[,i])==TRUE){
    table<-data.frame(table(year12_16[,i]))
    na<-table %>% arrange(desc(Freq))
    null[i]<-na[na$Var1=="NULL",][,2]<= 30893/3
  }
}


table(null)
head(null)

#form dataset
year_char<-year12_16[,which(null==TRUE)]
year_num<-year12_16[,is.na(null)]
year_new<-cbind(year_num,year_char)
head(year_new)

#write.csv(year_new,file = 'year_1290.csv')

#further selection 
data<-read.csv("year_1290.csv")

X<-data[,c("PREDDEG","CONTROL","REGION","DISTANCEONLY","UGDS","PCTPELL","PCTFLOAN","INC_PCT_LO","DEP_STAT_PCT_IND","IND_INC_PCT_LO","DEP_INC_PCT_LO","PAR_ED_PCT_1STGEN","WDRAW_DEBT_MDN","LO_INC_DEBT_MDN","MD_INC_DEBT_MDN","HI_INC_DEBT_MDN","DEP_DEBT_MDN","NOPELL_DEBT_MDN","FEMALE_DEBT_MDN","MALE_DEBT_MDN","FIRSTGEN_DEBT_MDN","NOTFIRSTGEN_DEBT_MDN","D_PCTPELL_PCTFLOAN","MN_EARN_WNE_P8","MD_EARN_WNE_P8","MD_EARN_WNE_P10","AGE_ENTRY","HIGHDEG","UGDS_WOMEN","UGDS_MEN","UGDS_HISP","UGDS_ASIAN","UGDS_AIAN","UGDS_NHPI","UGDS_2MOR")]


datax<-apply(X,2,as.numeric)

m<-apply(datax,2,function(x) median(x,na.rm = TRUE))
for(i in 1:ncol(datax)){
  for(j in 1:nrow(datax)){
    if(is.na(datax[j,i])){
      datax[j,i] <- m[i]
    }
  }
}

Y<-data[,"RPY_3YR_RT"]
datay<-as.numeric(Y)

studentloan<-data.frame(cbind(datay,datax))
#write.csv(studentloan,'studentloan.csv')

studentloan <- read.csv("~/Desktop/Spring 18/ML in Fin/Project/studentloan.csv")




