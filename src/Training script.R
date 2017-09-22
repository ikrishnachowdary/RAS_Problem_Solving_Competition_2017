require(readr)
require(stringi)


### personal folder
Training_Data <- read_delim("C:/Users/chatr/Downloads/informs/Training Data.tab", "\t", escape_double = FALSE, trim_ws = TRUE, skip = 0, n_max = 1000)

### changing the timestamp readings from character to posixct format
Training_Data$vnder_edr_ts_cst= as.POSIXct(Training_Data$vnder_edr_ts_cst,format="%d %b %Y :%H:%M:%OS")
### order the observations according to timestamp
Training_Data= Training_Data[order(Training_Data$vnder_edr_ts_cst),]

names(Training_Data)
sapply(Training_Data,function(x) sum(is.na(x)))
Training_Data= Training_Data[complete.cases(Training_Data),]


###############  Removing observations with speed less than 20    ###########################
require(dplyr)
Training_Data=filter(Training_Data,Training_Data$EDR_EQP_SPD > 20)



####  to remove observations with peak kips value more than 90  #############
kipsvalue= subset(Training_Data, Training_Data$WHL_PEAK_KIPS>=90)
table(kipsvalue$LOAD_EMPTY)
kipsvalue= subset(kipsvalue, kipsvalue$LOAD_EMPTY %in% "E")

Training_Data=Training_Data[!(row.names(Training_Data) %in% row.names(kipsvalue)),]


############ removing cars with frequency of 1  ##############

z1=as.data.frame(table(Training_Data$unique_id))
kkk= subset(z1, z1$Freq %in% 1)
Training_Data=Training_Data[!(Training_Data$unique_id %in% kkk$Var1),]

#### splitting the data according to unique wheel id

a=strsplit(Training_Data$unq_whl_id,split =  "-")
temp1=t(as.data.frame(a))
temp1=as.data.frame(temp1)

row.names(temp1)=c(1:nrow(temp1))
Training_Data=cbind(temp1$V1,Training_Data)
row.names(Training_Data)=c(1:nrow(Training_Data))

colnames(Training_Data)[1]=c("unique_id")


library(plyr)

##### function to identify "E-->L" patterns
Krishna <- function(DB){
  uids <- unique(DB$unique_id)
  newdb <- list()
  
  for(uid in uids){
    temp <- list()
    temp <- subset(DB,(DB$unique_id %in% uid))
    if(nrow(temp) >= 2){
      
      for(i in 1:(nrow(temp)-1)){
        if(temp$LOAD_EMPTY[i] == 'E'){
          if(temp$LOAD_EMPTY[i+1] == 'L'){
            if((temp$vnder_edr_ts_cst[i+1] - temp$vnder_edr_ts_cst[i])<=10){
              newdb <- rbind(newdb,temp[i,],temp[i+1,]) 
            }
          }
        }
      }
    }
  }
  return(newdb)
}

for (i in c(1:69)){
  inds <- seq((i-1)*100000+1,i*100000,1)
  tmp <- Training_Data[inds, ]  
  tryCatch({tmp2 <- Krishna(tmp); write.csv(tmp2, file = paste("C:/Users/chowdary/Downloads/refined/cleaned_train_", i, ".csv", sep=""))})
}


###################### function to replace the (WHL_AVG_KIPS,WHL_PEAK_KIPS,WHL_DYN_KIPS,WHL_DYN_RATIO,weight)values from loaded cars to empty cars

naveen <- function(DT){
  newdb <- list()
  for(i in 1:(nrow(DT)-1)){
    if(DT$LOAD_EMPTY[i] == 'E'){
      if(DT$LOAD_EMPTY[i+1] == 'L'){
        newdb <- rbind(newdb,DT[i,],DT[i+1,]) 
        newdb$WHL_AVG_KIPS[i]=newdb$WHL_AVG_KIPS[i+1]
        newdb$WHL_PEAK_KIPS[i]=newdb$WHL_PEAK_KIPS[i+1]
        newdb$WHL_DYN_KIPS[i]=newdb$WHL_DYN_KIPS[i+1]
        newdb$WHL_DYN_RATIO[i]=newdb$WHL_DYN_RATIO[i+1]
        newdb$weight[i]=newdb$weight[i+1]
      }
    }
  }
  newdb=subset(newdb,newdb$LOAD_EMPTY %in% "E")
  return(newdb)
}


####reading all the 68 csv files   #######
getwd()
setwd("C:/Users/chowdary/Downloads/refined")

files = list.files(pattern="*.csv")

for (k in 1:length(files)){
  temp <- read.csv(files[k])
  temp$weight= temp$EQP_GRS_TONS- temp$TARE
    tryCatch({tmp2 <- naveen(temp); write.csv(tmp2, file = paste("C:/Users/chowdary/Downloads/only_e/empty_", k, ".csv", sep=""))})
}


### reading all "E" files
getwd()
#####grad folder
setwd("C:/Users/chowdary/Dropbox/informs comp/only_e")

#### personal folder
setwd("C:/Users/chatr/Dropbox/informs comp/only_e")
files = list.files(pattern="*.csv")

dataset = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

## removing "X.1","X","unique_id" variables 
dataset=dataset[,-c(1,2,3)]


###### splitting the data into atomic parts
Training_Data= dataset

rm(dataset)

names(Training_Data)

sapply(Training_Data,function(x) sum(is.na(x)))

Training_Data= Training_Data[complete.cases(Training_Data),]



#### splitting unq_whl_id

unq_whl_id_split<-function(unq_whl_id)
{
  a=strsplit(Training_Data$unq_whl_id,split =  "-")
  temp1=t(as.data.frame(a))
  temp1=as.data.frame(temp1)
  wheelid_1=as.data.frame(temp1$V1)
  colnames(wheelid_1)=c("v1")
  row.names(wheelid_1)=c(1:nrow(Training_Data))
  colnames(temp1)[2]=c("Start_year")
  colnames(temp1)[3]=c("Start_month")
  colnames(temp1)[5]=c("End_month")
  colnames(temp1)[6]=c("End_date")
  row.names(temp1)=c(1:nrow(Training_Data))
  start_date<-substr(temp1$V4,1,2)
  end_year<-substr(temp1$V4,5,8)
  unq_date<-data.frame(temp1[,2:3],start_date,end_year,temp1[,5:6])
  row.names(unq_date)=c(1:nrow(Training_Data))
  car_initial<-sub("^([[:alpha:]]*).*","\\1",substr(wheelid_1$v1,1,4))
  car_initial<-as.data.frame(car_initial)
  eqp_axle_nbr<-substr(wheelid_1$v1,stri_length(wheelid_1$v1),stri_length(wheelid_1$v1))
  eqp_axle_nbr<-as.data.frame(eqp_axle_nbr)
  
  axle_side<-substr(wheelid_1$v1,stri_length(wheelid_1$v1)-1,stri_length(wheelid_1$v1)-1)
  axle_side<-as.data.frame(axle_side)
  car_number<-regmatches(substr(wheelid_1$v1,1,stri_length(wheelid_1$v1)-2),gregexpr("[[:digit:]]+",substr(wheelid_1$v1,1,stri_length(wheelid_1$v1)-2)))
  car_number<-t(as.data.frame(car_number))
  row.names(car_number)=c(1:nrow(Training_Data))
  unq_whl_id<-data.frame(car_initial,car_number,axle_side,eqp_axle_nbr,unq_date)
  return(unq_whl_id)
  
}
unq_whl_id=unq_whl_id_split(unq_whl_id)


#Function for train_type
trn_id_split= function(trn_id){
  train_type<-as.data.frame(substr(Training_Data$trn_id,1,1))
  colnames(train_type)=c("Train_type")
  temp2<-regmatches(Training_Data$trn_id,regexpr("[[:alpha:]]+",Training_Data$trn_id))
  
  Train_symbol<-as.data.frame(substr(temp2,2,stri_length(temp2)))
  colnames(Train_symbol)=c("Train_symbol")
  
  origin_station<-substr(Train_symbol$Train_symbol,1,3)
  origin_station= as.data.frame(origin_station)
  
  destination_station<-substr(Train_symbol$Train_symbol,4,stri_length(Train_symbol$Train_symbol))
  destination_station= as.data.frame(destination_station)
  
  Train_priority<-as.data.frame(substr(Training_Data$trn_id,stri_length(Training_Data$trn_id),stri_length(Training_Data$trn_id)))
  colnames(Train_priority)=c("Train_Priority")
  trn_id= cbind(train_type,origin_station,destination_station,Train_priority)
  return(trn_id)
}

trn_id=trn_id_split(trn_id)
newdata= cbind(unq_whl_id,trn_id)

########## splitting vnder_edr_ts_cst ########## 
# Splitting timestamp for the kips reading (vnder_edr_ts_cst)
kips_ts_split= function(vnder_edr_ts_cst){
  temp4=t(as.data.frame(strsplit(Training_Data$vnder_edr_ts_cst, split = " ")))
  temp4= as.data.frame(temp4)
  
  kipread_year= substr(temp4$V1, 1,4)
  kipread_month= substr(temp4$V1, 6,7)
  kipread_day=substr(temp4$V1, 9,10)
  
  kipread_hour= substr(temp4$V2, 1,2)
  kipread_minute= substr(temp4$V2, 4,5)
  kipread_secs=substr(temp4$V2, 7,8)
  
  vnder_edr_ts_cst= data.frame(kipread_day,kipread_month,kipread_year, kipread_hour, kipread_minute, kipread_secs)
  rownames(vnder_edr_ts_cst)=c(1:nrow(Training_Data))
  
  return(vnder_edr_ts_cst)
}

vnder_edr_ts_cst=kips_ts_split(vnder_edr_ts_cst)
newdata= cbind(newdata,vnder_edr_ts_cst)

##### splitting AAR_CT_C to car_type and car_characteristic

AAR_CT_C_split= function(AAR_CT_C){
  car_type<-substr(Training_Data$AAR_CT_C,1,1)
  car_characteristic<-substr(Training_Data$AAR_CT_C,2,stri_length(Training_Data$AAR_CT_C))
  AAR_C<-data.frame(car_type,car_characteristic)
  return(AAR_C)
}
AAR_CT_C=AAR_CT_C_split(AAR_CT_C)

newdata= cbind(newdata,Training_Data[,c(5:7)],Training_Data[,c(10:17)],AAR_CT_C, Training_Data[,c(19:23)])


write.csv(newdata, file="C:/Users/chatr/Dropbox/informs comp/finalset.csv")
