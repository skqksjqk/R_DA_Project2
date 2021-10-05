#R version 4.0.5

library(ggplot2) # 3.3.3
library(readxl) # 1.3.1
library(dplyr) # 1.0.5
library(car) # 3.0-10
library(Metrics) # 0.1.4

data <-read_excel("data_set.xlsx")
data <- as.data.frame(data)
#범주형 변수 풍향 factor형으로 변환
data$풍향 <- as.factor(data$풍향)

#사용 안할 변수 제거
data <- data %>% select(-mean_PM25,-계절)

#월 생성후 factor형으로 변환
data <- data %>% mutate(월 = substr(data$일시,6,7))
data$월 <- as.factor(data$월)

#일시 제거
data <- data[-1]

#월,풍향 원 핫 인코딩
for(v in unique(data$월)) data[paste0("월_",v)] <- ifelse(data$월==v,1,0)
data <- data %>% select(-월) 
for(v in unique(data$풍향)) data[paste0("풍향_",v)] <- ifelse(data$풍향==v,1,0)
data <- data %>% select(-풍향) 
data2 <- data

### 데이터 전처리 - IQR 및 log 변환 ###

data2$day_tr_sum <- ifelse(data2$day_tr_sum<3944541 , 3944541 , data2$day_tr_sum)
data2$day_tr_sum <- ifelse(data2$day_tr_sum>10667256 , 10667256 , data2$day_tr_sum)
data2$벙커C유 <- ifelse(data2$벙커C유>5.12903226 , 5.12903226 , data2$벙커C유)
data2$기타 <- ifelse(data2$기타>52.03226 , 52.03226 , data2$기타)

data2$bei_PM10 <- log(data2$bei_PM10)
data2$shan_PM10 <- log(data2$shan_PM10)
data2$어제PM10 <- log(data2$어제PM10)
data2$어제중국PM10 <- log(data2$어제중국PM10)

# 일강수량 데이터에 0이 존재하여 1을 더한 후, 로그 변환
data2$일강수량 <- (data2$일강수량+1)
data2$일강수량 <- log(data2$일강수량)

### 정규화 ###
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
a <- data2 %>% select(-mean_PM10) 
data_norm <- sapply(a,normalize)
data_norm <- as.data.frame(data_norm)
str(data_norm)
summary(data_norm)
data_norm <- cbind(data_norm,data2 %>% select(mean_PM10))
str(data_norm)

### train , test 데이터 추출 ###
which = sample(1:1825,1460)
data_train = data_norm[which,]
data_test = data_norm[-which,]

# 모든 예측 모델에 동일한 train, test 적용하기 위해 따로 저장해둔 data set
#data_train <- read_excel("train_data.xlsx")
#data_test <- read_excel("test_data.xlsx") 
