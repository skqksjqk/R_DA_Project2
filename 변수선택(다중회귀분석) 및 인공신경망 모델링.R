library(neuralnet)

#상관계수 높은 것 0.6이상 제거
data_train <- data_train %>% select(-mean_SO2, -mean_NO2, -평균기온, -경유, -등유, -LPG, -평균상대습도)

### 회귀분석 ###
result <- lm(mean_PM10~. ,data=data_train)
summary(result)
vif(result)
max(vif(result))

# 등분산성 4 이상 제거
data_train <- data_train %>% select(-월_12,-풍향_북북동)
data_train <- data_train %>% select(-풍향_서)
data_train <- data_train %>% select(-평균증기압)

#후진소거법으로 변수 제거
data_train <- data_train %>% select(-벙커C유)
data_train <- data_train %>% select(-bei_PM10)
data_train <- data_train %>% select(-풍향_남남동)
data_train <- data_train %>% select(-풍향_남남서)
data_train <- data_train %>% select(-풍향_남동)
data_train <- data_train %>% select(-풍향_남서)
data_train <- data_train %>% select(-어제중국PM10)
data_train <- data_train %>% select(-기타)
data_train <- data_train %>% select(-풍향_북)
data_train <- data_train %>% select(-풍향_남)
data_train <- data_train %>% select(-풍향_북동)
data_train <- data_train %>% select(-풍향_동남동)
data_train <- data_train %>% select(-풍향_동)
data_train <- data_train %>% select(-풍향_동북동)
data_train <- data_train %>% select(-day_tr_sum)
data_train <- data_train %>% select(-휘발유)
data_train <- data_train %>% select(-월_01)
data_train <- data_train %>% select(-월_09)
data_train <- data_train %>% select(-월_06)
data_train <- data_train %>% select(-월_08)
data_train <- data_train %>% select(-shan_PM10)
data_train <- data_train %>% select(-월_11)
data_train <- data_train %>% select(-일교차)
data_train <- data_train %>% select(-월_10)
data_train <- data_train %>% select(-월_02)
data_train <- data_train %>% select(-월_07)

# 최종 선정된 변수 
data_train1 <- data_train
result_line <- lm(mean_PM10~., data=data_train1)

### 1) 회귀식 예측 ###

# 테스트데이터 예측
pred_line = predict(result_line, data_test)
RMSE(pred_line, data_test$mean_PM10)
mse(pred_line, data_test$mean_PM10)
wilcox.test(pred_line, data_test$mean_PM10)

# 트레인데이터 예측
pred_line_train = predict(result_line, data_train1)
RMSE(pred_line_train, data_train1$mean_PM10)
mse(pred_line_train, data_train1$mean_PM10)

### 2) 인공신경망 예측 ###

# 인공신경망 정규화
data_train1$mean_PM10 <- (data_train1$mean_PM10-min(data$mean_PM10))/(max(data$mean_PM10)-min(data$mean_PM10))
data_test$mean_PM10 <-(data_test$mean_PM10-min(data$mean_PM10))/(max(data$mean_PM10)-min(data$mean_PM10))

# 인공신경망 모델 생성
# 몇 번의 테스트 후, 가장 정확성이 높은 hidden=c(3)으로 모델 선정
nn_line <- neuralnet(formula = mean_PM10~., data=data_train1,hidden=3)

# 테스트데이터 예측
pred_nn_line <- compute(nn_line , data_test[-51])
pred_pm10_line <- pred_nn_line$net.result

# 역정규화 (수치를 보기 위하여)
pred_pm10_line <- pred_pm10_line*(max(data$mean_PM10)-min(data$mean_PM10))+min(data$mean_PM10)
test_real <- data_test$mean_PM10*(max(data$mean_PM10)-min(data$mean_PM10))+min(data$mean_PM10)

# 결과 확인
cor(pred_pm10_line, test_real)
RMSE(pred_pm10_line, test_real)
wilcox.test(pred_pm10_line, test_real)

# 트레인데이터 예측
pred_nn_line_train <- compute(nn_line , data_train1[-14])
pred_pm10_line_train <- pred_nn_line_train$net.result

# 역정규화
pred_pm10_line_train <- pred_pm10_line_train*(max(data$mean_PM10)-min(data$mean_PM10))+min(data$mean_PM10)
train_real <- data_train1$mean_PM10*(max(data$mean_PM10)-min(data$mean_PM10))+min(data$mean_PM10)

# 결과 확인
cor(pre_str2_train,train_real)
RMSE(pre_str2_train,train_real)
wilcox.test(pre_str2_train,train_real)