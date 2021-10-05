library(Boruta)
library(neuralnet)

# 상관계수 높은 변수 제외
train <- subset(data_train, select=-c(mean_SO2, mean_NO2, 평균기온,경유,등유,LPG, 평균상대습도))

# 다중 공선성 높은 변수 제외
train <- subset(train, select=-c(월_12, 풍향_북북동))
train <- subset(train, select=-c(풍향_서))
train <- subset(train, select=-c(평균증기압))

## Boruta 적용하여 변수 선택 ##
set.seed(1234)
boruta.train <- Boruta(mean_PM10 ~., data=train, doTrace=2)
final.boruta <- TentativeRoughFix(boruta.train)
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)

# 최종 선택된 변수
data_br <- subset(train, select=c(mean_PM10, 어제PM10, 어제중국PM10, 일교차, 일강수량, 평균풍속, 평균전운량, day_tr_sum, 휘발유, 벙커C유, 기타, mean_CO, mean_O3, bei_PM10, shan_PM10, 월_01, 월_02, 월_03, 월_04, 월_05, 월_06, 월_07, 월_08, 월_09, 월_10, 풍향_북동, 풍향_동북동))

### 인공신경망 예측 ###

# 종속변수 정규화
data_br$mean_PM10 <- (data_br$mean_PM10-min(data$mean_PM10))/(max(data$mean_PM10)-min(data$mean_PM10))
data_test$mean_PM10 <-(data_test$mean_PM10-min(data$mean_PM10))/(max(data$mean_PM10)-min(data$mean_PM10))

# 인공신경망 모델 생성
# 몇 번의 테스트 후, 가장 정확성이 높은 hidden=c(3)으로 모델 선정
boruta_model <- neuralnet(formula = mean_PM10~., data=data_br, hidden=c(3))

# test data에 예측하기
pred_tt <- compute(boruta_model, data_test[-51])
pred_tt_result <- pred_tt$net.result

# 역정규화
pred_tt_back <- pred_tt_result*(max(data$mean_PM10)-min(data$mean_PM10))+min(data$mean_PM10)
test_real <- data_test$mean_PM10*(max(data$mean_PM10)-min(data$mean_PM10))+min(data$mean_PM10)

# 결과 확인
cor(pred_tt_back, test_real)
RMSE(pred_tt_back, test_real)
wilcox.test(pred_tt_back, test_real)

# train data 예측
pred_tr <- compute(boruta_model, data_train[-51])
pred_tr_result <- pred_tr$net.result

# 역정규화
pred_tr_back <- pred_tr_result*(max(data$mean_PM10)-min(data$mean_PM10))+min(data$mean_PM10)
train_real <- check_br$mean_PM10*(max(data$mean_PM10)-min(data$mean_PM10))+min(data$mean_PM10)

# 결과 확인
cor(pred_tr_back,train_real)
RMSE(pred_tr_back,train_real)
wilcox.test(pred_tr_back, train_real)