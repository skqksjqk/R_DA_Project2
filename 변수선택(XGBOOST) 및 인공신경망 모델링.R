library(xgboost)
library(neuralnet)

train_data = as.data.frame(data_train)
test_data = as.data.frame(data_test)

### 변수추출 - XGboost ###

train_label = train_data[,51]
test_label = test_data[,51]
train_data$mean_PM10 = NULL
test_data$mean_PM10 = NULL

train_data = as.matrix(train_data)
test_data = as.matrix(test_data)

dtrain = xgb.DMatrix(data=train_data, label=train_label)
dtest = xgb.DMatrix(data=test_data, label=test_label)

# train a model using our training data
model_tuned <- xgboost(data = dtrain, # the data           
                       max.depth = 3, # the maximum depth of each decision tree
                       nround = 10, # number of boosting rounds
                       early_stopping_rounds = 5,
                       gamma = 1) # add a regularization term

importance_matrix = xgb.importance(model=model_tuned)
importance_matrix$Feature # XGBoost Features

### 인공신경망 예측 ###

# 선택된 변수 추출
data_train1 = data_train %>% 
  select(importance_matrix$Feature,mean_PM10) 
data_train1 = as.data.frame(data_train1)

# 종속변수 정규화
data_train1$mean_PM10 <- (data_train1$mean_PM10-min(data$mean_PM10))/(max(data$mean_PM10)-min(data$mean_PM10))
data_test$mean_PM10 <-(data_test$mean_PM10-min(data$mean_PM10))/(max(data$mean_PM10)-min(data$mean_PM10))

# 인공신경망 모델 생성
# 몇 번의 테스트 후, 가장 정확성이 높은 hidden=c(3,3)으로 모델 선정
xgb_model <- neuralnet(formula = mean_PM10~., data=data_train1, hidden=c(3,3),act.fct = "relu")

# test data 예측
pred_xgb_model = compute(xgb_model , data_test[-51])
pred_xgb_model_result <- pred_xgb_model$net.result

# 역정규화
pred_xgb_model_result <- pred_xgb_model_result*(max(data$mean_PM10)-min(data$mean_PM10))+min(data$mean_PM10)
test_real <- data_test$mean_PM10*(max(data$mean_PM10)-min(data$mean_PM10))+min(data$mean_PM10)

# 결과 확인
cor(pred_xgb_model_result, test_real)
RMSE(pred_xgb_model_result, test_real)
wilcox.test(pred_xgb_model_result, test_real)

# train data 예측
pred_xgb_model_train <- compute(xgb_model, data_train1[-17])
pred_xgb_model_result_train<- pred_xgb_model_train$net.result

# 역정규화
pred_xgb_model_result_train <- pred_xgb_model_result_train*(max(data$mean_PM10)-min(data$mean_PM10))+min(data$mean_PM10)
train_real <- data_train1$mean_PM10*(max(data$mean_PM10)-min(data$mean_PM10))+min(data$mean_PM10)

# 결과 확인
cor(pred_xgb_model_result_train, train_real)
RMSE(pred_xgb_model_result_train, train_real)
wilcox.test(pred_xgb_model_result_train, train_real)