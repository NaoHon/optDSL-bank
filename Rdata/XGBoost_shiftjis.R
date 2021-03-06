##########使用ライブラリ##########
library(dplyr)
library(xgboost)
library(caret)
library(pROC)

#データ読み込み
train<-read.csv("C:/study/bank/motodata/train.csv", header=T)
test<-read.csv("C:/study/bank/motodata/test.csv", header=TRUE)

#カテゴリー変数を数値変数に変換
train_v<-as.data.frame(predict(dummyVars(~.,data=train), train))
test_v<-as.data.frame(predict(dummyVars(~.,data=test), test))


#目的変数作成
y_train <- train$y

#行列に変換
x_train <-as.matrix(dplyr::select(train_v,-id,-y))
x_test <-as.matrix(dplyr::select(test_v,-id,-y))

###Hold Out
#構築データの割合
rate<-0.7

#構築データ数(小数の切捨て)
num<-as.integer(nrow(x_train)*rate)

#再現性のため乱数シードを固定
set.seed(17)

#sample(ベクトル, ランダムに取得する個数, 復元抽出の有無)
row<-sample(1:nrow(x_train), num, replace=FALSE)

#構築データ
x_train_train<-x_train[row,]

#検証データ
x_train_test<-x_train[-row,]

#目的変数作成
y_train_train<- y_train[row]
y_train_test<- y_train[-row]

#パラメータの設定
set.seed(17)
param <- list(objective = "binary:logistic", #ロジスティック回帰で確率出力
              eval_metric = "auc", #評価指標
              eta=0.07, #学習率
              max_depth=3, #決定木の階層
              min_child_weight=10, #最小ノード数
              colsample_bytree=0.4, #使用する変数割合
              gamma=0.9, #損失還元最小値
              subsample=1 #使用する学習データ割合
)

#CVによる学習数探索
xgbcv <- xgb.cv(param=param, data=x_train_train, label=y_train_train,
                nrounds=500, #学習回数
                nfold=5, #CV数
                nthread=1 #使用するCPU数
)

#モデル構築
set.seed(17)
# model_xgb <- xgboost(param=param, data = x_train_train, label=y_train_train,
#                      nrounds=which.max(xgbcv$evaluation_log$test.auc.mean), nthread=1, imprtance=TRUE)
model_xgb <- xgboost(param=param, data = x_train_train, label=y_train_train,
                      nrounds=xgbcv$evaluation_log$test_auc_mean, nthread=1, imprtance=TRUE)



#train_testのAUC
pred<-predict(model_xgb, x_train_test)
auc<-roc(y_train_test, pred)
print(auc)


#変数重要度
imp<- xgb.importance(names(dplyr::select(train_v,-id,-y)), model=model_xgb)
print(imp)


pred_test<-predict(model_xgb, x_test)
#CSV出力
submit1<-data.frame(id=test$id, score=pred_test)
write.table(submit1,
            file="C:/study/bank/submit/submit_train_20171027_xgboost_1.csv",
            quote=F, sep=",", row.names=F, col.names=F)

