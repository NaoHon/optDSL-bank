##########使用ライブラリ##########
library(dplyr)
library(xgboost)
library(caret)
library(pROC)

#データ読み込み
train<-read.csv("C:/study/bank/motodata/train.csv", header=T)
test<-read.csv("C:/study/bank/motodata/test.csv", header=TRUE)


##########変数加工##########

#age線形性の確認
check<-train %>%
  #年代ごとの対数オッズを計算
  dplyr::mutate(age_c = floor(age/10)*10)%>% 
  #カテゴリ追加、10毎、切捨
  dplyr::group_by(age_c) %>%
  #グルプとアングルプはセット
  dplyr::summarise(p=mean(y)) %>%
  dplyr::ungroup(.) %>%
  dplyr::mutate(log_odds=log(p/(1-p)))

#グラフの出力
g<-ggplot(check, aes(x=age_c, y=log_odds)) + geom_line()
plot(g)

#変数加工(ageを50で折り返し)
train2<-train %>%
  #absは絶対値
  dplyr::mutate(age2=abs(50-age))

#再び線形性の確認
check<-train2 %>%
  #age2での年代ごとの対数オッズを計算
  dplyr::mutate(age_c = floor(age2/10)*10) %>%
  dplyr::group_by(age_c) %>%
  dplyr::summarise(p=mean(y)) %>%
  dplyr::ungroup(.) %>%
  dplyr::mutate(log_odds=log(p/(1-p)))

#グラフの出力
g<-ggplot(check, aes(x=age_c, y=log_odds)) + geom_line()
plot(g)

#外れ値を丸める
#balance外れ値の確認
hist(train2$balance)

#パーセンタイル点の確認

#丸め(ifelse(条件式, 真のとき返す値, 偽のとき返す値)) quantile小さい順で何パーセント分を返す
train2<-train2 %>%
  dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                quantile(balance,probs=.95),
                                balance))
#外れ値の確認
hist(train2$balance2)

#相関係数の確認
cor(train2[,c("age2", "balance2")])

#外れ値の確認
hist(train2$balance)
hist(train2$duration)
hist(train2$campaign)
hist(train2$pdays)
hist(train2$previous)

#パーセンタイル点の確認

#丸め(ifelse(条件式, 真のとき返す値, 偽のとき返す値)) quantile小さい順で何パーセント分を返す
train2<-train2 %>%
  dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                quantile(balance,probs=.95),
                                balance))
train2<-train2 %>%
  dplyr::mutate(duration2=ifelse(duration >= quantile(duration,probs=.95),
                                quantile(duration,probs=.95),
                                duration))
train2<-train2 %>%
  dplyr::mutate(campaign2=ifelse(campaign >= quantile(campaign,probs=.95),
                                quantile(campaign,probs=.95),
                                campaign))
train2<-train2 %>%
  dplyr::mutate(pdays2=ifelse(pdays >= quantile(pdays,probs=.95),
                                quantile(pdays,probs=.95),
                                pdays))
train2<-train2 %>%
  dplyr::mutate(previous2=ifelse(previous >= quantile(previous,probs=.95),
                                quantile(previous,probs=.95),
                                previous))
#外れ値の確認
hist(train2$balance2)
hist(train2$duration2)
hist(train2$campaign2)
hist(train2$pdays2)
hist(train2$previous2)

#testにもtrainと同様の加工
test2<-test %>%
  dplyr::mutate(age2=abs(50-age),
                balance2=ifelse(balance >= quantile(balance,probs=.95),
                                quantile(balance,probs=.95),
                                balance),
                duration2=ifelse(duration >= quantile(duration,probs=.95),
                                quantile(duration,probs=.95),
                                duration),
                campaign2=ifelse(campaign >= quantile(campaign,probs=.95),
                                quantile(campaign,probs=.95),
                                campaign),
                pdays2=ifelse(pdays >= quantile(pdays,probs=.95),
                                quantile(pdays,probs=.95),
                              pdays),
                previous2=ifelse(previous >= quantile(previous,probs=.95),
                                quantile(previous,probs=.95),
                                previous))

# train<-train2
# test<-test2
# train<-dplyr::select(train2,-age,-balance)
# test<-dplyr::select(test2,-age,-balance)
train<-dplyr::select(train2,-age,-balance,-duration,-campaign,-pdays,-previous)
test<-dplyr::select(test2,-age,-balance,-duration,-campaign,-pdays,-previous)
##########変数加工##########

#カテゴリー変数を数値変数に変換
train_v<-as.data.frame(predict(dummyVars(~.,data=train), train))
test_v<-as.data.frame(predict(dummyVars(~.,data=test), test))


#目的変数作成
y_train <- train$y

#行列に変換
x_train <-as.matrix(dplyr::select(train_v,-id,-y))
x_test <-as.matrix(dplyr::select(test_v,-id))

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
                nrounds=10000, #学習回数
                nfold=5, #CV数
                nthread=1 #使用するCPU数
)

#モデル構築
set.seed(17)
model_xgb <- xgboost(param=param, data = x_train_train, label=y_train_train,
                     nrounds=which.max(xgbcv$evaluation_log$test_auc_mean), nthread=1, imprtance=TRUE)
# model_xgb <- xgboost(param=param, data = x_train_train, label=y_train_train,
#                      nrounds=10000, nthread=1, imprtance=TRUE)



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
            file="C:/study/bank/submit/submit_train_20171103_xgboost_3.csv",
            quote=F, sep=",", row.names=F, col.names=F)

