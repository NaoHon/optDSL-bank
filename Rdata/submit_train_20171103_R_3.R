#複数モデルの組み合わせ（平均）

####################submit_20171103_1_logi#######################################################
#使用ライブラリ
library(pROC)
library(dplyr)
library(ggplot2)

#データ読込
train<-read.csv("C:/study/bank/motodata/train.csv", header=T)
test<-read.csv("C:/study/bank/motodata/test.csv", header=T)

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


# ###ステップワイズ法 AICで判断、探索的
# logi_model_all <- glm(
#   y ~ .,    #目的変数と説明変数の指定(全て使う場合はy~.)
#   data=train,             #学習データ
#   family=binomial(link="logit") #ロジスティック回帰を指定
# )
# 
# #ステップワイズ
# step.model_all <- step(logi_model_all)
# 
# #選択された変数の確認
# summary(step.model_all)
# 



#精度確認のためHold Out
rate<-0.7
num<-as.integer(nrow(train)*rate)
set.seed(17)
row<-sample(1:nrow(train), num, replace=FALSE)

train_train<-train[row,]
train_test<-train[-row,]

logi_model<- glm(
  y ~ job + marital + education + housing + loan + contact + day + 
    month + poutcome + age2 + balance2 + duration2 + campaign2 + 
    pdays2 + previous2,    #目的変数と説明変数の指定(全て使う場合はy~.)
  data=train_train,             #学習データ
  family=binomial(link="logit") #ロジスティック回帰を指定
)

#モデルの中身を見る
summary(logi_model)

#モデルの精度確認
#モデルの当てはめ
pred_train_test<- predict(logi_model, newdata=train_test, type="response")

#AUC確認
auc<-roc(train_test$y, pred_train_test)$auc
auc


###Submit
logi_model2 <- glm(
  y ~ job + marital + education + housing + loan + contact + day + 
    month + poutcome + age2 + balance2 + duration2 + campaign2 + 
    pdays2 + previous2,    #目的変数と説明変数の指定(全て使う場合はy~.)
  data=train,             #学習データ
  family=binomial(link="logit") #ロジスティック回帰を指定
)


pred_logi <- predict(logi_model2, newdata=test, type="response")

#submitの形式で出力(CSV)
#データ加工
out<-data.frame(test2$id, pred_logi)

# #出力
# write.table(out, #出力データ
#             "C:/study/bank/submit/submit_20171103_1_logi.csv", #出力先
#             quote=FALSE, #文字列を「"」で囲む有無
#             col.names=FALSE, #変数名(列名)の有無
#             row.names=FALSE, #行番号の有無
#             sep="," #区切り文字の指定
# )

####################submit_train_20171103_RandomForest_1#######################################################

#使用ライブラリ
library(randomForest)
library(dplyr)
library(pROC)

#データ読み込み
train<-read.csv("c:/study/bank/motodata/train.csv", header=T)
test<-read.csv("C:/study/bank/motodata/test.csv", header=TRUE)

# ###Hold Out
# #構築データの割合
# rate<-0.7
# 
# #構築データ数(小数の切捨て)
# num<-as.integer(nrow(train)*rate)
# 
# #再現性のため乱数シードを固定
# set.seed(17)
# 
# #sample(ベクトル, ランダムに取得する個数, 復元抽出の有無)
# row<-sample(1:nrow(train), num, replace=FALSE)
# 
# #構築データ
# rf_train_train<-train[row,] %>%
#   dplyr::select(-id, -y)
# 
# #検証データ
# rf_train_test<-train[-row,] %>%
#   dplyr::select(-id, -y)
# 
# #目的変数作成
# y_train_train<- train[row,] %>%
#   dplyr::select(y)
# y_train_test<- train[-row,] %>%
#   dplyr::select(y)
# 
# #再現性のため乱数シードを固定
# set.seed(17)
# rf<-randomForest(rf_train_train, #学習データ(説明変数)
#                  as.factor(y_train_train$y), #学習データ(目的変数)
#                  mtry=4, #1本の木に使用する変数の数
#                  sampsize=nrow(rf_train_train)*0.3, #モデル構築に使用するデータ数
#                  nodesize=100, #生成する各決定木のノードが含むサンプル最小数
#                  maxnodes=30, #生成する各決定木の終端ノードの最大数
#                  ntree=5000, #生成する決定木の数
#                  imprtance=T #変数重要度の有無
# )
# 
# ##train_testのAUC
# #prediction(予測結果,目的変数(1 or 0))
# pred <-predict(rf, newdata=rf_train_test, type="prob")[,2]
# auc<-roc(y_train_test$y, pred)
# print(auc)

##########################投稿用
#構築データ
rf_train<-train %>%
  dplyr::select(-id, -y)

#検証データ
rf_train_test<-test %>%
  dplyr::select(-id)

#目的変数作成
y_train<- train %>%
  dplyr::select(y)

rf<-randomForest(rf_train, #学習データ(説明変数)
                 as.factor(y_train$y), #学習データ(目的変数)
                 mtry=4, #1本の木に使用する変数の数
                 sampsize=nrow(rf_train)*0.3, #モデル構築に使用するデータ数
                 nodesize=100, #生成する各決定木のノードが含むサンプル最小数
                 maxnodes=30, #生成する各決定木の終端ノードの最大数
                 ntree=5000, #生成する決定木の数
                 imprtance=T #変数重要度の有無
)
pred_rf <-predict(rf, newdata=test, type="prob")[,2]
# #CSV出力
# submit1<-data.frame(id=test$id, score=pred)
# write.table(submit1,
#             file="C:/study/bank/submit/submit_train_20171103_RandomForest_1.csv",
#             quote=F, sep=",", row.names=F, col.names=F)


####################submit_train_20171103_RandomForest_1#######################################################


####################submit_train_20171103_xgboost_#######################################################
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


#train_testのAUC
pred<-predict(model_xgb, x_train_test)
auc<-roc(y_train_test, pred)
print(auc)


#変数重要度
imp<- xgb.importance(names(dplyr::select(train_v,-id,-y)), model=model_xgb)
print(imp)


pred_xgb<-predict(model_xgb, x_test)
#CSV出力
# submit1<-data.frame(id=test$id, score=pred_xgb)
# write.table(submit1,
#             file="C:/study/bank/submit/submit_train_20171103_xgboost_.csv",
#             quote=F, sep=",", row.names=F, col.names=F)

####################submit_train_20171103_xgboost_#######################################################
#submitの形式で出力(CSV)
#データ加工
out<-data.frame(test$id, (pred_logi+4*pred_rf+6*pred_xgb)/10)
write.table(out,
            file="C:/study/bank/submit/submit_train_20171103_R_3.csv",
            quote=F, sep=",", row.names=F, col.names=F)

