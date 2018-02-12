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
logi_pred_train_test<- predict(logi_model, newdata=train_test, type="response")

#AUC確認
auc<-roc(train_test$y, logi_pred_train_test)$auc
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

###Hold Out
#構築データの割合
rate<-0.7

#構築データ数(小数の切捨て)
num<-as.integer(nrow(train)*rate)

#再現性のため乱数シードを固定
set.seed(17)

#sample(ベクトル, ランダムに取得する個数, 復元抽出の有無)
row<-sample(1:nrow(train), num, replace=FALSE)

#構築データ
rf_train_train<-train[row,] %>%
  dplyr::select(-id, -y)

#検証データ
rf_train_test<-train[-row,] %>%
  dplyr::select(-id, -y)

#目的変数作成
y_train_train<- train[row,] %>%
  dplyr::select(y)
y_train_test<- train[-row,] %>%
  dplyr::select(y)

#再現性のため乱数シードを固定
set.seed(17)

#チューニング
tuneRF(rf_train_train, as.factor(y_train_train$y),doBest=TRUE)

rf<-randomForest(rf_train_train, #学習データ(説明変数)
                 as.factor(y_train_train$y), #学習データ(目的変数)
                 mtry=8, #1本の木に使用する変数の数
                 sampsize=nrow(rf_train_train)*0.3, #モデル構築に使用するデータ数
                 nodesize=100, #生成する各決定木のノードが含むサンプル最小数
                 maxnodes=30, #生成する各決定木の終端ノードの最大数
                 ntree=5000, #生成する決定木の数
                 imprtance=T #変数重要度の有無
)

plot(rf)

##train_testのAUC
#prediction(予測結果,目的変数(1 or 0))
rf_pred_train_test <-predict(rf, newdata=rf_train_test, type="prob")[,2]
auc<-roc(y_train_test$y, rf_pred_train_test)
print(auc)

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
                 mtry=8, #1本の木に使用する変数の数
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
xgb_pred_train_test<-predict(model_xgb, x_train_test)
auc<-roc(y_train_test, xgb_pred_train_test)
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


####################submit_train_20171103_ens_tree_logi_1#######################################################

##スタッキングの実装例(投稿用)

#使用ライブラリ
library(dplyr)
library(rpart)
library(pROC)
library(ggplot2)

#データ読込
train<-read.csv("C:/study/bank/motodata/train.csv", header=TRUE)
test<-read.csv("C:/study/bank/motodata/test.csv", header=TRUE)


########精度確認用################################################
#構築データの割合
rate<-0.7

#構築データ数(小数の切捨て)
num<-as.integer(nrow(train)*rate)

########ランダムに構築データを取得
#再現性のため乱数シードを固定
set.seed(17)

#sample(ベクトル, ランダムに取得する個数, 復元抽出の有無)
row<-sample(1:nrow(train), num, replace=FALSE)

#構築データ
train_train<-train[row,]

#検証データ
train_test<- train[-row,]

#カテゴリー変数を数値変数に変換
train_v<-as.data.frame(predict(dummyVars(~.,data=train_train), train_train))
test_v<-as.data.frame(predict(dummyVars(~.,data=train_test), train_test))
#行列に変換
x_train <-as.matrix(dplyr::select(train_v,-id,-y))
x_test <-as.matrix(dplyr::select(test_v,-id))


##スタッキングの実装
#今回は決定木(rpart)とロジスティック回帰(glm)をロジスティック回帰(glm)でアンサンブル

#学習データをK個にグループ分け
K<-5
#sample(ベクトル, ランダムに取得する個数, 復元抽出の有無, ベクトルの各要素が抽出される確率)
train_train$cv_group<-sample(1:K, nrow(train_train), replace=TRUE, prob=rep(1/K, K))

#構築, 検証データ予測スコアの初期化
score_train_tree<-NULL
score_train_logi<-NULL
score_train_xgb<-NULL
score_test_tree<-NULL
score_test_logi<-NULL
score_test_xgb<-NULL

y<-NULL

#クロスバリデーション
for(j in 1:K){
  #構築, 検証データに分ける
  train_tmp<-train_train %>%
    dplyr::filter(cv_group!=j) %>%
    dplyr::select(-cv_group)
  test_tmp<-train_train %>%
    dplyr::filter(cv_group==j) %>%
    dplyr::select(-cv_group)
  
  #カテゴリー変数を数値変数に変換
  train_v_tmp<-as.data.frame(predict(dummyVars(~.,data=train_tmp), train_tmp))
  test_v_tmp<-as.data.frame(predict(dummyVars(~.,data=test_tmp), test_tmp))
  
  
  #目的変数作成
  y_train <- train_tmp$y
  
  #行列に変換
  x_train_tmp <-as.matrix(dplyr::select(train_v_tmp,-id,-y))
  x_test_tmp <-as.matrix(dplyr::select(test_v_tmp,-id))
  
  ###Hold Out
  #構築データの割合
  rate<-0.7
  
  #構築データ数(小数の切捨て)
  num<-as.integer(nrow(x_train_tmp)*rate)
  
  #再現性のため乱数シードを固定
  set.seed(17)
  
  #sample(ベクトル, ランダムに取得する個数, 復元抽出の有無)
  row<-sample(1:nrow(x_train_tmp), num, replace=FALSE)
  
  #構築データ
  x_train_train<-x_train_tmp[row,]
  
  #検証データ
  x_train_test<-x_train_tmp[-row,]
  
  #目的変数作成
  y_train_train<- y_train[row]
  y_train_test<- y_train[-row]
  
  #パラメータの設定
  set.seed(17)
  param <- list(objective = "binary:logistic", #ロジスティック回帰で確率出力
                eval_metric = "auc", #評価指標
                eta=0.07, #学習率
                max_depth=5, #決定木の階層
                min_child_weight=10, #最小ノード数
                colsample_bytree=0.4, #使用する変数割合
                gamma=0.9, #損失還元最小値
                subsample=1 #使用する学習データ割合
  )
  
  #CVによる学習数探索
  xgbcv <- xgb.cv(param=param, data=x_train_train, label=y_train_train,
                  nrounds=5000, #学習回数
                  nfold=5, #CV数
                  nthread=1 #使用するCPU数
  )
  
  #モデル構築
  set.seed(17)
  model_xgb_tmp <- xgboost(param=param, data = x_train_train, label=y_train_train,
                       nrounds=which.max(xgbcv$evaluation_log$test_auc_mean), nthread=1, imprtance=TRUE)
  
  
  #train_testのAUC
  xgb_pred_train_test_tmp<-predict(model_xgb_tmp, x_train_test)
  auc<-roc(y_train_test, xgb_pred_train_test_tmp)
  print(auc) 
  
  
  
  #構築データでモデル構築(決定木)
  tree_tmp<-rpart(y~., data=train_tmp,
                  maxdepth=10, minbucket=12, cp=0.000008,
                  method="class", parms=list(split="gini"))
  
  #構築データでモデル構築(ロジスティック回帰)
  logi_tmp<-glm(y~., data=train_tmp, family=binomial(link="logit"))
  
  #モデル構築に使用していないデータの予測値と目的変数
  pred_train_tree<-predict(tree_tmp, test_tmp)[,2]
  pred_train_logi<-predict(logi_tmp, test_tmp, type="response")
  pred_train_xgb<-predict(model_xgb_tmp, x_test_tmp)
  y<-c(y, test_tmp$y)
  
  score_train_tree<-c(score_train_tree, pred_train_tree)
  score_train_logi<-c(score_train_logi, pred_train_logi)
  score_train_xgb<-c(score_train_xgb, pred_train_xgb)
  
  #検証データの予測値
  pred_test_tree<-predict(tree_tmp, train_test)[,2]
  pred_test_logi<-predict(logi_tmp, train_test, type="response")
  pred_test_xgb<-predict(model_xgb_tmp, x_test)
  
  score_test_tree<-cbind(score_test_tree, pred_test_tree)
  score_test_logi<-cbind(score_test_logi, pred_test_logi)
  score_test_xgb<-cbind(score_test_xgb, pred_test_xgb)
  
}

#余計な変数削除
train_train<-train_train %>%
  dplyr::select(-cv_group)

#検証データの予測値の平均
#apply(データ, 1, 関数)で行ごとに関数を適用する
score_test_tree<-apply(score_test_tree, 1, mean)
score_test_logi<-apply(score_test_logi, 1, mean)
score_test_xgb<-apply(score_test_xgb, 1, mean)
m_dat_test1<-data.frame(tree=score_test_tree, logi=score_test_logi,xgb=score_test_xgb,y=train_test$y)


#メタモデル用変数作成
m_dat_train<-data.frame(tree=score_train_tree, logi=score_train_logi,xgb=score_train_xgb, y=y)

#メタモデル構築(今回はロジスティック回帰)
m_logi<-glm(y~., data=m_dat_train, family=binomial(link="logit"))

##検証データ適用1
#メタモデル適用
stack_pred_train_test<-predict(m_logi, m_dat_test1, type="response")
auc<-roc(m_dat_test1$y, stack_pred_train_test)
print(auc)

############################################################################


#データ読込
train<-read.csv("C:/study/bank/motodata/train.csv", header=TRUE)
test<-read.csv("C:/study/bank/motodata/test.csv", header=TRUE)
##スタッキングの実装
#今回は決定木(rpart)とロジスティック回帰(glm)をロジスティック回帰(glm)でアンサンブル
########ランダムに構築データを取得
#再現性のため乱数シードを固定
set.seed(17)

#学習データをK個にグループ分け
K<-5
#sample(ベクトル, ランダムに取得する個数, 復元抽出の有無, ベクトルの各要素が抽出される確率)
train$cv_group<-sample(1:K, nrow(train), replace=TRUE, prob=rep(1/K, K))

#カテゴリー変数を数値変数に変換
train_v<-as.data.frame(predict(dummyVars(~.,data=train), train))
test_v<-as.data.frame(predict(dummyVars(~.,data=test), test))
#行列に変換
x_train <-as.matrix(dplyr::select(train_v,-id,-y))
x_test <-as.matrix(dplyr::select(test_v,-id))

#構築, 検証データ予測スコアの初期化
score_train_tree<-NULL
score_train_logi<-NULL
score_train_xgb<-NULL
score_test_tree<-NULL
score_test_logi<-NULL
score_test_xgb<-NULL

y<-NULL

#クロスバリデーション
for(j in 1:K){
  #構築, 検証データに分ける
  train_tmp<-train %>%
    dplyr::filter(cv_group!=j) %>%
    dplyr::select(-cv_group)
  test_tmp<-train %>%
    dplyr::filter(cv_group==j) %>%
    dplyr::select(-cv_group)
  
  #カテゴリー変数を数値変数に変換
  train_v_tmp<-as.data.frame(predict(dummyVars(~.,data=train_tmp), train_tmp))
  test_v_tmp<-as.data.frame(predict(dummyVars(~.,data=test_tmp), test_tmp))
  
  
  #目的変数作成
  y_train <- train_tmp$y
  
  #行列に変換
  x_train_tmp <-as.matrix(dplyr::select(train_v_tmp,-id,-y))
  x_test_tmp <-as.matrix(dplyr::select(test_v_tmp,-id))
  
  ###Hold Out
  #構築データの割合
  rate<-0.7
  
  #構築データ数(小数の切捨て)
  num<-as.integer(nrow(x_train_tmp)*rate)
  
  #再現性のため乱数シードを固定
  set.seed(17)
  
  #sample(ベクトル, ランダムに取得する個数, 復元抽出の有無)
  row<-sample(1:nrow(x_train_tmp), num, replace=FALSE)
  
  #構築データ
  x_train_train_tmp<-x_train_tmp[row,]
  
  #検証データ
  x_train_test_tmp<-x_train_tmp[-row,]
  
  #目的変数作成
  y_train_tmp<- y_train[row]
  y_test_tmp<- y_train[-row]
  
  #パラメータの設定
  set.seed(17)
  param <- list(objective = "binary:logistic", #ロジスティック回帰で確率出力
                eval_metric = "auc", #評価指標
                eta=0.07, #学習率
                max_depth=5, #決定木の階層
                min_child_weight=10, #最小ノード数
                colsample_bytree=0.4, #使用する変数割合
                gamma=0.9, #損失還元最小値
                subsample=1 #使用する学習データ割合
  )
  
  #CVによる学習数探索
  xgbcv <- xgb.cv(param=param, data=x_train_train_tmp, label=y_train_tmp,
                  nrounds=5000, #学習回数
                  nfold=5, #CV数
                  nthread=1 #使用するCPU数
  )
  
  #モデル構築
  set.seed(17)
  model_xgb_tmp <- xgboost(param=param, data = x_train_train_tmp, label=y_train_tmp,
                           nrounds=which.max(xgbcv$evaluation_log$test_auc_mean), nthread=1, imprtance=TRUE)
  
  
  #testのAUC
  xgb_pred_test_tmp<-predict(model_xgb_tmp, x_train_test_tmp)
  auc<-roc(y_test_tmp, xgb_pred_test_tmp)
  print(auc) 
  
  
  
  #構築データでモデル構築(決定木)
  tree_tmp<-rpart(y~., data=train_tmp,
                  maxdepth=10, minbucket=12, cp=0.000008,
                  method="class", parms=list(split="gini"))
  
  #構築データでモデル構築(ロジスティック回帰)
  logi_tmp<-glm(y~., data=train_tmp, family=binomial(link="logit"))
  
  #モデル構築に使用していないデータの予測値と目的変数
  pred_train_tree<-predict(tree_tmp, test_tmp)[,2]
  pred_train_logi<-predict(logi_tmp, test_tmp, type="response")
  pred_train_xgb<-predict(model_xgb_tmp, x_test_tmp)
  y<-c(y, test_tmp$y)
  
  score_train_tree<-c(score_train_tree, pred_train_tree)
  score_train_logi<-c(score_train_logi, pred_train_logi)
  score_train_xgb<-c(score_train_xgb, pred_train_xgb)
  
  #検証データの予測値
  pred_test_tree<-predict(tree_tmp, test)[,2]
  pred_test_logi<-predict(logi_tmp, test, type="response")
  pred_test_xgb<-predict(model_xgb_tmp, x_test)
  
  score_test_tree<-cbind(score_test_tree, pred_test_tree)
  score_test_logi<-cbind(score_test_logi, pred_test_logi)
  score_test_xgb<-cbind(score_test_xgb, pred_test_xgb)
  
}

#余計な変数削除
train<-train %>%
  dplyr::select(-cv_group)

#検証データの予測値の平均
#apply(データ, 1, 関数)で行ごとに関数を適用する
score_test_tree<-apply(score_test_tree, 1, mean)
score_test_logi<-apply(score_test_logi, 1, mean)
score_test_xgb<-apply(score_test_xgb, 1, mean)
m_dat_test1<-data.frame(tree=score_test_tree, logi=score_test_logi,xgb=score_test_xgb)


#メタモデル用変数作成
m_dat_train<-data.frame(tree=score_train_tree, logi=score_train_logi,xgb=score_train_xgb, y=y)

#メタモデル構築(今回はロジスティック回帰)
m_logi<-glm(y~., data=m_dat_train, family=binomial(link="logit"))

##検証データ適用1
#メタモデル適用
stack_pred_test<-predict(m_logi, m_dat_test1, type="response")

#CSV出力
# submit1<-data.frame(id=test$id, score=pred_ens_tree_logi)
# write.table(submit1,
#             file="C:/study/bank/submit/submit_train_20171103_ens_tree_logi_1.csv",
#             quote=F, sep=",", row.names=F, col.names=F)
####################submit_train_20171103_ens_tree_logi_1#######################################################


#重み付け検証
lw <- 0
rw <- 0
xw <- 0
sw <- 0
x <-  NULL


while (lw <= 7) {
  while (rw <= 7) {
    while (xw <= 7) {
      while (sw <= 7) {
        if((lw+rw+xw+sw)==10){
          auc<-roc(train_test$y, (lw*logi_pred_train_test
                                  +rw*rf_pred_train_test
                                  +xw*xgb_pred_train_test
                                  +sw*stack_pred_train_test)/10)
          
          x<- rbind(x, c(lw,rw,xw,sw,auc$auc))
        }
        sw <- sw + 0.5
      }
      xw <- xw + 0.5
      sw <- 0
    }
    rw <- rw + 0.5
    sw <- 0
    xw <- 0
  }
  lw <- lw + 0.5
  sw <- 0
  xw <- 0
  rw <- 0
}

x[order(x[,5], decreasing=T),]

#submitの形式で出力(CSV)
#データ加工
out<-data.frame(test$id, (2.5*pred_rf+6.5*pred_xgb+1*stack_pred_test)/10)
write.table(out,
            file="C:/study/bank/submit/submit_train_20171104_ens_lg_rf_xgb_ens_tree_lg_xgb_3.csv",
            quote=F, sep=",", row.names=F, col.names=F)





