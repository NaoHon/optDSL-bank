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
                 ntree=500, #生成する決定木の数
                 imprtance=T #変数重要度の有無
)

plot(rf)


##train_testのAUC
#prediction(予測結果,目的変数(1 or 0))
pred <-predict(rf, newdata=rf_train_test, type="prob")[,2]
auc<-roc(y_train_test$y, pred)
print(auc)

##変数重要度
print(importance(rf))

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
                 ntree=500, #生成する決定木の数
                 imprtance=T #変数重要度の有無
)
pred <-predict(rf, newdata=test, type="prob")[,2]
#CSV出力
submit1<-data.frame(id=test$id, score=pred)
write.table(submit1,
            file="C:/study/bank/submit/submit_train_20171103_RandomForest_1.csv",
            quote=F, sep=",", row.names=F, col.names=F)

