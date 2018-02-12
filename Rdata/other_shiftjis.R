#使用ライブラリ
library(pROC)
library(dplyr)
library(glmnet)
library(caret)

#データ読込
train<-read.csv("C:/bank/motodata/train.csv", header=T)
test<-read.csv("C:/bank/motodata/test.csv", header=T)

#カテゴリー変数を数値変数に変換
train_v<-as.data.frame(predict(dummyVars(~.,data=train), train))
test_v<-as.data.frame(predict(dummyVars(~.,data=test), test))

#欠損値の除去(0に変換)
train_v[is.na(train_v)]<-0
test_v[is.na(test_v)]<-0

#説明変数作成(行列に変換)
lasso_train <-Matrix(as.matrix(dplyr::select(train_v,-id,-y)), sparse=T)
lasso_test  <-Matrix(as.matrix(dplyr::select(test_v,-id)), sparse=T)

#目的変数作成
y_train <- train_v[,"y"]

###Hold Out
#構築データの割合
rate<-0.7

#構築データ数(小数の切捨て)
num<-as.integer(nrow(lasso_train)*rate)

#再現性のため乱数シードを固定
set.seed(17)

#sample(ベクトル, ランダムに取得する個数, 復元抽出の有無)
row<-sample(1:nrow(lasso_train), num, replace=FALSE)

#構築データ
lasso_train_train<-lasso_train[row,]

#検証データ
lasso_train_test<-lasso_train[-row,]

#目的変数作成
y_train_train<- as.numeric(train[row, "y"])
y_train_test<- as.numeric(train[-row, "y"])

###Lasso回帰モデル構築(alpha=1でLasso, alpha=0でRidge, 0<alpha<1でElastic Netとなる)
set.seed(17)
lasso <- cv.glmnet(lasso_train_train, y=as.numeric(y_train_train), alpha=0,
                   nfold=6, family="binomial", nlambda=100, type.measure="auc")

#検証データへ当てはめ
pred <-predict(lasso, newx=lasso_train_test, s="lambda.min", type="response")[,1]

#AUC確認
auc<-roc(y_train_test, pred)$auc
auc