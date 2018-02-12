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


##スタッキングの実装
#今回は決定木(rpart)とロジスティック回帰(glm)をロジスティック回帰(glm)でアンサンブル

#学習データをK個にグループ分け
K<-10
#sample(ベクトル, ランダムに取得する個数, 復元抽出の有無, ベクトルの各要素が抽出される確率)
train_train$cv_group<-sample(1:K, nrow(train_train), replace=TRUE, prob=rep(1/K, K))

#構築, 検証データ予測スコアの初期化
score_train_tree<-NULL
score_train_logi<-NULL
score_test_tree<-NULL
score_test_logi<-NULL
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
  
  #構築データでモデル構築(決定木)
  tree_tmp<-rpart(y~., data=train_tmp,
                  maxdepth=10, minbucket=12, cp=0.000008,
                  method="class", parms=list(split="gini"))
  
  #構築データでモデル構築(ロジスティック回帰)
  logi_tmp<-glm(y~., data=train_tmp, family=binomial(link="logit"))
  
  #モデル構築に使用していないデータの予測値と目的変数
  pred_train_tree<-predict(tree_tmp, test_tmp)[,2]
  pred_train_logi<-predict(logi_tmp, test_tmp, type="response")
  y<-c(y, test_tmp$y)
  
  score_train_tree<-c(score_train_tree, pred_train_tree)
  score_train_logi<-c(score_train_logi, pred_train_logi)
  
  
  #検証データの予測値
  pred_test_tree<-predict(tree_tmp, train_test)[,2]
  pred_test_logi<-predict(logi_tmp, train_test, type="response")
  
  score_test_tree<-cbind(score_test_tree, pred_test_tree)
  score_test_logi<-cbind(score_test_logi, pred_test_logi)
  
}

#余計な変数削除
train_train<-train_train %>%
  dplyr::select(-cv_group)

#検証データの予測値の平均
#apply(データ, 1, 関数)で行ごとに関数を適用する
score_test_tree<-apply(score_test_tree, 1, mean)
score_test_logi<-apply(score_test_logi, 1, mean)
m_dat_test1<-data.frame(tree=score_test_tree, logi=score_test_logi,y=train_test$y)


#メタモデル用変数作成
m_dat_train<-data.frame(tree=score_train_tree, logi=score_train_logi, y=y)

#メタモデル構築(今回はロジスティック回帰)
m_logi<-glm(y~., data=m_dat_train, family=binomial(link="logit"))

##検証データ適用1
#メタモデル適用
pred_test_m_logi1<-predict(m_logi, m_dat_test1, type="response")
auc<-roc(m_dat_test1$y, pred_test_logi)
print(auc)

############################################################################ 



##スタッキングの実装
#今回は決定木(rpart)とロジスティック回帰(glm)をロジスティック回帰(glm)でアンサンブル
#再現性のため乱数シードを固定
set.seed(17)
#学習データをK個にグループ分け
K<-5
#sample(ベクトル, ランダムに取得する個数, 復元抽出の有無, ベクトルの各要素が抽出される確率)
train$cv_group<-sample(1:K, nrow(train), replace=TRUE, prob=rep(1/K, K))

#構築, 検証データ予測スコアの初期化
score_train_tree<-NULL
score_train_logi<-NULL
score_test_tree<-NULL
score_test_logi<-NULL
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
  
  #構築データでモデル構築(決定木)
  tree_tmp<-rpart(y~., data=train_tmp,
                  maxdepth=10, minbucket=12, cp=0.000008,
                  method="class", parms=list(split="gini"))
  
  #構築データでモデル構築(ロジスティック回帰)
  logi_tmp<-glm(y~., data=train_tmp, family=binomial(link="logit"))
  
  #モデル構築に使用していないデータの予測値と目的変数
  pred_train_tree<-predict(tree_tmp, test_tmp)[,2]
  pred_train_logi<-predict(logi_tmp, test_tmp, type="response")
  y<-c(y, test_tmp$y)
  
  score_train_tree<-c(score_train_tree, pred_train_tree)
  score_train_logi<-c(score_train_logi, pred_train_logi)
  
  
  #検証データの予測値
  pred_test_tree<-predict(tree_tmp, test)[,2]
  pred_test_logi<-predict(logi_tmp, test, type="response")
  
  score_test_tree<-cbind(score_test_tree, pred_test_tree)
  score_test_logi<-cbind(score_test_logi, pred_test_logi)
  
}

#余計な変数削除
train<-train %>%
  dplyr::select(-cv_group)

#検証データの予測値の平均
#apply(データ, 1, 関数)で行ごとに関数を適用する
score_test_tree<-apply(score_test_tree, 1, mean)
score_test_logi<-apply(score_test_logi, 1, mean)
m_dat_test1<-data.frame(tree=score_test_tree, logi=score_test_logi)


#メタモデル用変数作成
m_dat_train<-data.frame(tree=score_train_tree, logi=score_train_logi, y=y)

#メタモデル構築(今回はロジスティック回帰)
m_logi<-glm(y~., data=m_dat_train, family=binomial(link="logit"))

##検証データ適用1
#メタモデル適用
pred_test_m_logi1<-predict(m_logi, m_dat_test1, type="response")


#CSV出力
submit1<-data.frame(id=test$id, score=pred_test_m_logi1)
write.table(submit1,
            file="C:/study/bank/submit/submit_train_20171103_ens_tree_logi_1.csv",
            quote=F, sep=",", row.names=F, col.names=F)

