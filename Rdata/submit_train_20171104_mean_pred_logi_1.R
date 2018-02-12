#バギングの実装例

#使用ライブラリ
library(dplyr)
library(rpart)
library(pROC)
library(ggplot2)

#データ読込
train<-read.csv("C:/study/bank/motodata/train.csv", header=TRUE)
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


###########精度確認
#構築データの割合
rate<-0.7

#構築データ数(小数の切捨て)
num<-as.integer(nrow(train)*rate)

########ランダムに構築データを取得########
#再現性のため乱数シードを固定
set.seed(17)

#sample(ベクトル, ランダムに取得する個数, 復元抽出の有無)
row<-sample(1:nrow(train), num, replace=FALSE)

#構築データ
train_train<-train[row,]

#検証データ
train_test<- train[-row,]


##バギングの実装

#モデル構築回数
L<-10

#rate_M*(train_trainの行数)の数だけデータを復元抽出
rate_M<-0.1
M<-as.integer(nrow(train_train)*rate_M)

#再現性のため乱数シードを固定
set.seed(17)

#バギング(モデルは全て決定木)
auc<-NULL
tree_tmp<-as.list(NULL)
for(i in 1:L){
  
  #sample(ベクトル, ランダムに取得する個数, 復元抽出の有無)
  row<-sample(1:nrow(train_train), M, replace=TRUE)
  
  #復元抽出による構築データ
  train_tmp<-train_train[row,]
  
  #ロジスティック回帰
  # logi_model<- glm(
  #   y~.,    #目的変数と説明変数の指定(全て使う場合はy~.)
  #   data=train_tmp,             #学習データ
  #   family=binomial(link="logit") #ロジスティック回帰を指定
  # ) 
  
  logi_model<- glm(
    y ~ job + marital + education + housing + loan + contact + day + 
      month + poutcome + age2 + balance2 + duration2 + campaign2 + 
      pdays2 + previous2,    #目的変数と説明変数の指定(全て使う場合はy~.)
    data=dplyr::select(train_tmp, -id),             #学習データ
    family=binomial(link="logit") #ロジスティック回帰を指定
  )
  
  #検証データへ当てはめ
  pred_test<-predict(logi_model, newdata=dplyr::select(train_test, -id, -y), type="response")
  #AUCの計算
  #roc(目的変数(1 or 0), 予測結果)
  auc_tmp<-roc(train_test$y, pred_test)
  auc <- c(auc, as.numeric(auc_tmp$auc))
  
  #予測結果をとっておく
  {
    if(i==1){score_tmp<-pred_test}
    else{score_tmp<-data.frame(score_tmp, pred_test)}
  }
  
}

#予測結果の平均
#apply(データ, 1, 関数)でデータを行ごとに横ベクトルとして関数に適用
score<-apply(score_tmp, 1, mean)

#AUCの計算
#roc(目的変数(1 or 0), 予測結果)
auc_tmp<-roc(train_test$y, score)
auc <- c(auc, as.numeric(auc_tmp$auc))

#結果の確認
model<-c(as.character(1:L),"ALL_mean")
dat<-data.frame(model, AUC=auc)

print(dat)

g<-ggplot(dat, aes(x=model, y=AUC)) + geom_bar(stat="identity")
plot(g)

###########


#データ読込
train<-read.csv("C:/study/bank/motodata/train.csv", header=TRUE)
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


###########
##バギングの実装

#モデル構築回数
L<-10

#rate_M*(train_trainの行数)の数だけデータを復元抽出
rate_M<-0.1
M<-as.integer(nrow(train)*rate_M)

#再現性のため乱数シードを固定
set.seed(17)

#バギング(モデルは全て決定木)
auc<-NULL
tree_tmp<-as.list(NULL)
for(i in 1:L){
  
  #sample(ベクトル, ランダムに取得する個数, 復元抽出の有無)
  row<-sample(1:nrow(train), M, replace=TRUE)
  
  #復元抽出による構築データ
  train_tmp<-train[row,]
  
  #ロジスティック回帰
  # logi_model<- glm(
  #   y~.,    #目的変数と説明変数の指定(全て使う場合はy~.)
  #   data=train_tmp,             #学習データ
  #   family=binomial(link="logit") #ロジスティック回帰を指定
  # ) 
  
  logi_model<- glm(
    y ~ job + marital + education + housing + loan + contact + day + 
      month + poutcome + age2 + balance2 + duration2 + campaign2 + 
      pdays2 + previous2,    #目的変数と説明変数の指定(全て使う場合はy~.)
    data=dplyr::select(train_tmp, -id),             #学習データ
    family=binomial(link="logit") #ロジスティック回帰を指定
  )
  
  #検証データへ当てはめ
  pred_test<-predict(logi_model, newdata=dplyr::select(test, -id), type="response")

  #予測結果をとっておく
  {
    if(i==1){score_tmp<-pred_test}
    else{score_tmp<-data.frame(score_tmp, pred_test)}
  }
  
}

#予測結果の平均
#apply(データ, 1, 関数)でデータを行ごとに横ベクトルとして関数に適用
mean_pred_logi<-apply(score_tmp, 1, mean)

#CSV出力
submit1<-data.frame(id=test$id, score=mean_pred_logi)
write.table(submit1,
            file="C:/study/bank/submit/submit_train_20171104_mean_pred_logi_1.csv",
            quote=F, sep=",", row.names=F, col.names=F)

###########

