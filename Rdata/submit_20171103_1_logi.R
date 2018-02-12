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


###参考：ステップワイズ法 AICで判断、探索的
logi_model_all <- glm(
  y ~ .,    #目的変数と説明変数の指定(全て使う場合はy~.)
  data=train,             #学習データ
  family=binomial(link="logit") #ロジスティック回帰を指定
)

#ステップワイズ
step.model_all <- step(logi_model_all)

#選択された変数の確認
summary(step.model_all)




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


pred_test <- predict(logi_model2, newdata=test, type="response")

#submitの形式で出力(CSV)
#データ加工
out<-data.frame(test2$id, pred_test)

# #出力
# write.table(out, #出力データ
#             "C:/study/bank/submit/submit_20171103_1_logi.csv", #出力先
#             quote=FALSE, #文字列を「"」で囲む有無
#             col.names=FALSE, #変数名(列名)の有無
#             row.names=FALSE, #行番号の有無
#             sep="," #区切り文字の指定
# )

