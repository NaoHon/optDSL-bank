#�g�p���C�u����
library(pROC)
library(dplyr)
library(ggplot2)

#�f�[�^�Ǎ�
train<-read.csv("C:/study/bank/motodata/train.csv", header=T)
test<-read.csv("C:/study/bank/motodata/test.csv", header=T)

##########�ϐ����H##########

#age���`���̊m�F
check<-train %>%
  #�N�ゲ�Ƃ̑ΐ��I�b�Y���v�Z
  dplyr::mutate(age_c = floor(age/10)*10)%>% 
  #�J�e�S���ǉ��A10���A�؎�
  dplyr::group_by(age_c) %>%
  #�O���v�ƃA���O���v�̓Z�b�g
  dplyr::summarise(p=mean(y)) %>%
  dplyr::ungroup(.) %>%
  dplyr::mutate(log_odds=log(p/(1-p)))

#�O���t�̏o��
g<-ggplot(check, aes(x=age_c, y=log_odds)) + geom_line()
plot(g)

#�ϐ����H(age��50�Ő܂�Ԃ�)
train2<-train %>%
  #abs�͐�Βl
  dplyr::mutate(age2=abs(50-age))

#�Ăѐ��`���̊m�F
check<-train2 %>%
  #age2�ł̔N�ゲ�Ƃ̑ΐ��I�b�Y���v�Z
  dplyr::mutate(age_c = floor(age2/10)*10) %>%
  dplyr::group_by(age_c) %>%
  dplyr::summarise(p=mean(y)) %>%
  dplyr::ungroup(.) %>%
  dplyr::mutate(log_odds=log(p/(1-p)))

#�O���t�̏o��
g<-ggplot(check, aes(x=age_c, y=log_odds)) + geom_line()
plot(g)

#�O��l���ۂ߂�
#balance�O��l�̊m�F
hist(train2$balance)

#�p�[�Z���^�C���_�̊m�F

#�ۂ�(ifelse(������, �^�̂Ƃ��Ԃ��l, �U�̂Ƃ��Ԃ��l)) quantile���������ŉ��p�[�Z���g����Ԃ�
train2<-train2 %>%
  dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                quantile(balance,probs=.95),
                                balance))
#�O��l�̊m�F
hist(train2$balance2)

#���֌W���̊m�F
cor(train2[,c("age2", "balance2")])

#�O��l�̊m�F
hist(train2$balance)
hist(train2$duration)
hist(train2$campaign)
hist(train2$pdays)
hist(train2$previous)

#�p�[�Z���^�C���_�̊m�F

#�ۂ�(ifelse(������, �^�̂Ƃ��Ԃ��l, �U�̂Ƃ��Ԃ��l)) quantile���������ŉ��p�[�Z���g����Ԃ�
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
#�O��l�̊m�F
hist(train2$balance2)
hist(train2$duration2)
hist(train2$campaign2)
hist(train2$pdays2)
hist(train2$previous2)

#test�ɂ�train�Ɠ��l�̉��H
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
##########�ϐ����H##########


###�Q�l�F�X�e�b�v���C�Y�@ AIC�Ŕ��f�A�T���I
logi_model_all <- glm(
  y ~ .,    #�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
  data=train,             #�w�K�f�[�^
  family=binomial(link="logit") #���W�X�e�B�b�N��A���w��
)

#�X�e�b�v���C�Y
step.model_all <- step(logi_model_all)

#�I�����ꂽ�ϐ��̊m�F
summary(step.model_all)




#���x�m�F�̂���Hold Out
rate<-0.7
num<-as.integer(nrow(train)*rate)
set.seed(17)
row<-sample(1:nrow(train), num, replace=FALSE)

train_train<-train[row,]
train_test<-train[-row,]

logi_model<- glm(
  y ~ job + marital + education + housing + loan + contact + day + 
    month + poutcome + age2 + balance2 + duration2 + campaign2 + 
    pdays2 + previous2,    #�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
  data=train_train,             #�w�K�f�[�^
  family=binomial(link="logit") #���W�X�e�B�b�N��A���w��
)

#���f���̒��g������
summary(logi_model)

#���f���̐��x�m�F
#���f���̓��Ă͂�
pred_train_test<- predict(logi_model, newdata=train_test, type="response")

#AUC�m�F
auc<-roc(train_test$y, pred_train_test)$auc
auc


###Submit
logi_model2 <- glm(
  y ~ job + marital + education + housing + loan + contact + day + 
    month + poutcome + age2 + balance2 + duration2 + campaign2 + 
    pdays2 + previous2,    #�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
  data=train,             #�w�K�f�[�^
  family=binomial(link="logit") #���W�X�e�B�b�N��A���w��
)


pred_test <- predict(logi_model2, newdata=test, type="response")

#submit�̌`���ŏo��(CSV)
#�f�[�^���H
out<-data.frame(test2$id, pred_test)

# #�o��
# write.table(out, #�o�̓f�[�^
#             "C:/study/bank/submit/submit_20171103_1_logi.csv", #�o�͐�
#             quote=FALSE, #��������u"�v�ň͂ޗL��
#             col.names=FALSE, #�ϐ���(��)�̗L��
#             row.names=FALSE, #�s�ԍ��̗L��
#             sep="," #��؂蕶���̎w��
# )
