#�o�M���O�̎�����

#�g�p���C�u����
library(dplyr)
library(rpart)
library(pROC)
library(ggplot2)

#�f�[�^�Ǎ�
train<-read.csv("C:/study/bank/motodata/train.csv", header=TRUE)
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


###########���x�m�F
#�\�z�f�[�^�̊���
rate<-0.7

#�\�z�f�[�^��(�����̐؎̂�)
num<-as.integer(nrow(train)*rate)

########�����_���ɍ\�z�f�[�^���擾########
#�Č����̂��ߗ����V�[�h���Œ�
set.seed(17)

#sample(�x�N�g��, �����_���Ɏ擾�����, �������o�̗L��)
row<-sample(1:nrow(train), num, replace=FALSE)

#�\�z�f�[�^
train_train<-train[row,]

#���؃f�[�^
train_test<- train[-row,]


##�o�M���O�̎���

#���f���\�z��
L<-10

#rate_M*(train_train�̍s��)�̐������f�[�^�𕜌����o
rate_M<-0.1
M<-as.integer(nrow(train_train)*rate_M)

#�Č����̂��ߗ����V�[�h���Œ�
set.seed(17)

#�o�M���O(���f���͑S�Č����)
auc<-NULL
tree_tmp<-as.list(NULL)
for(i in 1:L){
  
  #sample(�x�N�g��, �����_���Ɏ擾�����, �������o�̗L��)
  row<-sample(1:nrow(train_train), M, replace=TRUE)
  
  #�������o�ɂ��\�z�f�[�^
  train_tmp<-train_train[row,]
  
  #���W�X�e�B�b�N��A
  # logi_model<- glm(
  #   y~.,    #�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
  #   data=train_tmp,             #�w�K�f�[�^
  #   family=binomial(link="logit") #���W�X�e�B�b�N��A���w��
  # ) 
  
  logi_model<- glm(
    y ~ job + marital + education + housing + loan + contact + day + 
      month + poutcome + age2 + balance2 + duration2 + campaign2 + 
      pdays2 + previous2,    #�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
    data=dplyr::select(train_tmp, -id),             #�w�K�f�[�^
    family=binomial(link="logit") #���W�X�e�B�b�N��A���w��
  )
  
  #���؃f�[�^�֓��Ă͂�
  pred_test<-predict(logi_model, newdata=dplyr::select(train_test, -id, -y), type="response")
  #AUC�̌v�Z
  #roc(�ړI�ϐ�(1 or 0), �\������)
  auc_tmp<-roc(train_test$y, pred_test)
  auc <- c(auc, as.numeric(auc_tmp$auc))
  
  #�\�����ʂ��Ƃ��Ă���
  {
    if(i==1){score_tmp<-pred_test}
    else{score_tmp<-data.frame(score_tmp, pred_test)}
  }
  
}

#�\�����ʂ̕���
#apply(�f�[�^, 1, �֐�)�Ńf�[�^���s���Ƃɉ��x�N�g���Ƃ��Ċ֐��ɓK�p
score<-apply(score_tmp, 1, mean)

#AUC�̌v�Z
#roc(�ړI�ϐ�(1 or 0), �\������)
auc_tmp<-roc(train_test$y, score)
auc <- c(auc, as.numeric(auc_tmp$auc))

#���ʂ̊m�F
model<-c(as.character(1:L),"ALL_mean")
dat<-data.frame(model, AUC=auc)

print(dat)

g<-ggplot(dat, aes(x=model, y=AUC)) + geom_bar(stat="identity")
plot(g)

###########


#�f�[�^�Ǎ�
train<-read.csv("C:/study/bank/motodata/train.csv", header=TRUE)
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


###########
##�o�M���O�̎���

#���f���\�z��
L<-10

#rate_M*(train_train�̍s��)�̐������f�[�^�𕜌����o
rate_M<-0.1
M<-as.integer(nrow(train)*rate_M)

#�Č����̂��ߗ����V�[�h���Œ�
set.seed(17)

#�o�M���O(���f���͑S�Č����)
auc<-NULL
tree_tmp<-as.list(NULL)
for(i in 1:L){
  
  #sample(�x�N�g��, �����_���Ɏ擾�����, �������o�̗L��)
  row<-sample(1:nrow(train), M, replace=TRUE)
  
  #�������o�ɂ��\�z�f�[�^
  train_tmp<-train[row,]
  
  #���W�X�e�B�b�N��A
  # logi_model<- glm(
  #   y~.,    #�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
  #   data=train_tmp,             #�w�K�f�[�^
  #   family=binomial(link="logit") #���W�X�e�B�b�N��A���w��
  # ) 
  
  logi_model<- glm(
    y ~ job + marital + education + housing + loan + contact + day + 
      month + poutcome + age2 + balance2 + duration2 + campaign2 + 
      pdays2 + previous2,    #�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
    data=dplyr::select(train_tmp, -id),             #�w�K�f�[�^
    family=binomial(link="logit") #���W�X�e�B�b�N��A���w��
  )
  
  #���؃f�[�^�֓��Ă͂�
  pred_test<-predict(logi_model, newdata=dplyr::select(test, -id), type="response")

  #�\�����ʂ��Ƃ��Ă���
  {
    if(i==1){score_tmp<-pred_test}
    else{score_tmp<-data.frame(score_tmp, pred_test)}
  }
  
}

#�\�����ʂ̕���
#apply(�f�[�^, 1, �֐�)�Ńf�[�^���s���Ƃɉ��x�N�g���Ƃ��Ċ֐��ɓK�p
mean_pred_logi<-apply(score_tmp, 1, mean)

#CSV�o��
submit1<-data.frame(id=test$id, score=mean_pred_logi)
write.table(submit1,
            file="C:/study/bank/submit/submit_train_20171104_mean_pred_logi_1.csv",
            quote=F, sep=",", row.names=F, col.names=F)

###########
