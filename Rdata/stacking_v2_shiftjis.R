##�X�^�b�L���O�̎�����(���e�p)

#�g�p���C�u����
library(dplyr)
library(rpart)
library(pROC)
library(ggplot2)

#�f�[�^�Ǎ�
train<-read.csv("C:/study/bank/motodata/train.csv", header=TRUE)
test<-read.csv("C:/study/bank/motodata/test.csv", header=TRUE)

##�X�^�b�L���O�̎���
#����͌����(rpart)�ƃ��W�X�e�B�b�N��A(glm)�����W�X�e�B�b�N��A(glm)�ŃA���T���u��

#�Č����̂��ߗ����V�[�h���Œ�
set.seed(17)

#�w�K�f�[�^��K�ɃO���[�v����
K<-5
#sample(�x�N�g��, �����_���Ɏ擾�����, �������o�̗L��, �x�N�g���̊e�v�f�����o�����m��)
train$cv_group<-sample(1:K, nrow(train), replace=TRUE, prob=rep(1/K, K))

#�\�z, ���؃f�[�^�\���X�R�A�̏�����
score_train_tree<-NULL
score_train_logi<-NULL
score_test_tree<-NULL
score_test_logi<-NULL
y<-NULL

#�N���X�o���f�[�V����
for(j in 1:K){
  #�\�z, ���؃f�[�^�ɕ�����
  train_tmp<-train %>%
    dplyr::filter(cv_group!=j) %>%
    dplyr::select(-cv_group)
  test_tmp<-train %>%
    dplyr::filter(cv_group==j) %>%
    dplyr::select(-cv_group)
  
  #�\�z�f�[�^�Ń��f���\�z(�����)
  tree_tmp<-rpart(y~., data=train_tmp,
                  maxdepth=10, minbucket=12, cp=0.000008,
                  method="class", parms=list(split="gini"))
  
  #�\�z�f�[�^�Ń��f���\�z(���W�X�e�B�b�N��A)
  logi_tmp<-glm(y~., data=train_tmp, family=binomial(link="logit"))
  
  #���f���\�z�Ɏg�p���Ă��Ȃ��f�[�^�̗\���l�ƖړI�ϐ�
  pred_train_tree<-predict(tree_tmp, test_tmp)[,2]
  pred_train_logi<-predict(logi_tmp, test_tmp, type="response")
  y<-c(y, test_tmp$y)
  
  score_train_tree<-c(score_train_tree, pred_train_tree)
  score_train_logi<-c(score_train_logi, pred_train_logi)
  
  
  #���؃f�[�^�̗\���l
  pred_test_tree<-predict(tree_tmp, test)[,2]
  pred_test_logi<-predict(logi_tmp, test, type="response")
  
  score_test_tree<-cbind(score_test_tree, pred_test_tree)
  score_test_logi<-cbind(score_test_logi, pred_test_logi)
  
}

#�]�v�ȕϐ��폜
train<-train %>%
  dplyr::select(-cv_group)

#���؃f�[�^�̗\���l�̕���
#apply(�f�[�^, 1, �֐�)�ōs���ƂɊ֐���K�p����
score_test_tree<-apply(score_test_tree, 1, mean)
score_test_logi<-apply(score_test_logi, 1, mean)
m_dat_test1<-data.frame(tree=score_test_tree, logi=score_test_logi)


#���^���f���p�ϐ��쐬
m_dat_train<-data.frame(tree=score_train_tree, logi=score_train_logi, y=y)

#���^���f���\�z(����̓��W�X�e�B�b�N��A)
m_logi<-glm(y~., data=m_dat_train, family=binomial(link="logit"))


##���؃f�[�^�K�p1
#���^���f���K�p
pred_test_m_logi1<-predict(m_logi, m_dat_test1, type="response")

#CSV�o��
submit1<-data.frame(id=test$id, score=pred_test_m_logi1)
write.table(submit1,
            file="C:/bank/submit/submit_train_0619_ens_tree_logi_1.csv",
            quote=F, sep=",", row.names=F, col.names=F)



##���؃f�[�^�K�p2
#�\�z�f�[�^�S�̂Ń��f���\�z
tree<-rpart(y~., data=train,
            maxdepth=10, minbucket=12, cp=0.000008,
            method="class", parms=list(split="gini"))
logi<-glm(y~., data=train, family=binomial(link="logit"))

#���؃f�[�^�̗\���l
pred_test_tree<-predict(tree, test)[,2]
pred_test_logi<-predict(logi, test, type="response")


#���^���f���p�ϐ��쐬
m_dat_test2<-data.frame(tree=pred_test_tree, logi=pred_test_logi)

#���^���f���K�p
pred_test_m_logi2<-predict(m_logi, m_dat_test2, type="response")

#CSV�o��
submit2<-data.frame(id=test$id, score=pred_test_m_logi2)
write.table(submit2,
            file="C:/bank/submit/submit_train_0619_ens_tree_logi_2.csv",
            quote=F, sep=",", row.names=F, col.names=F)

