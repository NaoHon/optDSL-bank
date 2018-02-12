#�g�p���C�u����
library(pROC)
library(dplyr)
library(glmnet)
library(caret)

#�f�[�^�Ǎ�
train<-read.csv("C:/bank/motodata/train.csv", header=T)
test<-read.csv("C:/bank/motodata/test.csv", header=T)

#�J�e�S���[�ϐ��𐔒l�ϐ��ɕϊ�
train_v<-as.data.frame(predict(dummyVars(~.,data=train), train))
test_v<-as.data.frame(predict(dummyVars(~.,data=test), test))

#�����l�̏���(0�ɕϊ�)
train_v[is.na(train_v)]<-0
test_v[is.na(test_v)]<-0

#�����ϐ��쐬(�s��ɕϊ�)
lasso_train <-Matrix(as.matrix(dplyr::select(train_v,-id,-y)), sparse=T)
lasso_test  <-Matrix(as.matrix(dplyr::select(test_v,-id)), sparse=T)

#�ړI�ϐ��쐬
y_train <- train_v[,"y"]

###Hold Out
#�\�z�f�[�^�̊���
rate<-0.7

#�\�z�f�[�^��(�����̐؎̂�)
num<-as.integer(nrow(lasso_train)*rate)

#�Č����̂��ߗ����V�[�h���Œ�
set.seed(17)

#sample(�x�N�g��, �����_���Ɏ擾�����, �������o�̗L��)
row<-sample(1:nrow(lasso_train), num, replace=FALSE)

#�\�z�f�[�^
lasso_train_train<-lasso_train[row,]

#���؃f�[�^
lasso_train_test<-lasso_train[-row,]

#�ړI�ϐ��쐬
y_train_train<- as.numeric(train[row, "y"])
y_train_test<- as.numeric(train[-row, "y"])

###Lasso��A���f���\�z(alpha=1��Lasso, alpha=0��Ridge, 0<alpha<1��Elastic Net�ƂȂ�)
set.seed(17)
lasso <- cv.glmnet(lasso_train_train, y=as.numeric(y_train_train), alpha=0,
                   nfold=6, family="binomial", nlambda=100, type.measure="auc")

#���؃f�[�^�֓��Ă͂�
pred <-predict(lasso, newx=lasso_train_test, s="lambda.min", type="response")[,1]

#AUC�m�F
auc<-roc(y_train_test, pred)$auc
auc