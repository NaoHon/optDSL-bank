#?o?M???O?̎��???


#?g?p???C?u????
library(dplyr)
library(rpart)
library(pROC)
library(ggplot2)

##?f?[?^????
#???x?����????̂Ńz?[???h?A?E?g?@?ɂ????\?z?f?[?^?ƌ??f?[?^?ɕ?????
#?f?[?^?Ǎ?
train<-read.csv("C:/study/bank/motodata/train.csv", header=TRUE)

#?\?z?f?[?^?̊???
rate<-0.7

#?\?z?f?[?^??(?????̐؎̂?)
num<-as.integer(nrow(train)*rate)

########?????_???ɍ\?z?f?[?^???擾########
#?Č????̂??ߗ????V?[?h???Œ?
set.seed(17)

#sample(?x?N?g??, ?????_???Ɏ擾??????, ???????o?̗L??)
row<-sample(1:nrow(train), num, replace=FALSE)

#?\?z?f?[?^
train_train<-train[row,]

#???f?[?^
train_test<- train[-row,]

##???r?p?ɕ??ʂɍ\?z
#?????؍쐬
tree_tmp<-rpart(y~., data=dplyr::select(train_train, -id),
                maxdepth=10, minbucket=12, cp=0.000008,
                method="class", parms=list(split="gini"))

#???f?[?^?֓??Ă͂?
pred_test<-predict(tree_tmp, dplyr::select(train_test, -id, -y))[,2]

#AUC?̌v?Z
#roc(?ړI?ϐ?(1 or 0), ?\??????)
auc<-roc(train_test$y, pred_test)
print(auc)


##?o?M???O?̎��?
#?????͑S?Č?????(rpart)???g?p

#???f???\?z????
L<-10

#rate_M*(train_train?̍s??)?̐??????f?[?^?𕜌????o
rate_M<-0.1
M<-as.integer(nrow(train_train)*rate_M)

#?Č????̂??ߗ????V?[?h???Œ?
set.seed(17)

#?o?M???O(???f???͑S?Č?????)
auc<-NULL
tree_tmp<-as.list(NULL)
for(i in 1:L){
  
  #sample(?x?N?g??, ?????_???Ɏ擾??????, ???????o?̗L??)
  row<-sample(1:nrow(train_train), M, replace=TRUE)
  
  #???????o?ɂ????\?z?f?[?^
  train_tmp<-train_train[row,]
  
  #?????؍쐬
  tree_tmp<-rpart(y~., data=dplyr::select(train_tmp, -id),
                  maxdepth=10, minbucket=12, cp=0.000008,
                  method="class", parms=list(split="gini"))
  
  #???f?[?^?֓??Ă͂?
  pred_test<-predict(tree_tmp, dplyr::select(train_test, -id, -y))[,2]
  
  #AUC?̌v?Z
  #roc(?ړI?ϐ?(1 or 0), ?\??????)
  auc_tmp<-roc(train_test$y, pred_test)
  auc <- c(auc, as.numeric(auc_tmp$auc))
  
  #?\?????ʂ??Ƃ��Ă???
{
    if(i==1){score_tmp<-pred_test}
    else{score_tmp<-data.frame(score_tmp, pred_test)}
  }

}

#?\?????ʂ̕???
#apply(?f?[?^, 1, ?֐?)?Ńf?[?^???s???Ƃɉ??x?N?g???Ƃ??Ċ֐??ɓK?p
score<-apply(score_tmp, 1, mean)

#AUC?̌v?Z
#roc(?ړI?ϐ?(1 or 0), ?\??????)
auc_tmp<-roc(train_test$y, score)
auc <- c(auc, as.numeric(auc_tmp$auc))

#???ʂ̊m?F
model<-c(as.character(1:L),"ALL_mean")
dat<-data.frame(model, AUC=auc)

print(dat)

g<-ggplot(dat, aes(x=model, y=AUC)) + geom_bar(stat="identity")
plot(g)
