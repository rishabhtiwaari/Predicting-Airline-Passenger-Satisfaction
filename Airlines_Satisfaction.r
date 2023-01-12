
#====================================================================
# STA Group Project
#
# Dataset: Airlines Satisfaction
#
#====================================================================


# Directory
dir = 'C:/Users/NICOLAY/OneDrive - The University of Texas at Austin/AA_MSBA_Texas_McCombs/01_Courses/01_Summer/03_STA380_Intro_to_Machine_Learning_69184/05_Homeworks/01_STA_GP_ML/01_data/'

# Load data
train <- read.csv(paste0(dir,"train.csv"))
test <- read.csv(paste0(dir,"test.csv"))
#View(train)
#View(test)
#attach(train)

#---------------------------------------------------
# Dependent Variable (Binary) - Transformation
#---------------------------------------------------
#Train
xtabs( ~ train$satisfaction, train)
xtabs( ~ train$satisfaction, train)/nrow(train)

#transformation
train$satisfied[train$satisfaction=='neutral or dissatisfied'] = 0
train$satisfied[train$satisfaction=='satisfied'] = 1

train$satisfied <- as.factor(train$satisfied)

#Test
xtabs( ~ test$satisfaction, test)
xtabs( ~ test$satisfaction, test)/nrow(test)

#transformation
test$satisfied[test$satisfaction=='neutral or dissatisfied'] = 0
test$satisfied[test$satisfaction=='satisfied'] = 1

test$satisfied <- as.factor(test$satisfied)


#---------------------------------------------------
# Target distribution
#---------------------------------------------------
dta = c('Train',
        'Test',
        nrow(train),
        nrow(test),
        xtabs( ~ train$satisfaction, train)[2],
        xtabs( ~ test$satisfaction, test)[2],
        xtabs( ~ train$satisfaction, train)[1],
        xtabs( ~ test$satisfaction, test)[1],
        (xtabs( ~ train$satisfaction, train)/nrow(train))[2],
        (xtabs( ~ test$satisfaction, test)/nrow(test))[2] )

dist_satisfied = data.frame(matrix(dta,nrow = 2,ncol = 5))
colnames(dist_satisfied) = c('Sample','Actual','Satisfied=1','Satisfied=0','%Satisfied=1')

# Barplot: % Satisfied customers
bp = barplot(as.double(dist_satisfied$`%Satisfied=1`), 
        main = '%Satisfied customers', names.arg = c('Train','Test'),
        xlab = 'Sample', ylab = '%', ylim = c(0, .5))
text(bp, 0, round(as.double(dist_satisfied$`%Satisfied=1`), 3),cex=1,pos=3) 

# View Target distribution
dist_satisfied

#View(dist_satisfied)
#clipr::write_clip(dist_satisfied)


#---------------------------------------------------
# Transform Variables: Categorical to Numerical
#---------------------------------------------------

#Gender
dist_Gender = xtabs( ~ train$Gender, train)/nrow(train)
train$Gender_N[train$Gender=='Female'] = dist_Gender[1]
train$Gender_N[train$Gender=='Male'] = dist_Gender[2]

#Customer.Type
dist_Customer.Type = xtabs( ~ train$Customer.Type, train)/nrow(train)
train$Customer.Type_N[train$Customer.Type=='disloyal Customer'] = dist_Customer.Type[1]
train$Customer.Type_N[train$Customer.Type=='Loyal Customer'] = dist_Customer.Type[2]

#Type.of.Travel
dist_Type.of.Travel = xtabs( ~ train$Type.of.Travel, train)/nrow(train)
train$Type.of.Travel_N[train$Type.of.Travel=='Business travel'] = dist_Type.of.Travel[1]
train$Type.of.Travel_N[train$Type.of.Travel=='Personal Travel'] = dist_Type.of.Travel[2]

#Class
dist_Class = xtabs( ~ train$Class, train)/nrow(train)
train$Class_N[train$Class=='Business'] = dist_Class[1]
train$Class_N[train$Class=='Eco'] = dist_Class[2]
train$Class_N[train$Class=='Eco Plus'] = dist_Class[3]


# Replicating in Test (with Train results)
#-------------------------------------------
#Gender
test$Gender_N[test$Gender=='Female'] = dist_Gender[1]
test$Gender_N[test$Gender=='Male'] = dist_Gender[2]

#Customer.Type
test$Customer.Type_N[test$Customer.Type=='disloyal Customer'] = dist_Customer.Type[1]
test$Customer.Type_N[test$Customer.Type=='Loyal Customer'] = dist_Customer.Type[2]

#Type.of.Travel
test$Type.of.Travel_N[test$Type.of.Travel=='Business travel'] = dist_Type.of.Travel[1]
test$Type.of.Travel_N[test$Type.of.Travel=='Personal Travel'] = dist_Type.of.Travel[2]

#Class
test$Class_N[test$Class=='Business'] = dist_Class[1]
test$Class_N[test$Class=='Eco'] = dist_Class[2]
test$Class_N[test$Class=='Eco Plus'] = dist_Class[3]


#################################################################################

#------------------------------------------------------------
#------------------------------------------------------------

# List of Predictors (X Variables)
#--------------------------------------
c_x_var_names = names(train[,-c(1,2,3,4,6,7,25,26)])
c_x_var_names

#------------------------------------------------------------
#------------------------------------------------------------
# Function to plot counting graphs

library(ggplot2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#------------------------------------------------------------
#------------------------------------------------------------

#------------------------------------------------------------
# 1st group
group_var_names_1 = c("Age","Flight.Distance","Inflight.wifi.service",
                      "Departure.Arrival.time.convenient","Ease.of.Online.booking","Gate.location",
                      "Food.and.drink","Online.boarding","Seat.comfort")

group_var_names = group_var_names_1

myplots <- list()  # new empty list
for(i in 1:ncol(train[group_var_names])){
  col <- group_var_names[i]
  if (typeof(train[[group_var_names[i]]]) == "character"){
    ggp <- ggplot(train, aes_string(x = col)) +
      geom_bar(fill = "cornflowerblue", color = "black")
  } else {
    ggp <- ggplot(train, aes_string(x = col)) +
      geom_histogram(bins = 30, fill = "cornflowerblue", color = "black")
  }
  myplots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = myplots, cols = 3)
#------------------------------------------------------------

#------------------------------------------------------------
# 2nd group
group_var_names_2 = c("Inflight.entertainment","On.board.service","Leg.room.service",
                      "Baggage.handling","Checkin.service","Inflight.service",
                      "Cleanliness","Departure.Delay.in.Minutes","Arrival.Delay.in.Minutes")

group_var_names = group_var_names_2

myplots <- list()  # new empty list
for(i in 1:ncol(train[group_var_names])){
  col <- group_var_names[i]
  if (typeof(train[[group_var_names[i]]]) == "character"){
    ggp <- ggplot(train, aes_string(x = col)) +
      geom_bar(fill = "cornflowerblue", color = "black")
  } else {
    ggp <- ggplot(train, aes_string(x = col)) +
      geom_histogram(bins = 30, fill = "cornflowerblue", color = "black")
  }
  myplots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = myplots, cols = 3)
#------------------------------------------------------------

#------------------------------------------------------------
# 3rd group
group_var_names_3 = c("Gender","Customer.Type",
                      "Type.of.Travel","Class")

group_var_names = group_var_names_3

myplots <- list()  # new empty list
for(i in 1:ncol(train[group_var_names])){
  col <- group_var_names[i]
  if (typeof(train[[group_var_names[i]]]) == "character"){
    ggp <- ggplot(train, aes_string(x = col)) +
      geom_bar(fill = "cornflowerblue", color = "black")
  } else {
    ggp <- ggplot(train, aes_string(x = col)) +
      geom_histogram(bins = 30, fill = "cornflowerblue", color = "black")
  }
  myplots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = myplots, cols = 2)
#------------------------------------------------------------


#------------------------------------------------------------
# Univariate Analysis
#------------------------------------------------------------

stats = data.frame(matrix(nrow=22,ncol=12))
colnames(stats) = c('Variable','NA','Pct_NA','Zeros','Pct_Zeros',
                    'Min','p01','p25','p50','p75','p99','Max')
  
# Missing Values (NA)
for(i in 1:length(c_x_var_names)){
  stats[i,1] = c_x_var_names[i]
  stats[i,2] = sum(is.na(train[[c_x_var_names[i]]])|is.null(train[[c_x_var_names[i]]]))
  stats[i,3] = sum(is.na(train[[c_x_var_names[i]]])|is.null(train[[c_x_var_names[i]]]))/nrow(train)
  stats[i,4] = sum(train[[c_x_var_names[i]]]==0, na.rm=T)
  stats[i,5] = sum(train[[c_x_var_names[i]]]==0, na.rm=T)/nrow(train)
  stats[i,6] = min(train[[c_x_var_names[i]]], na.rm=T)
  stats[i,7] = quantile(train[[c_x_var_names[i]]], c(.01), na.rm=T)
  stats[i,8] = quantile(train[[c_x_var_names[i]]], c(.25), na.rm=T)
  stats[i,9] = quantile(train[[c_x_var_names[i]]], c(.50), na.rm=T)
  stats[i,10] = quantile(train[[c_x_var_names[i]]], c(.75), na.rm=T)
  stats[i,11] = quantile(train[[c_x_var_names[i]]], c(.99), na.rm=T)
  stats[i,12] = max(train[[c_x_var_names[i]]], na.rm=T)
}

#View(stats)
stats

#clipr::write_clip(stats)


#------------------------------------------------------------
# Bivariate Analysis
#------------------------------------------------------------
#install.packages('pROC')

#calculate AUC
library(pROC)

# Create empty table
gini_bivar_table = data.frame(variable=character(),gini_bivar=double())

# AUC*2 - 1
for(i in 1:length(c_x_var_names)){
  gini = auc(train$satisfied,get(c_x_var_names[i], train))*2-1
  x = c(c_x_var_names[i],as.double(gini))
  gini_bivar_table[i,] = x
}

#Convert gini value from character to double
gini_bivar_table[,2] = as.double(gini_bivar_table[,2])


# Reordering by gini Bivar
ind = order(gini_bivar_table[,2], decreasing = TRUE)
gini_bivar_table =gini_bivar_table[ind,]
rownames(gini_bivar_table) <- 1:nrow(gini_bivar_table)


var_legend = gini_bivar_table$variable[1:10]
for (i in 1:length(var_legend)){
  var_legend[i] = paste(i,var_legend[i])
}

# Plotting Bivariate Analysis
plot(gini_bivar_table$gini_bivar,axes=F,pch=16,col='red',ylab='%Gini',xlab = '',main = 'Bivariate relationship - Gini Index')
axis(1,labels=gini_bivar_table$variable,at=1:length(c_x_var_names),las=1)
axis(2)
for(i in 1:length(c_x_var_names)) lines(c(i,i),c(0,gini_bivar_table$gini_bivar[i]),lwd=3,col='blue')
legend("topright", inset=.02, var_legend, cex=0.8)

#View(gini_bivar_table)
gini_bivar_table

#clipr::write_clip(gini_bivar_table)


#################################################################################

# Drop missing values
train2 <- na.omit(train)

#################################################################################

#------------------------------------------------------------
# Machine Learning Models
#------------------------------------------------------------

#-----------------------------------------
# Tree
#-----------------------------------------
library(tree)


#first get a big tree using a small value of mindev
temp = tree(satisfied ~
            Age + Flight.Distance +
            Inflight.wifi.service + Departure.Arrival.time.convenient +
            Ease.of.Online.booking + Gate.location +
            Food.and.drink + Online.boarding +
            Seat.comfort + Inflight.entertainment +
            On.board.service + Leg.room.service +
            Baggage.handling + Checkin.service +
            Inflight.service + Cleanliness +
            Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes +
            Gender_N + Customer.Type_N +
            Type.of.Travel_N + Class_N
            ,
            data=train2,
            mindev=.0001)
cat('first big tree size: \n')
print(length(unique(temp$where)))


# Let iterate by Number of final leaves (when pruning)
#-------------------------------------------------------------
misclass_iter = data.frame(matrix(ncol=4))
colnames(misclass_iter) = c('Leaves','MisClass_Error','AUC','Gini')

for(i in 2:length(unique(temp$where))){
  #then prune it down to one with x leaves
  train.tree=prune.tree(temp,best=i)
  cat('pruned tree size: \n')
  print(length(unique(train.tree$where)))
  
  #get testing fitted values
  test.fit = predict(train.tree, newdata = test) 
  
  misclass_xtab = table(test$satisfied,ifelse(test.fit[,2]>0.5,1,0))
  misclass_error = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
  
  auc = auc(test$satisfied,test.fit[,2])
  
  x = c(i,misclass_error, auc, auc*2-1)
  misclass_iter[i-1,] = x
}

misclass_iter_tree = misclass_iter
# View Misclassification table
View(misclass_iter)

# Output of Misclasification Error Table
write.csv(misclass_iter_tree,paste0(dir,"/","01_MisClassification_Error_DT.csv"))

# Plot MisClassification Error
plot(misclass_iter_tree[,2],main = 'Misclassification Error (Tree)', xlab = '# of Leaves', ylab = '% of Misclassification (t=0.5)')


# # Plot Gini Index
# plot(misclass_iter_tree[,4],main = 'Gini Index', xlab = '# of Leaves', ylab = 'Gini')


#-----------------------------------------
# Variable importance - Tree
#-----------------------------------------
library(rpart)
library(rpart.plot)

# For now, we are choosing leaves = 50
big.tree = rpart(satisfied ~
              Age + Flight.Distance +
              Inflight.wifi.service + Departure.Arrival.time.convenient +
              Ease.of.Online.booking + Gate.location +
              Food.and.drink + Online.boarding +
              Seat.comfort + Inflight.entertainment +
              On.board.service + Leg.room.service +
              Baggage.handling + Checkin.service +
              Inflight.service + Cleanliness +
              Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes +
              Gender_N + Customer.Type_N +
              Type.of.Travel_N + Class_N
            ,
            data=train2,
            method = 'class',
            cp=-1,
            minbucket = round(nrow(train)*0.01,0))
nbig = length(unique(big.tree$where))
cat('size of big tree: ',nbig,'\n')
#--------------------------------------------------
#look at cross-validation
par(mfrow=c(1,1))
plotcp(big.tree)

rpart.plot(big.tree,extra=2)

#Prunning the tree (Note: As we can note, we have a corner solution (no optimal!))
# For now, we are choosing cp=.00057
#ptree = prune(big.tree,cp=.00057)
#nptree = length(unique(ptree$where))
#cat('size of prune tree: ',nptree,'\n')
ptree = big.tree

# Variable Importance
var_Imp = ptree$variable.importance


# names of top variables for legend
var_legend = names(ptree$variable.importance)[1:10]
for (i in 1:length(var_legend)){
  var_legend[i] = paste(i,var_legend[i])
}

# Plotting Variable Importance
plot(var_Imp,axes=F,pch=16,col='red',ylab='Relative Importance',xlab = '',main = 'Variable Importance - Tree')
axis(1,names(ptree$variable.importance),at=1:length(var_Imp),las=1)
axis(2)
for(i in 1:length(var_Imp)) lines(c(i,i),c(0,ptree$variable.importance[i]),lwd=4,col='blue')
legend("topright", inset=.02, var_legend, cex=0.8)


# Prediction error and AUC/Gini

#get training fitted values
train.fit = predict(ptree, type = 'prob')

misclass_xtab = table(train2$satisfied,ifelse(train.fit[,2]>0.5,1,0))
misclass_error_train = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
misclass_error_train

auc_train = auc(train2$satisfied,train.fit[,2])
gini_train = auc_train*2-1
auc_train
gini_train


#get testing fitted values
test.fit = predict(ptree, type = 'prob', newdata = test)

misclass_xtab = table(test$satisfied,ifelse(test.fit[,2]>0.5,1,0))
misclass_error_test = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
misclass_error_test

auc_test = auc(test$satisfied,test.fit[,2])
gini_test = auc*2-1
auc_test
gini_test

#--------------------------------------------
# Comparison table
compare_table = data.frame(matrix(ncol=7))
colnames(compare_table) = c('Model','Accuracy_Train','AUC_Train','Gini_Train',
                            'Accuracy_Test','AUC_Test','Gini_Test')

compare_table[1,1] = 'Tree'
compare_table[1,2] = 1 - misclass_error_train
compare_table[1,3] = auc_train
compare_table[1,4] = gini_train
compare_table[1,5] = 1 - misclass_error_test
compare_table[1,6] = auc_test
compare_table[1,7] = gini_test
#--------------------------------------------


#################################################################################

#-----------------------------------------
# Random Forest
#-----------------------------------------
#install.packages('randomForest')

library(randomForest)


#--------------------------------------------------
#get rf fits for different number of trees
#note: to get this to work I had to use maxnodes parameter of randomForest!!!
set.seed(2022)
n = nrow(train)

ntreev = c(50,100,150,200,250,300,350,400,450,500)
nset = length(ntreev)
fmat = matrix(0,n,nset)
mnod = c(10,20,30,40,50,60,70,80,90,100)

misclass_iter = data.frame(matrix(ncol=5))
colnames(misclass_iter) = c('B','MaxNodes','MisClass_Error','AUC','Gini')


for(i in 1:nset) {
  for(mn in 1:length(mnod)){
    cat('doing Airlines rf: ',i,mn," -  B =",ntreev[i]," - MN =",mnod[mn],'\n')
    rffit = randomForest(satisfied ~
                           Age + Flight.Distance +
                           Inflight.wifi.service + Departure.Arrival.time.convenient +
                           Ease.of.Online.booking + Gate.location +
                           Food.and.drink + Online.boarding +
                           Seat.comfort + Inflight.entertainment +
                           On.board.service + Leg.room.service +
                           Baggage.handling + Checkin.service +
                           Inflight.service + Cleanliness +
                           Departure.Delay.in.Minutes + #Arrival.Delay.in.Minutes +
                           Gender_N + Customer.Type_N +
                           Type.of.Travel_N + Class_N
                         ,data=train,ntree=ntreev[i],maxnodes=mnod[mn])
    #fmat[,i] = predict(rffit)
    
    test.fit = predict(rffit, newdata = test) 
    
    misclass_xtab = table(test$satisfied,test.fit)
    misclass_error = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
    
    #auc = auc(test$satisfied,test.fit[,2])
    auc = 0
    
    x = c(ntreev[i],mnod[mn],misclass_error, auc, auc*2-1)
    misclass_iter[5*(i-1)+mn,] = x
  }
}


#View(misclass_iter)
misclass_iter

# Output of Misclasification Error Table
write.csv(misclass_iter,paste0(dir,"/","02_MisClassification_Error_RF.csv"))

# Plot MisClassification Error
plot(misclass_iter[order(misclass_iter$NVars,misclass_iter$B),][,3],main = 'Misclassification Error (RF)', xlab = 'Max Nodes * B', ylab = '% of Misclassification (t=0.5)')


#-----------------------------------------
## Finding Best Parameters
#-----------------------------------------
library(tidyverse)

# Find B parameter
dta_group_B = data.frame(
  misclass_iter %>%
    group_by(B) %>%
    summarise(avg = mean(MisClass_Error)
    ))

B_param = dta_group_B[which.min(dta_group_B[,2]),1]

# Find MaxNodes parameter
dta_group_MN = data.frame(
  misclass_iter %>%
    group_by(MaxNodes) %>%
    summarise(avg = mean(MisClass_Error)
    ))

MN_param = dta_group_MN[which.min(dta_group_MN[,2]),1]

#-----------------------------------------


#-----------------------------------------
# Variable importance - RF
#-----------------------------------------
rm(rffit)

# For now, we are choosing B = 120 and Max Nodes = 40
rffit = randomForest(satisfied ~
                       Age + Flight.Distance +
                       Inflight.wifi.service + Departure.Arrival.time.convenient +
                       Ease.of.Online.booking + Gate.location +
                       Food.and.drink + Online.boarding +
                       Seat.comfort + Inflight.entertainment +
                       On.board.service + Leg.room.service +
                       Baggage.handling + Checkin.service +
                       Inflight.service + Cleanliness +
                       Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes +
                       Gender_N + Customer.Type_N +
                       Type.of.Travel_N + Class_N
                     ,data=train2,ntree=B_param,maxnodes=MN_param)

# Fitted values in train
train_rf.fit = predict(rffit, type = 'prob')

# Fitted values in test
test_rf.fit = predict(rffit, type = 'prob', newdata = test)

#get training fitted values
misclass_xtab = table(train2$satisfied,ifelse(train_rf.fit[,2]>0.5,1,0))
misclass_error_rf_train = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
misclass_error_rf_train

auc_train = auc(train2$satisfied,train_rf.fit[,2])
gini_train = auc_train*2-1
auc_train
gini_train


#get testing fitted values
misclass_xtab = table(test$satisfied,ifelse(test_rf.fit[,2]>0.5,1,0))
misclass_error_rf_test = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
misclass_error_rf_test

auc_test = auc(test$satisfied,test_rf.fit[,2])
gini_test = auc*2-1
auc_test
gini_test

#Variable Importance Graph
varImpPlot(rffit)

#importance(rffit)

#--------------------------------------------
# Comparison table
compare_table[2,1] = 'Random Forest'
compare_table[2,2] = 1 - misclass_error_rf_train
compare_table[2,3] = auc_train
compare_table[2,4] = gini_train
compare_table[2,5] = 1 - misclass_error_rf_test
compare_table[2,6] = auc_test
compare_table[2,7] = gini_test
#--------------------------------------------


#################################################################################

#-----------------------------------------
# Boosting
#-----------------------------------------
#install.packages('gbm')

library(gbm) #boost package

# Transform Dependent Variable as numeric (double)
train2$satisfied = as.numeric(train2$satisfied)
train2$satisfied = train2$satisfied - 1

#--------------------------------------------------
#fit boosting for various number of trees
set.seed(2022)
n = nrow(train)

ntreev = c(100,200,300,400,500,600,700,800,900,1000)
nset = length(ntreev)
fmat = matrix(0,n,nset)
shr = c(0.05,0.10,0.15,0.20)

misclass_iter = data.frame(matrix(ncol=5))
colnames(misclass_iter) = c('B','Shrinkage','MisClass_Error','AUC','Gini')


for(i in 1:nset) {
  for(s in 1:length(shr)){
    cat('doing Airlines boost: B=',ntreev[i],' - Shrinkage=',shr[s],'\n')
    boostfit = gbm(satisfied ~
                     Age + Flight.Distance +
                     Inflight.wifi.service + Departure.Arrival.time.convenient +
                     Ease.of.Online.booking + Gate.location +
                     Food.and.drink + Online.boarding +
                     Seat.comfort + Inflight.entertainment +
                     On.board.service + Leg.room.service +
                     Baggage.handling + Checkin.service +
                     Inflight.service + Cleanliness +
                     Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes +
                     Gender_N + Customer.Type_N +
                     Type.of.Travel_N + Class_N
                   ,data=train2,distribution='bernoulli',
                   interaction.depth=4,n.trees=ntreev[i],shrinkage=shr[s])
    #train_gbt.fit = predict(boostfit,n.trees=ntreev[i])
    
    test_gbt.fit = predict(boostfit, newdata = test, type = 'response')
    
    #misclass_xtab = table(test$satisfied,test_bt.fit)
    misclass_xtab= table(test$satisfied,ifelse(test_gbt.fit>0.5,1,0))
    misclass_error_gbt_test = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
    misclass_error_gbt_test
    
    auc = auc(test$satisfied,test_gbt.fit)
    
    x = c(ntreev[i],shr[s],misclass_error_gbt_test, auc, auc*2-1)
    misclass_iter[4*(i-1)+s,] = x
  }
}

misclass_iter_gbt = misclass_iter
#View(misclass_iter_gbt)

# Output of Misclasification Error Table
write.csv(misclass_iter_gbt,paste0(dir,"/","03_MisClassification_Error_BT.csv"))

# Plot MisClassification Error
plot(misclass_iter_gbt[,3],main = 'Misclassification Error (GBT)', xlab = 'B * Shrinkage', ylab = '% of Misclassification (t=0.5)')

#[order(misclass_iter$Shrinkage,misclass_iter$B),]

#-----------------------------------------
## Finding Best Parameters
#-----------------------------------------
library(tidyverse)

# Find B parameter
dta_group_B = data.frame(
  misclass_iter %>%
    group_by(B) %>%
    summarise(avg = mean(MisClass_Error)
    ))

B_param = dta_group_B[which.min(dta_group_B[,2]),1]

# Find MaxNodes parameter
dta_group_Sk = data.frame(
  misclass_iter %>%
    group_by(Shrinkage) %>%
    summarise(avg = mean(MisClass_Error)
    ))

Sk_param = dta_group_Sk[which.min(dta_group_Sk[,2]),1]

#-----------------------------------------


#--------------------------------------------------------------------
# Variable Importance
#--------------------------------------------------------------------

# Gradient Boosting Trees
boostfit = gbm(satisfied ~
                 Age + Flight.Distance +
                 Inflight.wifi.service + Departure.Arrival.time.convenient +
                 Ease.of.Online.booking + Gate.location +
                 Food.and.drink + Online.boarding +
                 Seat.comfort + Inflight.entertainment +
                 On.board.service + Leg.room.service +
                 Baggage.handling + Checkin.service +
                 Inflight.service + Cleanliness +
                 Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes +
                 Gender_N + Customer.Type_N +
                 Type.of.Travel_N + Class_N
               ,data=train2, distribution ='bernoulli',
               interaction.depth=4, n.trees=B_param, shrinkage=Sk_param)


#Variable Importance
par(mfrow=c(1,1))
p=length(c_x_var_names)
vsum=summary(boostfit,plotit=FALSE) #this will have the variable importance info
row.names(vsum)=NULL #drop varable names from rows.

var_legend = vsum$var[1:10]
for (i in 1:length(var_legend)){
  var_legend[i] = paste(i,var_legend[i])
}

#plot variable importance
plot(vsum$rel.inf,axes=F,pch=16,col='red',xlab='',ylab = 'Relative Importance', main = 'Variable Importance - BT')
axis(1,labels=vsum$var,at=1:p)
axis(2)
for(i in 1:p) lines(c(i,i),c(0,vsum$rel.inf[i]),lwd=4,col='blue')
legend("topright", inset=.02, var_legend, cex=0.8)

# View Variable Importance table
vsum

#--------------------------------------------------------
# Fitted values in train
train_gbt.fit = predict(boostfit, type = 'response')

# Fitted values in test
test_gbt.fit = predict(boostfit, type = 'response', newdata = test)

#get training fitted values
misclass_xtab = table(train2$satisfied,ifelse(train_gbt.fit>0.5,1,0))
misclass_error_gbt_train = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
misclass_error_gbt_train

auc_train = auc(train2$satisfied,train_gbt.fit)
gini_train = auc_train*2-1
auc_train
gini_train


#get testing fitted values
misclass_xtab = table(test$satisfied,ifelse(test_gbt.fit>0.5,1,0))
misclass_error_gbt_test = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
misclass_error_gbt_test

auc_test = auc(test$satisfied,test_rf.fit)
gini_test = auc*2-1
auc_test
gini_test

#--------------------------------------------
# Comparison table
compare_table[3,1] = 'Boosting Trees'
compare_table[3,2] = 1 - misclass_error_gbt_train
compare_table[3,3] = auc_train
compare_table[3,4] = gini_train
compare_table[3,5] = 1 - misclass_error_gbt_test
compare_table[3,6] = auc_test
compare_table[3,7] = gini_test
#--------------------------------------------


#################################################################################

#-----------------------------------------
# Logistic Regression
#-----------------------------------------

# Drop missing values
train2 <- na.omit(train)


# Ligistic Regression
glm.fit = glm(satisfied ~
                Age + Flight.Distance +
                Inflight.wifi.service + Departure.Arrival.time.convenient +
                Ease.of.Online.booking + Gate.location +
                Food.and.drink + Online.boarding +
                Seat.comfort + Inflight.entertainment +
                On.board.service + Leg.room.service +
                Baggage.handling + Checkin.service +
                Inflight.service + Cleanliness +
                Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes +
                Gender_N + Customer.Type_N +
                Type.of.Travel_N + Class_N
                ,
                data=train2,
                family = 'binomial')

summary(glm.fit)

#get training fitted values
train.fit = predict(glm.fit, type = 'response') 

misclass_xtab = table(train2$satisfied,ifelse(train.fit>0.5,1,0))
misclass_lr_error = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
misclass_lr_error

auc_train = auc(train2$satisfied,train.fit)
gini_train = auc_train*2-1
auc_train
gini_train


#get testing fitted values
test.fit = predict(glm.fit, type = 'response', newdata = test) 

misclass_xtab = table(test$satisfied,ifelse(test.fit>0.5,1,0))
misclass_error_test = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
misclass_error_test

auc_test = auc(test$satisfied,test.fit)
gini_test = auc_test*2-1
auc_test
gini_test

#--------------------------------------------
# Comparison table
compare_table[4,1] = 'Logistic Regression'
compare_table[4,2] = 1 - misclass_error_lr_train
compare_table[4,3] = auc_train
compare_table[4,4] = gini_train
compare_table[4,5] = 1 - misclass_error_lr_test
compare_table[4,6] = auc_test
compare_table[4,7] = gini_test
#--------------------------------------------


#----------------------------------------------------------------------------


# # Variable Droping
# glm.fit = glm(satisfied ~
#                 Age +
#                 Inflight.wifi.service + Departure.Arrival.time.convenient +
#                 Ease.of.Online.booking +
#                 Online.boarding +
#                 Seat.comfort + 
#                 On.board.service + Leg.room.service +
#                 Baggage.handling + Checkin.service +
#                 Inflight.service + Cleanliness +
#                 Arrival.Delay.in.Minutes +
#                 Customer.Type_N +
#                 Type.of.Travel_N + Class_N
#               ,
#               data=train2,
#               family = 'binomial')
# 
# summary(glm.fit)
# 
# #get testing fitted values
# train.fit = predict(glm.fit) 
# 
# misclass_xtab = table(train2$satisfied,ifelse(train.fit>0.5,1,0))
# misclass_error_lr_train = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
# misclass_error_lr_train
# 
# auc = auc(train2$satisfied,train.fit)
# auc
# auc*2-1
# 
# 
# #get testing fitted values
# test.fit = predict(glm.fit, newdata = test) 
# 
# misclass_xtab = table(test$satisfied,ifelse(test.fit>0.5,1,0))
# misclass_error_lr_test = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
# misclass_error_lr_test
# 
# auc = auc(test$satisfied,test.fit)
# auc
# auc*2-1




#----------------------------------------------------------------------------
#----------------------------------------------------------------------------


#------------------------------------------------------------
# Transform Variables to Woe's (Weight of Evidence)
#------------------------------------------------------------

lst_woe_vars = c_x_var_names

#---------------------------------------------------------------

library(tidyverse)

for(k in 1:22){
  #k = 1
  print(lst_woe_vars[k])
  #---------------------------------------------------------------
  
  woe.tree = rpart(satisfied ~ get(lst_woe_vars[k]), data=train2, method = 'class', 
                   minbucket = round(nrow(train2)*0.05,0),cp=-1,maxdepth = 5)
  
  #nptree = length(unique(woe.tree$where))
  #print(nptree)
  #rpart.plot(woe.tree,extra=2)
  
  name_pred = paste0('pred_',lst_woe_vars[k])
  train2[[name_pred]] = predict(woe.tree)[,2]
  #View(train2)
  #table(train2[[lst_woe_vars[k]]],train2$satisfied)#/nrow(train2)

  #---------------------------------------------------------------
  
  tmp_table =
  train2 %>%
    group_by(get(name_pred)) %>% 
    summarise(min = min(get(lst_woe_vars[k]), na.rm=TRUE),
              max = max(get(lst_woe_vars[k]), na.rm=TRUE),
              total = n(),
              event = sum(satisfied==1),
              nonevent = sum(satisfied==0),
              pct_event = sum(satisfied==1)/sum(train2$satisfied==1),
              pct_nonevent = sum(satisfied==0)/sum(train2$satisfied==0),
              woe = log( (sum(satisfied==1)/sum(train2$satisfied==1)) / (sum(satisfied==0)/sum(train2$satisfied==0)) )
              ) %>%
    arrange(min)
  
  tmp_table = data.frame(tmp_table)
  tmp_table
  
  n_cuts = nrow(tmp_table)-1
  woe_name = paste0('woe_',lst_woe_vars[k]) 
    
  woe = tmp_table[n_cuts+1,9]
  train2[[woe_name]] = woe
  test[[woe_name]] = woe
  
  for(i in (1:n_cuts)){
    cut = (tmp_table[n_cuts-i+1,3]+tmp_table[n_cuts-i+2,2])/2
    woe = tmp_table[n_cuts-i+1,9]
    train2[[woe_name]][train2[[lst_woe_vars[k]]]<=cut] = woe
    test[[woe_name]][test[[lst_woe_vars[k]]]<=cut] = woe
  }

}

#View(train2)
#table(train2[[name_pred]],train2[[woe_name]])


#-------------------------------------------------------------------------------

# Logistic Regression with Woes
glm.fit = glm(satisfied ~
                woe_Age + woe_Flight.Distance +
                woe_Inflight.wifi.service + woe_Departure.Arrival.time.convenient +
                woe_Ease.of.Online.booking + woe_Gate.location +
                woe_Food.and.drink + woe_Online.boarding +
                woe_Seat.comfort + woe_Inflight.entertainment +
                woe_On.board.service + woe_Leg.room.service +
                woe_Baggage.handling + woe_Checkin.service +
                woe_Inflight.service + woe_Cleanliness +
                woe_Departure.Delay.in.Minutes + woe_Arrival.Delay.in.Minutes +
                woe_Gender_N + woe_Customer.Type_N +
                woe_Type.of.Travel_N + woe_Class_N
              ,
              data=train2,
              family = 'binomial')

summary(glm.fit)

#get training fitted values
train.fit = predict(glm.fit) 

misclass_xtab = table(train2$satisfied,ifelse(train.fit>0.5,1,0))
misclass_error_lr_woe_train = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
misclass_error_lr_woe_train

auc_train = auc(train2$satisfied,train.fit)
gini_train = auc_train*2-1
auc_train
gini_train


#get testing fitted values
test.fit = predict(glm.fit, type = 'response', newdata = test) 

misclass_xtab = table(test$satisfied,ifelse(test.fit>0.5,1,0))
misclass_error_lr_woe_test = (misclass_xtab[1,2]+misclass_xtab[2,1])/sum(misclass_xtab)
misclass_error_lr_woe_test

auc_test = auc(test$satisfied,test.fit)
gini_test = auc_test*2-1
auc_test
gini_test

#--------------------------------------------
# Comparison table
compare_table[5,1] = 'Log Reg - woes'
compare_table[5,2] = 1 - misclass_error_lr_woe_train
compare_table[5,3] = auc_train
compare_table[5,4] = gini_train
compare_table[5,5] = 1 - misclass_error_lr_woe_test
compare_table[5,6] = auc_test
compare_table[5,7] = gini_test
#--------------------------------------------


# Output of Misclasification Error Table
write.csv(compare_table,paste0(dir,"/","04_Comparison_Table.csv"))


######################################################################################
######################################################################################










