library(readr)
library(igraph)
library(dplyr)
library(tidyr)
library(readr)
library(igraph)
library(ggplot2)

rm(list = ls())
setwd("C:/Users/Daniel Weinberg/Desktop/MSBA 2020/Fall/Social Network Analysis/Survivor/Final Project")

#import the data
S1 <- read_csv("S1/Season 1 Raw.csv" ,col_names = FALSE)
S2 <- read_csv("S2/Season 2 Raw.csv" ,col_names = FALSE)
S3 <- read_csv("S3/Season 3 Raw.csv" ,col_names = FALSE)
S4 <- read_csv("S4/Season 4 Raw.csv" ,col_names = FALSE)
S5 <- read_csv("S5/Season 5 Raw.csv" ,col_names = FALSE)
S6 <- read_csv("S6/Season 6 Raw.csv" ,col_names = FALSE)
S7 <- read_csv("S7/Season 7 Raw.csv" ,col_names = FALSE)
S8 <- read_csv("S8/Season 8 Raw.csv" ,col_names = FALSE)
S9 <- read_csv("S9/Season 9 Raw.csv" ,col_names = FALSE)
S10 <- read_csv("S10/Season 10 Raw.csv" ,col_names = FALSE)
S11 <- read_csv("S11/Season 11 Raw.csv" ,col_names = FALSE)
S12 <- read_csv("S12/Season 12 Raw.csv" ,col_names = FALSE)
S13 <- read_csv("S13/Season 13 Raw.csv" ,col_names = FALSE)
S14 <- read_csv("S14/Season 14 Raw.csv" ,col_names = FALSE)
S15 <- read_csv("S15/Season 15 Raw.csv" ,col_names = FALSE)
S16 <- read_csv("S16/Season 16 Raw.csv" ,col_names = FALSE)
S17 <- read_csv("S17/Season 17 Raw.csv" ,col_names = FALSE)
S18 <- read_csv("S18/Season 18 Raw.csv" ,col_names = FALSE)
S19 <- read_csv("S19/Season 19 Raw.csv" ,col_names = FALSE)
S20 <- read_csv("S20/Season 20 Raw.csv" ,col_names = FALSE)
S21 <- read_csv("S21/Season 21 Raw.csv" ,col_names = FALSE)
S22 <- read_csv("S22/Season 22 Raw.csv" ,col_names = FALSE)
S23 <- read_csv("S23/Season 23 Raw.csv" ,col_names = FALSE)
S24 <- read_csv("S24/Season 24 Raw.csv" ,col_names = FALSE)
S25 <- read_csv("S25/Season 25 Raw.csv" ,col_names = FALSE)
S26 <- read_csv("S26/Season 26 Raw.csv" ,col_names = FALSE)
S27 <- read_csv("S27/Season 27 Raw.csv" ,col_names = FALSE)
S28 <- read_csv("S28/Season 28 Raw.csv" ,col_names = FALSE)
S29 <- read_csv("S29/Season 29 Raw.csv" ,col_names = FALSE)
S30 <- read_csv("S30/Season 30 Raw.csv" ,col_names = FALSE)
S31 <- read_csv("S31/Season 31 Raw.csv" ,col_names = FALSE)
S32 <- read_csv("S32/Season 32 Raw.csv" ,col_names = FALSE)
S33 <- read_csv("S33/Season 33 Raw.csv" ,col_names = FALSE)
S34 <- read_csv("S34/Season 34 Raw.csv" ,col_names = FALSE)
S35 <- read_csv("S35/Season 35 Raw.csv" ,col_names = FALSE)
S36 <- read_csv("S36/Season 36 Raw.csv" ,col_names = FALSE)
S37 <- read_csv("S37/Season 37 Raw.csv" ,col_names = FALSE)
S38 <- read_csv("S38/Season 38 Raw.csv" ,col_names = FALSE)

#lists of each vote
vote_1=list()
vote_2=list()
vote_3=list()
vote_4=list()
vote_5=list()
vote_6=list()
vote_7=list()
vote_8=list()
vote_9=list()
vote_10=list()
vote_11=list()
vote_12=list()
vote_13=list()
vote_14=list()
vote_15=list()
vote_16=list()
main_list = list(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21,S22,S23,S24,
                 S25,S26,S27,S28,S29,S30,S31,S32,S33,S34,S35,S36,S37,S38)
count=0

for(m in main_list){
  count=count+1
  m=as.data.frame(m)
  row.names(m) = m[,1]
  df=m
  df[ , ] =0
  dim(df)
  X=''
  if(dim(df)[1] > dim(df)[2]){
    diff = dim(df)[1]-dim(df)[2]
    for(i in seq(diff)){
      col_name = paste(X, dim(df)[2] +i ,sep="_")
      df[col_name] <-0
    }
  }else if(dim(df)[1] < dim(df)[2]){
    diff=dim(df)[2]-dim(df)[1]  
    for(i in seq(diff)){
      df=df[ ,-ncol(df)]
    }
  }else{
    df=df
  }
  colnames(df)=row.names(df) 
  
  for(i in 2: (ncol(m))){
    voted_out = na.omit(unique(m[[i]]))
    for(j in seq_along(voted_out)){
      
      t= voted_out[j]
      
      a= m[m[i]==t , ]
      
      q=  na.omit(a$X1)
      
      df[q,q ]= df[q,q]+1  
      diag(df)<-0
        
    }
    
    if(i ==2){
      name1 <- paste("Season", count, sep="")
      vote_1[[name1]] <-df
    } else if(i ==3){
      name2 <- paste("Season", count, sep="")
      vote_2[[name2]] <-df
    } else if(i ==4){
      sEOG <- paste("Season", count, sep="")
      vote_3[[sEOG]] <-df
    } else if(i ==5){
      sEOG <- paste("Season", count, sep="")
      vote_4[[sEOG]] <-df
    } else if(i ==6){
      sEOG <- paste("Season", count, sep="")
      vote_5[[sEOG]] <-df
    } else if(i ==7){
      sEOG <- paste("Season", count, sep="")
      vote_6[[sEOG]] <-df
    } else if(i ==8){
      sEOG <- paste("Season", count, sep="")
      vote_7[[sEOG]] <-df
    } else if(i ==9){
      sEOG <- paste("Season", count, sep="")
      vote_8[[sEOG]] <-df
    } else if(i ==10){
      sEOG <- paste("Season", count, sep="")
      vote_9[[sEOG]] <-df
    } else if(i ==11){
      sEOG <- paste("Season", count, sep="")
      vote_10[[sEOG]] <-df
    } else if(i ==12){
      sEOG <- paste("Season", count, sep="")
      vote_11[[sEOG]] <-df
    } else if(i ==13){
      sEOG <- paste("Season", count, sep="")
      vote_12[[sEOG]] <-df
    } else if(i ==14){
      sEOG <- paste("Season", count, sep="")
      vote_13[[sEOG]] <-df
    } else if(i ==15){
      sEOG <- paste("Season", count, sep="")
      vote_14[[sEOG]] <-df
    } else if(i ==16){
      sEOG <- paste("Season", count, sep="")
      vote_15[[sEOG]] <-df
    } else {
      name <- paste("Season", count, sep="")
      vote_16[[name]] <-df
    }
   

    }
  
}

g1 <- list()
g2 <- list()
g3 <- list()
g4 <- list()
g5 <- list()
g6 <- list()
g7 <- list()
g8 <- list()
g9 <- list()
g10 <- list()
g11 <- list()
g12 <- list()
g13 <- list()
g14 <- list()
g15 <- list()
g16 <- list()

     

# Ep 1
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(g1[[name]])) next
  g1[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_1[[name]]))
}

# Ep 2
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(g2[[name]])) next
  g2[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_2[[name]]))
}

# Ep 3
for (i in 1:38){
  name <- paste("Season", i, sep="")
  g3[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_3[[name]]))
}

# Ep 4
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(g4[[name]])) next
  g4[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_4[[name]]))
}

# Ep 5
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(g5[[name]])) next
  g5[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_5[[name]]))
}

# Ep 6
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(g6[[name]])) next
  g6[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_6[[name]]))
}

# Ep 7
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(g7[[name]])) next
  g7[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_7[[name]]))
}

# Ep 8
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(g8[[name]])) next
  g8[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_8[[name]]))
}

# Ep 9
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(g9[[name]])) next
  g9[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_9[[name]]))
}

# Ep 10
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(g10[[name]])) next
  g10[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_10[[name]]))
}

# Ep 11
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(g11[[name]])) next
  g11[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_11[[name]]))
}

# Ep 12
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(g12[[name]])) next
  g12[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_12[[name]]))
}

# Ep 13
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(g13[[name]])) next
  g13[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_13[[name]]))
}

# Ep 14
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(vote_14[[name]])){
  g14[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_14[[name]]))
  }
}

# Ep 15
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(vote_15[[name]])){
    g15[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_15[[name]]))
  }
}

# Ep 16
for (i in 1:38){
  name <- paste("Season", i, sep="")
  if( !is.null(vote_16[[name]])){
    g16[[name]] <- graph_from_adjacency_matrix(as.matrix(vote_16[[name]]))
  }
}

# Stats
stat1 <- list() 
stat2 <- list()
stat3 <- list()
stat4 <- list()
stat5 <- list()
stat6 <- list()
stat7 <- list()
stat8 <- list()
stat9 <- list()
stat10 <- list()
stat11 <- list()
stat12 <- list()
stat13 <- list()
stat14 <- list()
stat15 <- list()
stat16 <- list()


#function to create statistics

statistics = function(graph,stat){
  for (i in 1:38){
    name <- paste("Season", i, sep="")
    if(is.null(graph[[name]])) next
    temp_g <- graph[[name]]
    players <- get.data.frame(temp_g, what="vertices")
    for (j in 1:nrow(players)){
      players$perc[j] <- (1 - j/nrow(players))
    }
    players$closeness <- closeness(temp_g)
    players$betweenness <- betweenness(temp_g)
    players$degree <- degree(temp_g, mode="in")
    players$eigencentrality <- eigen_centrality(temp_g)$vector
    players$pagerank <- page.rank(temp_g)$vector
    #players$similarity <- similarity(temp_g, mode = 'all', method = "jaccard")
                                                                                                          
    stat[[name]] <- players
  }
  return(stat)
}


#statistics for each vote and season wise
stat1 = statistics(g1,stat1)
stat2 = statistics(g2,stat2)
stat3 = statistics(g3,stat3)
stat4 = statistics(g4,stat4)
stat5 = statistics(g5,stat5 )
stat6 = statistics(g6,stat6)
stat7 = statistics(g7,stat7)
stat8 = statistics(g8,stat8)
stat9 = statistics(g9,stat9)
stat10 = statistics(g10,stat10)
stat11 = statistics(g11,stat11)
stat12 = statistics(g12,stat12)
stat13 = statistics(g13,stat13)
stat14 = statistics(g14,stat14)
stat15 = statistics(g15,stat15)
stat16 = statistics(g16,stat16)

#function to create a big dataframe vote wise for training dataset
Create_train = function(stat){
  big_df=data.frame(name=character() ,perc=double(),closeness=double(),betweenness=double(), 
                      degree=double(),eigencentrality=double(),pagerank =double())
  for (i in 1:length(stat)-1){
  name <- paste("Season", i, sep="")
  big_df = rbind(stat[[name]], big_df)
  }
  return(big_df)
}

#function to create a test dataframe

Create_test= function(stat){
  i=length(stat)
  name <- paste("Season", i, sep="")
  return(stat[[name]])
}


#############################################################################################PREDICTIONS
MSE=c()

###############vote1 prediction

#create training data set
vote_1_train= Create_train(stat1)

#create test dataset
vote_1_test=  Create_test(stat1)
vote_1_test= vote_1_test[ , -c(1,2)]
row.names(vote_1_test) <-NULL

#Target variable
vote_1_y_hat=  Create_test(stat1)["perc"]
row.names(vote_1_y_hat) <- NULL

#run regression
vote_1_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_1_train) 
summary(vote_1_regression)

#prediction
predict_1 = predict(vote_1_regression,vote_1_test)

#MSE
MSE[1] =sum(((predict_1 - vote_1_y_hat)^2))


#################vote 2 prediction


#create training data set
vote_2_train= Create_train(stat2)

#create test dataset
vote_2_test=  Create_test(stat2)
vote_2_test= vote_2_test[ , -c(1,2)]
row.names(vote_2_test) <-NULL

#Target variable
vote_2_y_hat=  Create_test(stat2)["perc"]
row.names(vote_2_y_hat) <- NULL

#run regression
vote_2_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_2_train) 
summary(vote_2_regression)

#prediction
predict_2 = predict(vote_2_regression,vote_2_test)

#MSE
MSE[2] =sum(((predict_2 - vote_2_y_hat)^2))





#################vote 3 prediction

#create training data set
vote_3_train= Create_train(stat3)

#create test dataset
vote_3_test=  Create_test(stat3)
vote_3_test= vote_3_test[ , -c(1,2)]
row.names(vote_3_test) <-NULL

#Target variable
vote_3_y_hat=  Create_test(stat3)["perc"]
row.names(vote_3_y_hat) <- NULL

#run regression
vote_3_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_3_train) 
summary(vote_3_regression)

#prediction
predict_3 = predict(vote_3_regression,vote_3_test)

#MSE
MSE[3] =sum(((predict_3 - vote_3_y_hat)^2))





#################vote 4 prediction

#create training data set
vote_4_train= Create_train(stat4)

#create test dataset
vote_4_test=  Create_test(stat4)
vote_4_test= vote_4_test[ , -c(1,2)]
row.names(vote_4_test) <-NULL

#Target variable
vote_4_y_hat=  Create_test(stat4)["perc"]
row.names(vote_4_y_hat) <- NULL

#run regression
vote_4_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_4_train) 
summary(vote_4_regression)

#prediction
predict_4 = predict(vote_4_regression,vote_4_test)

#MSE
MSE[4] =sum(((predict_4 - vote_4_y_hat)^2))




#################vote 5 prediction

#create training data set
vote_5_train= Create_train(stat5)

#create test dataset
vote_5_test=  Create_test(stat5)
vote_5_test= vote_5_test[ , -c(1,2)]
row.names(vote_5_test) <-NULL

#Target variable
vote_5_y_hat=  Create_test(stat5)["perc"]
row.names(vote_5_y_hat) <- NULL

#run regression
vote_5_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_5_train) 
summary(vote_5_regression)

#prediction
predict_5 = predict(vote_5_regression,vote_5_test)

#MSE
MSE[5] =sum(((predict_5 - vote_5_y_hat)^2))




#################vote 6 prediction

#create training data set
vote_6_train= Create_train(stat6)

#create test dataset
vote_6_test=  Create_test(stat6)
vote_6_test= vote_6_test[ , -c(1,2)]
row.names(vote_6_test) <-NULL

#Target variable
vote_6_y_hat=  Create_test(stat6)["perc"]
row.names(vote_6_y_hat) <- NULL

#run regression
vote_6_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_6_train) 
summary(vote_6_regression)

#prediction
predict_6 = predict(vote_2_regression,vote_6_test)

#MSE
MSE[6] =sum(((predict_6 - vote_6_y_hat)^2))





#################vote 7 prediction

#create training data set
vote_7_train= Create_train(stat7)

#create test dataset
vote_7_test=  Create_test(stat7)
vote_7_test= vote_7_test[ , -c(1,2)]
row.names(vote_7_test) <-NULL

#Target variable
vote_7_y_hat=  Create_test(stat7)["perc"]
row.names(vote_7_y_hat) <- NULL

#run regression
vote_7_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_7_train) 
summary(vote_7_regression)

#prediction
predict_7 = predict(vote_7_regression,vote_7_test)

#MSE
MSE[7] =sum(((predict_7 - vote_7_y_hat)^2))





#################vote 8 prediction

#create training data set
vote_8_train= Create_train(stat8)

#create test dataset
vote_8_test=  Create_test(stat8)
vote_8_test= vote_8_test[ , -c(1,2)]
row.names(vote_8_test) <-NULL

#Target variable
vote_8_y_hat=  Create_test(stat8)["perc"]
row.names(vote_8_y_hat) <- NULL

#run regression
vote_8_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_8_train) 
summary(vote_8_regression)

#prediction
predict_8 = predict(vote_8_regression,vote_8_test)

#MSE
MSE[8] =sum(((predict_8 - vote_8_y_hat)^2))




#################vote 9 prediction

#create training data set
vote_9_train= Create_train(stat9)

#create test dataset
vote_9_test=  Create_test(stat9)
vote_9_test= vote_9_test[ , -c(1,2)]
row.names(vote_9_test) <-NULL

#Target variable
vote_9_y_hat=  Create_test(stat9)["perc"]
row.names(vote_9_y_hat) <- NULL

#run regression
vote_9_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_9_train) 
summary(vote_9_regression)

#prediction
predict_9 = predict(vote_9_regression,vote_9_test)

#MSE
MSE[9] =sum(((predict_9 - vote_9_y_hat)^2))



#################vote 10 prediction

#create training data set
vote_10_train= Create_train(stat10)

#create test dataset
vote_10_test=  Create_test(stat10)
vote_10_test= vote_10_test[ , -c(1,2)]
row.names(vote_10_test) <-NULL

#Target variable
vote_10_y_hat=  Create_test(stat10)["perc"]
row.names(vote_10_y_hat) <- NULL

#run regression
vote_10_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_10_train) 
summary(vote_10_regression)

#prediction
predict_10 = predict(vote_10_regression,vote_10_test)

#MSE
MSE[10] =sum(((predict_10 - vote_10_y_hat)^2))




#################vote 11 prediction

#create training data set
vote_11_train= Create_train(stat11)

#create test dataset
vote_11_test=  Create_test(stat11)
vote_11_test= vote_11_test[ , -c(1,2)]
row.names(vote_2_test) <-NULL

#Target variable
vote_11_y_hat=  Create_test(stat11)["perc"]
row.names(vote_11_y_hat) <- NULL

#run regression
vote_11_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_11_train) 
summary(vote_11_regression)

#prediction
predict_11= predict(vote_11_regression,vote_11_test)

#MSE
MSE[11] =sum(((predict_11 - vote_11_y_hat)^2))



#################vote 12 prediction

#create training data set
vote_12_train= Create_train(stat12)

#create test dataset
vote_12_test=  Create_test(stat12)
vote_12_test= vote_12_test[ , -c(1,2)]
row.names(vote_12_test) <-NULL

#Target variable
vote_12_y_hat=  Create_test(stat12)["perc"]
row.names(vote_12_y_hat) <- NULL

#run regression
vote_12_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_12_train) 
summary(vote_12_regression)

#prediction
predict_12 = predict(vote_12_regression,vote_12_test)

#MSE
MSE[12] =sum(((predict_12 - vote_12_y_hat)^2))




#################vote 13 prediction

#create training data set
vote_13_train= Create_train(stat13)

#create test dataset
vote_13_test=  Create_test(stat13)
vote_13_test= vote_13_test[ , -c(1,2)]
row.names(vote_13_test) <-NULL

#Target variable
vote_13_y_hat=  Create_test(stat13)["perc"]
row.names(vote_13_y_hat) <- NULL

#run regression
vote_13_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_13_train) 
summary(vote_13_regression)

#prediction
predict_13 = predict(vote_13_regression,vote_13_test)

#MSE
MSE[13] =sum(((predict_13- vote_13_y_hat)^2))

#################vote 14 prediction

#create training data set
vote_14_train= Create_train(stat14)

#create test dataset
vote_14_test=  Create_test(stat14)
vote_14_test= vote_14_test[ , -c(1,2)]
row.names(vote_14_test) <-NULL

#Target variable
vote_14_y_hat=  Create_test(stat14)["perc"]
row.names(vote_14_y_hat) <- NULL

#run regression
vote_14_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_13_train) 
summary(vote_14_regression)

#prediction
predict_14 = predict(vote_14_regression,vote_14_test)

#MSE
MSE[14] =sum(((predict_14- vote_14_y_hat)^2))

#################vote 15 prediction

#create training data set
vote_15_train= Create_train(stat15)

#create test dataset
vote_15_test=  Create_test(stat15)
vote_15_test= vote_15_test[ , -c(1,2)]
row.names(vote_15_test) <-NULL

#Target variable
vote_15_y_hat=  Create_test(stat15)["perc"]
row.names(vote_15_y_hat) <- NULL

#run regression
vote_15_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_13_train) 
summary(vote_15_regression)

#prediction
predict_15 = predict(vote_15_regression,vote_15_test)

#MSE
MSE[15] =sum(((predict_15- vote_15_y_hat)^2))

#################vote 16 prediction

#create training data set
vote_16_train= Create_train(stat16)

#create test dataset
vote_16_test=  Create_test(stat16)
vote_16_test= vote_16_test[ , -c(1,2)]
row.names(vote_16_test) <-NULL

#Target variable
vote_16_y_hat=  Create_test(stat16)["perc"]
row.names(vote_16_y_hat) <- NULL

#run regression
vote_16_regression=lm(perc~closeness+betweenness+degree+eigencentrality+pagerank ,data=vote_13_train) 
summary(vote_16_regression)

#prediction
predict_16 = predict(vote_16_regression,vote_16_test)

#MSE
MSE[16] =sum(((predict_16- vote_16_y_hat)^2))

##############################PLOT MSE  vs Number of votes
K=1:16
plot_df= data.frame(MSE, K)
ggplot(plot_df, aes(x=K ,y=MSE))+
  geom_line() +
  theme_classic() + 
  ggtitle('MSE by Vote')

plot_df

# Core Periphery Code
unlist <- unlist(stat1$Season1$perc)
unlist_df <- as.data.frame(unlist)

?flatten

CP1$Season1$perc <- stat1[stat1$Season1$perc >= 0.8]
CP1



# Core periphery code
# datamatrix must contain column of rank, up_to_vote_num, all other values

stats1$Season1Rank <- seq.int(nrow(stats1


# create rank_pct
datamatrix$rank_pct <- (rank / sum(nrow(Season_X)))

# sort by rank_pct
datamatrix <- datamatrix[order(datamatrix$rank_pct), ]

# subsetting top 20 percent of rank_pct 
datamatrix$rank_pct  <- datamatrix[datamatrix$rank_pct >= .8]

# creating a new dataframe called corevalue
corevalue <- setNames(data.frame(matrix(ncol = 2, nrow = length(unique(datamatrix$up_to_vote_num)))),
                      c("Vote Number", "Coreness"))

# adding corevalues to corevalue
for (i in 1:17){
  subdata <- datamatrix[datamatrix$up_to_vote_num <= i,]
  graph <- graph.data.frame(subdata[, c(?:?)], directed = FALSE)
  corevalue[i, ] <- data.frame(i, mean(coreness(graph)))
}

# graphing 
plot(corevalue)

