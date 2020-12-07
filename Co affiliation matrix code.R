#CHeck excel file
#change the directory 
#import file and 
#edit line 64 to "Season name"

library(readr)

setwd("C:/Users/banva/Desktop/MSBA/Fall/Social Networks/Final Project/S1")

#import the data
S <- read_csv("Season 1 Raw.csv" ,col_names = FALSE)

#change to data frame
S=as.data.frame(S)
class(S)

row.names(S) = S[,1]
  
#Setting up coaffiliation matrix
df=S
df[ , ] =0
dim(df)

#just creating an empty variable for next loop
X=''

#check the dimensions of df.. 
dim(df)

#let's make number of cols=number of rows in df
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
  end()
}


#Now assign column names as row names
colnames(df)=row.names(df) 

dim(df) #let's check the dimensions


for(i in 2: (ncol(S))){
voted_out = na.omit(unique(S[[i]]))
  for(j in seq_along(voted_out)){
    
    t= voted_out[j]
    
    a= S[S[i]==t , ]
   
    q=  na.omit(a$X1)
    
    df[q,q ]= df[q,q]+1  
  
      if(i != ncol(S)){
    file_name1= paste("S1_E" ,i  ,sep = ""  )
    file_name=paste(file_name1 , ".csv" , sep="")
    write.csv(df, file = file_name)
      }else{
        write.csv(df, file= "Final matrix.csv" )
      }
    
    }
}








