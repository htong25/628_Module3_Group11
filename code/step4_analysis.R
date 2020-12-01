library(dplyr)
library(tidyr)
library(ggplot2)
script <- read.csv("~/Desktop/STAT-628/module3/bar_arrow.csv", stringsAsFactors=FALSE)
# str(script)
# dim(script)
# names(script)
data = script


stars = data$stars_x
stars$stars = data$stars_x
#Distributio of stars
hist(as.numeric(data$stars_x))

# Correlation plot
library(corrplot)
index=c(which(colnames(data)=="bad"),which(colnames(data)=="worst"),which(colnames(data)=="awful"),which(colnames(data)=="horrible"),
        which(colnames(data)=="mediocre"),which(colnames(data)=="average"),which(colnames(data)=="adequate"),which(colnames(data)=="ordinary"),
        which(colnames(data)=="decent"),which(colnames(data)=="warm"),which(colnames(data)=="nice"),which(colnames(data)=="affordable"),
        which(colnames(data)=="fantastic"),which(colnames(data)=="excellent"),which(colnames(data)=="wonderful"),which(colnames(data)=="terrific"))

data.cor=data[,c(1,index)]
for (i in 1:(length(index)+1)) {
  data.cor[,i]=as.numeric(data.cor[,i])
}
M<-cor(data.cor)
corrplot(M)


#Function to make barplot of frequencies and ratings
plotWordStar <- function(stars,data,vec,mfrow = c(4,4)) {
  par(mfrow = mfrow)
  col_data = colnames(data)
  for(i in 1:length(vec)) {
    index = which(col_data == vec[i])
    if(length(index) == 0) {
      warning(paste(vec[i],"not detected"))
      next
    } 

    data_vec = as.numeric(data[,index])
    
    starsY = rep(0,5)
    for(j in 1:5) {
      element = data_vec[data[,"stars_x"]==j]
      starsY[j]  = sum(element > 0,na.rm=TRUE) / sum(stars$stars == j)
      
    }
    barplot(starsY,main=vec[i],xlab="Stars",ylab="Word Freq", ylim = c(0,max(starsY)))
    
  }  
}

#Sanity Check Words
vec1 = c('bad', 'worst', 'awful', 'horrible',
        'mediocre', 'average', 'adequate', 'ordinary',
        'decent', 'warm', 'nice', 'affordable', 
        'fantastic', 'excellent', 'wonderful', 'terrific'
)
#Alcohol Words
vec2 = c('vodka', 'gin', 'whiskey', 'rum',
        'tequila', 'wine', 'brandy','champagne', 
        'mojito', 'martini', 'bloody', 'manhattan',
        'beer', 'cocktail','gimlet','mule'
)

# Ambiance & Service & Food Words
vec3 = c('parking', 'waiting', 'service',
        'bartender','dim', 'music',
        'pizza',
        'chip', 'burger', 'fry', 'wing'
        
)

#Make three set of plots 
plotWordStar(stars,data,vec1,mfrow = c(4,4))
plotWordStar(stars,data,vec2,mfrow = c(4,4))
plotWordStar(stars,data,vec3,mfrow = c(4,4))

#After visually examined the trend in frequencies and ratings,
#we want to use t-test to check if there exist significant difference between
#the rating with a specific word and the rating without it
get_comparison <- function (data , word){
  col_data = colnames(data)
  index = which(col_data == word) #Get index of this word
  word_index = which(data[,index]>0) #Get index of review mentioned this word
  no_word_star = data[-word_index,1] # Star of reviews not mentioned this word
  with_word_star = data[word_index,1] # Star of reviews that mentioned this word
  #c(mean(with_word_star) ,mean(no_word_star))#Test their mean
  test_result = t.test(no_word_star, with_word_star, var.equal = FALSE)
  pval=test_result$p.value
  est = test_result$estimate
  est=round(est[1]-est[2],3)
  
  if (pval<=0.05){sign = "significant"}
  else{sign = "NOT significant"}
  cat("Begin:",word,"\n")
  cat("[mean rating without this word, mean rating with this word]\n")
  cat(test_result$estimate,"\n")
  cat("(without) - (with) is",est,"\n")
  cat("p-value is",pval,"=> the difference is",sign,"\n")
  cat("End","\n")
  cat("\n")
}

for (word in vec1){
  get_comparison(data,word)
}

for (word in vec2){
  get_comparison(data,word)
}

for (word in vec3){
  get_comparison(data,word)
}



  
  

  