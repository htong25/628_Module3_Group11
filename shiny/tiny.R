# Shiny
# Shikun Liu, sliu673@wisc.edu
# Jiayi Xu, jxu273@wisc.edu


#library(dplyr)
library(ggplot2)
library(shiny)
library(DT)

script <- read.csv("tinytiny.csv", stringsAsFactors=FALSE)
data = script # To keep data

# length(unique(data$name)) # 318
uni_bar_names = unique(data$name)

#Manually detected the words without enough reviews
not_enough = c(1,6,19,21,23,26,30,31,32,33,34,35,36)
uni_bar_names = uni_bar_names[-not_enough]

#function to get t-test 
get_comparison <- function (data , words){
  pvals = c()
  col_data = colnames(data)
  i=1
  for (word in words){
    index = which(col_data == word) #Get index of this word
    word_index = which(data[,index]>0) #Get index of review mentioned this word
    no_word_star = data[-word_index,1] # Star of reviews not mentioned this word
    with_word_star = data[word_index,1] # Star of reviews that mentioned this word
    #c(mean(with_word_star) ,mean(no_word_star))#Test their mean
    result = t.test(no_word_star, with_word_star, var.equal = FALSE)$p.value
    pvals[i] = round(result,3)
    i=i+1
  }
  return(pvals)
}

get_max_col <- function (data , word){
  name = word
  new_data = data[data$name == name,] 
  cut_data = new_data[,(14:length(names(new_data)))]
  max_col=1
  cur_sum=0
  max_sum=sum(cut_data[,1])
  col_=c()
  for (i in 1:length(names(cut_data))) {
    col_[i]=sum(cut_data[,i])
  }
  sort_sum = sort(unique(col_),decreasing = TRUE)
  order_name = c()
  for (i in 1:length(sort_sum)) {
    order_name = append(order_name, names(cut_data)[which(col_==sort_sum[i])])
  }
  order_name = order_name [0:200]
  return(order_name)
}

get_order_sum <- function (data , word){
  name = word
  new_data = data[data$name == name,] 
  cut_data = new_data[,(14:length(names(new_data)))]
  max_col=1
  cur_sum=0
  max_sum=sum(cut_data[,1])
  col_=c()
  for (i in 1:length(names(cut_data))) {
    col_[i]=sum(cut_data[,i])
  }
  sort_sum = sort((col_),decreasing = TRUE)
  sort_sum = sort_sum[0:200]
  return(sort_sum)
}

get_avg_star <- function (data , word){
  name = word
  new_data = data[data$name == name,] 
  return(mean(new_data$stars_x) )
}


ui <- fluidPage(
  titlePanel("Analysis of bars in Madison on Yelp"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "bar",
                  label = "Choose a bar:",
                  choices = uni_bar_names),
      submitButton("Click it!")
    ),
    
    mainPanel(
      dataTableOutput("prediction"),
      helpText("if p-value <0.05, it means the average rating without this word has a significant difference from the average rating with this word"),
      tags$hr(),
      textOutput("average_stars"),
      plotOutput(outputId = "Plot"),
      tags$hr(),
      helpText("This app is developed by Shikun Liu. Please contact sliu673@wisc.edu if you have any questions.")
    )
  )
)


server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$bar)
  })
  
  
  output$prediction <-  renderDataTable(DT::datatable({
    name_lists = get_max_col(data, input$bar)
    r = data.frame("name"=name_lists,"frequency"=get_order_sum(data, input$bar), "p-value for t-test" = get_comparison(data,name_lists))
    r
  }))
  
   
  output$average_stars <- renderText({
    mean_stars = get_avg_star(data, input$bar)
    paste("The mean average stars is:",round(mean_stars,2))
  })
  
  output$Plot <- renderPlot({
    starsY = rep(0,5)
    new_data = data[data$name == input$bar,] 
    for(j in 1:5) {
      element = sum(new_data$stars_x==j)
      starsY[j]  = element / length(new_data$stars_x)
    }
    names(starsY)=c(1:5)
    barplot(starsY,main=input$bar, xlab="Stars",ylab="Word Freq", ylim = c(0,max(starsY)))
  })

}
    
    
    
  
    

shinyApp(ui = ui, server = server)
