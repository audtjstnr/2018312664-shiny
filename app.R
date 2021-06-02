#load packages
library(shiny)
library(data.table)
library(lubridate)
library(ggplot2)
library(tidyverse)

FData <- fread("data/FData.csv")
FData1 <- FData
FData1$지역 <- ifelse(FData1$지역=="서울",1,
                    ifelse(FData1$지역=="부산",2,
                           ifelse(FData1$지역=="광주",3,
                                  ifelse(FData1$지역=="대전",4,
                                         ifelse(FData1$지역=="대구",5,6)))))
FData1$승차2 <- ifelse(FData1$지역==1,FData1$승차/9668465,
                    ifelse(FData1$지역 == 3,FData1$승차/1450062,
                           ifelse(FData1$지역 == 5, FData1$승차/2418346,
                                  ifelse(FData1$지역 == 2, FData1$승차/3391946, 
                                         ifelse(FData1$지역==4,FData1$승차/1463882, FData1$승차/2942828)))))
FData1$하차 <- ifelse(FData1$지역==1,FData1$하차/9668465,
                    ifelse(FData1$지역 == 3,FData1$하차/1450062,
                           ifelse(FData1$지역 == 5, FData1$하차/2418346,
                                  ifelse(FData1$지역 == 2, FData1$하차/3391946, 
                                         ifelse(FData1$지역==4,FData1$하차/1463882, FData1$하차/2942828)))))

FData1$요일 <- ifelse(FData1$요일=="평일",1,2)
FData1$일자 <- ymd(FData1$일자)


#ui-1 지역마다 호선 변수 받는 이름 각각 다름
ui <- fluidPage(
  headerPanel("지하철 승객수와 날씨의 관계"),
  fluidRow(
    column(4,selectInput("region",strong(h3("지역")),choices=list("전체"=0,"서울"=1,"부산"=2,"광주"=3,"대전"=4,"대구"=5,"인천"=6),selected=0)),
    column(4,selectInput("weather",strong(h3("날씨변수")),choices=list("기온"="temp","전운량"="cloud","강수량"="rain"),selected="temp")),
    column(4,radioButtons("aboard",strong(h3("승차/하차")),choices=list("승차"="aboard","하차"="disaboard"),selected="aboard"))),
  fluidRow(
    column(4,dateRangeInput("date",strong(h3("날짜")),start="2019-01-01",end="2020-12-31",min="2019-01-01",max="2020-12-31")),
    column(4,radioButtons("dow",strong(h3("평일/주말")),choices=list("평일"=1,"주말"=2),selected=1)),
    column(4,conditionalPanel(condition="input.region==1",
                              selectInput("seoulline",strong(h3("서울호선")),choices=list("전체"=0,"1호선"=1,"2호선"=2,"3호선"=3,"4호선"=4,"5호선"=5,"6호선"=6,"7호선"=7,"8호선"=8),selected=0))),
    column(4,conditionalPanel(condition="input.region==2",
                              selectInput("busanline",strong(h3("부산호선")),choices=list("전체"=0,"1호선"=1,"2호선"=2,"3호선"=3,"4호선"=4),selected=0))),
    column(4,conditionalPanel(condition="input.region==6",
                              selectInput("incheonline",strong(h3("인천호선")),choices=list("전체"=0,"1호선"=1,"2호선"=2),selected=0))),
    column(4,conditionalPanel(condition="input.region==5",
                              selectInput("daeguline",strong(h3("대구호선")),choices=list("전체"=0,"1호선"=1,"2호선"=2,"3호선"=3),selected=0)))),
  mainPanel(plotOutput("mainplot")))


#server
server <- function(input,output){
  w <- reactive({switch(input$weather,temp = "기온",cloud = "전운량",rain = "강수량")})
  ab <- reactive({switch(input$aboard,aboard = "승차",disaboard = "하차")})
  rainData <- reactive({
    FData2 <- FData1%>%filter(요일 %in% input$dow)%>%
      filter(일자>=input$date[1]&일자<=input$date[2])
    if(input$region==3|input$region==4){
      if(w()=="강수량"){
        a <- FData2%>%filter(지역 %in% input$region & 강수량>0)
        return(a)
      }else{
        a <- FData2%>%filter(지역 %in% input$region)
        return(a)
      }
    }else if(input$region==1){
      if(w()=="강수량"){
        if(input$seoulline==0){
          a <- FData2%>%filter(지역 %in% input$region & 강수량>0)
          return(a)
        }else{
          a <- FData2%>%filter(지역 %in% input$region & 강수량>0 & 호선 %in% input$seoulline)
          return(a)
        }
      }else{
        if(input$seoulline==0){
          a <- FData2%>%filter(지역 %in% input$region)
          return(a)
        }else{
          a <- FData2%>%filter(지역 %in% input$region & 호선 %in% input$seoulline)
          return(a)
        }
      }
    }else if(input$region==2){
      if(w()=="강수량"){
        if(input$busanline==0){
          a <- FData2%>%filter(지역 %in% input$region & 강수량>0)
          return(a)
        }else{
          a <- FData2%>%filter(지역 %in% input$region & 강수량>0 & 호선 %in% input$busanline)
          return(a)
        }
      }else{
        if(input$busanline==0){
          a <- FData2%>%filter(지역 %in% input$region)
          return(a)
        }else{
          a <- FData2%>%filter(지역 %in% input$region & 호선 %in% input$busanline)
          return(a)
        }
      }
    }else if(input$region==5){
      if(w()=="강수량"){
        if(input$daeguline==0){
          a <- FData2%>%filter(지역 %in% input$region & 강수량>0)
          return(a)
        }else{
          a <- FData2%>%filter(지역 %in% input$region & 강수량>0 & 호선 %in% input$daeguline)
          return(a)
        }
      }else{
        if(input$daeguline==0){
          a <- FData2%>%filter(지역 %in% input$region & 지역 %in% input$region)
          return(a)
        }else{
          a <- FData2%>%filter(지역 %in% input$region & 호선 %in% input$daeguline)
          return(a)
        }
      }
    }else if(input$region==6){
      if(w()=="강수량"){
        if(input$incheonline==0){
          a <- FData2%>%filter(지역 %in% input$region & 강수량>0)
          return(a)
        }else{
          a <- FData2%>%filter(지역 %in% input$region & 강수량>0 & 호선 %in% input$incheonline)
          return(a)
        }
      }else{
        if(input$incheonline==0){
          a <- FData2%>%filter(지역 %in% input$region & 지역 %in% input$region)
          return(a)
        }else{
          a <- FData2%>%filter(지역 %in% input$region & 호선 %in% input$incheonline)
          return(a)
        }
      }
    }else if(input$region==0){
      if(w()=="강수량"){
        a <- FData2%>%filter(강수량>0)
        return(a)
      }else{
        return(FData2)
      }
      
      
    }
  })
  output$mainplot <- renderPlot({
    p <- ggplot(data = rainData(), aes_string(x=w(),y=ab()))
    region_name <- list("1"="서울","2"="부산","3"="광주","4"="대전","5"="대구","6"="인천")
    region_labeller <- function(variable,value){
      return(region_name[value])}
    if(input$region==0){
      p+geom_col(aes(fill=as.factor(지역)))+
        scale_fill_brewer("Spectral")+
        ylab(paste(ab()," (각 지역별 인구의 비율 합계)"))+
        labs(fill="지역")+
        scale_fill_discrete(labels=c("서울","부산","광주","대전","대구","인천"))+
        facet_wrap(~지역,nrow=2,labeller=region_labeller)
    }else{
      p+geom_col(fill="Darkblue")+
        ylab(paste(ab()," (각 지역별 인구의 비율 합계)"))}
  })
}

#runapp
shinyApp(ui, server)
