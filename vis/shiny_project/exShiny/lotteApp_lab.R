library(shiny)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
load(file="data/data1.rda")

ui <- fluidPage(
    
    # Application title
    headerPanel("webScraping"), 
    
    
    # Sidebar with a slider input a variable
    sidebarLayout(
        sidebarPanel(
            # radio button위젯 사용
            radioButtons("spot", "name of movie :",
                         c("그대가 조국" = "그대가 조국",
                           "쥬라기 월드: 도미니언" = "쥬라기 월드: 도미니언",
                           "범죄도시2 " = "범죄도시2 ",
                           "닥터 스트레인지: 대혼돈의 멀티버스" = "닥터 스트레인지: 대혼돈의 멀티버스 ")),
            hr(),
            tags$img(src="inf.jpg", width="200px", height="200px")),
        
        # Show output 
        mainPanel(
            h2(textOutput("caption")),
            plotOutput("plot"),
            tableOutput("table")
            
            
            
            )
    )  
     
)
#### server

server <- function(input, output) {
    # reactive function(title과 dataInput)
  
  dataInput <- reactive({
    temp <- input$spot
    subset(data1, 점포ID==temp)
  })
  
  title <- reactive(
    paste(input$spot, "감성 분석 파이 차트")
  )
    
    # title을 renderText에 전송
    output$caption <- renderText(title())
    
    
    # plot을 renderPlot에 전송, 여러줄을 쓸때는 집합기호{} 사용
    output$plot <- renderPlot({
    ggplot(dataInput(),aes(거래월,상품대분류명))+
        geom_tile(aes(fill=amount))+
        scale_fill_gradientn(colors = brewer.pal(5,name = "RdBu"))+
        ggtitle("월별 거래처에 대한 매출액")+
        xlab("거래월")+
        ylab("상품대분류명")+
        scale_x_continuous(breaks = seq(1:12))
    })
    
  
}


#Run the application 
shinyApp(ui = ui, server = server)
