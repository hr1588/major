
library(shiny)
load("titles.rda")
load("predict.rda")
load("reviews.rda")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title을 입력함 
    titlePanel("영화평론 감성분석"),

    # Sidebar with a slider input for number of bins 
    # 위젯에 해당하는 것이 radioButtons
    sidebarLayout(
        sidebarPanel(
            radioButtons("title", "영화제목:",
                         c("범죄도시2"="A",
                           "닥터 스트레인지: 대혼돈의 멀티버스" = "B",
                           "그대가 조국"="C")),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3(textOutput("caption")),
           plotOutput("moviePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # reactive function 
    dataInput <- reactive({
      temp = switch (input$title,
                    "A" = "범죄도시2",
                    "B" = "닥터 스트레인지: 대혼돈의 멀티버스",
                    "C" = "그대가 조국")
      t = which(titles == temp)
      predict[t]
    })
    #
    output$caption <- renderText({
      s = dataInput()
      p = length(s[s==1])
      n = length(s[s==-1])
      polarity = (p-n)/(p+n)
      paste("Polarity : ", round(polarity, 3)*100)
       })
        
    output$moviePlot <- renderPlot({
        # generate bins based on input$bins from ui.R
       temp = table(dataInput())
       names(temp) = c("부정","중립","긍정")
       pie(temp, main = "Sentiment Analysis on Movie Review(ref : naver)",
           col = c("red","green","blue"))

    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# 범죄도시2, 닥터 스트레인지, 그대가 조국 3가지 영화로 감성분석을 해본 결과 실습 당시의 데이터를 기준으로는
# 범죄도시2의 극성평가가 상당히 긍정적인 것을 확인할 수 있었고, 닥터 스트레인지는 정확히 중립이 나오는 것을 확인할 수 있었습니다.
# 마블의 기대작임에도 평가가 좋지 않은 이유는 사람들의 기대에 부응하지 못했거나 혹은 영화가 개봉한지 얼마 안되서 아직 사람들의 평가가 부족할 수도 있겠다는 생각이 들었습니다.




