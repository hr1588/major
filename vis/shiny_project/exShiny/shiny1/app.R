#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
# UI : 사용자 입력, 출력이 표시
ui <- fluidPage(

    # Application title(제목)
    headerPanel("1717709 이동현"),

    # Sidebar with a slider input for number of bins (사이드바 조절)
    sidebarLayout(
        sidebarPanel(
            sliderInput("obs", # server의 input에 투입
                        "Number of observations:",
                        min = 1,
                        max = 1000,
                        value = 500) # 실행시켰을 때의 값
        ),

        # Show a plot of the generated distribution(메인 패널)
        mainPanel(
           plotOutput("d") # output의 hist가 투입
        )
    )
)

# Define server logic required to draw a histogram
# server의 파라미터(function)는 절대 변경되면 안되며, output 뒤에 session이 생략
# session은 사용자 정보를 기억
server <- function(input, output) {

    output$d <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- rnorm(input$obs)

        # draw the histogram with the specified number of bins
        hist(x, main = "정규분포 히스토그램", col = 'darkblue', border = 'white',
             xlab = 'x', ylab = '빈도')
    })
}

# Run the application (UI와 server를 통합해서 실행)
shinyApp(ui = ui, server = server)
