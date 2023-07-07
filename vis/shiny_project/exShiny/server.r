library(shiny)

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
