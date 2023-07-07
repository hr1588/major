# install.packages("bslib")
library(shiny)
library(bslib)

# Define UI for application that draws a histogram
# UI : 사용자 입력, 출력이 표시
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "slate"),
  # Application title(제목)
  headerPanel("1717709 이동현"),
  tags$h1("빅데이터 시각화"),
  # Sidebar with a slider input for number of bins (사이드바 조절)
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", # server의 input에 투입
                  "Number of observations:",
                  min = 1,
                  max = 1000,
                  value = 500), # 실행시켰을 때의 값
      hr(),
      tags$img(src="inf.jpg", width = "250", height = "200", margin = "10")
    ),
    
    # Show a plot of the generated distribution(메인 패널)
    mainPanel(
      plotOutput("d"), # output의 hist가 투입
      tags$hr(),
      tags$p("안녕하세요 반갑습니다")
    )
  )
)

