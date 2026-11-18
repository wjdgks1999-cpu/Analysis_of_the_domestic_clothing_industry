# 기존 입력 식별자와 다섯 탭을 유지하는 사용자 화면이다.
# 역할: 검증된 문맥 객체로 화면을 만들며 외부 호출이나 저장은 하지 않는다.
build_ui <- function(context) {
  list2env(context, envir = environment())
  staff_values <- dat.basic$종업원수[is.finite(dat.basic$종업원수)]
  staff_range <- if (length(staff_values)) range(staff_values) else c(0, 1)
  navbarPage(
    "제이더블유 브라우저",
    
    header = shinyjs::useShinyjs(),
    
    ### 데이터 추출파트 ###
    tabPanel("데이터 추출",
             fluidPage(
               fluidRow(
                 column(2, #표준산업분류의 text input
                        textInput(inputId = "bigcluster", label = "대분류"),
                        textInput(inputId = "midcluster", label = "중분류"),
                        textInput(inputId = "smallcluster", label = "소분류"),
                        textInput(inputId = "secluster", label = "세분류"),
                        textInput(inputId = "sesecluster", label = "세세분류"),
                        textInput(inputId = "industry", label = "주요사업"),
                        selectInput(inputId = "region", label = "지역",
                                    choices = region.list, multiple = T)
                 ),
                 column(2, #기업개요의 text input
                        selectInput(inputId = "method.select", label = "재무정보 조회 방법",
                                    choices = method.selecting, multiple = F),
                        selectInput(inputId = "year.select", label = "재무정보의 년도",
                                    choices = available_years, multiple = TRUE),
                        selectInput(inputId = "class.company.select", label = "개인법인구분",
                                    choices = class.company),
                        selectInput(inputId = "staff.count.onoff.select", label = "종업원수 결측값 포함여부", 
                                    choices = staff.count.onoff),
                        sliderInput(inputId = "staff.count.slider", label = "종업원수(결측값 포함시 작동X)",
                                    min = staff_range[1], max = staff_range[2],
                                    value = c(min(dat.basic$종업원수, na.rm = T), max(dat.basic$종업원수, na.rm = T))),
                        actionButton(inputId = "extracting", label = "데이터 추출",class = "btn-danger")#UI 액션버튼추가중 ####
                 ),
                 
                 
                 column(8, #아웃풋을 놓는 곳이다.
                        tabsetPanel(
                          tabPanel(title = "결측치 정보", 
                                   DT::dataTableOutput("na.table"),
                                   br(),
                                   downloadButton(outputId = "download.basic", label = "데이터 다운로드")
                          ),
                          
                          tabPanel(title = "개요요약",
                                   plotOutput("sesebar"),
                                   plotOutput("main.industry"),
                                   plotOutput("selproduct")
                          ),
                          
                          tabPanel(title = "재무요약",
                                   plotOutput("jabon.ker"),
                                   plotOutput("income.ker"),
                                   plotOutput("money.ker")
                          )
                        )
                 )
               ),
               
               br(),
               hr(),
               br(),
               
               fluidRow(
                 tabsetPanel(
                   tabPanel(title = "표준산업분류",
                            column(2, DT::dataTableOutput("w")),
                            column(2, DT::dataTableOutput("ww")),
                            column(2, DT::dataTableOutput("www")),
                            column(3, DT::dataTableOutput("wwww")),
                            column(3, DT::dataTableOutput("wwwww"))
                   ),
                   
                   tabPanel(title = "표준산업분류 세부검색",
                            column(4,
                                   selectInput(inputId = "select.big", label = "대분류 목록", choices = unique(industry.code$대분류)),
                                   selectInput(inputId = "select.mid", label = "중분류 목록", choices = NULL),
                                   selectInput(inputId = "select.small", label = "소분류 목록", choices = NULL),
                                   selectInput(inputId = "select.se", label = "세분류 목록", choices = NULL)
                            ),
                            column(8,
                                   DT::dataTableOutput("industry.table")))
                 )
               )
             )
             
    ),
    
    ### 개요정보 파트 ###
    tabPanel("개요정보",
             fluidPage(
               fluidRow(
                 column(2,
                        selectInput(inputId = "select.gaeyo.inform", label = "개요정보 선택",
                                    choices = gaeyo.select)),
                 column(10,
                        plotOutput("gaeyo.bar", click = "click.gaeyo"),
                        br(),
                        downloadButton(outputId = "download.gaeyo", label = "데이터 다운로드"))
               ),
               br(),
               hr(),
               br(),
               fluidRow(
                 column(12,
                        DT::dataTableOutput("gaeyo.table"))
               )
             )
    ),
    
    ### 재무정보 파트 ###
    tabPanel("재무정보",
             fluidPage(
               fluidRow(
                 column(2,
                        selectInput(inputId = "select.jaemu.outlier", label = "이상치(outlier) 포함 여부", choices = outlier.select),
                        selectInput(inputId = "select.jaemu.inform", label = "재무정보 선택", choices = jaemu.select),
                        selectInput(inputId = "select.jaemu.year", label = "재무년도 선택", choices = finance.date, multiple = T)
                 ),
                 column(10,
                        plotOutput("jaemu.hist", brush = brushOpts(id = "brush.jaemu", direction = "x", resetOnNew = T)),
                        br(),
                        downloadButton(outputId = "download.jaemu", label = "데이터 다운로드"))
               ),
               br(),
               hr(),
               br(),
               fluidRow(
                 column(12,
                        DT::dataTableOutput("data.brush")
                 )
               )
             )
    ),
    
    ### 가설검정 파트 ###
    tabPanel("가설검정",
             fluidPage(
               fluidRow(
                 column(2,
                        selectInput(inputId = "met", label = "대상재무정보", choices = jaemu.select)),
                 column(2,
                        selectInput(inputId = "g1.s", label = "대상년도1", choices = finance.date)),
                 column(2,
                        selectInput(inputId = "g2.s", label = "대상년도2", choices = finance.date)),
                 column(2,
                        selectInput(inputId = "altern", label = "대립가설 형태", choices = alter))
               ),
               fluidRow(
                 column(6,
                        plotOutput(outputId = "g1plot")),
                 column(6,
                        plotOutput(outputId = "g2plot"))
               ),
               fluidRow(
                 column(12,
                        h4("대상년도1"),
                        DT::dataTableOutput("g1summary"),
                        br(),
                        h4("대상년도2"),
                        DT::dataTableOutput("g2summary"),
                        br(),
                        h4("가설검정 결과"),
                        verbatimTextOutput(outputId = "result.test")
                 )
               )
             )
    ),
    
    ### AI 파트 ###
    tabPanel("AI 분석",
             fluidPage(
               fluidRow(
                 column(4,
                        selectInput(inputId = "ai.selecting", label = "AI 선택", choices = ai.select,
                        )
                 ),
                 column(8,
                        textOutput("ai.infor", container = tags$h4)
                 )
               ),
               fluidRow(
                 column(2,
                        numericInput(inputId = "ai1", label = "총자산세전순이익률", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai2", label = "총자산순이익률", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai3", label = "자기자본세전순이익률", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai19", label = "총자산회전율", 
                                     value = 0, min = -999, max = 999)
                 ),
                 
                 column(2,
                        numericInput(inputId = "ai4", label = "자기자본순이익률", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai5", label = "자본금세전순이익률", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai6", label = "자본금순이익률", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai20", label = "자기자본회전율", 
                                     value = 0, min = -999, max = 999)
                 ),
                 
                 column(2,
                        numericInput(inputId = "ai7", label = "매출액세전순이익률", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai8", label = "매출액순이익률", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai9", label = "매출액영업이익률", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai21", label = "자본금회전율", 
                                     value = 0, min = -999, max = 999)
                 ),
                 
                 column(2,
                        numericInput(inputId = "ai10", label = "매출원가대매출액", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai11", label = "자기자본비율", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai12", label = "유동비율", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai22", label = "비유동자산회전율", 
                                     value = 0, min = -999, max = 999)
                 ),
                 
                 column(2,
                        numericInput(inputId = "ai13", label = "비유동비율", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai14", label = "비유동장기적합률", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai15", label = "부채비율", 
                                     value = 0, min = -999, max = 999)
                 ),
                 
                 column(2,
                        numericInput(inputId = "ai16", label = "유동부채비율", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai17", label = "비유동부채비율", 
                                     value = 0, min = -999, max = 999),
                        numericInput(inputId = "ai18", label = "순운전자본대총자본", 
                                     value = 0, min = -999, max = 999),
                        actionButton(inputId = "start.pred", label = "예측시작!", class = "btn-danger")
                 )
               ),
               br(),
               hr(),
               br(),
               
               fluidRow(
                 column(4,
                        tabsetPanel(
                          tabPanel(title = "재무지표",
                                   DT::dataTableOutput("index.informing")
                          ),
                          
                          tabPanel(title = "입력 데이터",
                                   DT::dataTableOutput("now.predict.data")
                                   )
                        )
                 ),
                 
                 column(8, 
                        tabsetPanel(
                          tabPanel(title = "AI 예측",
                                   textOutput("before.resulting", container = tags$h5),
                                   br(),
                                   textOutput("after.resulting", container = tags$h5)
                          ),
                          
                          tabPanel(title = "AI 학습정보",
                                   verbatimTextOutput("ai.performance")
                                   
                          ),
                          
                          tabPanel(title = "변수 중요도",
                                   plotOutput("ai.variable.importance")      
                          )
                        )
                 )
               )
             )
    )
  )
}
