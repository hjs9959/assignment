library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(leaflet)
library(dplyr)
library(tidyr)
library(rsconnect)



## -------------------------------------------------------------------
## 1. DATA PREPARATION
## -------------------------------------------------------------------

# (1) 국내 시도별 데이터
province_data <- data.frame(
  region = c("서울", "부산", "대구", "인천", "광주", "대전", "울산", "세종",
             "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주"),
  latitude = c(37.5665, 35.1796, 35.8722, 37.4563, 35.1595, 36.3504, 35.5384,
               36.5042, 37.2752, 37.8228, 36.6363, 36.5184, 35.7175, 34.6166,
               36.4919, 35.4606, 33.4996),
  longitude = c(126.9780, 129.0756, 128.6025, 126.7052, 126.8526, 127.3845,
                129.3114, 127.2654, 127.0095, 128.1555, 127.4910, 126.8000,
                127.1530, 126.5960, 128.8889, 128.2132, 126.5312),
  births = c(
    39456,  # 서울
    12866,  # 부산
    9410,   # 대구
    13659,  # 인천
    6172,   # 광주
    7194,   # 대전
    5082,   # 울산
    2761,   # 세종
    68817,  # 경기
    6688,   # 강원
    7580,   # 충북
    9436,   # 충남
    6622,   # 전북
    7828,   # 전남
    10186,  # 경북
    13049,  # 경남
    3222    # 제주
  ),
  fertility = c(0.581, 0.683, 0.754, 0.762, 0.699, 0.792, 0.859, 1.028, 
                0.789, 0.889, 0.882, 0.883, 0.808, 1.028, 0.897, 0.820, 0.826),
  class_size = c(21.7, 21.1, 22.2, 22.0, 20.7, 20.0, 21.8, 20.4,
                 22.5, 17.8, 19.4, 19.5, 17.7, 17.0, 19.0, 20.3, 21.6),
  teacher_ratio = c(15.0, 14.5, 14.8, 14.5, 13.9, 14.0, 14.7, 13.8,
                    15.5, 13.0, 13.5, 13.8, 12.9, 12.5, 13.3, 14.0, 14.0),
  ageing_index = c(
    221.96,  # 서울
    247.25,  # 부산
    201.17,  # 대구
    158.03,  # 인천
    150.88,  # 광주
    164.90,  # 대전
    144.18,  # 울산
    63.33,   # 세종
    141.52,  # 경기
    256.69,  # 강원
    201.42,  # 충북
    197.48,  # 충남
    247.51,  # 전북
    261.74,  # 전남
    259.16,  # 경북
    193.99,  # 경남
    147.74   # 제주
  )
)

# (2) 시계열 데이터 (만족도 점수 기준)
trend_data <- data.frame(
  Year = 2013:2023,
  ClassSize = c(25.2, 24.8, 24.5, 24.2, 23.9, 23.5, 23.1, 22.8, 22.5, 22.0, 21.7),
  PrivateEdu = c(68.8, 69.4, 70.1, 71.2, 72.5, 73.8, 74.8, 67.1, 75.5, 78.3, 79.1),
  Violence = c(11.7, 13.2, 14.5, 15.1, 16.4, 17.2, 18.1, 9.8, 15.6, 17.8, 19.2),
  Satisfaction = c(3.9, 3.9, 3.8, 3.8, 3.7, 3.8, 3.7, 3.5, 3.6, 3.4, 3.3) 
)

## -------------------------------------------------------------------
## 2. USER INTERFACE
## -------------------------------------------------------------------
ui <- navbarPage(
  title = "🎓 청소년으로 인구와 행복의 사이를 잇다",
  theme = bs_theme(version = 5, bootswatch = "zephyr"),
  
  # ------------------------------------------------------------------
  # TAB 1: 1–2차시 (트리맵 + AI 질문 연습)
  # ------------------------------------------------------------------
  tabPanel(
    "🔎 1단계: 텅 빈 놀이터와 사라진 웃음",
    div(class = "container-fluid p-4",
        fluidRow(
          column(6,
                 div(class = "card border-danger h-100",
                     div(class = "card-header bg-danger text-white", h5("📺 왜 아기 울음소리가 줄었을까?")),
                     div(class = "card-body",
                         p("우리나라가 세계에서 가장 빠르게 '할머니, 할아버지 나라'가 되고 있대요."),
                         tags$a(href = "https://kosis.kr/edu/webtoon/detail.do", target = "_blank",
                                class = "btn btn-outline-danger btn-sm mb-2", "📚 만화로 보는 인구 이야기"), 
                         br(),
                         tags$a(href = "https://news.sbs.co.kr/news/endPage.do?news_id=N1005420185", target = "_blank",
                                class = "btn btn-outline-danger btn-sm", "📰 뉴스: 2045년, 노인 비중 세계 1위?")
                     )
                 )
          ),
          column(6,
                 div(class = "card border-primary h-100",
                     div(class = "card-header bg-primary text-white", h5("📺 우리는 행복할까?")),
                     div(class = "card-body",
                         p("물질적으로 풍요롭지만, 마음은 힘든 어린이들의 이야기를 들어보세요."),
                         tags$a(href = "https://n.news.naver.com/mnews/article/016/0000382397", target = "_blank",
                                class = "btn btn-outline-primary btn-sm mb-2", "📰 뉴스: 행복지수 꼴찌의 비밀"),
                         br(),
                         tags$a(href = "https://www.youtube.com/embed/zP9GYLCklMI", target = "_blank",
                                class = "btn btn-outline-primary btn-sm", "🎬 영상: 어린이의 눈으로 본 행복")
                     )
                 )
          )
        ),
        
        br(),
        
        # [AI 미션]
        fluidRow(
          column(6,
                 div(class = "card border-warning",
                     div(class = "card-header bg-warning text-dark", h5("🤖 미션 1: 고령화 질문 만들기")),
                     div(class = "card-body",
                         p("AI에게 물어볼 좋은 질문을 생각해보고 아래에 적어보세요."),
                         div(id = "chatbox1", class = "border rounded p-3 mb-3",
                             style = "height: 200px; overflow-y: auto; background-color: #f8f9fa;",
                             uiOutput("chat_output_1")),
                         tags$a(href = "https://wooriai.use.go.kr/start/", target = "_blank",
                                class = "btn btn-outline-secondary btn-sm mb-2 w-100", "🌐 우리아이AI 바로가기 (클릭)"),
                         textInput("input_mission1", "", placeholder = "예: 왜 우리나라는 아기가 적게 태어나?"),
                         actionButton("send_mission1", "질문 기록하기", class = "btn btn-warning w-100 mt-2")
                     )
                 )
          ),
          column(6,
                 div(class = "card border-success",
                     div(class = "card-header bg-success text-white", h5("🤖 미션 2: 행복 질문 만들기")),
                     div(class = "card-body",
                         p("청소년 행복에 대해 궁금한 점을 적고 링크로 가서 물어보세요."),
                         div(id = "chatbox2", class = "border rounded p-3 mb-3",
                             style = "height: 200px; overflow-y: auto; background-color: #f8f9fa;",
                             uiOutput("chat_output_2")),
                         tags$a(href = "https://wooriai.use.go.kr/start/", target = "_blank",
                                class = "btn btn-outline-secondary btn-sm mb-2 w-100", "🌐 우리아이AI 바로가기 (클릭)"),
                         textInput("input_mission2", "", placeholder = "예: 청소년 행복을 높이려면 어떻게 해야 해?"),
                         actionButton("send_mission2", "질문 기록하기", class = "btn btn-success w-100 mt-2")
                     )
                 )
          )
        ),
        br(),
        
        # [1] 트리맵 섹션
        fluidRow(
          column(12,
                 div(class = "card border-secondary",
                     div(class = "card-body", 
                         plotlyOutput("treemap_plot", height = "400px")
                     ),
                     div(class = "card-footer text-muted", h6("💡 네모 크기가 클수록 아기가 많이 태어난 지역이에요."))
                 )
          )
        ),
        br(),
        div(class = "card border-info",
            div(class = "card-header bg-info text-white", h5("📝 1단계 탐구 노트")),
            div(class = "card-body",
                p(strong("Q1. 그래프에서 가장 큰 네모(출생아 수가 많은 곳)는 어디인가요?")), textInput("q1", "", placeholder = "지역 이름 쓰기"),
                p(strong("Q2. 반대로 가장 작은 네모(아기가 적은 곳)는 어디인가요?")), textInput("q2", "", placeholder = "지역 이름 쓰기"),
                p(strong("Q3. 아기가 적게 태어나는 지역의 초등학교는 10년 뒤 어떻게 변할까요?")), textAreaInput("q3", "", placeholder = "예: 학생 수가 줄어서 빈 교실이 생길 것 같아요.", rows = 2),
                actionButton("save_worksheet_1", "탐구 노트 저장", class = "btn btn-info w-100 mt-2")
            )
        )
    )
  ),
  
  # ------------------------------------------------------------------
  # TAB 2: 3–4차시 (버블맵 + 산점도)
  # ------------------------------------------------------------------
  tabPanel(
    "🗺️ 2단계: 지도 속 인구 비밀 찾기",
    div(class = "container-fluid p-4",
        div(class = "mb-3",
            h4("🕵️‍♀️ 데이터 돋보기: 우리 지역 자세히 보기"),
            p("지도의 색깔을 바꿔보며 인구의 특징을 찾아보고, 산점도에서 두 가지 정보가 어떤 관계인지 추리해봅시다.")
        ),
        
        # [2] 버블맵 섹션
        div(class = "card border-light shadow-sm mb-4",
            div(class = "card-header bg-light", 
                h4("🗺️ [버블맵] 원의 크기로 보는 인구 지도", style="font-weight:bold; color:#2c3e50; margin:0;")
            ),
            div(class = "card-body",
                fluidRow(
                  column(3, 
                         div(class="alert alert-secondary",
                             h6(strong("🛠️ 지도 설정")),
                             selectInput("map_var", "지도 색칠하기:",
                                         choices = list("출생아 수"="births", "합계출산율"="fertility", "학급당 학생 수"="class_size", "고령화지수"="ageing_index"), 
                                         selected="births"),
                             p(class="small text-muted", "▲ 주제를 바꾸면 지도의 색깔이 변해요! (원 크기는 '출생아 수'로 고정)")
                         )
                  ),
                  column(9, leafletOutput("korea_map", height = "450px"))
                )
            )
        ),
        
        # [3] 산점도 섹션
        div(class = "card border-secondary shadow-sm mb-4",
            div(class = "card-body",
                fluidRow(
                  column(3,
                         div(class="alert alert-light border",
                             h6(strong("📊 그래프 설정")),
                             selectInput("x_var", "가로축(X) 선택:",
                                         choices = list("출생아 수"="births", "합계출산율"="fertility", "학급당 학생 수"="class_size", "고령화지수"="ageing_index"), 
                                         selected="class_size"),
                             selectInput("y_var", "세로축(Y) 선택:",
                                         choices = list("출생아 수"="births", "합계출산율"="fertility", "학급당 학생 수"="class_size", "고령화지수"="ageing_index"), 
                                         selected="ageing_index"),
                             hr(),
                             p(class="small text-muted", "💡 가로축과 세로축을 바꿔가며 점들이 어떻게 움직이는지 관찰해보세요.")
                         )
                  ),
                  column(9, 
                         plotlyOutput("scatter_plot", height = "400px"),
                         div(class = "small text-muted mt-2 text-center", "점 하나는 '지역'을 의미합니다. 점 위에 마우스를 올려보세요!")
                  )
                )
            )
        ),
        
        # ------------------------------------------------------------
        # [확장된] 2단계 탐구 노트 (다양한 인사이트 질문)
        # ------------------------------------------------------------
        div(class = "card border-info",
            div(class = "card-header bg-info text-white", h5("📝 2단계 탐구 노트: 데이터 탐정 리포트")),
            div(class = "card-body",
                
                # 미션 1: 관계 탐정
                h6(strong("🔎 미션 1: 관계 탐정"), style="color:#0984e3;"),
                div(class="row mb-3",
                    div(class="col-md-6",
                        strong("Q1. 그래프에서 [출생아 수]와 [학급당 학생 수]를 골라보세요. 두 정보는 어떤 관계인가요?"),
                        textAreaInput("ws34_q1", label=NULL, rows=2, placeholder="예: 아기가 많이 태어나는 지역은 학생 수도 많아서 교실이 붐비는 것 같아요.")
                    ),
                    div(class="col-md-6",
                        strong("Q2. 내가 자유롭게 두 가지 정보를 골라 비교해보고, 발견한 점을 적어보세요."),
                        textInput("ws34_q2_choice", "내가 고른 것: (예: 고령화지수 vs 교사1인당 학생수)"),
                        textAreaInput("ws34_q2_result", label=NULL, rows=2, placeholder="발견한 점: 시골 지역은 학생 수가 적어서 선생님이 학생을 더 꼼꼼히 봐줄 수 있을 것 같아요.")
                    )
                ),
                hr(),
                
                # 미션 2: 범인(아웃라이어) 찾기
                h6(strong("🕵️ 미션 2: 범인(아웃라이어) 찾기"), style="color:#d63031;"),
                div(class="row mb-3",
                    div(class="col-md-6",
                        strong("Q3. 다른 점들과 동떨어져 혼자 튀는 '특이한 지역'은 어디인가요?"),
                        textInput("ws34_q3", label=NULL, placeholder="지역 이름:")
                    ),
                    div(class="col-md-6",
                        strong("Q4. 그 지역은 왜 남들과 다를까요? 이유를 추리해보세요."),
                        textAreaInput("ws34_q4", label=NULL, rows=2, placeholder="추리: 그곳은 신도시라서 젊은 부부들이 많이 이사를 갔기 때문일 거예요.")
                    )
                ),
                hr(),
                
                # 미션 3: 미래 예측 & 정책 제안
                h6(strong("🔮 미션 3: 미래 예측 & 시장님 되어보기"), style="color:#fdcb6e;"),
                div(class="row mb-3",
                    div(class="col-md-12",
                        strong("Q5. '고령화 지수'가 높고 '출생아 수'가 적은 지역의 10년 뒤 학교는 어떤 모습일까요?"),
                        textAreaInput("ws34_q5", label=NULL, rows=2, placeholder="예: 학생이 없어서 학교가 문을 닫거나, 양로원과 학교가 합쳐질 수도 있어요.")
                    )
                ),
                div(class="row mb-3",
                    div(class="col-md-12",
                        strong("Q6. 만약 여러분이 그 지역의 시장님이라면, 어떤 정책을 만들고 싶나요?"),
                        textAreaInput("ws34_q6", label=NULL, rows=2, placeholder="정책: 아이를 낳으면 집을 공짜로 주거나, 학교에 재미있는 로봇 수업을 만들어서 학생들을 오게 할래요.")
                    )
                ),
                hr(),
                
                # 미션 4: 나의 선택
                h6(strong("🏡 미션 4: 나의 선택"), style="color:#00b894;"),
                div(class="row",
                    div(class="col-md-6",
                        strong("Q7. 데이터(학생 수, 출산율 등)를 보고 내가 살고 싶은 지역을 골라주세요."),
                        textInput("ws34_q7", label=NULL, placeholder="살고 싶은 지역:")
                    ),
                    div(class="col-md-6",
                        strong("Q8. 그 지역을 선택한 '데이터 근거'는 무엇인가요?"),
                        textAreaInput("ws34_q8", label=NULL, rows=2, placeholder="이유: 학급당 학생 수가 적당해서 선생님과 친하게 지낼 수 있을 것 같아서요.")
                    )
                ),
                
                br(),
                actionButton("save_ws34", "탐구 리포트 제출하기", class = "btn btn-info w-100")
            )
        )
    )
  ),
  
  # ------------------------------------------------------------------
  # TAB 3: 5–6차시 (나비차트 + 산점도)
  # ------------------------------------------------------------------
  tabPanel(
    "🦋 3단계: 넓어진 교실의 미스터리",
    div(class = "container-fluid p-4",
        div(class = "alert alert-primary shadow-sm",
            h4("🤔 미스터리 사건: 교실은 좋아졌는데 왜 바쁠까?", style="font-weight:bold; color:#0d6efd;"),
            p("데이터를 보니 학생 수가 줄어서 교실은 넓고 쾌적해졌대요.", br(),
              "그런데 왜 우리는 더 행복하다고 느끼지 못할까요? 아래 '나비 차트'에서 단서를 찾아보세요.")
        ),
        
        # [4] 나비 차트
        fluidRow(
          column(12,
                 div(class="card border-dark mb-4",
                     div(class="card-body",
                         fluidRow(
                           column(3,
                                  div(class="alert alert-light border",
                                      h6(strong("📊 데이터 선택")),
                                      radioButtons("butterfly_var", "오른쪽 날개 데이터:", 
                                                   choices = c("사교육 참여율(학원)"="PrivateEdu", "학교폭력 피해율"="Violence")),
                                      hr(),
                                      p(class="small text-muted", "파란 날개(왼쪽)는 '학생 수(환경)', 빨간 날개(오른쪽)는 '사교육/폭력(현실)'이에요.")
                                  )
                           ),
                           column(9,
                                  plotlyOutput("butterfly_plot", height="400px"),
                                  div(class="text-center mt-2", style="font-weight:bold; color:#d63384;",
                                      "💡 두 날개가 양쪽으로 멀어질수록, 학교 환경과 우리의 생활이 서로 다르게 가고 있다는 뜻이에요!")
                           )
                         )
                     )
                 )
          )
        ),
        
        # [5] 산점도 (만족도)
        fluidRow(
          column(12,
                 div(class="card border-secondary mb-4",
                     div(class="card-body",
                         plotlyOutput("scatter_satisfaction", height="400px"),
                         div(class="alert alert-light mt-2",
                             p("만약 학생 수가 줄어들 때 만족도가 올라간다면 점들이 왼쪽 위(↖️)로 가야 해요."),
                             p(strong("그런데 점들이 옆으로만 가거나 아래로 내려가고 있나요?"), "그렇다면 학생 수 감소가 행복을 보장해주진 않는다는 증거예요!")
                         )
                     )
                 )
          )
        ),
        div(class = "card border-success",
            div(class = "card-header bg-success text-white", h5("📝 3단계 탐구 노트 (최종 결론)")),
            div(class = "card-body",
                div(class="mb-3", strong("Q1. 나비 차트를 보고 발견한 사실은 무엇인가요?"),
                    textAreaInput("ws56_q1", label=NULL, rows=2, placeholder="답: 학생 수는 줄어드는데 학원을 다니는 비율은 오히려 늘어나고 있어요.")),
                div(class="mb-3", strong("Q2. 교실 환경이 좋아져도 우리가 행복하지 않은 '진짜 이유'는 무엇일까요?"),
                    textAreaInput("ws56_q3", label=NULL, rows=3, placeholder="내 생각: 경쟁이 더 심해져서 늦게까지 공부하느라 쉴 시간이 없기 때문입니다.")),
                actionButton("save_ws56", "최종 탐구 노트 저장", class = "btn btn-success w-100")
            )
        )
    )
  ),
  
  # ------------------------------------------------------------------
  # TAB 4: 7–8차시 (5개 시각화 선택)
  # ------------------------------------------------------------------
  tabPanel(
    "📢 4단계: 데이터로 세상에 알리기",
    div(class = "container-fluid p-4",
        
        div(class = "text-center mb-4",
            h2("📰 데이터 탐정 리포트 제작", style="font-weight:bold; color:#2c3e50;"),
            p("여러분이 찾은 중요한 사실(데이터)을 친구들과 어른들에게 알려주세요!", style="font-size:1.2rem;"),
            p("앞에서 본 그래프나 지도를 직접 캡처해서 포스터에 넣으면 훨씬 설득력이 높아집니다.")
        ),
        
        fluidRow(
          column(12,
                 div(class = "card border-primary shadow-sm mb-4",
                     div(class = "card-header bg-primary text-white", h4("1️⃣ 포스터 내용 기획하기")),
                     div(class = "card-body",
                         fluidRow(
                           column(6,
                                  textInput("poster_title", "📢 포스터 제목 (헤드라인)", 
                                            placeholder = "예: 교실은 넓어졌는데 왜 학원은 더 많이 갈까?"),
                                  
                                  selectInput("poster_evidence", "🔍 포스터에 넣을 '증거(데이터)' 선택하기", 
                                              choices = c(
                                                "1단계: [트리맵] 시도별 출생아 수 분포",
                                                "2단계: [버블맵] 대한민국 고령화율 버블 지도",
                                                "2단계: [산점도] 두 정보의 관계 확인하기",
                                                "3단계: [나비 차트] 환경 vs 현실",
                                                "3단계: [산점도] 학생 수와 만족도의 관계"
                                              )),
                                  p(class="text-muted small", "※ 선택한 그래프가 있는 탭으로 돌아가서 화면을 캡처(Shift+Win+S)해오세요!")
                           ),
                           column(6,
                                  textAreaInput("poster_content", "📝 데이터 설명 (기사 내용)", 
                                                rows = 4, placeholder = "예: 나비 차트를 보면 학생 수는 줄어드는데(파란색), 사교육은 점점 늘어나는 것(빨간색)을 볼 수 있습니다. 이것은 우리가 경쟁 때문에 힘들다는 증거입니다."),
                                  actionButton("save_plan", "기획안 저장하기", class="btn btn-primary w-100 mt-2")
                           )
                         )
                     )
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 div(class = "card border-info shadow-sm h-100",
                     div(class = "card-header bg-info text-white", h4("📸 꿀팁: 그래프 캡처하는 법")),
                     div(class = "card-body",
                         tags$ul(
                           tags$li(strong("윈도우(Windows):"), " 키보드의 [Shift] + [윈도우키] + [S] 를 동시에 누르세요."),
                           tags$li(strong("크롬북/노트북:"), " [Print Screen] 키를 누르거나 캡처 도구를 실행하세요."),
                           tags$li(strong("저장:"), " 캡처한 이미지를 캔바나 패들렛에 [Ctrl] + [V]로 붙여넣기 하세요.")
                         ),
                         div(class="alert alert-light border", 
                             "💡 1~3단계 탭을 다시 눌러서 원하는 그래프를 찾아 캡처해오세요!")
                     )
                 )
          ),
          column(6,
                 div(class = "card border-warning shadow-sm h-100",
                     div(class = "card-header bg-warning text-dark", h4("2️⃣ 디자인 도구 (무료)")),
                     div(class = "card-body text-center",
                         p("캡처한 그래프를 넣어서 멋진 뉴스나 포스터를 만들어보세요."),
                         tags$a(href="https://www.canva.com/ko_kr/create/posters/", target="_blank",
                                class="btn btn-outline-dark btn-lg w-100 mb-2", "🎨 캔바(Canva)로 만들기"),
                         tags$a(href="https://wrtn.ai/", target="_blank",
                                class="btn btn-outline-dark btn-lg w-100", "🤖 뤼튼(Wrtn)에게 글 다듬기")
                     )
                 )
          )
        ),
        
        br(), 
        
        fluidRow(
          column(12,
                 div(class = "card border-success shadow-sm",
                     div(class = "card-header bg-success text-white", h4("3️⃣ 데이터 갤러리 (전시회)")),
                     div(class = "card-body",
                         h5("여러분이 만든 '데이터 포스터'를 아래 패들렛에 올려주세요."),
                         p("1. [+] 버튼 누르기 → 2. 제목 쓰기 → 3. 만든 이미지 업로드"),
                         
                         tags$iframe(src = "https://padlet.com/hjs9959/padlet-j45y4wmsttvvj1qf", 
                                     height = "600px", width = "100%", style = "border:none; border-radius: 10px;"),
                         
                         div(class="text-center mt-3",
                             tags$a(href = "https://padlet.com/hjs9959/padlet-j45y4wmsttvvj1qf", target = "_blank",
                                    class = "btn btn-success", "🚀 패들렛 새 창으로 열기")
                         )
                     )
                 )
          )
        )
    )
  )
)

## -------------------------------------------------------------------
## 3. SERVER LOGIC
## -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # [1-2차시] 챗봇 로직 (NO SIMULATION)
  chat_history_1 <- reactiveVal(list(p(class = "text-muted", "📝 질문 연습장입니다.")))
  chat_history_2 <- reactiveVal(list(p(class = "text-muted", "📝 질문 연습장입니다.")))
  
  observeEvent(input$send_mission1, {
    user_msg <- input$input_mission1
    if (nzchar(user_msg)) {
      current <- chat_history_1()
      current <- append(current, tags$p(tags$b("👤 내 질문: "), user_msg))
      current <- append(current, tags$div(style='color:#e67e22; font-size:0.9rem; margin-bottom:10px;', "👉 위 '우리아이AI' 링크를 클릭해서 이 질문을 AI에게 직접 물어보세요!"))
      chat_history_1(current)
      updateTextInput(session, "input_mission1", value = "")
    }
  })
  
  observeEvent(input$send_mission2, {
    user_msg <- input$input_mission2
    if (nzchar(user_msg)) {
      current <- chat_history_2()
      current <- append(current, tags$p(tags$b("👤 내 질문: "), user_msg))
      current <- append(current, tags$div(style='color:#2ecc71; font-size:0.9rem; margin-bottom:10px;', "👉 위 '우리아이AI' 링크를 클릭해서 이 질문을 AI에게 직접 물어보세요!"))
      chat_history_2(current)
      updateTextInput(session, "input_mission2", value = "")
    }
  })
  
  output$chat_output_1 <- renderUI({ tags$div(chat_history_1()) })
  output$chat_output_2 <- renderUI({ tags$div(chat_history_2()) })
  
  # [1] 트리맵 (그래프 내부 제목)
  output$treemap_plot <- renderPlotly({
    plot_ly(data = province_data, type = "treemap", labels = ~region, parents = rep("대한민국", nrow(province_data)),
            values = ~births, textinfo = "label+value", marker = list(colorscale = "Blues")) %>% 
      layout(
        title = list(text = "<b>📊 [트리맵] 시도별 출생아 수 분포</b>", font = list(size = 20, color="black")),
        margin = list(t = 50)
      )
  })
  
  # [3-4차시] 지도
  color_pal <- reactive({
    var <- input$map_var
    colorNumeric(palette = "YlOrRd", domain = province_data[[var]])
  })
  
  output$korea_map <- renderLeaflet({
    pal <- color_pal()
    label_vec <- c(births="출생아 수", fertility="합계출산율", class_size="학급당 학생 수", teacher_ratio="교사 1인당 학생 수", ageing_index="고령화지수")
    leaflet(data = province_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = ~sqrt(births) / 20, 
        color = pal(province_data[[input$map_var]]),
        fillColor = pal(province_data[[input$map_var]]),
        fillOpacity = 0.8, stroke = TRUE,
        popup = ~paste0("<strong>", region, "</strong><br>", label_vec[[input$map_var]], ": ", province_data[[input$map_var]])
      ) %>%
      addLegend(position = "bottomright", pal = pal, values = province_data[[input$map_var]],
                title = label_vec[[input$map_var]], opacity = 0.7)
  })
  
  # [3] 산점도 (동적 내부 제목)
  output$scatter_plot <- renderPlotly({
    pretty_names <- c(births="👶 출생아 수", fertility="🤰 합계출산율", class_size="🏫 학급당 학생 수", teacher_ratio="👩‍🏫 교사 1명당 학생 수", ageing_index="🧓 고령화지수")
    xvar <- input$x_var; yvar <- input$y_var
    
    # 동적 제목 생성
    dynamic_title <- paste0("<b>📈 [산점도] ", pretty_names[[xvar]], " vs ", pretty_names[[yvar]], "</b>")
    
    p <- ggplot(province_data, aes_string(x = xvar, y = yvar, label = "region")) +
      geom_point(aes(color = region), size = 5, alpha = 0.8) +
      geom_text(vjust = 1.5, size = 3) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "#1f77b4") +
      labs(x = pretty_names[[xvar]], y = pretty_names[[yvar]]) +
      theme_minimal(base_size = 14) + theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("label", xvar, yvar)) %>%
      layout(title = list(text = dynamic_title, font = list(size = 20, color="black")), margin = list(t = 60))
  })
  
  # [4] 나비 차트 (그래프 내부 제목)
  output$butterfly_plot <- renderPlotly({
    right_var <- input$butterfly_var 
    right_label <- ifelse(right_var == "PrivateEdu", "사교육 참여율(%)", "학교폭력 피해율(‰)")
    bf_data <- trend_data %>%
      select(Year, ClassSize, all_of(right_var)) %>%
      mutate(Left = -ClassSize, Right = .[[right_var]])
    
    plot_ly(bf_data) %>%
      add_trace(x = ~Left, y = ~Year, type = 'bar', orientation = 'h', name = '학급당 학생 수 (감소)',
                marker = list(color = '#3498db'),
                text = ~paste0(ClassSize, "명"), hoverinfo = "text") %>%
      add_trace(x = ~Right, y = ~Year, type = 'bar', orientation = 'h', name = paste0(right_label, " (증가)"),
                marker = list(color = '#e74c3c'),
                text = ~paste0(get(right_var)), hoverinfo = "text") %>%
      layout(
        title = list(text = "<b>🦋 [나비 차트] 환경 vs 현실</b>", font = list(size = 20, color="black")),
        barmode = 'overlay',
        xaxis = list(title = "← 교실 환경 개선  |  부정적 요인 증가 →", showticklabels = FALSE), 
        yaxis = list(title = "연도", tickmode="linear"),
        legend = list(orientation = "h", x = 0.1, y = 1.1),
        margin = list(l = 100, r = 20, t = 60, b = 30)
      )
  })
  
  # [5] 산점도 (만족도) - 내부 제목 + 모든 연도 표시
  output$scatter_satisfaction <- renderPlotly({
    p <- ggplot(trend_data, aes(x = ClassSize, y = Satisfaction)) +
      geom_point(aes(color = factor(Year)), size = 5) +
      geom_path(color = "gray", arrow = arrow(length = unit(0.2, "cm"))) +
      # [수정] 모든 연도 텍스트 표시
      geom_text(aes(label = Year), vjust = -1.2, size = 3.5, fontface="bold") +
      labs(x = "🏫 학급당 학생 수 (명)", y = "😊 학교생활 만족도 (점)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(
        title = list(text = "<b>📉 [산점도] 학생 수와 만족도의 관계</b>", font = list(size = 20, color="black")),
        margin = list(t = 50)
      )
  })
  
  # 알림 이벤트
  observeEvent(input$save_worksheet_1, { showNotification("✅ 1단계 탐구 노트 저장 완료!", type = "message") })
  observeEvent(input$save_ws34, { showNotification("✅ 2단계 탐구 노트 저장 완료!", type = "message") })
  observeEvent(input$save_ws56, { showNotification("✅ 최종 탐구 노트 저장 완료!", type = "message") })
  observeEvent(input$save_plan, { showNotification("✅ 포스터 기획안이 저장되었습니다! 이제 그래프를 캡처해서 포스터를 만들어보세요.", type = "message") })
}

## -------------------------------------------------------------------
## Launch the Shiny application
shinyApp(ui = ui, server = server)