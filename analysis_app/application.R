# 역할: 계산 오류를 업무 원문 노출 없이 화면 안내로 변환한다.
ui_result <- function(expression, message) {
  tryCatch(force(expression), error = function(error) {
    # 역할: 반응식의 대기 상태는 유지하고 실제 오류만 지정된 안내로 바꾼다.
    if (inherits(error, "shiny.silent.error")) stop(error)
    shiny::validate(shiny::need(FALSE, message))
  })
}

# 역할: 인증과 기능별 반응식을 세션마다 생성하고 공유 데이터는 읽기 전용으로 사용한다.
build_server <- function(context, credentials, adapters = default_model_adapters()) {
  # 역할: 하나의 접속 세션에 인증·조회·분석·다운로드 출력을 연결한다.
  function(input, output, session) {
    authentication <- shinymanager::secure_server(check_credentials = shinymanager::check_credentials(credentials))
    authorize <- shiny::reactive({ authorize_user(authentication$user) })
    extraction <- register_extraction(input, output, session, context, authorize)
    overview <- register_overview(input, output, session, extraction$basic)
    financial <- register_finance(input, output, session, extraction$finance, context)
    hypothesis <- register_hypothesis(input, output, session, extraction$finance, context$finance.date)
    prediction <- register_prediction(input, output, session, context, adapters, authorize)
    register_downloads(input, output, session, context, extraction$basic, overview$data, financial$data)
  }
}

# 역할: 명시적으로 제공된 문맥과 인증으로 앱을 조립하며 합성 자료를 자동 대입하지 않는다.
create_application <- function(context, credentials, adapters = default_model_adapters()) {
  context <- prepare_context(context)
  ui <- shinymanager::secure_app(build_ui(context))
  shiny::shinyApp(ui, build_server(context, credentials, adapters))
}
