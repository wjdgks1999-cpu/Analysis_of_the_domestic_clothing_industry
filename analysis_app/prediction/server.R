# 역할: 실제 모형 호출과 테스트용 대체 모형의 계약을 한 곳에 정의한다.
default_model_adapters <- function() {
  list(probability = model_probabilities, performance = training_performance, importance = model_importance)
}

# 역할: 버튼 시점의 입력을 저장하고 모형 변경·확률·학습 성능·변수 중요도를 연결한다.
register_prediction <- function(input, output, session, context, adapters, authorize) {
  selected_model <- shiny::reactive({ authorize(); shiny::req(input$ai.selecting); context$ai.list[[model_index(input$ai.selecting)]] })
  data <- shiny::eventReactive(input$start.pred, {
    authorize()
    values <- vapply(seq_len(22L), function(index) {
      # 역할: 화면 입력을 기존 ai1부터 ai22 순서로 수집한다.
      value <- input[[paste0("ai", index)]]
      if (length(value) != 1L) NA_real_ else as.numeric(value)
    }, numeric(1))
    ui_result(prediction_frame(values, context$naming.ai.var), "재무지표 22개에 유효한 숫자를 입력하세요.")
  })
  probabilities <- shiny::reactive({ ui_result(adapters$probability(selected_model(), data()), "모형과 입력 변수의 호환성을 확인하세요.") })
  output$ai.infor <- shiny::renderText({ paste0("◎ 정보 : 선택하신 AI는 ", input$ai.selecting, " 기업들의 재무정보를 코로나 전후로 예측합니다.") })
  output$now.predict.data <- DT::renderDataTable({
    values <- data()
    DT::datatable(data.frame(재무정보 = names(values), 입력값 = paste0(round(unlist(values, use.names = FALSE) * 100, 2), "%")), options = list(dom = "tp"))
  })
  output$index.informing <- DT::renderDataTable({ authorize(); DT::datatable(context$index.infor) })
  output$before.resulting <- shiny::renderText({ paste0("△ 해당 재무정보가 코로나 이전의 재무일 확률은 ", round(probabilities()[["이전"]] * 100, 2), "%로 예측됩니다.") })
  output$after.resulting <- shiny::renderText({ paste0("△ 해당 재무정보가 코로나 이후의 재무일 확률은 ", round(probabilities()[["이후"]] * 100, 2), "%로 예측됩니다.") })
  output$ai.performance <- shiny::renderPrint({
    cat("학습 데이터 기준 성능입니다. 별도 평가 데이터의 성능이 아닙니다.\n")
    print(ui_result(adapters$performance(selected_model()), "저장 모형의 학습 자료와 실행 패키지를 확인하세요."))
  })
  output$ai.variable.importance <- shiny::renderPlot({
    importance <- ui_result(adapters$importance(selected_model()), "저장 모형의 변수 중요도를 확인하세요.")
    ggplot2::ggplot(importance, ggplot2::aes(x = reorder(variables, importance), y = importance)) +
      ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::xlab("재무정보") + ggplot2::ylab("중요도") +
      ggplot2::labs(title = "AI가 생각하는 변수 중요도") + ggplot2::theme_minimal()
  }, res = 96)
  list(data = data, probabilities = probabilities)
}
