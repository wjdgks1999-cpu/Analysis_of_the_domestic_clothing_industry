# 역할: 동일 기업의 유효한 두 연도 쌍으로 그림·요약·검정을 일관되게 연결한다.
register_hypothesis <- function(input, output, session, finance, analysis_years) {
  pair <- shiny::reactive({
    shiny::req(input$met, input$g1.s, input$g2.s)
    shiny::validate(shiny::need(all(c(input$g1.s, input$g2.s) %in% analysis_years), "분석 가능한 연도를 선택하세요."))
    ui_result(paired_values(finance(), input$met, input$g1.s, input$g2.s), "서로 다른 두 연도와 고유한 기업·연도 자료가 필요합니다.")
  })
  bounds <- shiny::reactive({ whisker_bounds(c(pair()$data$a.value, pair()$data$b.value)) })
  output$g1plot <- shiny::renderPlot({ finance_plot(pair()$data, "a.value", paste(input$met, "(단위:백만원)"), bounds(), single = TRUE, color = "red") + ggplot2::labs(title = "대상년도1") })
  output$g2plot <- shiny::renderPlot({ finance_plot(pair()$data, "b.value", paste(input$met, "(단위:백만원)"), bounds(), single = TRUE, color = "blue") + ggplot2::labs(title = "대상년도2") })
  output$g1summary <- DT::renderDataTable({ DT::datatable(paired_summary(pair()$data$a.value), options = list(dom = "t")) })
  output$g2summary <- DT::renderDataTable({ DT::datatable(paired_summary(pair()$data$b.value), options = list(dom = "t")) })
  output$result.test <- shiny::renderPrint({
    current <- pair()
    cat("대응 기업:", current$matched, "/ 유효한 쌍:", nrow(current$data), "/ 결측·비유한 값 제외:", current$excluded, "\n")
    print(ui_result(paired_test(current, input$altern), "유효한 쌍이 없거나 모든 차이가 0입니다. 비교 조건을 확인하세요."))
  })
  list(pair = pair)
}
