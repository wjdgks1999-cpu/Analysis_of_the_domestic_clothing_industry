# 역할: 분석 연도 선택·이상치 옵션·브러시 선택과 기업 재무표를 연결한다.
register_finance <- function(input, output, session, finance, context) {
  years <- shiny::reactive({ sort(intersect(as.character(unique(finance()$결산일자)), context$finance.date)) })
  shiny::observeEvent(input$extracting, {
    shiny::updateSelectInput(session, "select.jaemu.year", choices = years())
    shiny::updateSelectInput(session, "g1.s", choices = years())
    shiny::updateSelectInput(session, "g2.s", choices = years())
  })
  values <- shiny::reactive({
    shiny::req(input$select.jaemu.inform)
    financial_values(finance(), input$select.jaemu.inform, intersect(input$select.jaemu.year, years()))
  })
  visible <- shiny::reactive({
    shiny::req(input$select.jaemu.outlier)
    if (input$select.jaemu.outlier == "포함") values() else filter_outliers(values(), "a", by_year = TRUE, inclusive = TRUE)
  })
  output$jaemu.hist <- shiny::renderPlot({ finance_plot(visible(), label = paste(input$select.jaemu.inform, "(단위:백만원)"), fill = if (input$select.jaemu.outlier == "포함") "grey" else NULL) }, res = 96)
  brushed <- shiny::reactive({
    data <- visible()
    if (is.null(input$brush.jaemu)) return(data[FALSE, , drop = FALSE])
    shiny::brushedPoints(data, input$brush.jaemu, xvar = "a")
  })
  selected_data <- shiny::reactive({
    selected <- brushed()
    all_finance <- finance()
    all_finance$결산일자 <- as.character(all_finance$결산일자)
    data <- dplyr::left_join(selected, all_finance, by = c("ID", "결산일자"))
    data$a <- NULL
    data <- dplyr::left_join(data, context$company.name.jaemu, by = "ID")
    dplyr::select(data, ID, 결산일자, 기업명, 유동자산:자본총계)
  })
  output$data.brush <- DT::renderDataTable({
    data <- selected_data()
    DT::datatable(data[, setdiff(names(data), "ID"), drop = FALSE])
  })
  list(data = selected_data, values = values, visible = visible, years = years)
}
