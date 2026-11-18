# 역할: 추출 버튼 시점의 조건을 고정하고 개요·재무·분류표 출력을 연결한다.
register_extraction <- function(input, output, session, context, authorize) {
  selection <- shiny::eventReactive(input$extracting, {
    authorize()
    ui_result(extract_data(context, shiny::reactiveValuesToList(input)), "검색 조건이나 정규식 형식을 확인하세요.")
  })
  basic <- shiny::reactive({ shiny::req(selection()); selection()$basic })
  finance <- shiny::reactive({ shiny::req(selection()); selection()$finance })
  output$na.table <- DT::renderDataTable({
    shiny::req(selection())
    DT::datatable(missing_summary(selection(), context$naming), options = list(dom = "tp"))
  })
  output$sesebar <- shiny::renderPlot({ count_plot(overview_counts(basic(), "세세분류"), "세세분류") }, res = 96)
  output$main.industry <- shiny::renderPlot({ count_plot(overview_counts(basic(), "주요사업"), "주요사업") }, res = 96)
  output$selproduct <- shiny::renderPlot({ count_plot(overview_counts(basic(), "상품명"), "상품명") }, res = 96)
  fields <- c(jabon.ker = "자본총계", income.ker = "매출액", money.ker = "영업활동후현금흐름")
  for (id in names(fields)) local({
    output_id <- id
    field <- fields[[id]]
    output[[output_id]] <- shiny::renderPlot({
      data <- finance()
      data <- data[as.character(data$결산일자) %in% context$finance.date, , drop = FALSE]
      data <- filter_outliers(data, field, by_year = FALSE, inclusive = FALSE)
      finance_plot(data, field, paste0(field, "(단위:백만원)"))
    }, res = 96)
  })
  for (index in seq_len(5L)) local({
    column <- c("대분류", "중분류", "소분류", "세분류", "세세분류")[[index]]
    output_id <- c("w", "ww", "www", "wwww", "wwwww")[[index]]
    output[[output_id]] <- DT::renderDataTable({
      authorize()
      table <- data.frame(unique(context$industry.code.all[[column]]))
      names(table) <- column
      DT::datatable(table, options = list(dom = "tp"), filter = list(position = "top"))
    })
  })
  big <- shiny::reactive({ authorize(); shiny::req(input$select.big); dplyr::filter(context$industry.code, 대분류 == input$select.big) })
  shiny::observeEvent(big(), { shiny::updateSelectInput(session, "select.mid", choices = unique(big()$중분류)) })
  mid <- shiny::reactive({ shiny::req(input$select.mid); dplyr::filter(big(), 중분류 == input$select.mid) })
  shiny::observeEvent(mid(), { shiny::updateSelectInput(session, "select.small", choices = unique(mid()$소분류)) })
  small <- shiny::reactive({ shiny::req(input$select.small); dplyr::filter(mid(), 소분류 == input$select.small) })
  shiny::observeEvent(small(), { shiny::updateSelectInput(session, "select.se", choices = unique(small()$세분류)) })
  output$industry.table <- DT::renderDataTable({
    shiny::req(input$select.se)
    DT::datatable(dplyr::filter(small(), 세분류 == input$select.se), options = list(dom = "tp"))
  })
  list(selection = selection, basic = basic, finance = finance)
}
