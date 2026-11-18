# 역할: 개요 항목의 집계·클릭·선택 기업을 연결하며 렌더링 중 선택 상태를 변경하지 않는다.
register_overview <- function(input, output, session, basic) {
  selected <- shiny::reactiveVal(NULL)
  counts <- shiny::reactive({ shiny::req(input$select.gaeyo.inform); overview_counts(basic(), input$select.gaeyo.inform) })
  shiny::observeEvent(list(input$select.gaeyo.inform, basic()), { selected(NULL) }, ignoreInit = TRUE)
  shiny::observeEvent(input$click.gaeyo, {
    table <- counts()
    ordered <- levels(reorder(table$a, table$n))
    index <- round(input$click.gaeyo$y)
    if (length(index) == 1L && is.finite(index) && index >= 1L && index <= length(ordered)) selected(ordered[index])
  })
  selected_data <- shiny::reactive({ shiny::req(input$select.gaeyo.inform); overview_selection(basic(), input$select.gaeyo.inform, selected()) })
  output$gaeyo.bar <- shiny::renderPlot({ count_plot(counts(), input$select.gaeyo.inform, selected()) }, res = 96)
  output$gaeyo.table <- DT::renderDataTable({
    data <- selected_data()
    DT::datatable(data[, setdiff(names(data), "ID"), drop = FALSE])
  })
  list(data = selected_data, counts = counts, selected = selected)
}
