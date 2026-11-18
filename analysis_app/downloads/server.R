# 역할: 세 다운로드에 동일한 원본 기업 선택 규칙을 적용하고 ID 포함 차이는 보존한다.
register_downloads <- function(input, output, session, context, basic, overview, financial) {
  targets <- list(basic = basic, gaeyo = overview, jaemu = financial)
  filenames <- c(basic = "extraction.xlsx", gaeyo = "summary.xlsx", jaemu = "finance.xlsx")
  for (kind in names(targets)) local({
    current_kind <- kind
    data_source <- targets[[kind]]
    id <- paste0("download.", kind)
    shinyjs::disable(id)
    shiny::observe({
      data <- tryCatch(data_source(), error = function(error) {
        # 역할: 조회 대기·실패 상태에서는 다운로드 버튼을 비활성화한다.
        NULL
      })
      shinyjs::toggleState(id, condition = is.data.frame(data) && nrow(data) > 0L)
    })
    output[[id]] <- shiny::downloadHandler(
      # 역할: 기존 기능별 다운로드 파일명을 반환한다.
      filename = function() filenames[[current_kind]],
      # 역할: 사용자가 요청한 임시 출력 경로에 선택 기업의 원본을 저장한다.
      content = function(file) {
        data <- data_source()
        shiny::req(nrow(data) > 0L)
        write_export(export_data(context$raw.data, data$ID, include_id = current_kind == "basic"), file)
      }
    )
  })
  invisible(NULL)
}
