# 역할: 다섯 반복 열 또는 단일 열의 빈도를 기존처럼 중복 건수 기준으로 집계한다.
overview_counts <- function(data, field) {
  columns <- if (field %in% c("주요사업", "매출처", "상품명")) paste0(field, 1:5) else field
  require_columns(data, columns, "개요 집계")
  values <- unlist(lapply(data[columns], as.character), use.names = FALSE)
  frame <- data.frame(a = values[!is.na(values)])
  head(dplyr::count(frame, a, sort = TRUE), 20)
}

# 역할: 클릭한 항목과 일치하는 기업을 선택하고 기존 표시 열을 반환한다.
overview_selection <- function(data, field, selected) {
  columns <- if (field %in% c("주요사업", "매출처", "상품명")) paste0(field, 1:5) else field
  keep <- rep(FALSE, nrow(data))
  for (column in columns) keep <- keep | data[[column]] %in% selected
  output_columns <- c("ID", "기업명", "사업자번호", "주요사업(사업목적)", "종업원수", "개인법인구분", "공기업구분",
                      paste0("상품명", 1:5), paste0("매출처", 1:5), "주소")
  data[keep, output_columns, drop = FALSE]
}

# 역할: 기존 가로 빈도 그래프를 만들고 선택 항목의 색만 계산한다.
count_plot <- function(counts, label, selected = NULL) {
  shiny::validate(shiny::need(nrow(counts) > 0, "조회 조건에 해당하는 자료가 없습니다."))
  counts$selected <- ifelse(counts$a %in% selected, "yes", "no")
  ggplot2::ggplot(counts, ggplot2::aes(x = reorder(a, n), y = n, label = n, fill = selected)) +
    ggplot2::geom_col() + ggplot2::geom_text(hjust = 0) + ggplot2::coord_flip() +
    ggplot2::xlab(label) + ggplot2::ylab("빈도수") +
    ggplot2::scale_fill_manual(values = c(yes = "blue", no = "grey"), guide = "none")
}
