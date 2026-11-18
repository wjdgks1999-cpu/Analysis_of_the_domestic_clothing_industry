# 역할: 선택된 기업의 원본 모든 행·연도를 유지하고 기존 다운로드별 ID 포함 차이를 적용한다.
export_data <- function(raw, selected_ids, include_id = FALSE) {
  result <- raw[raw$ID %in% selected_ids, , drop = FALSE]
  if (!include_id) result <- result[, setdiff(names(result), "ID"), drop = FALSE]
  result
}

# 역할: 사용자 다운로드 요청으로 받은 임시 경로에만 Excel을 기록한다.
write_export <- function(data, path) {
  openxlsx::write.xlsx(data, path, overwrite = TRUE)
}
