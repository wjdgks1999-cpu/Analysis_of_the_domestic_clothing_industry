# 역할: 기업 ID로 두 연도를 짝짓고 유효한 쌍과 제외된 쌍의 수를 함께 반환한다.
paired_values <- function(finance, field, first, second) {
  if (length(first) != 1L || length(second) != 1L || first == second) stop("서로 다른 두 분석 연도를 선택하세요.", call. = FALSE)
  require_columns(finance, c("ID", "결산일자", field), "가설검정")
  if (anyDuplicated(finance[c("ID", "결산일자")])) stop("기업·연도 중복 자료는 검정에 사용할 수 없습니다.", call. = FALSE)
  a <- finance[as.character(finance$결산일자) == first, c("ID", "결산일자", field), drop = FALSE]
  b <- finance[as.character(finance$결산일자) == second, c("ID", "결산일자", field), drop = FALSE]
  paired <- dplyr::inner_join(a, b, by = "ID")
  names(paired)[2:5] <- c("a.date", "a.value", "b.date", "b.value")
  valid <- is.finite(paired$a.value) & is.finite(paired$b.value)
  list(data = paired[valid, , drop = FALSE], matched = nrow(paired), excluded = sum(!valid))
}

# 역할: 실제 검정에 사용되는 한쪽 유효 표본의 기술통계를 계산한다.
paired_summary <- function(values) {
  if (!length(values)) return(data.frame(표본수 = 0L, 최소값 = NA_real_, 중앙값 = NA_real_, 평균 = NA_real_, 최대값 = NA_real_, 표준편차 = NA_real_))
  data.frame(표본수 = length(values), 최소값 = min(values), 중앙값 = median(values), 평균 = round(mean(values), 1), 최대값 = max(values), 표준편차 = round(stats::sd(values), 1))
}

# 역할: 원래 두 연도의 비교 방향과 대립가설을 유지해 대응표본 검정을 수행한다.
paired_test <- function(pair, alternative) {
  if (!nrow(pair$data)) stop("두 연도에 공통으로 존재하는 유효한 쌍이 없습니다.", call. = FALSE)
  if (all(pair$data$a.value == pair$data$b.value)) stop("모든 유효한 쌍의 차이가 0이므로 검정 결과를 해석할 수 없습니다.", call. = FALSE)
  stats::wilcox.test(pair$data$a.value, pair$data$b.value, paired = TRUE, alternative = alternative)
}
