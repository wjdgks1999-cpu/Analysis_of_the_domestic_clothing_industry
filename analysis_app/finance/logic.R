# 역할: 그림을 만들지 않고 기존 상자그림 수염의 경계값을 계산한다.
whisker_bounds <- function(values) {
  values <- values[is.finite(values)]
  if (!length(values)) return(c(NA_real_, NA_real_))
  as.numeric(grDevices::boxplot.stats(values)$stats[c(1, 5)])
}

# 역할: 전체/연도별 경계와 경계값 포함 여부를 구분해 기존 이상치 차이를 보존한다.
filter_outliers <- function(data, value, by_year = FALSE, inclusive = TRUE) {
  if (!nrow(data)) return(data)
  groups <- if (by_year) unique(as.character(data$결산일자)) else "all"
  results <- lapply(groups, function(group) {
    # 역할: 해당 연도 또는 전체 자료에 지정된 경계 규칙을 적용한다.
    current <- if (by_year) data[as.character(data$결산일자) == group, , drop = FALSE] else data
    bounds <- whisker_bounds(current[[value]])
    values <- current[[value]]
    keep <- is.finite(values) & if (inclusive) values >= bounds[1] & values <= bounds[2] else values > bounds[1] & values < bounds[2]
    current[!is.na(keep) & keep, , drop = FALSE]
  })
  do.call(rbind, results)
}

# 역할: 선택 지표와 연도를 표준 세 열로 반환해 그림과 범위 선택에 공유한다.
financial_values <- function(finance, field, years) {
  require_columns(finance, c("ID", "결산일자", field), "재무")
  result <- finance[as.character(finance$결산일자) %in% years, c("ID", "결산일자", field), drop = FALSE]
  names(result)[3] <- "a"
  result$결산일자 <- as.character(result$결산일자)
  result
}

# 역할: 밀도를 계산할 표본이 부족한 경우 안내하고 충분한 연도만 그린다.
finance_plot <- function(data, value = "a", label, bounds = NULL, single = FALSE, color = "red", fill = NULL) {
  if (is.null(fill)) fill <- NA_character_
  valid <- is.finite(data[[value]])
  if (!is.null(bounds)) valid <- valid & data[[value]] >= bounds[1] & data[[value]] <= bounds[2]
  data <- data[!is.na(valid) & valid, , drop = FALSE]
  if (!single) {
    counts <- table(data$결산일자)
    data <- data[as.character(data$결산일자) %in% names(counts[counts >= 2]), , drop = FALSE]
  }
  shiny::validate(shiny::need(nrow(data) >= 2, "분포를 그릴 유효 표본이 부족합니다."))
  if (single) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[value]])) + ggplot2::geom_density(adjust = 1, alpha = 0.1, color = color, fill = "grey")
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[value]], colour = as.factor(결산일자))) + ggplot2::geom_density(adjust = 1, alpha = 0.1, fill = fill) + ggplot2::labs(colour = "년도")
  }
  plot <- plot + ggplot2::xlab(label) + ggplot2::scale_x_continuous(labels = scales::comma, limits = bounds)
  plot
}
