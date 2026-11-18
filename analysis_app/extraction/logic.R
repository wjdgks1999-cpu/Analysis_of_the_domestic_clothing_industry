# 역할: 공백 제거·or 분리·정규식 부분 일치라는 기존 검색 규칙을 적용한다.
filter_text <- function(data, column, query) {
  if (is.null(query) || !nzchar(query)) return(data)
  patterns <- strsplit(gsub(" ", "", query), "or")[[1]]
  values <- gsub(" ", "", as.character(data[[column]]))
  selected <- rep(FALSE, nrow(data))
  for (pattern in patterns) {
    matches <- tryCatch(stringr::str_detect(values, pattern), error = function(error) {
      # 역할: 원문 검색어를 노출하지 않고 정규식 오류를 안내한다.
      stop("검색 정규식의 형식을 확인하세요.", call. = FALSE)
    })
    selected <- selected | (!is.na(matches) & matches)
  }
  data[selected, , drop = FALSE]
}

# 역할: 기업 조건과 연도 보유 조건을 적용하되 선택 기업의 재무 연도는 모두 보존한다.
extract_data <- function(context, criteria) {
  basic <- context$dat.basic
  columns <- c(bigcluster = "대분류", midcluster = "중분류", smallcluster = "소분류",
               secluster = "세분류", sesecluster = "세세분류", industry = "주요사업(사업목적)")
  for (name in names(columns)) basic <- filter_text(basic, columns[[name]], criteria[[name]])
  if (length(criteria$region)) basic <- basic[basic$시도 %in% criteria$region, , drop = FALSE]
  if (identical(criteria$staff.count.onoff.select, "미포함")) {
    bounds <- criteria$staff.count.slider
    if (length(bounds) != 2L || any(!is.finite(bounds))) stop("종업원 수 범위를 확인하세요.", call. = FALSE)
    keep <- !is.na(basic$종업원수) & basic$종업원수 >= bounds[1] & basic$종업원수 <= bounds[2]
    basic <- basic[keep, , drop = FALSE]
  }
  if (!is.null(criteria$class.company.select) && criteria$class.company.select != "전체") {
    basic <- basic[!is.na(basic$개인법인구분) & basic$개인법인구분 == criteria$class.company.select, , drop = FALSE]
  }
  finance <- context$dat.eco
  if (length(criteria$year.select)) {
    availability <- context$dat.eco.table
    matches <- lapply(criteria$year.select, function(year) {
      # 역할: 특정 연도를 보유한 기업을 기존 문자열 조건으로 식별한다.
      result <- stringr::str_detect(as.character(availability$결산년도), year)
      !is.na(result) & result
    })
    keep <- Reduce(if (identical(criteria$method.select, "And")) `&` else `|`, matches)
    finance <- finance[finance$ID %in% availability$ID[keep], , drop = FALSE]
  }
  basic <- basic[basic$ID %in% finance$ID, , drop = FALSE]
  finance <- finance[finance$ID %in% basic$ID, , drop = FALSE]
  missing <- context$dat.eco.na.table[context$dat.eco.na.table$ID %in% finance$ID, , drop = FALSE]
  list(basic = basic, finance = finance, missing = missing)
}

# 역할: 기존 일곱 결측 항목을 같은 분모로 집계하며 빈 결과의 비율을 정의한다.
missing_summary <- function(selection, labels) {
  basic <- selection$basic
  columns <- c("세세분류", "주요사업(사업목적)", "주소", "종업원수", "상품코드1", "매출처1")
  counts <- c(vapply(columns, function(column) {
    # 역할: 해당 개요 열의 결측 개수를 계산한다.
    sum(is.na(basic[[column]]))
  }, integer(1)), sum(is.na(selection$missing$결측치)))
  total <- nrow(basic)
  data.frame(내용 = labels, 결측치수 = counts, 유효수 = total - counts, 전체수 = total,
             `결측치 퍼센트` = if (total) paste0(round(counts / total * 100, 1), "%") else "해당 없음",
             check.names = FALSE, row.names = NULL)
}
