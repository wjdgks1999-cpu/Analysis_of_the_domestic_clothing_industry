# 역할: 객체·열·식별자 계약을 확인하며 업무 원문을 오류 메시지에 넣지 않는다.
require_columns <- function(data, columns, label) {
  if (!is.data.frame(data) || !all(columns %in% names(data))) {
    stop(paste(label, "자료의 필수 열 또는 표 형식을 확인하세요."), call. = FALSE)
  }
}

# 역할: 파일 안의 객체를 독립 환경에 읽어 전역 변수 덮어쓰기를 방지한다.
load_context <- function(path) {
  if (!nzchar(path) || !file.exists(path)) stop("데이터·모형 문맥 파일을 지정하세요.", call. = FALSE)
  storage <- new.env(parent = emptyenv())
  load(path, envir = storage)
  prepare_context(as.list(storage))
}

# 역할: 기존 외부 객체를 검증하고 조회용 전체 연도와 분석용 연도를 구분한다.
prepare_context <- function(context) {
  needed <- c("dat.basic", "dat.eco", "dat.eco.table", "dat.eco.na.table", "raw.data",
              "company.name.jaemu", "industry.code", "industry.code.all", "region.list",
              "method.selecting", "class.company", "staff.count.onoff", "gaeyo.select",
              "outlier.select", "jaemu.select", "alter", "ai.select", "naming", "naming.ai.var",
              "ai.list", "index.infor")
  if (!all(needed %in% names(context))) {
    stop(paste("문맥 객체 누락:", paste(setdiff(needed, names(context)), collapse = ", ")), call. = FALSE)
  }
  context <- context[needed]
  basic_columns <- c("ID", "기업명", "사업자번호", "대분류", "중분류", "소분류", "세분류", "세세분류",
                     "주요사업(사업목적)", "시도", "주소", "종업원수", "개인법인구분", "공기업구분", "상품코드1",
                     paste0("주요사업", 1:5), paste0("상품명", 1:5), paste0("매출처", 1:5))
  require_columns(context$dat.basic, basic_columns, "기업 개요")
  require_columns(context$dat.eco, c("ID", "결산일자", "유동자산", "자본총계", "매출액", "영업활동후현금흐름", unname(context$jaemu.select)), "재무")
  require_columns(context$dat.eco.table, c("ID", "결산년도"), "연도 보유")
  require_columns(context$dat.eco.na.table, c("ID", "결측치"), "재무 결측 현황")
  require_columns(context$company.name.jaemu, c("ID", "기업명"), "기업명")
  require_columns(context$raw.data, "ID", "다운로드 원본")
  for (name in c("industry.code", "industry.code.all")) {
    require_columns(context[[name]], c("대분류", "중분류", "소분류", "세분류", "세세분류"), "산업분류")
  }
  for (name in c("dat.basic", "dat.eco.table", "dat.eco.na.table", "company.name.jaemu")) {
    if (anyNA(context[[name]]$ID) || anyDuplicated(context[[name]]$ID)) {
      stop(paste(name, "자료의 ID는 결측 없이 고유해야 합니다."), call. = FALSE)
    }
  }
  finance <- context$dat.eco
  if (anyNA(finance[c("ID", "결산일자")]) || anyDuplicated(finance[c("ID", "결산일자")])) {
    stop("재무 자료의 기업·연도 조합은 결측 없이 고유해야 합니다.", call. = FALSE)
  }
  if (!nrow(context$dat.basic) || !nrow(finance) || !is.numeric(context$dat.basic$종업원수)) {
    stop("기업·재무 자료와 숫자형 종업원 수가 필요합니다.", call. = FALSE)
  }
  for (name in unique(c("유동자산", "자본총계", "매출액", "영업활동후현금흐름", unname(context$jaemu.select)))) {
    if (!is.numeric(finance[[name]])) stop("재무지표는 숫자형이어야 합니다.", call. = FALSE)
  }
  if (length(context$naming.ai.var) != 22L || anyNA(context$naming.ai.var) || anyDuplicated(context$naming.ai.var) || length(context$ai.list) != 5L || length(context$naming) != 7L) {
    stop("모형의 변수 순서·모형 수 또는 결측 현황 이름을 확인하세요.", call. = FALSE)
  }
  years <- unique(as.character(finance$결산일자))
  if (any(!grepl("^[0-9]{4}$", years))) stop("결산일자는 네 자리 연도여야 합니다.", call. = FALSE)
  context$available_years <- sort(years)
  context$finance.date <- setdiff(context$available_years, c("2016", "2022"))
  context
}
