# 역할: 업무 데이터나 원래 모형을 포함하지 않는 합성 문맥을 생성한다.
synthetic_context <- function() {
  ids <- sprintf("S%02d", seq_len(15L))
  basic <- data.frame(ID = ids, 기업명 = paste("합성기업", seq_along(ids)), 사업자번호 = paste0("SYNTHETIC-", ids),
                      대분류 = "제조업", 중분류 = "의복 제조업", 소분류 = "겉옷 제조업", 세분류 = "일반 의복", 세세분류 = "합성 분류",
                      `주요사업(사업목적)` = "의복 제조", 시도 = rep(c("서울", "부산", "대구"), 5), 주소 = "합성 주소",
                      종업원수 = c(NA, seq_len(14) * 10), 개인법인구분 = rep(c("법인", "개인", "법인"), 5), 공기업구분 = "아니오", 상품코드1 = "SYNTHETIC", check.names = FALSE)
  for (index in seq_len(5)) {
    basic[[paste0("주요사업", index)]] <- if (index == 1L) "의복" else NA_character_
    basic[[paste0("상품명", index)]] <- if (index == 1L) "합성상품" else NA_character_
    basic[[paste0("매출처", index)]] <- if (index == 1L) "합성거래처" else NA_character_
  }
  basic$주요사업2[1] <- "의복"
  finance <- expand.grid(ID = ids, 결산일자 = as.character(2016:2022), stringsAsFactors = FALSE)
  finance <- finance[!(finance$ID == "S02" & finance$결산일자 %in% c("2016", "2017", "2022")) &
                       !(finance$ID == "S03" & finance$결산일자 != "2020") &
                       !(finance$ID == "S04" & !finance$결산일자 %in% c("2016", "2022")), ]
  value <- match(finance$ID, ids) * 100 + (as.integer(finance$결산일자) - 2016) * 13
  finance$유동자산 <- value
  finance$매출액 <- value * 2
  finance$영업활동후현금흐름 <- value * 0.1
  finance$자본총계 <- value * 0.7
  availability <- data.frame(ID = ids, 결산년도 = vapply(ids, function(id) {
    # 역할: 합성 기업이 보유한 연도를 기존 보유 현황 형식으로 묶는다.
    paste(finance$결산일자[finance$ID == id], collapse = ",")
  }, character(1)))
  industry <- unique(basic[c("대분류", "중분류", "소분류", "세분류", "세세분류")])
  list(dat.basic = basic, dat.eco = finance, dat.eco.table = availability,
       dat.eco.na.table = data.frame(ID = ids, 결측치 = c(NA, rep(0, 14))),
       raw.data = merge(basic, finance, by = "ID"), company.name.jaemu = basic[c("ID", "기업명")],
       industry.code = industry, industry.code.all = industry, region.list = unique(basic$시도),
       method.selecting = c("And", "Or"), finance.date = as.character(2017:2021),
       class.company = c("전체", "법인", "개인"), staff.count.onoff = c("포함", "미포함"),
       gaeyo.select = c("시도", "개인법인구분", "주요사업", "매출처", "상품명"),
       outlier.select = c("포함", "미포함"), jaemu.select = c("매출액", "자본총계", "영업활동후현금흐름"),
       alter = c("two.sided", "greater", "less"),
       ai.select = c("전체대상", "도매 및 상품 중개업", "소매업; 자동차 제외", "의복, 의복 액세서리 및 모피제품 제조업", "섬유제품 제조업; 의복 제외"),
       naming = c("세세분류", "주요사업", "주소", "종업원 수", "상품", "매출처", "재무정보"),
       naming.ai.var = paste0("synthetic_feature_", seq_len(22)), ai.list = as.list(seq_len(5)),
       index.infor = data.frame(지표 = paste0("합성지표", seq_len(22)), 설명 = "테스트용이며 실제 모형이 아님"))
}

# 역할: 외부 예측 없이 모형 선택과 입력 연결만 검증하는 대체 함수를 만든다.
synthetic_adapters <- function() {
  list(probability = function(model, data) {
    # 역할: 합성 모형 번호로 정해진 확률을 반환한다.
    data.frame(이전 = rep(model / 10, nrow(data)), 이후 = rep(1 - model / 10, nrow(data)))
  }, performance = function(model) {
    # 역할: 실제 성능 지표로 오인하지 않도록 합성 검사임을 반환한다.
    "합성 모형 연결 확인: 실제 학습 성능이 아닙니다."
  }, importance = function(model) {
    # 역할: 그래프 연결만 확인할 고정 합성 중요도를 반환한다.
    data.frame(variables = c("synthetic_feature_1", "synthetic_feature_2"), importance = c(10, 20))
  })
}

# 역할: 필터를 지정하지 않은 기본 조회 조건을 제공한다.
default_criteria <- function() {
  list(bigcluster = "", midcluster = "", smallcluster = "", secluster = "", sesecluster = "", industry = "",
       region = NULL, staff.count.onoff.select = "포함", staff.count.slider = c(0, 140), class.company.select = "전체", year.select = NULL, method.select = "And")
}
