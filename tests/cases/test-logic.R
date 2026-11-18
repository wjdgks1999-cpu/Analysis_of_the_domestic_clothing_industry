testthat::test_that("원래 추출 로직의 합성 기준 결과와 모든 기업·연도 선택이 일치한다", {
  baseline <- readRDS("../fixtures/extraction-baseline.rds")
  for (case in baseline) {
    actual <- app$extract_data(context, case$criteria)
    testthat::expect_identical(as.character(actual$basic$ID), case$basic_ids)
    testthat::expect_identical(paste(actual$finance$ID, actual$finance$결산일자), case$finance_keys)
  }
})

testthat::test_that("조회 연도와 분석 연도는 구분되고 입력 자료는 변경되지 않는다", {
  raw <- synthetic_context()
  before <- serialize(raw, NULL)
  prepared <- app$prepare_context(raw)
  testthat::expect_identical(prepared$available_years, as.character(2016:2022))
  testthat::expect_identical(prepared$finance.date, as.character(2017:2021))
  app$extract_data(prepared, default_criteria())
  testthat::expect_identical(serialize(raw, NULL), before)
})

testthat::test_that("문맥 계약은 누락·중복 ID·잘못된 재무 단위를 조기에 거부한다", {
  invalid <- synthetic_context(); invalid$ai.list <- NULL
  testthat::expect_error(app$prepare_context(invalid), "문맥 객체 누락")
  invalid <- synthetic_context(); invalid$dat.basic$ID[2] <- invalid$dat.basic$ID[1]
  testthat::expect_error(app$prepare_context(invalid), "고유")
  invalid <- synthetic_context(); invalid$dat.eco <- rbind(invalid$dat.eco, invalid$dat.eco[1, ])
  testthat::expect_error(app$prepare_context(invalid), "기업·연도")
  invalid <- synthetic_context(); invalid$dat.eco$매출액 <- as.character(invalid$dat.eco$매출액)
  testthat::expect_error(app$prepare_context(invalid), "숫자형")
  invalid <- synthetic_context(); invalid$naming.ai.var <- letters
  testthat::expect_error(app$prepare_context(invalid), "변수 순서")
})

testthat::test_that("문맥 로드는 파일을 보존하고 전역 객체를 덮어쓰지 않는다", {
  path <- tempfile(fileext=".RData")
  on.exit(unlink(path))
  storage <- list2env(synthetic_context())
  save(list=ls(storage),envir=storage,file=path)
  before <- readBin(path,"raw",n=file.info(path)$size)
  loaded <- app$load_context(path)
  testthat::expect_equal(nrow(loaded$dat.basic),15)
  testthat::expect_false(exists("dat.basic",envir=globalenv(),inherits=FALSE))
  testthat::expect_identical(readBin(path,"raw",n=file.info(path)$size),before)
  testthat::expect_error(app$load_context(""),"문맥 파일")
})

testthat::test_that("빈 조회와 잘못된 정규식은 안전하게 구분된다", {
  criteria <- default_criteria(); criteria$industry <- "없는합성사업"
  selection <- app$extract_data(context,criteria)
  testthat::expect_equal(nrow(selection$basic),0)
  summary <- app$missing_summary(selection,context$naming)
  testthat::expect_true(all(summary$전체수 == 0))
  testthat::expect_true(all(summary[["결측치 퍼센트"]] == "해당 없음"))
  testthat::expect_error(app$filter_text(context$dat.basic,"대분류","["),"정규식")
})

testthat::test_that("종업원 수 결측 포함 선택은 기존처럼 범위 제한도 적용하지 않는다", {
  criteria <- default_criteria(); criteria$staff.count.slider <- c(0,1)
  testthat::expect_equal(nrow(app$extract_data(context,criteria)$basic),15)
  criteria$staff.count.onoff.select <- "미포함"
  testthat::expect_equal(nrow(app$extract_data(context,criteria)$basic),0)
})

testthat::test_that("반복 개요 항목은 건수를 세되 클릭 결과의 기업은 중복되지 않는다", {
  counts <- app$overview_counts(context$dat.basic,"주요사업")
  testthat::expect_equal(counts$n[counts$a == "의복"],16)
  selected <- app$overview_selection(context$dat.basic,"주요사업","의복")
  testthat::expect_equal(nrow(selected),15)
  testthat::expect_false(anyDuplicated(selected$ID)>0)
  testthat::expect_equal(nrow(app$overview_selection(context$dat.basic,"시도",NULL)),0)
})

testthat::test_that("이상치의 전체·연도별 기준과 경계 포함 차이를 보존한다", {
  data <- data.frame(ID=1:12,결산일자=rep(c("2019","2020"),each=6),a=c(1:5,100,101:105,1000))
  strict <- app$filter_outliers(data,"a",by_year=FALSE,inclusive=FALSE)
  inclusive <- app$filter_outliers(data,"a",by_year=TRUE,inclusive=TRUE)
  testthat::expect_identical(strict$ID,2:10)
  testthat::expect_identical(inclusive$ID,c(1:5,7:11))
  testthat::expect_true(all(is.na(app$whisker_bounds(c(NA,Inf)))))
  testthat::expect_equal(nrow(app$filter_outliers(data[FALSE,],"a")),0)
})

testthat::test_that("대응 자료와 요약표는 같은 유효한 쌍을 사용한다", {
  finance <- data.frame(ID=c("a","b","c","a","b","c"),결산일자=rep(c("2019","2020"),each=3),지표=c(1,NA,5,3,4,9))
  pair <- app$paired_values(finance,"지표","2019","2020")
  testthat::expect_identical(pair$data$ID,c("a","c"))
  testthat::expect_equal(pair$matched,3)
  testthat::expect_equal(pair$excluded,1)
  testthat::expect_equal(app$paired_summary(pair$data$a.value)$표본수,2)
  actual <- app$paired_test(pair,"less")
  expected <- stats::wilcox.test(c(1,5),c(3,9),paired=TRUE,alternative="less")
  testthat::expect_equal(actual$p.value,expected$p.value)
  testthat::expect_equal(actual$statistic,expected$statistic)
})

testthat::test_that("동일 연도·중복 기업연도·빈 쌍·차이 없는 쌍은 검정을 막는다", {
  testthat::expect_error(app$paired_values(context$dat.eco,"매출액","2019","2019"),"서로 다른")
  duplicated <- rbind(context$dat.eco,context$dat.eco[1,])
  testthat::expect_error(app$paired_values(duplicated,"매출액","2019","2020"),"중복")
  empty <- list(data=data.frame(a.value=numeric(),b.value=numeric()))
  testthat::expect_error(app$paired_test(empty,"less"),"유효한 쌍")
  same <- list(data=data.frame(a.value=1:3,b.value=1:3))
  testthat::expect_error(app$paired_test(same,"less"),"차이가 0")
})

testthat::test_that("다운로드는 선택 기업의 모든 연도와 기존 ID 포함 차이를 유지한다", {
  raw <- context$raw.data; before <- serialize(raw,NULL)
  basic <- app$export_data(raw,"S01",TRUE)
  overview <- app$export_data(raw,"S01",FALSE)
  testthat::expect_identical(sort(as.character(basic$결산일자)),as.character(2016:2022))
  testthat::expect_true("ID" %in% names(basic))
  testthat::expect_false("ID" %in% names(overview))
  testthat::expect_identical(serialize(raw,NULL),before)
  path <- tempfile(fileext=".xlsx"); on.exit(unlink(path))
  app$write_export(overview,path)
  testthat::expect_equal(nrow(openxlsx::read.xlsx(path)),7)
})

testthat::test_that("예측은 22개 변수의 순서와 회전율을 포함한 0.01 변환을 유지한다", {
  data <- app$prediction_frame(1:22,context$naming.ai.var)
  testthat::expect_identical(names(data),context$naming.ai.var)
  testthat::expect_equal(unname(unlist(data)),(1:22)*0.01)
  testthat::expect_identical(vapply(context$ai.select,app$model_index,integer(1)),setNames(1:5,context$ai.select))
  testthat::expect_error(app$prediction_frame(c(rep(1,21),NA),context$naming.ai.var),"22개")
  testthat::expect_error(app$model_index("없는모형"),"모형")
})

testthat::test_that("폰트와 인증은 명시적 외부 설정을 요구하며 자동 다운로드가 없다", {
  testthat::expect_error(app$authorize_user(NULL), class="shiny.silent.error")
  testthat::expect_error(app$authorize_user(""), class="shiny.silent.error")
  testthat::expect_true(app$authorize_user("synthetic-reviewer"))
  testthat::expect_false(app$configure_font(""))
  testthat::expect_error(app$configure_font("not-existing-font.ttf"),"폰트")
  testthat::expect_error(app$load_credentials(""),"인증 설정")
})

testthat::test_that("실제 caret 메서드는 합성 모형의 확률·학습 성능·중요도 계약을 만족한다", {
  set.seed(17)
  training <- data.frame(x = rnorm(60), y = rnorm(60), outcome = factor(rep(c("이전", "이후"),30)))
  model <- caret::train(outcome ~ ., data = training, method = "glm", family = stats::binomial(),
                        trControl = caret::trainControl(method = "none", classProbs = TRUE))
  probabilities <- app$model_probabilities(model,training[1:2,c("x","y")])
  testthat::expect_equal(nrow(probabilities),2)
  testthat::expect_true(all(c("이전","이후") %in% names(probabilities)))
  testthat::expect_equal(unname(rowSums(probabilities)),c(1,1))
  performance <- app$training_performance(model)
  testthat::expect_s3_class(performance,"confusionMatrix")
  testthat::expect_equal(sum(performance$table),60)
  testthat::expect_true(all(c("variables","importance") %in% names(app$model_importance(model))))
})
