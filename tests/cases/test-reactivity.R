testthat::test_that("추출 조건은 버튼 시점에만 적용되고 세션 간에 공유되지 않는다", {
  # 역할: 실제 추출 모듈을 대체 인증 조건 아래에서 연결한다.
  server <- function(input, output, session) {
    # 역할: 이 서버 테스트에서만 인증 완료 상태를 제공한다.
    authorize <- function() TRUE
    state <- app$register_extraction(input,output,session,context,authorize)
  }
  shiny::testServer(server, {
    do.call(session$setInputs,c(default_criteria(),list(extracting=1)))
    testthat::expect_equal(nrow(state$basic()),15)
    session$setInputs(region="서울")
    testthat::expect_equal(nrow(state$basic()),15)
    session$setInputs(extracting=2)
    testthat::expect_equal(nrow(state$basic()),5)
  })
  shiny::testServer(server, {
    do.call(session$setInputs,c(default_criteria(),list(extracting=1)))
    testthat::expect_equal(nrow(state$basic()),15)
  })
})

testthat::test_that("인증되지 않은 세션은 추출·모형 출력을 계산할 수 없다", {
  # 역할: 인증이 거부된 실제 추출·예측 모듈을 테스트한다.
  server <- function(input, output, session) {
    # 역할: 테스트 세션의 인증 거부 상태를 유지한다.
    authorize <- function() shiny::req(FALSE)
    extracted <- app$register_extraction(input,output,session,context,authorize)
    prediction <- app$register_prediction(input,output,session,context,synthetic_adapters(),authorize)
  }
  shiny::testServer(server, {
    do.call(session$setInputs,c(default_criteria(),list(extracting=1)))
    testthat::expect_error(extracted$basic(),class="shiny.silent.error")
    session$setInputs(ai.selecting="전체대상",start.pred=1)
    testthat::expect_error(prediction$data(),class="shiny.silent.error")
  })
})

testthat::test_that("예측 입력은 버튼에서 갱신되고 모형 선택은 현재 입력에 즉시 적용된다", {
  # 역할: 대체 모형으로 실제 예측 반응식의 의존 관계를 검증한다.
  server <- function(input, output, session) {
    # 역할: 테스트 내에서만 인증 완료 상태를 제공한다.
    authorize <- function() TRUE
    prediction <- app$register_prediction(input,output,session,context,synthetic_adapters(),authorize)
  }
  shiny::testServer(server, {
    inputs <- setNames(as.list(1:22),paste0("ai",1:22))
    do.call(session$setInputs,c(inputs,list(ai.selecting="전체대상",start.pred=1)))
    testthat::expect_equal(prediction$data()[[1]],0.01)
    testthat::expect_equal(prediction$probabilities()$이전,0.1)
    session$setInputs(ai1=80)
    testthat::expect_equal(prediction$data()[[1]],0.01)
    session$setInputs(ai.selecting="도매 및 상품 중개업")
    testthat::expect_equal(prediction$probabilities()$이전,0.2)
    session$setInputs(start.pred=2)
    testthat::expect_equal(prediction$data()[[1]],0.8)
  })
})

testthat::test_that("개요 클릭은 기업 선택에 연결되고 집계 항목 변경 시 선택을 초기화한다", {
  # 역할: 합성 개요 자료로 실제 클릭 선택 반응식을 검증한다.
  server <- function(input,output,session) {
    basic <- shiny::reactive(context$dat.basic)
    overview <- app$register_overview(input,output,session,basic)
  }
  shiny::testServer(server, {
    session$setInputs(select.gaeyo.inform="주요사업")
    session$setInputs(click.gaeyo=list(y=1))
    testthat::expect_equal(nrow(overview$data()),15)
    session$setInputs(select.gaeyo.inform="시도")
    testthat::expect_equal(nrow(overview$data()),0)
  })
})

testthat::test_that("화면은 기존 다섯 탭과 22개 예측 입력을 포함한다", {
  suppressPackageStartupMessages(library(shiny))
  html <- as.character(app$build_ui(context))
  for (label in c("데이터 추출","개요정보","재무정보","가설검정","AI 분석")) testthat::expect_match(html,label,fixed=TRUE)
  for (index in 1:22) testthat::expect_match(html,paste0('id="ai',index,'"'),fixed=TRUE)
  testthat::expect_match(html,'value="2016"',fixed=TRUE)
  testthat::expect_match(html,'value="2022"',fixed=TRUE)
})
