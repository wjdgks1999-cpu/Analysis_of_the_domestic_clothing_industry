# 역할: 기존 다섯 선택 항목을 저장 모형의 순서와 연결한다.
model_index <- function(selection) {
  choices <- c("전체대상", "도매 및 상품 중개업", "소매업; 자동차 제외", "의복, 의복 액세서리 및 모피제품 제조업", "섬유제품 제조업; 의복 제외")
  index <- match(selection, choices)
  if (length(index) != 1L || is.na(index)) stop("지원하는 모형을 선택하세요.", call. = FALSE)
  index
}

# 역할: 입력 22개의 순서와 기존 0.01 변환을 유지하고 실제 변수명은 문맥에서 받는다.
prediction_frame <- function(values, variable_names) {
  if (length(values) != 22L || length(variable_names) != 22L || any(!is.finite(values))) stop("재무지표 22개에 유효한 숫자가 필요합니다.", call. = FALSE)
  result <- as.data.frame(as.list(values * 0.01))
  names(result) <- variable_names
  result
}

# 역할: 원래 모형의 예측 메서드를 호출하며 업무 원문이 포함될 수 있는 오류는 감춘다.
model_probabilities <- function(model, data) {
  if (inherits(model, "train") && !requireNamespace("caret", quietly = TRUE)) stop("caret 패키지가 필요합니다.", call. = FALSE)
  result <- tryCatch(stats::predict(model, data, type = "prob"), error = function(error) {
    # 역할: 모형 내부 오류를 입력 자료가 드러나지 않는 안내로 바꾼다.
    stop("모형과 입력 변수·실행 패키지의 호환성을 확인하세요.", call. = FALSE)
  })
  if (!is.data.frame(result) || !all(c("이전", "이후") %in% names(result)) || nrow(result) != nrow(data)) stop("모형의 이전·이후 확률 출력 형식을 확인하세요.", call. = FALSE)
  values <- as.matrix(result[c("이전", "이후")])
  if (!is.numeric(values) || any(!is.finite(values)) || any(values < 0 | values > 1) || any(abs(rowSums(values) - 1) > 1e-6)) stop("모형 확률의 범위를 확인하세요.", call. = FALSE)
  result
}

# 역할: 저장 모형의 학습 자료에서 계산한 혼동행렬임을 명확하게 유지한다.
training_performance <- function(model) {
  data <- model$trainingData
  if (!is.data.frame(data) || !".outcome" %in% names(data)) stop("모형에 학습 자료가 없습니다.", call. = FALSE)
  tryCatch(caret::confusionMatrix(stats::predict(model, data), as.factor(data$.outcome)), error = function(error) {
    # 역할: 학습 자료가 포함될 수 있는 오류 원문을 노출하지 않는다.
    stop("학습 자료 기준 성능을 계산할 수 없습니다. 모형 환경을 확인하세요.", call. = FALSE)
  })
}

# 역할: 저장 모형의 기존 변수 중요도를 반환하며 새 모형을 학습하지 않는다.
model_importance <- function(model) {
  importance <- tryCatch(caret::varImp(model, scale = TRUE)$importance, error = function(error) {
    # 역할: 모형별 중요도 계산 실패를 안전한 안내로 바꾼다.
    stop("모형의 변수 중요도와 실행 패키지의 호환성을 확인하세요.", call. = FALSE)
  })
  if (!"Overall" %in% names(importance)) stop("Overall 변수 중요도가 필요합니다.", call. = FALSE)
  data.frame(variables = row.names(importance), importance = importance$Overall)
}
