# 역할: shinymanager가 검증한 사용자 정보가 준비된 세션만 계산을 허용한다.
authorize_user <- function(user) {
  shiny::req(is.character(user), length(user) == 1L, !is.na(user), nzchar(user))
  TRUE
}

# 역할: 인증 설정을 외부 RDS에서 읽고 마스킹된 기본값으로 실행하지 않는다.
load_credentials <- function(path) {
  if (!nzchar(path) || !file.exists(path)) stop("인증 설정 파일을 지정하세요.", call. = FALSE)
  credentials <- readRDS(path)
  require_columns(credentials, c("user", "password"), "인증")
  if (!nrow(credentials) || anyNA(credentials$user) || anyNA(credentials$password) ||
      any(!nzchar(credentials$user)) || any(!nzchar(credentials$password)) || anyDuplicated(credentials$user) ||
      any(grepl("^[*]+$", credentials$password))) stop("유효한 인증 설정이 필요합니다.", call. = FALSE)
  credentials
}

# 역할: 지정된 로컬 폰트만 등록하며 실행 중 폰트 다운로드는 하지 않는다.
configure_font <- function(path = Sys.getenv("CLOTHING_FONT_PATH")) {
  if (!nzchar(path)) return(invisible(FALSE))
  if (!file.exists(path)) stop("로컬 폰트 파일을 확인하세요.", call. = FALSE)
  sysfonts::font_add("clothing", regular = path)
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 112)
  ggplot2::theme_set(ggplot2::theme_get() + ggplot2::theme(text = ggplot2::element_text(family = "clothing")))
  invisible(TRUE)
}
