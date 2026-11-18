# 역할: 기능 파일의 정의만 독립 환경에 불러오며 앱 실행·자료 읽기·다운로드를 하지 않는다.
load_application_code <- function(directory, target = new.env(parent = globalenv())) {
  files <- c("data/context.R", "authentication/config.R", "extraction/logic.R", "overview/logic.R",
             "finance/logic.R", "hypothesis/logic.R", "prediction/logic.R", "downloads/logic.R",
             "extraction/server.R", "overview/server.R", "finance/server.R", "hypothesis/server.R",
             "prediction/server.R", "downloads/server.R", "ui.R", "application.R")
  for (file in files) sys.source(file.path(directory, file), envir = target, keep.source = TRUE)
  target
}
