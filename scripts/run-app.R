# 역할: 프로젝트 루트에서 명시적으로 실행한 경우에만 로컬 분석 앱을 시작한다.
library_path <- Sys.getenv("CLOTHING_R_LIBRARY", unset = ".runtime/library")
if (dir.exists(library_path)) .libPaths(c(normalizePath(library_path), .libPaths()))
if (!dir.exists("analysis_app")) stop("프로젝트 루트에서 실행하세요.")
shiny::runApp("analysis_app", host = "127.0.0.1", launch.browser = TRUE)
