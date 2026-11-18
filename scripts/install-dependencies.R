# 역할: 명시적 실행에서만 지정한 프로젝트 패키지 폴더에 필요한 의존성을 설치한다.
library_path <- Sys.getenv("CLOTHING_R_LIBRARY", unset = ".runtime/library")
dir.create(library_path, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(normalizePath(library_path), .libPaths()))
options(repos = c(CRAN = "https://cloud.r-project.org"))
packages <- c("shiny", "shinymanager", "dplyr", "ggplot2", "stringr", "DT", "scales", "openxlsx", "shinyjs", "showtext", "caret", "randomForest", "testthat")
install.packages(setdiff(packages, rownames(installed.packages())), lib = library_path)
