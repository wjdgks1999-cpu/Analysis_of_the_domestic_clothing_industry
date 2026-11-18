# 역할: 명시적인 Shiny 실행에서만 외부 설정을 읽고 인증된 분석 앱을 생성한다.
library(shiny)
source("bootstrap.R", local = TRUE, encoding = "UTF-8")
application <- load_application_code(".")
context <- application$load_context(Sys.getenv("CLOTHING_CONTEXT_PATH"))
credentials <- application$load_credentials(Sys.getenv("CLOTHING_CREDENTIALS_PATH"))
application$configure_font()
application$create_application(context, credentials)
