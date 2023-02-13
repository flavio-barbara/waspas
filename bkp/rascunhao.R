# library(readxl)
# choppers <- read_excel("./data/waspas_choppers.xlsx", col_names = F)
# save(choppers, file="data/waspas_choppers.RData")

load("data/waspas_choppers.RData")
data("waspas_choppers")

Matrix <- xlsx[4:nrow(xlsx), 2:ncol(xlsx)]
vetpesos<-xlsx[2,2:ncol(xlsx)]


normalized_matrix <- normalize(Matrix, vetpesos, initCol = 1, initRow = 1)

wsm <- calcWSM(normalized_matrix, vetpesos)

wpm <- calcWPM(normalized_matrix, vetpesos)



row_values_matrix<-read.csv("./data/row_values_matrix.csv", header = FALSE, sep = ",", quote = "\"",dec = ",")
flags_Cost_Benefit<-read.csv("./data/flags_Cost_Benefit.csv", header = FALSE, sep = ",", quote = "\"",dec = ",")
mytest <- normalize(row_values_matrix, flags_Cost_Benefit)

normalized_matrix <- normalize(row_values_matrix, flags_Cost_Benefit)

is.data.frame(row_values_matrix)
is.data.frame(flags_Cost_Benefit)

install.packages("pkgdown")

devtools::document(pkg = ".", roclets = NULL, quiet = FALSE)

usethis::use_news_md(open = rlang::is_interactive())

usethis::use_vignette("waspas-in-a-nutshell", "WASPAS in a nutshell")

usethis::use_testthat()

usethis::use_build_ignore(c("bkp"))
