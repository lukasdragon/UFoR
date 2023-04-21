library("devtools")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

document()

test()


build()
install()