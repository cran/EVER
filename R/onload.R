.onLoad <-function(libname, pkgname){
path <- read.dcf(file = system.file("DESCRIPTION", package = "EVER"))
write.dcf(path)
}