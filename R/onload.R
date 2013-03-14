.onLoad <-function(libname, pkgname){
path <- read.dcf(file = system.file("DESCRIPTION", package = pkgname, lib.loc = libname))
write.dcf(path)
}