rm(list=ls())

# create directories for output
file.loc <- paste0('../../output/save_r_package_list/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# save list of installed packages
tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
save(installedpkgs, file="../../output/save_r_package_list/installed_old.rda")

# load list of installed packages after update
tmp <- installed.packages()
installedpkgs <- load("../../output/save_r_package_list/installed_old.rda")
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
install.packages(missing)
update.packages(ask=FALSE)