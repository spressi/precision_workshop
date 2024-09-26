#if you can't update packages, deinstall and reinstall!
myPackages <- as.data.frame(installed.packages()) #all packages

myPackages <- subset(myPackages, !grepl("MRO", myPackages$LibPath)) #no MRO packages
myPackages <- myPackages[!(myPackages[,"Priority"] %in% c("base", "recommended")),] #no base or recommended packages

sapply(myPackages[,1], remove.packages, lib = unique(myPackages$LibPath))
