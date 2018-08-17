##9-1
pres.cycle <- cycle(presidents)

tapply(presidents, pres.cycle, mean, na.rm=TRUE)

##9-2

aq.groups <- cut(airquality$Wind, breaks=2*(1:11)-1)
tapply(airquality$Solar.R, aq.groups, mean, na.rm=TRUE)

##9-3

attach(swiss)
agr.groups <- cut(Agriculture, breaks=c(0, 10*(1:10)))
agr.cath <- cut(Catholic, breaks=c(0, 10*(1:10)))
fert <- tapply(Fertility, list(agr.groups, agr.cath), mean, na.rm=TRUE)
detach(swiss)
