if("coda" %in% rownames(installed.packages()) == FALSE) {install.packages("coda")}
if("mvtnorm" %in% rownames(installed.packages()) == FALSE) {install.packages("mvtnorm")}
if("devtools" %in% rownames(installed.packages()) == FALSE) {install.packages("devtools")}
if("rethinking" %in% rownames(installed.packages()) == FALSE) {devtools::install_github("rmcelreath/rethinking")}
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}

paquetes <- list("coda", "mvtnorm", "devtools", "rethinking", "tidyverse")


lapply(paquetes, FUN = library, character.only=T)

rm(list = c("paquetes"))
