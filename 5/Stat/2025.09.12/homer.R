nyiregyhaza <- read.table(file = "https://zempleni.elte.hu/nyir-51-88m.hom")
karcag <- read.table(file = "https://zempleni.elte.hu/karc-51-88.hom")

summary(nyiregyhaza)
summary(karcag)

order(nyiregyhaza[,1])[dim(nyiregyhaza)[1]]

