source("libraries.R")

confirmed_occupations <- read.table("data/offer_confirmed.csv", sep=";", comment.char = "", header = TRUE)
confirmed_occupations <- confirmed_occupations[,-1]

naive_bayes(confirmed_occupations, "related_skills")
