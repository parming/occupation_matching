### Helper devised to load and handle ESCO skills and occupations

# Load ESCO skills data into dataframe
esco_skills <<- read.table("data/esco_skills_es.csv", sep=",", comment.char = "", header = TRUE)
esco_occupations <<- read.table("data/esco_occupations_es.csv", sep=",", comment.char = "", header = TRUE)

# Get the dataframe row of the requested skill
esco_find_skill <- function(id) {
  esco_skills[esco_skills$conceptUri == id,]
}

# Get the name of the requested skill
esco_find_skill_name <- function(id) {
  as.character(esco_find_skill(id)$preferredLabel)
}

# Get the dataframe row of the requested skill
esco_find_occupation <- function(id) {
  esco_occupations[esco_occupations$conceptUri == id,]
}

# Get the name of the requested skill
esco_find_occupation_name <- function(id) {
  as.character(esco_find_occupation(id)$preferredLabel)
}

# Get the name of the requested skill but save the id
esco_combine_occupation_name <- function(id) {
  id <- as.character(id)
  id <- strsplit(id, "; ")[[1]][1]
  paste(id, esco_find_occupation_name(id), sep="; ")
}

esco_occupation_name <- function(id) {
  occupation <- strsplit(id, "; ")[[1]][2]
  strsplit(occupation, "/")[[1]][1]
}
