source("libraries.R")

str_to_num_infojobs <- function(str) {
  if (is.na(str) || str == "#SIN_ESPECIFICAR") {
    return(0)
  } else {
    return(str_to_num(str))
  }
}

### Load data

# Load infojobs JSON data as dataframe
# jobs <- setDF(rbindlist(jsonlite::fromJSON("data/infojobs.json"), fill=TRUE, use.names=TRUE))
jobs <- read.table("data/infojobs_processed_comarques.csv", sep=",", comment.char = "", quote='"', header = TRUE)

jobs <- plyr::rename(jobs, c(
  "FECHA_PUBLICACION_IJ"="publication_date",
  "TITULO_OFERTA"="job_title",
  "DESCRIPCION_OFERTA"="job_description",
  "REQUISITOS_MINIMOS"="min_requisites",
  "VACANTES"="vacancies",
  "CP"="postal_code",
  "POBLACION"="city",
  "DESC_PROVINCIA"="province",
  "DESC_PAIS"="country",
  "DESC_CONTRATO"="contract_type",
  "DESC_DETALLE_ESTUDIOS"="studies_level",
  "ESTUDIOS_MINIMOS"="min_studies",
  "CONOCIMIENTOS_NECESARIOS"="knowledge_tags",
  "SUELDO_MINIMO"="min_salary",
  "SUELDO_MAXIMO"="max_salary",
  "SUELDO_PERIODICIDAD"="salary_periodicity",
  "DESC_CATEGORIA"="job_category",
  "DESC_SUBCATEGORIA"="job_subcategory",
  "EXPERIENCIA_MINIMA"="min_experience",
  "DESC_JORNADAS"="job_working_hours",
  "DESC_NIVEL_LABORAL"="job_worker_lever",
  "IDIOMA"="lang",
  "DEPARTAMENTO"="department",
  "NORM_DEPARTAMENTO"="department_norm",
  "NORM_DESC_DETALLE_ESTUDIOS"="studies_description",
  "A..O"="year",
  "MES"="month",
  "COMARCA"="municipality",
  "lat"="lat",
  "lon"="long")
)

### Filter data

# Remove ads that are not classified as Engineering or IT
jobs <- jobs[jobs$job_category == "Informática y telecomunicaciones",]
# jobs <- jobs[jobs$job_category %in% c("Ingenieros y técnicos", "Informática y telecomunicaciones"),]

# Remove ads indicating too many vacancies
jobs <- jobs[!(jobs$vacancies > 10),]


### Massage data

# Add jobs ID to be able to follow them
jobs$id <- seq.int(nrow(jobs))

# Fix flawed importing
jobs$publication_date <- as.character(jobs$publication_date)
jobs$job_description <- as.character(jobs$job_description)
jobs$min_requisites <- as.character(jobs$min_requisites)

# Change salary to integer
jobs$min_salary <- str_to_num_infojobs(jobs$min_salary)
jobs$max_salary <- str_to_num_infojobs(jobs$max_salary)

# Find possible keywords in title
jobs$job_title <- str_clean(jobs$job_title)
jobs$knowledge_tags <- str_clean(jobs$knowledge_tags)

jobs <- apply_municipalities(jobs)

# Manually add broken and missing municipality data
# as.character(unique(jobs[jobs$municipality_id == "",]$municipality))
# 1974 empty municipalities detected
jobs <- jobs[jobs$municipality != "",]


### Calculate fields

# Calculate average gross max salary
jobs <- jobs %>% mutate(gross_yearly_salary = case_when(
  salary_periodicity == "Bruto/hora" ~ max_salary * 160 * 12,
  salary_periodicity == "Bruto/mes" ~ max_salary * 12,
  salary_periodicity == "Bruto/año" ~ max_salary)
)

# Empty information from previous analysis
esco_clean_cache()


# TODO: Move section out to Shiny
words_tags <- str_word_frequence(jobs$knowledge_tags, 1000)

tag_list <- as.list(setNames(words_tags$count, words_tags$tag))
list.save(tag_list, "data/infojobs_tags.yaml")

words_tags$tag <- as.character(words_tags$tag)
esco_map_tags(words_tags)



# Setup flags and result fields
jobs$match <- 0
jobs$manual_selection <- 0
jobs$related_occupation <- ""
jobs$requested_language <- NULL

detected_occupations <- read.table("data/detected_occupations.csv", sep=";", comment.char = "", header = TRUE)

# Available contract occupations
# "http://data.europa.eu/esco/occupation/9e2e6e1e-363b-4e1b-a673-7bc0f7343300; administrador de sistemas de TIC/administradora de sistemas de TIC",
# "http://data.europa.eu/esco/occupation/c40a2919-48a9-40ea-b506-1f34f693496d; desarrollador web/desarrolladora web",
# "http://data.europa.eu/esco/occupation/f2b15a0e-e65a-438a-affb-29b9d50b77d1; desarrollador de software/desarrolladora de software",
# "http://data.europa.eu/esco/occupation/8c57af09-719c-42b3-be40-6ed4946236cc; administrador de bases de datos/administradora de bases de datos",
# "http://data.europa.eu/esco/occupation/aaeec9a7-dc57-4485-890c-86b3eef735f9; agente del servicio de asistencia de TIC",
# "http://data.europa.eu/esco/occupation/81480b40-a318-47f3-9b2c-56b69ed67d95; administrador de redes de TIC/administradora de redes de TIC",

for(i in 1:nrow(detected_occupations)) {
  job_title <- as.character(detected_occupations$title[i])
  tag <- as.character(detected_occupations$tags[i])
  occupation <- as.character(detected_occupations$occupation[i])

  if (job_title == "") {
    index <- which(grepl(tag, jobs$knowledge_tags) & jobs$match == 0)
  } else if (tag == "") {
    index <- which(grepl(job_title, jobs$job_title) & jobs$match == 0)
  } else {
    index <- which(grepl(job_title, jobs$job_title) & grepl(tag, jobs$knowledge_tags) & jobs$match == 0)
  }

  if (nrow(jobs[index,]) > 0) {
    jobs[index,]$related_occupation <- occupation
    jobs[index,]$match <- 1
    jobs[index,]$manual_selection <- 1
  }

  print(paste(job_title, tag, sep = " - "))
  print(nrow(jobs[index,]))
}


# Execution test
# guess_jobs <- jobs[jobs$manual_selection == 1,] %>%
#   select(job_title, knowledge_tags, related_occupation)
guess_jobs <- head(jobs, 20000)

guess_jobs$related_occupation <- naive_bayes(head(jobs, 2000))



jobs_final <- jobs[jobs$related_occupation != "",] %>%
  select(publication_date, job_title, job_description, min_requisites, vacancies, postal_code,
        city, province, country, contract_type, studies_level, min_studies, knowledge_tags,
        min_salary, max_salary, salary_periodicity, job_category, job_subcategory,
        min_experience, job_working_hours, job_worker_lever, lang, department, department_norm,
        studies_description, year, month, municpality, lat, long, related_occupation)
write.csv2(jobs_final, "data/processed_ads_esco.csv")
