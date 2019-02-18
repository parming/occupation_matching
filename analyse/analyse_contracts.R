source("libraries.R")

### Load data

# Load infojobs JSON data as dataframe
# jobs <- setDF(rbindlist(jsonlite::fromJSON("data/infojobs.json"), fill=TRUE, use.names=TRUE))
contracts <- read.table("data/contractacio.csv", sep=",", comment.char = "", quote='"', header = TRUE)

contracts <- plyr::rename(contracts, c(
  "comarca"="municipality",
  "any"="year",
  "codi_grup_primari_ocupacio"="occupation_id",
  "grup_primari_ocupacio"="occupation_name",
  "codi_divisio_economica"="sector_id",
  "divisio_economica"="sector_name",
  "total_any"="year_total",
  "Family"="family")
)

# Set default municipality ids
contracts <- apply_municipalities(contracts)

# Manually add broken and missing municipality data
# as.character(unique(contracts[contracts$municipality_id == "",]$municipality))


contracts$related_occupation <- ""

# 18 Directors serveis TIC
# 63 Instructors ensenyament no reglat, TIC
# 89 Enginyers telecomunicacions
# 103 Enginyers t\303\250cnics telecomunicacions
# 197 Dissenyadors gr\303\240fics i multim\303\250dia
# [2653] Professionals vendes TIC
# [2711] Analistes de sistemes
# [2712] Analistes i dissenyadors programari
# [2713] Analistes, programadors i dissenyadors p\303\240gines web
# [2719] Analistes i dissenyadors programari ncaa
# [2721] Dissenyadors i administradors bases de dades
# [2722] Administradors sistemes i xarxes inform\303\240tiques
# [2723] Analistes xarxes inform\303\240tiques
# [2729] Especialistes bases dades i xarxes inform\303\240tiques ncaa
# [3811] T\303\250cnics operacions sistemes inform\303\240tics
# [3812] T\303\250cnics assist\303\250ncia usuari TIC
# [3813] T\303\250cnics xarxes inform\303\240tiques
# [3814] T\303\250cnics web
# [3820] Programadors inform\303\240tics
# [7533] Instal\302\267ladors i reparadors en TIC

# Definitions extracted from data, by contracts$family == "TIC"
contract_occupations <- data.frame(
  "id" = c(2711, 2712, 2713, 2719, 2721, 2722, 2723, 2729, 3811, 3812, 3813, 3814, 3820, 7533),
  "occupation" = c(
    "http://data.europa.eu/esco/occupation/9e2e6e1e-363b-4e1b-a673-7bc0f7343300; administrador de sistemas de TIC/administradora de sistemas de TIC",
    "http://data.europa.eu/esco/occupation/f2b15a0e-e65a-438a-affb-29b9d50b77d1; desarrollador de software/desarrolladora de software",
    "http://data.europa.eu/esco/occupation/c40a2919-48a9-40ea-b506-1f34f693496d; desarrollador web/desarrolladora web",
    "http://data.europa.eu/esco/occupation/f2b15a0e-e65a-438a-affb-29b9d50b77d1; desarrollador de software/desarrolladora de software",
    "http://data.europa.eu/esco/occupation/8c57af09-719c-42b3-be40-6ed4946236cc; administrador de bases de datos/administradora de bases de datos",
    "http://data.europa.eu/esco/occupation/81480b40-a318-47f3-9b2c-56b69ed67d95; administrador de redes de TIC/administradora de redes de TIC",
    "http://data.europa.eu/esco/occupation/81480b40-a318-47f3-9b2c-56b69ed67d95; administrador de redes de TIC/administradora de redes de TIC",
    "http://data.europa.eu/esco/occupation/8c57af09-719c-42b3-be40-6ed4946236cc; administrador de bases de datos/administradora de bases de datos",
    "http://data.europa.eu/esco/occupation/9e2e6e1e-363b-4e1b-a673-7bc0f7343300; administrador de sistemas de TIC/administradora de sistemas de TIC",
    "http://data.europa.eu/esco/occupation/aaeec9a7-dc57-4485-890c-86b3eef735f9; agente del servicio de asistencia de TIC",
    "http://data.europa.eu/esco/occupation/81480b40-a318-47f3-9b2c-56b69ed67d95; administrador de redes de TIC/administradora de redes de TIC",
    "http://data.europa.eu/esco/occupation/c40a2919-48a9-40ea-b506-1f34f693496d; desarrollador web/desarrolladora web",
    "http://data.europa.eu/esco/occupation/f2b15a0e-e65a-438a-affb-29b9d50b77d1; desarrollador de software/desarrolladora de software",
    "http://data.europa.eu/esco/occupation/3e7bf729-4442-4b9f-ad5e-83111963795c; técnico de TIC/técnica de TIC"
  )
)

# Manual fill in
mapply(function(id, occupation) {
  occupation <- esco_combine_occupation_name(occupation)
  contracts[contracts$occupation_id == id,]$related_occupation <<- occupation
}, contract_occupations$id, contract_occupations$occupation)

contracts_final <- contracts[contracts$related_occupation != "",]

write.csv2(contracts_final, "data/processed_contracts_esco.csv")
