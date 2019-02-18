source("libraries.R")

### Load data

# Load data
educations <- read.table("data/df_graduats.csv", sep=",", comment.char = "", quote='"', header = TRUE)

educations <- plyr::rename(educations, c(
  "Comarca"="municipality",
  "Any"="year",
  "Ensenyament"="name",
  "Estudis"="type",
  "Fam..lia.professional"="category",
  "Grau"="education_level",
  "Naturalesa"="private_or_public",
  "Nombre.de.graduats"="number",
  "Sexe"="gender")
)

# Set default municipality ids
educations <- apply_municipalities(educations)

# Manually add broken and missing municipality data
# as.character(unique(educations[educations$municipality_id == "",]$municipality))

educations$related_occupation <- ""

# CFPM - SISTEMES MICROINFORM\303\200TICS I XARXES
# CFPS - ADMINISTRACI\303\223 DE SISTEMES INFORM\303\200TICS
# CFPS - ADMINISTRACI\303\223 DE SISTEMES INFORM\303\200TICS EN XARXA
# CFPS - ANIMACIONS EN 3D, JOCS I ENTORNS INTERACTIUS
# CFPS - ANIMACI\303\223 EN 3D, JOCS i ENTORNS INTERACTIUS (MONS VIRTUALS, REALITAT AUGMENTADA i GAMIFICACI\303\223)
# CFPS - DESENVOLUPAMENT D'APLICACIONS INFORM\303\200TIQUES
# CFPS - DESENVOLUPAMENT D'APLICACIONS MULTIPLATAFORMA
# CFPS - DESENVOLUPAMENT D'APLICACIONS WEB
# CFPS - DESENVOLUPAMENT D'APLICACIONS WEB (BIOINFORM\303\200TICA)
# CFPS - SISTEMES DE TELECOMUNICACIONS I INFORM\303\200TICS
# 4.0 CFPS - DISSENY EN FABRICACI\303\223 MEC\303\200NICA
# 4.0 CFPS - DISSENY I EDICI\303\223 DE PUBL.IMPRESES I MULTIM\303\210DI
# 4.0 CFPS - DISSENY I GESTI\303\223 DE LA PRODUCCI\303\223 GR\303\200FIC
# 4.0 CFPS - PRODUCCI\303\223 EN IND\303\232STRIES D'ARTS GR\303\200FIQUES
# 4.0 CFPS - PROGRAMACI\303\203\302\223 DE PRODUCCI\303\203\302\223 EN FABRICACI\303\203\302\223 MECANICA
# 4.0 CFPS - PROGRAMACI\303\223 DE LA PRODUCCI\303\223 EN L'EMMOTLLAMENT DE METALLS I POL\303\215MERS
# 4.0 CFPS - PROGRAMACI\303\223 PRODUCCI\303\223 FABRICACI\303\223 MEC\303\200NICA
# 4.0 CFPS - SISTEMES DE REGULACI\303\223 I CONTROL AUTOM\303\200TIC
# 4.0 CFPS - SISTEMES DE TELECOMUNICACI\303\223 I INFORM\303\200TICS

education_occupations <- data.frame(
  "id" = c(
    "CFPM - SISTEMES MICROINFORMÀTICS I XARXES",
    "CFPS - ADMINISTRACIÓ DE SISTEMES INFORMÀTICS",
    "CFPS - ADMINISTRACIÓ DE SISTEMES INFORMÀTICS EN XARXA",
    "CFPS - ANIMACIONS EN 3D, JOCS I ENTORNS INTERACTIUS",
    "CFPS - ANIMACIÓ EN 3D, JOCS i ENTORNS INTERACTIUS (MONS VIRTUALS, REALITAT AUGMENTADA i GAMIFICACIÓ)",
    "CFPS - DESENVOLUPAMENT D'APLICACIONS INFORMÀTIQUES",
    "CFPS - DESENVOLUPAMENT D'APLICACIONS MULTIPLATAFORMA",
    "CFPS - DESENVOLUPAMENT D'APLICACIONS WEB",
    "CFPS - DESENVOLUPAMENT D'APLICACIONS WEB (BIOINFORM\303\200TICA)",
    "CFPS - SISTEMES DE TELECOMUNICACIONS I INFORM\303\200TICS"
  ),
  "occupation" = c(
    "http://data.europa.eu/esco/occupation/9e2e6e1e-363b-4e1b-a673-7bc0f7343300; administrador de sistemas de TIC/administradora de sistemas de TIC",
    "http://data.europa.eu/esco/occupation/9e2e6e1e-363b-4e1b-a673-7bc0f7343300; administrador de sistemas de TIC/administradora de sistemas de TIC",
    "http://data.europa.eu/esco/occupation/9e2e6e1e-363b-4e1b-a673-7bc0f7343300; administrador de sistemas de TIC/administradora de sistemas de TIC",
    "http://data.europa.eu/esco/occupation/f2b15a0e-e65a-438a-affb-29b9d50b77d1; desarrollador de software/desarrolladora de software",
    "http://data.europa.eu/esco/occupation/f2b15a0e-e65a-438a-affb-29b9d50b77d1; desarrollador de software/desarrolladora de software",
    "http://data.europa.eu/esco/occupation/f2b15a0e-e65a-438a-affb-29b9d50b77d1; desarrollador de software/desarrolladora de software",
    "http://data.europa.eu/esco/occupation/f2b15a0e-e65a-438a-affb-29b9d50b77d1; desarrollador de software/desarrolladora de software",
    "http://data.europa.eu/esco/occupation/c40a2919-48a9-40ea-b506-1f34f693496d; desarrollador web/desarrolladora web",
    "http://data.europa.eu/esco/occupation/c40a2919-48a9-40ea-b506-1f34f693496d; desarrollador web/desarrolladora web",
    "http://data.europa.eu/esco/occupation/81480b40-a318-47f3-9b2c-56b69ed67d95; administrador de redes de TIC/administradora de redes de TIC"
  )
)

# Auto fill in
mapply(function(id, occupation) {
  occupation <- esco_combine_occupation_name(occupation)
  educations[educations$name == as.character(id),]$related_occupation <<- occupation
}, education_occupations$id, education_occupations$occupation)

educations_final <- educations[educations$related_occupation != "",]

write.csv2(educations_final, "data/processed_educations_esco.csv")
