### Helper devised to set municipality names and fix other geodata

get_municipalities <- function() {
  municipality_ids <- c(1:42)
  municipality_names <- c(
    "Alt Camp",
    "Alt Empordà",
    "Alt Penedès",
    "Alt Urgell",
    "Alta Ribagorça",
    "Anoia",
    "Bages",
    "Baix Camp",
    "Baix Ebre",
    "Baix Empordà",
    "Baix Llobregat",
    "Baix Penedès",
    "Barcelonès",
    "Berguedà",
    "Cerdanya",
    "Conca de Barberà",
    "Garraf",
    "Garrigues",
    "Garrotxa",
    "Gironès",
    "Maresme",
    "Montsià",
    "Noguera",
    "Osona",
    "Pallars Jussà",
    "Pallars Sobirà",
    "Pla d'Urgell",
    "Pla de l'Estany",
    "Priorat",
    "Ribera d'Ebre",
    "Ripollès",
    "Segarra",
    "Segrià",
    "Selva",
    "Solsonès",
    "Tarragonès",
    "Terra Alta",
    "Urgell",
    "Val d'Aran",
    "Vallès Occidental",
    "Vallès Oriental",
    "Moianes"
  )
  
  do.call(rbind, Map(
    data.frame, municipality_id=municipality_ids, municipality=municipality_names)
  )
}

apply_municipalities <- function(df) {
  municipalities <- get_municipalities()
  
  df$municipality_id <- 0
  
  for (i in 1:nrow(municipalities)) {
    municipality <- as.character(municipalities[i,]$municipality)
    municipality_id <- as.numeric(municipalities[i,]$municipality_id)
    if (!nrow(df[df$municipality == municipality,]) == 0) {
      df[df$municipality == municipality, "municipality_id"] <- municipality_id
    }
  }
  
  df[df$municipality == "Alt Emporda", "municipality_id"] <- 2
  df[df$municipality == "Alt Penedes", "municipality_id"] <- 3
  df[df$municipality == "Alta Ribagorca", "municipality_id"] <- 5
  df[df$municipality == "Baix Emporda", "municipality_id"] <- 10
  df[df$municipality == "Baix Penedes", "municipality_id"] <- 12
  df[df$municipality == "Barcelones", "municipality_id"] <- 13
  df[df$municipality == "Bergueda", "municipality_id"] <- 14
  df[df$municipality == "Conca de Barbera", "municipality_id"] <- 16
  df[df$municipality == "Girones", "municipality_id"] <- 20
  df[df$municipality == "Montsia", "municipality_id"] <- 22
  df[df$municipality == "Pallars Jussa", "municipality_id"] <- 25
  df[df$municipality == "Pallars Sobira", "municipality_id"] <- 26
  df[df$municipality == "Pla Urgell", "municipality_id"] <- 27
  df[df$municipality == "Pla de Estany", "municipality_id"] <- 28
  df[df$municipality == "Ribera Ebre", "municipality_id"] <- 30
  df[df$municipality == "Ripolles", "municipality_id"] <- 31
  df[df$municipality == "Segria", "municipality_id"] <- 33
  df[df$municipality == "Solsones", "municipality_id"] <- 35
  df[df$municipality == "Tarragones", "municipality_id"] <- 36
  df[df$municipality == "Val Aran", "municipality_id"] <- 39
  df[df$municipality == "Vall Aran", "municipality_id"] <- 39
  df[df$municipality == "Aran", "municipality_id"] <- 39
  df[df$municipality == "Valles Occidental", "municipality_id"] <- 40
  df[df$municipality == "Valles Oriental", "municipality_id"] <- 41
  
  df
}
