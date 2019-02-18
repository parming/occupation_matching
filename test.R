source("libraries.R")

municipalities <- c(
  as.character(unique(educations_final$municipality)), 
  as.character(unique(contracts_final$municipality)), 
  as.character(unique(jobs$municpality))
)

municipalities <- unique(municipalities)

all_occupations <- c(
  "http://data.europa.eu/esco/occupation/c40a2919-48a9-40ea-b506-1f34f693496d; desarrollador web/desarrolladora web",
  "http://data.europa.eu/esco/occupation/b11e1742-5e28-4270-b081-b0193d85ee7d; desarrollador de bases de datos/desarrolladora de bases de datos",  
  "http://data.europa.eu/esco/occupation/9e2e6e1e-363b-4e1b-a673-7bc0f7343300; administrador de sistemas de TIC/administradora de sistemas de TIC",
  "http://data.europa.eu/esco/occupation/8b6388a4-4904-471b-9331-d3b1211f5525; gestor de proyectos de TIC/gestora de proyectos de TIC",
  "http://data.europa.eu/esco/occupation/aaeec9a7-dc57-4485-890c-86b3eef735f9; agente del servicio de asistencia de TIC",
  "http://data.europa.eu/esco/occupation/81480b40-a318-47f3-9b2c-56b69ed67d95; administrador de redes de TIC/administradora de redes de TIC",
  "http://data.europa.eu/esco/occupation/f2b15a0e-e65a-438a-affb-29b9d50b77d1; desarrollador de software/desarrolladora de software"
)

total <<- data.frame(matrix(ncol = 6, nrow = 0))
names(total) <- c("year","municipality", "occupation", "jobs", "contracts", "students")

for (year in 2013:2017) {
  
  for (i in 1:length(municipalities)) {
    municipality <- municipalities[i]
    
    for (j in 1:length(all_occupations)) {
      occupation <- all_occupations[j]
      
      job_count <- nrow(jobs[which(jobs$municpality == municipality & jobs$year == year & jobs$related_occupation == occupation),])
      contract_count <- sum(contracts_final[which(contracts_final$municipality == municipality & contracts_final$year == year & contracts_final$related_occupation == occupation),]$year_total)
      student_count <- sum(educations_final[which(educations_final$municipality == municipality & educations_final$year == year & educations_final$related_occupation == occupation),]$number)
      
      if (job_count + contract_count + student_count > 0) {
        subset <- data.frame(year, municipality, occupation, job_count, contract_count, student_count)
        names(subset)<- c("year","municipality", "occupation", "jobs", "contracts", "students")
        
        print(subset)
        
        total <<- rbind(total, subset)
      }
      
    }
  }
}

write.csv2(total, "data/processed_total.csv")
