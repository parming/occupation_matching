### Helper devised to make ESCO API calls

esco_base_search_url <- "https://ec.europa.eu/esco/api/suggest2?type=occupation&type=skill&alt=true&text="
esco_base_skill_url <- "https://ec.europa.eu/esco/api/resource/skill?uri="

# Load known ESCO api results
esco_cache <<- list.load("data/esco_searches.yaml")
esco_misses <<- list.load("data/esco_misses.yaml")
esco_manual <<- list.load("data/esco_manual.yaml")
esco_synonyms <<- list.load("data/esco_synonyms.yaml")

esco_search_api <- function(query) {
  
  result <- tryCatch({
    # Build search URL
    query <- URLencode(query)
    search_url <- paste(c(esco_base_search_url, query), collapse = "")
    
    # Download JSON from ESCO API
    search_data <- jsonlite::fromJSON(getURL(search_url))
    
    # Related occupations or skills
    occupations <- unlist(search_data$`_embedded`$results$uri)
    
    results <- unlist(lapply(occupations, esco_search_skill))
    
    # Select only the main occurring occupations
    frequencies <- table(results)
    common_occupations <- names(which(frequencies == max(frequencies)))
    
    # Put a name on the occupations
    common_occupations <- unlist(lapply(common_occupations, esco_combine_occupation_name))
    
    return(common_occupations)
  }, error=function(cond) {
    message(paste("Download failure"))
    message(query)
    message(cond)
    
    return(NULL)
  })
}

esco_search_skill <- function(uri) {
  
  if (grepl("occupation", uri)) {
    return(list(uri))
  }
  
  result <- tryCatch({
    # Build search URL
    search_url <- paste(c(esco_base_skill_url, uri), collapse = "")
    
    # Download JSON from ESCO API
    search_data <- jsonlite::fromJSON(getURL(search_url))
    
    # Related occupations
    essential_occupations <- unlist(search_data$`_links`$isEssentialForOccupation$uri)
    optional_occupations <- unlist(search_data$`_links`$isOptionalForOccupation$uri)
    
    occupations <- append(essential_occupations, optional_occupations)
    
    return(occupations)
  }, error=function(cond) {
    message(paste("Download failure"))
    message(query)
    message(cond)
    
    return(list(uri))
  })
  
}

esco_search_prepare <- function(query, count = 0) {
  
  # Apply synonyms
  query <- esco_query_synonym(query)
  
  # Abort search if result is already cached
  if (!is.null(esco_cache[[query]])) {
    return(NULL)
  }
  
  # Check ESCO API for suggestions
  result <- esco_search_api(query)
  
  # Save results and search misses in global list
  # TODO: Change to file appending
  if (is.null(result)) {
    
    # Know the number of misses
    esco_misses[[query]] <<- count
    list.save(esco_misses, "data/esco_misses.yaml")
  } else {
    
    # Save result to searches
    esco_cache[[query]] <<- result
    list.save(esco_cache, "data/esco_searches.yaml")
  }
  
  return(result)
}

esco_query_synonym <- function(query) {
  print(query)
  if (is.null(esco_synonyms[[query]])) {
    return(query)
  } else {
    return(esco_synonyms[[query]])
  }
}

# Find result in cache
esco_search <- function(query) {
  query <- esco_query_synonym(str_clean(query))
  esco_cache[[query]]
}

esco_map_tags <- function(tags) {
  mapply(esco_search_prepare, tags$tag, tags$count)
  esco_cache_translate()
  
  # Write down most recurring occupations
  inverted_results <- list_invert(list.clean(esco_cache, function(a) { length(a) > 20 }))
  inverted_results <- list.clean(inverted_results, function(a) { length(a) < 5 })
  list.save(inverted_results, "data/esco_searches_inverted.yaml")
}

# Translation to understand suggested occupations
esco_cache_translate <- function() {
  esco_cache <<- mapply(esco_cache_translate_list, names(esco_cache), esco_cache)
  list.save(esco_cache, "data/esco_searches.yaml")
}

esco_cache_translate_list <- function(key, values) {
  lapply(values, esco_combine_occupation_name)
}

# Function to fill out manual additions
esco_manual_add <- function(tag, professions) {
  esco_manual[[tag]] <<- professions
  list.save(esco_manual, "data/esco_manual.yaml")
}

# Function to remove manual additions
esco_manual_remove <- function(tag) {
  esco_manual[[tag]] <<- NULL
  list.save(esco_manual, "data/esco_manual.yaml")
}

# Cleaning of cache files
# TODO: Avoid empty list writing
esco_clean_cache <- function() {
  esco_synonyms <<- list.load("data/esco_synonyms.yaml")
  esco_cache <<- list.load("data/esco_manual.yaml")
  esco_misses <<- list()
  list.save(esco_cache, "data/esco_searches.yaml")
  list.save(esco_misses, "data/esco_misses.yaml")
}
