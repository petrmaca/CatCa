#' Nahraj data
#' 
#' @description Umožňuje přistupovat k datasetům ze synchronizované owncloud složky "used_data". Většina atributů načítá rovnou více datasetů. Stručný popis, viz níže. Pro přidání a dokumentaci datasetu upravte kód na githubu, nasynchronizujte data na owncloudu a znovu sestavte balík. Nebo kontaktujte správce.
#' 
#' @md
#' @param ... jméno/jména datasetu
#' 
#' @section Data(chars):
#' 
#' **CHARS** (data.table): Hydrologické charakteristiky povodi UPOV
#'   * UPOV_ID
#'   * NAZ_UTVAR Název útvaru
#'   
#' @section Data(geo):
#' 
#' **NADRZE** (shapefile): Významné vodní nádrže
#'   * položky atributové tabulky zatím nejasné
#'  
#' **UPOVS** (shapefile): Vrstva povodí    
#'   * UPOV_ID 
#' 
#' @section Data(qd):
#' 
#' **QD** (data.table): Denní průtoky
#' 
#' @section Data(routing): 
#' 
#' **TABA** (data.table): Tabulka následnosti povodí
#'  * FROM: UPOV_ID odkud (tece voda)
#'  * TO: UPOV_ID kam (tece voda)
#'  
#' **TABB** (data.table): Tabulka vsech hornich povodi pro danny UPOV
#'  * FROM:
#'  * TO:
#'    
#' **TABNAD** (data.table): Tabulka charakteristik nadrží
#' 
#' @section Data(uziv06):
#' 
#' **UZIV06** (data.table): užívání 2006-2016 v tis.m3/mes 
#'  * ?
#' 
#' **UZIV06mm** (data.table): užívání 2006-2016 v mm/mes
#'  * ?
#'  
#' @section Data(uziv79):
#' 
#' **UZIV79** (data.table): užívání 1979-2016 v tis.m3/mes
#'  * ?
#' 
#' **UZIV79mm** (data.table): užívání 1979-2016 v mm/mes
#'  * ?
#'  
#' @section Data(webapp_geo):
#' 
#' **JEZERA**
#' * ?
#' 
#' **POVODI**
#' * ?
#' 
#' **REKY**
#' * ?
#' 
#' **STANICE**
#' * ?
#' 
#'     
Data = function(...){

  E = expression(
    
    switch(name[j],
           
         'chars' = {
           CHARS = readRDS(file.path(.datadir, 'chmu', 'chars.rds'))
         },
         
         'geo' = {
           NADRZE = rgdal::readOGR(file.path(.datadir,"geo/nadrze.shp"), "nadrze")
           UPOVS = rgdal::readOGR(file.path(.datadir,"geo/UPOV_povodi.shp"), "UPOV_povodi")
         },
         
         'qd' = {
           QD = data.table(readRDS(file.path(.datadir, 'chmu/QD.rds')))
         },
         
         'routing' = {
           TABA = data.table::data.table(base::readRDS(file.path(.datadir,"routing/TABA.Rds")))
           TABB = data.table::data.table(base::readRDS(file.path(.datadir,"routing/TABB.Rds")))
           TABNAD = base::readRDS(file.path(.datadir,"routing/nadrze_tab.rds"))
         },
         
         'uziv06' = {
           UZIV06 = readRDS(file.path(.datadir, 'uzivani/06_16/uzivani_na_nahraz.rds'))
           UZIV06mm = readRDS(file.path(.datadir, 'uzivani/06_16/uzivani_na_nahraz_mm.rds'))
         },
         
         'uziv79' = {
           UZIV79 = readRDS(file.path(.datadir, 'uzivani/79_15/uzivani_upovid.rds'))
           UZIV79mm = readRDS(file.path(.datadir, 'uzivani/79_15/uzivani_upovid_mm.rds'))
         },
         
         'webapp_geo' = {
           REKY = rgdal::readOGR(file.path(.datadir,"webapp_geo/reky.shp"), "reky")
           JEZERA = rgdal::readOGR(file.path(.datadir,"webapp_geo/jezera.shp"), "jezera")
           POVODI = rgdal::readOGR(file.path(.datadir,"webapp_geo/povodi.shp"), "povodi")
           STANICE = rgdal::readOGR(file.path(.datadir,"webapp_geo/stanice.shp"), "stanice")
         }
         
  ) )
  
  name <- c(as.character(substitute(list(...))[-1L]))

  if (length(name) == 0) return(message('Dostupne datasety:\n\n', paste(names(E[[1]])[-(1:2)], collapse = '\n'), '\n'))

  give_paths()

  e = objects()

  for (j in 1:length(name)){
    eval(E)
  }

  e2 = objects()
  n = e2[!e2 %in% c(e, 'e', 'j', 'E')]

  for (i in 1:length(n)){
   assign(n[i], eval(parse(text = n[i])), envir = .GlobalEnv)
  }

  cat('\nLOADED OBJECTS: \n=====================\n')
  cat(n, sep = '\n')

}