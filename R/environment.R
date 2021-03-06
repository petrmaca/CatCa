#' Načte globální cesty
#'
#' @description Načte proměnné \code{.datadir} (synchronizovaná owncloud složka "used_data" z BILAN_UPOV) a \code{.workdir} (zatím nevyužito) na základě jména počítače. Před použitím je nutné PC registrovat - buď úpravou kódu na githubu nebo emailem na \email{hanel@fzp.czu.cz} ve struktuře
#' \preformatted{
#' jmeno_PC = { 
#'   .datadir = 'cesta do used_data' 
#'   .workdir = 'pracovni cesta' 
#' } 
#' }
#' 
#' @usage give_paths
#' @return Přiřadí do \code{.GlobalEnv} proměnné \code{.datadir} a \code{.workdir}
#' @export give_paths
give_paths <- function(){
where <- if(.Platform[["OS.type"]] == 'unix') (Sys.info()['nodename']) else (Sys.getenv('COMPUTERNAME'))

switch(where,
         'match' = {
           .datadir = "/home/owc/BILAN_UPOV/used_data/"
           .workdir = ""
         },
        'adam' = {
          .datadir = "/home/adam/Shared/BILAN_UPOV/used_data"
          .workdir = ""
        },  
         'desrt' = {
           .datadir <- "/home/owc/BILAN_UPOV/used_data/"
           .workdir <- ''
         },
         'DESKTOP-444RM63' = {
           .datadir <- "C://Users//PetrP//ownCloud//data//used_data//"
           .workdir <- "C://Users//PetrP//Documents//BILAN_OWNCL//"
         }, 
       "lest" = {
         .datadir = "/home/owc/BILAN_UPOV/used_data/"
         .workdir = ""
       },
       "bastap-pc-01" = {
         .datadir = "D:\\ownCloud\\Shared\\BILAN_UPOV\\used_data"
         .workdir = ""
       },
       "HERMANOVSKY-01" = {
         .datadir = "D://muj_cloud//Shared//BILAN_UPOV//used_data"
         .workdir = ""
       },

       "LEST" = {
         .datadir = "C://testR//data//"
         .workdir = "C://testR//"
       },
       'IRINA' = {
         .datadir = "C:\\Users\\Irina\\ownCloud\\Shared\\BILAN_UPOV\\used_data"
         .workdir = ""
       },
       'hubert' = {
         .datadir = "/home/hubert/ownCloud/Shared/BILAN_UPOV/used_data"
         .workdir = ""
       }

)

  if (is.null(.datadir)) {
    .datadir = NA
    warning('Pocitac ', where, 'nenalezen v environment.R - data nedostupna!\n Uprav environment.R a push na github.com/KVHEM/CatCa')
    }

  if (is.null(.workdir)) {
    .workdir = getwd()
    warning('Pocitac ', where, 'nenalezen v environment.R - pracovni adresar nastaven automaticky!')
  }

  assign('.datadir', .datadir, envir = .GlobalEnv)
  assign('.workdir', .workdir, envir = .GlobalEnv)
}



.onLoad <- function(libname, pkgname) {
  
  give_paths()
  invisible()
}

