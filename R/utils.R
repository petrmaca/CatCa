
A = function(id){
  structure(CHARS[ID %in% id, A * 1000000], names = id)
}


q2mm_day = Vectorize(function(q, area = NULL, id = NULL){
  
  if (is.null(area) & is.null(id)) return(NA)
  if (is.null(area) & !is.null(id)) area = A(id)
  
 (1000 * q * 3600 * 24) / (area) 
  
})

u2mm_day = function(u, area = NULL, id = NULL, dtm = NA){

  if (is.null(area) & is.null(id)) return(NA)
  if (is.null(area) & !is.null(id)) area = A(id)
  
  uu = u / area 
  if (any(is.na(dtm))) return(uu/30.4375)
  
  uu/days_in_month(dtm)
  
}

u2mm_month = function(u, area = NULL, id = NULL){
  
  if (is.null(area) & is.null(id)) return(NA)
  if (is.null(area) & !is.null(id)) area = A(id)
  
  uu = u / area 
  
  uu
  
}

char = function(id){
  CHARS[UPOV_ID %in% id]
}
