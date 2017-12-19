getTypeTitle <- function(LTB, LTI){
  type <- NULL

  # Voorkomt een warning. 
  if(!is.null(LTB) && !is.null(LTI)){
    # Of de button geselecteerd is of niet, hangt af van hoevaak er op de button geklikt is
    if(!(LTB %% 2) == 0 && !(LTI %% 2) == 0){
      type <- "bold.italic"
    }else if(!(LTI %% 2) == 0 && (LTB %% 2) == 0){
      type <- "italic"
    }else if(!(LTB %% 2) == 0 && (LTI %% 2) == 0){
      type <- "bold"
    }else if((LTB %% 2) == 0 && (LTI %% 2) == 0){
      type <- NULL
    }
  }else{
    invisible("LTB en LTI zijn null")
  }
 
  return(type)
}

getTypeSubtitle <- function(LTBS, LTIS){
  typeS <- NULL
  
  # Voorkomt een warning. 
  if(!is.null(LTBS) && !is.null(LTIS)){
    # Of de button geselecteerd is of niet, hangt af van hoevaak er op de button geklikt is
    if(!(LTBS %% 2) == 0 && !(LTIS %% 2) == 0){
      typeS <- "bold.italic"
    }else if(!(LTIS %% 2) == 0 && (LTBS %% 2) == 0){
      typeS <- "italic"
    }else if(!(LTBS %% 2) == 0 && (LTIS %% 2) == 0){
      typeS <- "bold"
    }else if((LTBS %% 2) == 0 && (LTIS %% 2) == 0){
      typeS <- NULL
    }
  }else{
    invisible("LTBS en LTIS zijn null")
  }
  return(typeS)
}

