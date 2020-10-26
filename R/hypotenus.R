#' A Hypotenuse Length Function
#'
#' The Pythagorean theorem states that the square of the hypotenuse (the side opposite the right angle) is equal to the sum of the squares of the other two side: c2=a2+b2. This function, given the lengths of two sides of the triangle, calculates the length of the third side.
#' @param a Length of first side
#' @param b Length of second side
#' @param c Length of opposite side
#' @export
#' @examples
#' hypotenuse_length()


hypotenuse_length <- function(a=NULL,b=NULL,c=NULL){
  if(is.null(c)){
    sides <- c(a, b)
    if(any(sides < 0)){
      message("sides must be positive")
    } else if(!is.numeric(x = sides)){
      message("sides can not be non-numeric")
    } else
    {
      c=sqrt(x = sum(sides ^ 2))
    }
  }
  else if (is.null(a)){
    sides=c(b,c)
    if(any(sides<0)){
      message("sides must be positive")
    }else if(!is.numeric(x=sides)){
      message("sides can not be non-numeric")
    }else{
      a=sqrt(c^2-b^2)
    }
  }
  else if (is.null(b)){
    sides=c(a,c)
    if(any(sides<0)){
      message("sides must be positive")
    }else if(!is.numeric(x=sides)){
      message("sides can not be non-numeric")
    }else{
      b=sqrt(c^2-a^2)
    }
  }
  mylist=c("a"=a,"b"=b,"c"=c)
  return(mylist)
}
