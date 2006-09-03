## Loop through all attached directories looking for
## regular expression pattern.

if (is.R()) { ## R

     objip <-
     function(pattern, where = search(), frame=NULL)
     {
       ## frame is ignored in the R version
       result <- list()
       for(i in match(where, search())) {
         obj <- objects(pos=i, pattern = pattern)
         if(length(obj) > 0)
           result[[where[i]]] <- obj
       }
       result
     }
     
   } else { ## S-Plus
     
     objip <-
       function(pattern, where=search(), frame=NULL)
     {
       ##Richard M. Heiberger, October 1998, revised July 2006
       result <- list()
       for(i in where) {
         obj <- objects(i, regexpr.pattern = pattern)
         if(length(obj) > 0) result[[i]] <- obj
       }
       for(i in frame) {
         obj <- objects(frame=i, regexpr.pattern = pattern)
         if(length(obj) > 0) result[[paste("Frame",i,sep=".")]] <- obj
       } 
       result
     }
     
   }
