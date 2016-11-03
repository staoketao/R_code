autopolsample[doc == 2 & renew == 1,autrenew := 1]
grep("PP",colnames(final.data))
'PPlus' %in% colnames(final.data)

tmp[1:200, c("SumInsTPO","SumInsPD","SumIns"),with=F]

    
    
missRep=function(x){
#   if (is.numeric(x)){
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x 
#   }
}
  else if (is.factor(x)) {
    tab <- table(x)
    l <- sum(is.na(x))
    sample(names(tab)[tab==max(tab)], l, TRUE)
    }
}

b_spline_creation <- function(vec, col_name,
                              knots_percentiles = seq(0,1, by=0.2),
                              polynomial_degrees = 3) {
    # vec is the vector to be converted to b-spline
    # col_name is the name of the column/variable
    require(splines)
    require(data.table)
    #remove0variance_col <- function(df) {df[,apply(df, 2, var, na.rm=TRUE) != 0]}
    remove0variance_col <- function(df) {df[, sapply(apply(df, 2, var, na.rm=TRUE), checkNon0), drop=FALSE]}
    knots <- quantile(vec, knots_percentiles)  # knots are selected on data records level, not distinct value level
    vec <- unique(vec)
    knots_ <- sort(unique(pmin(pmax(min(vec)+ (1e-10), knots), max(vec) - (1e-10))))
    bspline <- splines::bs(vec, df = polynomial_degrees, knots=knots_)
    bspline <- cbind(as.data.frame(vec), as.data.frame(bspline))
    colnames(bspline) <- c(col_name, paste(col_name,"_s", seq_len(ncol(bspline)-1),sep=""))
    bspline <- remove0variance_col(bspline)
    if (ncol(bspline) > 1 & FALSE) { #disable the drop of last column feature
        # drop last column since the original value is included here
        bspline <- bspline[,seq(1,ncol(bspline)-1)]
    }
    bspline <- data.table(bspline)
    eval(parse(text=paste("setkey(bspline, ",col_name,")", sep="")))
    print(paste("The data returned is", class(bspline[1])[1]))
    return(bspline)
}


dat2 = data.frame(apply(dat,2,missRep))

set.seed(1)
dat <- data.frame(x=sample(letters[1:3],20,TRUE), 
                  y=sample(letters[1:3],20,TRUE),
                  w=rnorm(20),
                  z=sample(letters[1:3],20,TRUE),                  
                  stringsAsFactors=FALSE)

dat[c(5,10,15),1] <- NA
dat[c(3,7),2] <- NA


ss=data.frame(apply(df,2,f))


DT[,value := ifelse(is.na(value), median(value, na.rm=TRUE), value), by=months]


b_spline_creation <- function(vec, col_name,
                              knots_percentiles = seq(0,1, by=0.2),
                              polynomial_degrees = 3)

tmp3[,paste0(names(tmp2)[i],"_s1")] <- data.frame(ns(tmp2[,i],4,knots=seq(qnt[1],qnt[2],length=5)[2:4])[,1])
