## many examples of xysplom
## these were removed from ?xysplom to keep its size under control.

## xysplom syntax options

tmp <- data.frame(y=rnorm(12), x=rnorm(12), z=rnorm(12), w=rnorm(12),
                  g=factor(rep(1:2,c(6,6))))
tmp2 <- tmp[,1:4]

xysplom(y ~ x + w , data=tmp)
xysplom(y ~ x + w , data=tmp, cartesian=FALSE)

xysplom(y + w ~ x , data=tmp, corr=TRUE)
xysplom(y + w ~ x , data=tmp, beta=TRUE)

xysplom(y + w ~ x , data=tmp, corr=TRUE, beta=TRUE, cartesian=FALSE, layout=c(1,2))

xysplom(y + w ~ x , data=tmp, abline=TRUE)
xysplom(y + w ~ x , data=tmp, corr=TRUE, abline=FALSE)

xysplom(y + x ~ z | g, data=tmp)
xysplom(y + x ~ z | g, data=tmp, layout=c(2,2))
xysplom(y + x ~ z | g, data=tmp, cartesian=FALSE)
xysplom(y + x ~ z | g, data=tmp, cartesian=FALSE, layout=c(2,2))

xysplom(y + x ~ z | g, data=tmp, cartesian=FALSE)

xysplom(w + y ~ x + z, data=tmp)
xysplom(w + y ~ x + z | g, data=tmp, layout=c(2,4))
xysplom(w + y ~ x + z | g, data=tmp, cartesian=FALSE)
xysplom(w + y ~ x + z | g, data=tmp, corr=TRUE, beta=TRUE, abline=FALSE)

xysplom(w + y ~ x + z, data=tmp, scales=list(relation="same"))
xysplom(w + y ~ x + z, data=tmp, x.relation="same")

xysplom(~ y + x + z , data=tmp)
xysplom(~ y + x + z | g, data=tmp)
xysplom(~ y + x + z | g, data=tmp, corr=TRUE)
xysplom(~ y + x + z | g, data=tmp, corr=TRUE, digits=2)
xysplom(~ y + x + z | g, data=tmp, corr=TRUE, layout=c(3,6), par.strip.text=list(cex=.5))

## These three examples run from R and the command line in S-Plus.
## They don't run from inside the Splus CMD check.
xysplom(~ tmp)
xysplom(~ tmp | tmp$g)
xysplom(tmp$y ~ tmp2 | tmp$g)

xysplom(g ~ x , data=tmp)
xysplom(x ~ g , data=tmp)


## Subscripting requires the x=, y= notation.
## Subscripting doesn't work with the y ~ x notation.

  try(xysplom( ~ tmp[, c("x","y")]))                   ## doesn't work
  try(xysplom(tmp2[, c("w","z")] ~ tmp[, c("x","y")])) ## doesn't work

## use this instead
  xysplom(x = tmp[, c("x","y")])
  xysplom(y   = tmp2[, c("w","z")],  x   = tmp[, c("x","y")])

## or, even better, use the y ~ x notation
  xysplom(~ x + y, data=tmp)
  xysplom(w + z ~ x + y, data=cbind(tmp, tmp2))


## xyplot in R has many similar capabilities with xysplom
if.R(r=
       xyplot(w + z ~ x + y, data=tmp, outer=TRUE)
     ,s={}
    )
