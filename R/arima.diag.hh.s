arima.diag.hh <-
  if.R(r=
       function(z, acf.resid = TRUE,
                lag.max = round(max(gof.lag + n.parms + 1, 10 * log10(n))),
                gof.lag = 15, resid = FALSE,
                std.resid = TRUE, plot = TRUE, type = "h", ...,
                x=eval(parse(text = series.name)))
       {
         n.parms <- NA
         n <- NA
         series.name <- NA
         NA
       }
       ,s=
       function(z, acf.resid = TRUE,
                lag.max = round(max(gof.lag + n.parms + 1, 10 * log10(n))),
                gof.lag = 15, resid = FALSE,
                std.resid = TRUE, plot = TRUE, type = "h", ...,
                x=eval(parse(text = series.name), local = sys.parent()))
{
	if(!is.list(z))
		stop("z must be a list")
	cname <- names(z)
	series.name <- z$series
	if(is.null(series.name))
		stop("missing component series in argument z\n")
	## x <- eval(parse(text = series.name), local = sys.parent())  ## rmh
	model <- z$model
	if(is.null(model))
		stop("missing component model in argument z\n")
	if(!is.null(z$reg.series)) {
		if(z$reg.series == "intercept")
			xreg <- eval(z$call$xreg, local = sys.parent())
		else xreg <- eval(parse(text = z$reg.series), local = sys.parent())
		reg.coef <- z$reg.coef
		if(is.null(reg.coef))
			stop("missing component reg.coef in argument z\n")
	}
	else {
		xreg <- NULL
		reg.coef <- NULL
	}
	save.acf <- acf.resid
	save.resid <- resid
	save.std.resid <- std.resid
	if(!is.null(xreg)) {
		new.data <- .arima.bind.xreg(x, xreg)
		x <- new.data$x
		xreg <- new.data$xreg
	}
	save.x <- x
	save.xreg <- xreg
	filt <- arima.filt(save.x, model = model, xreg = save.xreg, reg.coef = reg.coef)[c("pred", "var.pred",
		"n.used")]
	n <- filt$n.used
	resid <- save.x - filt$pred
	# ACF of residuals
	n.parms <- .arima.n.parms(model)
	if(!anyMissing(x)) {
		acf.list <- acf(resid, lag.max = lag.max, plot = FALSE)
		# partial acf too
		pacf.list <- acf(resid, lag.max = lag.max, type = "partial", plot = FALSE)
		# Bartlett's SE's for acf estimates, pp. 35 of Box & Jenkins
		N <- length(resid)
		se <- sqrt(c(1, 1 + 2 * cumsum(acf.list$acf[-1]^2))/N)
		if(gof.lag > 0) {
			if(gof.lag > n.parms) {
				# Box Ljung Diagnostics (Q-test), pp. 79 of Harvey
				nlags <- gof.lag - n.parms
				gof <- N * (N + 2) * cumsum((acf.list$acf[2:(gof.lag + 1)]^2)/(N - (1:gof.lag)
					))
				gof.df <- 1:nlags
				gof.p.value <- numeric(nlags)
				gof <- gof[(n.parms + 1):gof.lag]
				for(i in 1:nlags)
					gof.p.value[[i]] <- (1 - pchisq(gof[[i]], gof.df[[i]]))
				gof <- list(statistic = gof, df = gof.df, p.value = gof.p.value, lag =
					acf.list$lag[(n.parms + 1):gof.lag + 1])
			}
			else stop(paste("gof.lag must be bigger than", n.parms))
		}
	}
	else if(acf.resid | gof.lag > 0) {
		save.acf <- FALSE
		gof.lag <- 0
		cat("Warning: couldn't compute acf.list or gof due to NA's\n")
	}
	if(std.resid)
		std.resid <- resid/sqrt(filt$var.pred)
	diag <- c(if(save.acf) list(acf.list = acf.list), if(save.acf) list(pacf.list = pacf.list), if(
		save.acf) list(acf.se = se), if(gof.lag > n.parms) list(gof = gof), if(save.std.resid) list(
			std.resid = std.resid), if(save.resid) list(resid = resid), list(series = series.name),
		list(model.order = model$order), list(reg.coef = reg.coef))
	if(plot) {
		arima.diag.plot(diag, type = type, ...)
		invisible(diag)
	}
	else diag
}
)
