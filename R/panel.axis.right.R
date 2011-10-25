panel.axis.right <-
function (side = c("bottom", "left", "top", "right"), at = pretty(scale.range), 
    labels = TRUE, draw.labels = TRUE, check.overlap = FALSE, 
    outside = FALSE, ticks = TRUE, half = !outside, which.half = switch(side, 
        bottom = "lower", left = "upper", top = "upper", right = "lower"), 
    tck = as.numeric(ticks), rot = if (is.logical(labels)) 0 else c(90, 
        0), text.col = axis.text$col, text.alpha = axis.text$alpha, 
    text.cex = axis.text$cex, text.font = axis.text$font, text.fontfamily = axis.text$fontfamily, 
    text.fontface = axis.text$fontface, text.lineheight = axis.text$lineheight, 
    line.col = axis.line$col, line.lty = axis.line$lty, line.lwd = axis.line$lwd, 
    line.alpha = axis.line$alpha) 
{
    side <- match.arg(side)
    orientation <- if (outside) 
        "outer"
    else "inner"
    cpl <- current.panel.limits()
    scale.range <- range(switch(side, left = cpl$ylim, top = cpl$xlim, 
        right = cpl$ylim, bottom = cpl$xlim))
    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")
    rot <- rep(rot, length.out = 2)
    if (is.null(at) || length(at) == 0) 
        return()
    if (is.logical(labels)) 
        labels <- if (labels) 
            format(at, trim = TRUE)
        else NULL
    if (check.overlap) {
        pad <- lattice.getOption("skip.boundary.labels")
        scale.range <- lattice:::extend.limits(scale.range, prop = -pad)
    }
    keep.at <- at >= scale.range[1] & at <= scale.range[2]
    at <- at[keep.at]
    labels <- labels[keep.at]
    keep.labels <- TRUE
    nal <- length(at)/2 + 0.5
    all.id <- seq_along(at)
    lower.id <- all.id <= nal
    upper.id <- all.id >= nal
    axid <- if (half) {
        if (which.half == "lower") 
            lower.id
        else upper.id
    }
    else rep(TRUE, length(all.id))
    gp.line <- gpar(col = line.col, alpha = line.alpha, lty = line.lty, 
        lwd = line.lwd)
    gp.text <- gpar(col = text.col, cex = text.cex, alpha = text.alpha, 
        fontface = lattice:::chooseFace(text.fontface, text.font), fontfamily = text.fontfamily, 
        lineheight = text.lineheight)
    axis.units <- lattice.getOption("axis.units")[[orientation]][[side]]
    axis.settings <- trellis.par.get("axis.components")[[side]]
    tck.unit.x <- tck * axis.settings$tck * axis.units$tick$x
    tck.unit <- unit(x = tck.unit.x, units = axis.units$tick$units)
    lab.unit <- if (any(tck.unit.x > 0)) 
        tck.unit + unit(x = axis.settings$pad1 * axis.units$pad1$x, 
            units = axis.units$pad1$units)
    else unit(x = axis.settings$pad1 * axis.units$pad1$x, units = axis.units$pad1$units)
    orient.factor <- if (outside) 
        -1
    else 1
    if (ticks && any(tck.unit.x != 0)) 
        switch(side, bottom = grid.segments(x0 = unit(at[axid], 
            "native"), x1 = unit(at[axid], "native"), y0 = unit(0, 
            "npc"), y1 = orient.factor * tck.unit, name = trellis.grobname("ticks.bottom", 
            type = "panel"), gp = gp.line), top = grid.segments(x0 = unit(at[axid], 
            "native"), x1 = unit(at[axid], "native"), y0 = unit(1, 
            "npc"), y1 = unit(1, "npc") - orient.factor * tck.unit, 
            name = trellis.grobname("ticks.top", type = "panel"), 
            gp = gp.line), left = grid.segments(y0 = unit(at[axid], 
            "native"), y1 = unit(at[axid], "native"), x0 = unit(0, 
            "npc"), x1 = orient.factor * tck.unit, name = trellis.grobname("ticks.left", 
            type = "panel"), gp = gp.line), right = grid.segments(y0 = unit(at[axid], 
            "native"), y1 = unit(at[axid], "native"), x0 = unit(1, 
            "npc"), x1 = unit(1, "npc") - orient.factor * tck.unit, 
            name = trellis.grobname("ticks.right", type = "panel"), 
            gp = gp.line))
    if (draw.labels && !is.null(labels)) {
        {
            just <- if (outside) 
                switch(side, 
bottom = if (rot[1] == 0) c("centre", 
                  "top") else c("right", "centre"), top = if (rot[1] == 
                  0) c("centre", "bottom") else c("left", "centre"), 
                  
left = if (rot[2] == 90) c("centre", "bottom") else c("right", 
                    "centre"), right = if (rot[2] == 90) c("centre", 
                    "top") else c("right", "centre"))
            else switch(side, bottom = if (rot[1] == 0) c("centre", 
                "bottom") else c("left", "centre"), 
top = if (rot[1] == 
                0) c("centre", "top") else c("right", "centre"), 
                left = if (rot[2] == 90) c("centre", "top") else c("left", 
                  "centre"), 
right = if (rot[2] == 90) c("centre", 
                  "bottom") else c("right", "centre"))
        }
        switch(side, 
bottom = grid.text(label = labels[axid & 
            keep.labels], x = unit(at[axid & keep.labels], "native"), 
            y = orient.factor * lab.unit, rot = rot[1], check.overlap = check.overlap, 
            just = just, name = trellis.grobname("ticklabels.bottom", 
                type = "panel"), gp = gp.text), 
top = grid.text(label = labels[axid & 
            keep.labels], x = unit(at[axid & keep.labels], "native"), 
            y = unit(1, "npc") - orient.factor * lab.unit, rot = rot[1], 
            check.overlap = check.overlap, just = just, name = trellis.grobname("ticklabels.top", 
                type = "panel"), gp = gp.text), 
left = grid.text(label = labels[axid & 
            keep.labels], y = unit(at[axid & keep.labels], "native"), 
            x = orient.factor * lab.unit, rot = rot[2], check.overlap = check.overlap, 
            just = just, name = trellis.grobname("ticklabels.left", 
                type = "panel"), gp = gp.text), 
right = grid.text(label = labels[axid & 
            keep.labels], y = unit(at[axid & keep.labels], "native"), 
            x = unit(1, "npc") - (orient.factor-1) * lab.unit, rot = rot[2], 
            check.overlap = check.overlap, just = just, name = trellis.grobname("ticklabels.right", 
                type = "panel"), gp = gp.text))
    }
    invisible()
}


## panel.axis.right was written by David Winsemius, based on the
## lattice:::panel.axis function.
## https://stat.ethz.ch/pipermail/r-help/2011-October/292806.html

## source("c:/HOME/rmh/HH-R.package/HH/R/panel.axis.right.R")
