
StatAlluvium <- ggproto(
  "StatAlluvium", Stat,
  setup_data = function(data, params) {

    if (is.null(data$weight)) data$weight <- rep(1, nrow(data))

    # if group variable does not respect axes, print message and fix it
    interact <- interaction(data[, grep("^axis[0-9\\.]*", names(data))])
    if (!splinters(data$group, interact)) {
      message(paste0("'group' assignments do not respect axis assignments,",
        " and will be ignored."))
      data$group <- as.numeric(interact)
    }

    # aggregate over axes and groups by weight
    aggregate(
      formula = as.formula(paste("weight ~",
        paste(setdiff(names(data), "weight"),
          collapse = "+"))),
      data = data,
      FUN = sum
    )
  },
  compute_panel = function(data, scales, params,
    axis_width = 1/3) {

    axis_ind <- get_axes(names(data))

    # x and y coordinates of center of flow at each axis
    compute_alluvium <- function(i) {

      # order axis indices
       axis_seq <- axis_ind[zigzag_new(n = length(axis_ind), i = i, k = 1)]
      #axis_seq <- axis_ind[zigzag(n = length(axis_ind), i = i)]

       # order ribbons according to axes, in above order
      ribbon_seq <- do.call(order, data[axis_seq])

      # ribbon floors and ceilings along axis
      ymin_seq <- c(0, cumsum(data$weight[ribbon_seq]))
      ymax_seq <- c(cumsum(data$weight[ribbon_seq]), sum(data$weight))

      # ribbon breaks
      cbind(i,
        ymin_seq[order(ribbon_seq)],
        ymax_seq[order(ribbon_seq)])
    }

    alluvia <- do.call(rbind, lapply(1:length(axis_ind), compute_alluvium))
    colnames(alluvia) <- c("x", "ymin", "ymax")
    data <- data.frame(data, alluvia)

    # widths and x bounds
    data$xmin <- data$x - axis_width / 2
    data$xmax <- data$x + axis_width / 2
    data$width <- axis_width

    # y centers
    data$y <- (data$ymin + data$ymax) / 2

    data
  }
)


stat_alluvium <- function(mapping = NULL, data = NULL, geom = "alluvium",
  position = "identity", na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatAlluvium, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


GeomAlluvium <- ggproto(
  "GeomAlluvium", Geom,
  default_aes = aes(size = .5, linetype = 1,
    colour = 0, fill = "gray", alpha = .5),
  setup_data = function(data, params) data,
  draw_group = function(data, panel_scales, coord,
    ribbon_bend = 1/6) {

    first_row <- data[1, setdiff(names(data), c("x", "xmin", "xmax",
      "y", "ymin", "ymax",
      "width")),
      drop = FALSE]
    rownames(first_row) <- NULL
    if (nrow(data) == 1) {
      # spline coordinates (one axis)
      spline_data <- data.frame(
        x = data$x + data$width / 2 * c(-1, 1, 1, -1),
        y = data$ymin + first_row$weight * c(0, 0, 1, 1),
        shape = rep(0, 4)
      )
    } else {
      # spline coordinates (more than one axis)
      x_oneway <- rep(data$x, c(3, rep(4, nrow(data) - 2), 3)) +
        rep(data$width, c(3, rep(4, nrow(data) - 2), 3)) / 2 *
        c(-1, rep(c(1, 1, -1, -1), nrow(data) - 1), 1) +
        ribbon_bend * c(0, rep(c(0, 1, -1, 0), nrow(data) - 1), 0)
      y_oneway <- rep(data$ymin, c(3, rep(4, nrow(data) - 2), 3))
      shape_oneway <- c(0, rep(c(0, 1, 1, 0), nrow(data) - 1), 0)
      spline_data <- data.frame(
        x = c(x_oneway, rev(x_oneway)),
        y = c(y_oneway, rev(y_oneway) + first_row$weight),
        shape = rep(shape_oneway, 2)
      )
    }
    data <- data.frame(first_row, spline_data)

    # transform (after calculating spline paths)
    coords <- coord$transform(data, panel_scales)

    # graphics object
    grid::xsplineGrob(
      x = coords$x, y = coords$y, shape = coords$shape,
      open = FALSE,
      gp = grid::gpar(
        col = coords$colour, fill = coords$fill, alpha = coords$alpha,
        lty = coords$linetype, lwd = coords$size * .pt
      )
    )
  },
  draw_key = draw_key_polygon
)


geom_alluvium <- function(mapping = NULL, data = NULL, stat = "alluvium",
  na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
  ...) {
  layer(
    geom = GeomAlluvium, mapping = mapping, data = data, stat = stat,
    position = "identity", show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



get_axes <- function(x) {
  axis_ind <- grep("^axis[0-9\\.]*$", x)
  axis_ind[order(as.numeric(gsub("^axis", "", x[axis_ind])))]
}



ggalluvial <- function(...) {
  input_list <- list(...)
  if (!is.null(input_list[["formula"]]) | any(sapply(input_list, is.call))) {
    ggalluvial.formula(...)
  } else {
    ggalluvial.default(...)
  }
}


ggalluvial.default <- function(...) {
  input_list <- list(...)
  aes_input <- input_list[[which(sapply(input_list, class) == "uneval")]]
  axis_input <- aes_input[grep("^axis[0-9\\.]$", names(aes_input))]
  axis_breaks <- as.numeric(gsub("^axis", "", names(axis_input)))
  axis_labels <- unname(as.character(axis_input))
  ggplot(...) +
    geom_alluvium() +
    geom_stratum() +
    geom_text(stat = "stratum") +
    scale_x_continuous(breaks = axis_breaks, labels = axis_labels)
}


ggalluvial.formula <- function(formula, data = NULL, weight, ...) {
  formula <- stats::as.formula(formula)
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      data <- as.data.frame(data)
      if (!missing(weight)) {
        warning("Using weights from table; 'weight' argument ignored")
      }
      weight <- "Freq"
    }
  }

  rhs <- labels(stats::terms(formula))
  lhs <- setdiff(all.vars(formula), rhs)

  if (length(lhs) > 1) {
    stop("Multilpe variables on LHS of '%s'")
  }

  luv_data <- stats::model.frame(formula = formula, data = data)
  if (!missing(weight)) {
    if (is.character(weight)) {
      luv_data[[weight]] <- data[[weight]]
    } else {
      luv_data$weight <- weight
    }
  }

  formula_aes <- aes()
  if (!missing(weight)) formula_aes[["weight"]] <-
    if (is.character(weight)) as.name(weight) else as.name("weight")

  # choose categorical or time series format based on number of RHS variables
  dep_incl <- (length(formula) == 3)
  if (length(rhs) > 1) {

    formula_axes <- rhs
    for (i in 1:length(formula_axes)) {
      formula_aes[[paste0("axis", i)]] <- as.name(formula_axes[i])
    }
    if (dep_incl) formula_aes[["fill"]] <- as.name(lhs)

    ggalluvial.default(luv_data, formula_aes, ...)

  } else {

    formula_aes$x <- as.name(rhs)
    grp <- as.name(lhs)
    formula_aes$group <- grp
    formula_aes$fill <- grp
    formula_aes$colour <- grp

    ggplot(data = luv_data, mapping = formula_aes, ...) +
      geom_alluvium_ts()

  }
}



splinters <- function(x, y) {
  y_x <- interaction(x, y)
  all(match(x, unique(x)) == match(y_x, unique(y_x)))
}



StatStratum <- ggproto(
  "StatStratum", Stat,
  setup_data = function(data, params) {

    if (is.null(data$weight)) data$weight <- rep(1, nrow(data))

    data <- aggregate(
      formula = as.formula(paste("weight ~",
        paste(setdiff(names(data), "weight"),
          collapse = "+"))),
      data = data,
      FUN = sum
    )

    axis_ind <- get_axes(names(data))
    # stack axis-aggregated data with cumulative frequencies
    res_data <- do.call(rbind, lapply(unique(data$PANEL), function(p) {
      p_data <- subset(data, PANEL == p)
      do.call(rbind, lapply(1:length(axis_ind), function(i) {
        agg <- aggregate(x = p_data$weight, by = p_data[axis_ind[i]],
          FUN = sum)
        names(agg) <- c("label", "weight")
        cbind(pos = i, agg, cumweight = cumsum(agg$weight), PANEL = p)
      }))
    }))

    # add group
    cbind(res_data, group = 1:nrow(res_data))
  },
  compute_group = function(data, scales,
    axis_width = 1/3) {

    rownames(data) <- NULL
    rect_data <- data.frame(x = data$pos,
      y = (data$cumweight - data$weight / 2),
      width = axis_width)
    data.frame(data, rect_data)
  }
)


stat_stratum <- function(mapping = NULL, data = NULL, geom = "stratum",
  na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, ...) {
  layer(
    stat = StatStratum, data = data, mapping = mapping, geom = geom,
    position = "identity", show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


GeomStratum <- ggproto(
  "GeomStratum", GeomRect,
  default_aes = aes(size = .5, linetype = 1,
    colour = "black", fill = "white", alpha = 1),
  setup_data = function(data, params) {

    transform(data,
      xmin = x - width / 2, xmax = x + width / 2,
      ymin = y - weight / 2, ymax = y + weight / 2)
  },
  draw_key = draw_key_polygon
)


geom_stratum <- function(mapping = NULL, data = NULL, stat = "stratum",
  na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
  ...) {
  layer(
    geom = GeomStratum, mapping = mapping, data = data, stat = stat,
    position = "identity", show.legend = show.legend,
    inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
  )
}


zigzag <- function(n, i) {
  if(!(i %in% 1:n)) stop('the given index lies outside the range')

  # Radii
  r1 <- i - 1
  r2 <- n - i
  r <- min(r1, r2)

  # Attempt cohesion in the direction of the closer end
  leftward <- (i <= n / 2)

  # Setup
  sgn <- if(r1 == r2) 0 else (r2 - r1) / abs(r2 - r1)
  rem <- (i + sgn * (r + 1)):((n+1)/2 + sgn * (n-1)/2)
  zz <- (1 - 2 * leftward) * c(1, -1)

  # Order
  c(i,
    if(r == 0) c() else sapply(1:r, function(j) i + j * zz),
    if(sgn == 0) c() else rem)
}


# zigzag_new <- function(n, i, k=1) {
#
#   if(!(i %in% 1:n)) stop('the given index lies outside the range')
#
#   # Radii
#   r1 <- i - 1
#   r2 <- n - i
#   r <- min(r1, r2)
#
#   # Attempt cohesion in the direction of the closer end
#   leftward <- (i <= n / 2)
#
#   # Setup
#   sgn <- if(r1 == r2) 0 else (r2 - r1) / abs(r2 - r1)
#   rem <- (i + sgn * (r + 1)):((n+1)/2 + sgn * (n-1)/2)
#   zz <- (1 - 2 * leftward) * c(1, -1)
#
#   # Order
#   init <- c(i,
#     if(r == 0) c() else sapply(1:r, function(j) i + j * zz),
#     if(sgn == 0) c() else rem)
#
#   init1 <-  init[-which(init==k)]
#   init2 <-  init1[-which(init==i)]
#
#   if (i !=1) {
#     c(i, k, init2)
#   } else {
#     c(i, 2, init2)
#   }
#
# }

zigzag_new <- function(n, i, k=1) {

  full    <- 1:n
  front <- union(i,k)
  back  <- setdiff(full, front)
  c(front, back)
}


zigzag_new(n = 5, i = 2, k = 1)
