# =============================================================================
# DRSP GAM Template — Core GAM Fitting & Plotting Functions
# =============================================================================
# Reusable functions extracted from HERA_08_single_GAMs.Rmd.
# These handle GAM fitting, prediction, plotting, and reporting.
# =============================================================================

# ---- Required packages ----
required_packages <- c("dplyr", "ggplot2", "ggtext", "mgcv", "patchwork",
                       "gridExtra", "grid", "tibble", "tidyr", "scales", "glue")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Required package '", pkg, "' is not installed. Install with: install.packages('", pkg, "')")
  }
}

# =============================================================================
# Colorblind-safe color palette for DRSP outcomes
# =============================================================================
build_drsp_color_map <- function(outcomes) {
  # Base palette: Okabe-Ito + extensions (colorblind-friendly)
  base_colors <- c(
    "drsp_1"  = "#0072B2", "drsp_2"  = "#E69F00", "drsp_3"  = "#009E73",
    "drsp_4"  = "#D55E00", "drsp_5"  = "#CC79A7", "drsp_6"  = "#56B4E9",
    "drsp_7"  = "#F0E442", "drsp_8"  = "#000000", "drsp_9"  = "#999999",
    "drsp_10" = "#004488", "drsp_11" = "#BB5566", "drsp_12" = "#228833",
    "drsp_13" = "#EE6677", "drsp_14" = "#AA3377", "drsp_15" = "#66CCEE",
    "drsp_16" = "#332288", "drsp_17" = "#117733", "drsp_18" = "#44AA99",
    "drsp_19" = "#882255", "drsp_20" = "#DDCC77", "drsp_21" = "#CC6677",
    # Composites
    "drsp_NA"         = "#0072B2", "drsp_distress"   = "#E69F00",
    "drsp_labilsocsx" = "#009E73", "drsp_breastpain" = "#D55E00",
    "drsp_eatsx"      = "#CC79A7", "drsp_lowarousal" = "#56B4E9",
    "drsp_total"      = "#332288"
  )
  # Return only colors for outcomes that exist
  out <- base_colors[intersect(names(outcomes), names(base_colors))]
  # Fallback for any missing
  missing <- setdiff(names(outcomes), names(out))
  if (length(missing) > 0) {
    fallback_pal <- scales::hue_pal()(length(missing))
    names(fallback_pal) <- missing
    out <- c(out, fallback_pal)
  }
  out
}

# =============================================================================
# Domain color configuration
# =============================================================================
# Creates a self-contained domain-color helper list that can be used for
# classifying/coloring outcomes by affective vs physical domain.
# All downstream graphics code should use this object rather than relying
# on separate global variables.
#
# Arguments:
#   col_affective     — hex color for affective domain (default: "#0072B2")
#   col_physical      — hex color for physical domain  (default: "#D55E00")
#   all_physical      — character vector of all physical outcome variable names
#   all_affective     — character vector of all affective outcome variable names
#   outcomes_map      — named vector (variable → label) for reverse-lookup
#
# Returns a list with:
#   $affective        — hex color string
#   $physical         — hex color string
#   $all_physical     — character vector of physical outcome names
#   $all_affective    — character vector of affective outcome names
#   $label_colors()   — function(labels) → vector of hex colors per label
#   $fill_scale()     — ggplot2 scale_fill_manual for Affective/Physical
#   $color_scale()    — ggplot2 scale_color_manual for Affective/Physical
# =============================================================================
build_domain_colors <- function(col_affective = "#0072B2",
                                col_physical  = "#D55E00",
                                all_physical  = character(0),
                                all_affective = character(0),
                                outcomes_map  = character(0)) {
  # Closure-based label coloring function
  label_colors_fn <- function(labels) {
    vars <- names(outcomes_map)[match(labels, outcomes_map)]
    vars <- ifelse(is.na(vars), labels, vars)
    ifelse(vars %in% all_physical, col_physical, col_affective)
  }

  fill_scale_fn <- function(name = NULL) {
    ggplot2::scale_fill_manual(
      values = c("Affective" = col_affective, "Physical" = col_physical),
      name = name
    )
  }

  color_scale_fn <- function(name = NULL) {
    ggplot2::scale_color_manual(
      values = c("Affective" = col_affective, "Physical" = col_physical),
      name = name
    )
  }

  list(
    affective     = col_affective,
    physical      = col_physical,
    all_physical  = all_physical,
    all_affective = all_affective,
    label_colors  = label_colors_fn,
    fill_scale    = fill_scale_fn,
    color_scale   = color_scale_fn
  )
}

# =============================================================================
# Axis metadata for cycle phasing
# =============================================================================
build_axis_meta <- function(time_vars, phasing_config) {
  lapply(stats::setNames(time_vars, time_vars), function(tv) {
    # Determine phasing for this time variable
    if (is.list(phasing_config)) {
      phase <- phasing_config[[tv]]
      if (is.null(phase)) phase <- "menses"
    } else {
      # Check if the variable name hints at ovulation-centered
      if (grepl("ov", tv, ignore.case = TRUE)) {
        phase <- "ovulation"
      } else {
        phase <- phasing_config
      }
    }

    if (phase == "ovulation") {
      list(labels = c("Menses", "50%F", "Ovulation", "50%L", "Menses"),
           menses = c(-1, 1), ov = 0)
    } else {
      list(labels = c("Ovulation", "50%L", "Menses", "50%F", "Ovulation"),
           menses = 0, ov = c(-1, 1))
    }
  })
}

# =============================================================================
# Prediction grids (one per time variable)
# =============================================================================
build_pred_grids <- function(time_vars) {
  stats::setNames(lapply(time_vars, function(tv) {
    pg_df <- data.frame(seq(-1, 1, by = 0.05))
    names(pg_df) <- tv
    pg_df
  }), time_vars)
}

# =============================================================================
# Quarter coverage helpers
# =============================================================================
quarter_bin_counts <- function(vals) {
  vals <- vals[!is.na(vals)]
  qbin <- cut(vals,
              breaks = seq(-1, 1, length.out = 5),
              labels = paste0("Q", 1:4),
              include.lowest = TRUE, right = FALSE)
  counts <- as.integer(table(factor(qbin, levels = paste0("Q", 1:4))))
  stats::setNames(counts, paste0("Q", 1:4))
}

check_quarter_coverage <- function(data, cycle_var, sparse_cutoff = 2) {
  counts <- quarter_bin_counts(data[[cycle_var]])
  n_empty  <- sum(counts == 0)
  n_sparse <- sum(counts > 0 & counts < sparse_cutoff)
  list(
    quarter_counts  = counts,
    min_per_quarter = min(counts),
    n_empty         = n_empty,
    n_sparse        = n_sparse,
    passes          = n_empty == 0 && n_sparse <= 1
  )
}

is_quarter_sparse <- function(qcounts, sparse_cutoff = 2) {
  n_empty  <- sum(qcounts == 0)
  n_sparse <- sum(qcounts > 0 & qcounts < sparse_cutoff)
  n_empty > 0 || n_sparse > 1
}

# =============================================================================
# Dynamic Y-axis expansion
# =============================================================================
expand_ymax <- function(y_max_current, max_observed, absolute_max = 6) {
  if (is.na(max_observed) || !is.finite(max_observed) || max_observed <= y_max_current) {
    return(y_max_current)
  }
  min(ceiling(max_observed), absolute_max)
}

# =============================================================================
# Build outcome plot settings
# =============================================================================
build_outcome_plot_settings <- function(outcomes) {
  tibble::tribble(
    ~variable, ~y_axis_mode, ~y_min, ~y_max, ~ref_line
  ) -> base_settings

  rows <- lapply(names(outcomes), function(v) {
    is_composite <- !grepl("^drsp_\\d+$", v)
    tibble::tibble(
      variable    = v,
      y_axis_mode = "fixed",
      y_min       = 1,
      y_max       = 4.5,
      ref_line    = if (is_composite) NA_real_ else 4
    )
  })

  dplyr::bind_rows(rows)
}

# =============================================================================
# Core GAM fitting function
# =============================================================================
fit_outcome <- function(dat, outcome_var, tv, outcomes_map,
                        pred_grid_list, sparse_cutoff = 2, min_obs = 10) {
  log_out <- paste0(outcome_var, "_Log")
  keep <- c(log_out, tv)
  if (!all(keep %in% names(dat))) return(NULL)
  dat <- dat[stats::complete.cases(dat[, keep]), keep, drop = FALSE]
  if (nrow(dat) < min_obs) return(NULL)

  # Check for no variability in the log-transformed outcome
  out_variance <- stats::var(dat[[log_out]], na.rm = TRUE)
  no_variability <- !is.finite(out_variance) || out_variance < 1e-6

  # Quarter-based coverage check
  qcov <- check_quarter_coverage(data = dat, cycle_var = tv,
                                  sparse_cutoff = sparse_cutoff)

  message(glue::glue(
    "[{outcome_var}] N={nrow(dat)}, Quarter counts: ",
    "{paste(names(qcov$quarter_counts), qcov$quarter_counts, sep='=', collapse=', ')}, ",
    "Min per quarter: {qcov$min_per_quarter}, Empty: {qcov$n_empty}, ",
    "Sparse (>0 & <{sparse_cutoff}): {qcov$n_sparse}, Passes: {qcov$passes}",
    "{if (no_variability) ', No variability (raw data only)' else ''}"
  ))

  if (!qcov$passes) {
    message(glue::glue(
      "Skipping GAM for {outcome_var}: quarter coverage requirement not met ",
      "(need 4/4 quarters with \u22651 obs & at most 1 quarter with <{sparse_cutoff} obs; ",
      "empty={qcov$n_empty}, sparse={qcov$n_sparse})"
    ))
    return(NULL)
  }

  n_neg    <- sum(dat[[tv]] <  0, na.rm = TRUE)
  n_nonneg <- sum(dat[[tv]] >= 0, na.rm = TRUE)

  # No variability: return flat result
  if (no_variability) {
    mean_log <- mean(dat[[log_out]], na.rm = TRUE)
    flat_est <- exp(mean_log) - 1

    pg <- pred_grid_list[[tv]]
    pred <- cbind(pg,
                  estimate  = rep(flat_est, nrow(pg)),
                  conf.low  = rep(flat_est, nrow(pg)),
                  conf.high = rep(flat_est, nrow(pg)))
    pred$outcome_var   <- outcome_var
    pred$outcome_label <- outcomes_map[[outcome_var]]
    pred$p.value   <- NA_real_
    pred$raw_range <- 0
    pred$n         <- nrow(dat)
    pred$edf       <- NA_real_

    return(list(
      pred = pred,
      sig  = tibble::tibble(
        variable       = outcome_var,
        outcome_label  = outcomes_map[[outcome_var]],
        time_var       = tv,
        p.value        = NA_real_,
        edf            = NA_real_,
        F              = NA_real_,
        n              = nrow(dat),
        n_neg          = n_neg,
        n_nonneg       = n_nonneg,
        range          = 0,
        peak_time      = NA_real_,
        trough_time    = NA_real_,
        coverage_mode  = "no_variability"
      )
    ))
  }

  k <- min(6L, nrow(dat) - 1L)
  if (k < 3L) return(NULL)

  fml <- stats::as.formula(paste0(log_out, " ~ s(", tv, ", bs = 'cc', k = ", k, ")"))
  knots <- stats::setNames(list(c(-1, 1)), tv)

  mf <- try(mgcv::bam(fml, data = dat, knots = knots, method = "fREML",
                       discrete = TRUE, select = TRUE),
            silent = TRUE)
  if (inherits(mf, "try-error")) return(NULL)

  s  <- summary(mf)
  p  <- suppressWarnings(as.numeric(s$s.pv[1]))
  st <- s$s.table
  edf  <- suppressWarnings(as.numeric(st[1, "edf"]))
  Fval <- suppressWarnings(as.numeric(st[1, "F"]))

  pg <- pred_grid_list[[tv]]
  pr <- stats::predict(mf, newdata = pg, se.fit = TRUE, type = "link")
  est <- exp(pr$fit) - 1
  conf.low  <- exp(pr$fit - 1.96 * pr$se.fit) - 1
  conf.high <- exp(pr$fit + 1.96 * pr$se.fit) - 1

  peak_time   <- pg[[tv]][which.max(est)]
  trough_time <- pg[[tv]][which.min(est)]

  pred <- cbind(pg, estimate = est, conf.low = conf.low, conf.high = conf.high)
  pred$outcome_var   <- outcome_var
  pred$outcome_label <- outcomes_map[[outcome_var]]
  pred$p.value   <- p
  pred$raw_range <- diff(range(est, na.rm = TRUE))
  pred$n   <- nrow(dat)
  pred$edf <- edf

  list(
    pred = pred,
    sig  = tibble::tibble(
      variable      = outcome_var,
      outcome_label = outcomes_map[[outcome_var]],
      time_var      = tv,
      p.value       = p,
      edf           = edf,
      F             = Fval,
      n             = nrow(dat),
      n_neg         = n_neg,
      n_nonneg      = n_nonneg,
      range         = diff(range(est, na.rm = TRUE)),
      peak_time     = peak_time,
      trough_time   = trough_time,
      coverage_mode = "quarter_check"
    )
  )
}

# =============================================================================
# Annotated single-outcome plot
# =============================================================================
create_annotated_plot <- function(plot_df, tv, axis_meta_list,
                                   outcome_plot_settings, drsp_color_map,
                                   y_limits = NULL, raw_data = NULL,
                                   benchmark = NULL) {
  if (is.null(plot_df) || nrow(plot_df) == 0) return(NULL)

  DRSP_VAR_PATTERN <- "^drsp_\\d+$"
  y_limits_from_caller <- !is.null(y_limits)

  max_pt <- dplyr::slice_max(plot_df, order_by = estimate, n = 1, with_ties = FALSE)
  min_pt <- dplyr::slice_min(plot_df, order_by = estimate, n = 1, with_ties = FALSE)
  rng    <- dplyr::first(plot_df$raw_range)
  p_val  <- dplyr::first(plot_df$p.value)
  n_obs  <- dplyr::first(plot_df$n)
  edf_val <- dplyr::first(plot_df$edf)

  outcome_var <- dplyr::first(plot_df$outcome_var)
  line_col <- drsp_color_map[[outcome_var]]
  if (is.null(line_col) || is.na(line_col)) line_col <- "#0072B2"

  ps        <- outcome_plot_settings[outcome_plot_settings$variable == outcome_var, ]
  mode      <- if (nrow(ps) > 0) ps$y_axis_mode[1] else "fixed"
  ref_line  <- if (nrow(ps) > 0) ps$ref_line[1]    else 4
  y_fix_min <- if (nrow(ps) > 0) ps$y_min[1]       else 1
  y_fix_max <- if (nrow(ps) > 0) ps$y_max[1]       else 4.5

  if (is.null(y_limits)) {
    if (mode == "fixed" && !is.na(y_fix_min) && !is.na(y_fix_max)) {
      y_limits <- c(y_fix_min, y_fix_max)
    }
  }

  raw_pts <- NULL
  log_out <- paste0(outcome_var, "_Log")
  if (!is.null(raw_data) && outcome_var %in% names(raw_data) &&
      log_out %in% names(raw_data) && tv %in% names(raw_data)) {
    raw_pts <- raw_data[stats::complete.cases(raw_data[, c(log_out, tv)]),
                        c(outcome_var, tv), drop = FALSE]
    if (nrow(raw_pts) == 0) raw_pts <- NULL
  }

  if (!is.null(y_limits)) {
    raw_max <- if (!is.null(raw_pts) && nrow(raw_pts) > 0 && outcome_var %in% names(raw_pts)) {
      vals <- raw_pts[[outcome_var]]
      if (any(is.finite(vals))) max(vals[is.finite(vals)]) else NA_real_
    } else {
      NA_real_
    }
    if (y_limits_from_caller) {
      expanded_ymax <- expand_ymax(y_limits[2], raw_max)
    } else {
      pred_max <- if (any(is.finite(plot_df$estimate))) max(plot_df$estimate, na.rm = TRUE) else NA_real_
      max_val <- if (!is.na(raw_max) && !is.na(pred_max)) max(raw_max, pred_max) else
                  if (!is.na(raw_max)) raw_max else pred_max
      expanded_ymax <- expand_ymax(y_limits[2], max_val)
    }
    if (!is.na(expanded_ymax) && expanded_ymax > y_limits[2]) {
      y_limits[2] <- expanded_ymax
      y_fix_max <- expanded_ymax
    }
  }

  if (mode == "fixed" && !is.null(y_limits)) {
    y_fix_max <- max(y_fix_max, y_limits[2])
  }

  cpass_flag <- !is.null(rng) && is.finite(rng) && !is.null(benchmark) && is.finite(benchmark) && rng >= benchmark
  is_no_gam <- !is.finite(p_val) && !is.null(rng) && is.finite(rng) && rng == 0
  stars <- dplyr::case_when(
    is.finite(p_val) & p_val < 0.001 ~ "***",
    is.finite(p_val) & p_val < 0.01  ~ "**",
    is.finite(p_val) & p_val < 0.05  ~ "*",
    TRUE ~ ""
  )
  cpass_label <- if (cpass_flag) sprintf("\u25B2 \u2265 benchmark (%.2f) | ", benchmark) else ""
  edf_str <- if (!is.null(edf_val) && !is.na(edf_val)) sprintf(", EDF = %.2f", edf_val) else ""
  if (is_no_gam) {
    ann <- if (!is.null(n_obs) && !is.na(n_obs)) {
      sprintf("No variability (raw data only), N = %d", as.integer(n_obs))
    } else {
      "No variability (raw data only)"
    }
  } else {
    ann <- if (!is.null(n_obs) && !is.na(n_obs)) {
      sprintf("%s%s Range = %.2f%s, N = %d", cpass_label, stars, rng, edf_str, as.integer(n_obs))
    } else {
      sprintf("%s%s Range = %.2f%s", cpass_label, stars, rng, edf_str)
    }
  }
  meta <- axis_meta_list[[tv]]

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[tv]], y = estimate))

  if (!is.null(y_limits) && y_limits[2] > 4.5) {
    p <- p + ggplot2::annotate(
      "rect", xmin = -Inf, xmax = Inf,
      ymin = 4.5, ymax = y_limits[2],
      fill = "pink", alpha = 0.25
    )
  }

  if (!is.na(ref_line)) {
    p <- p + ggplot2::geom_hline(yintercept = ref_line, color = "red",
                                  linewidth = 0.4, alpha = 0.4)
  }

  if (!is.null(raw_pts)) {
    p <- p + ggplot2::geom_point(
      data = raw_pts,
      ggplot2::aes(x = .data[[tv]], y = .data[[outcome_var]]),
      color = line_col, alpha = 0.45, size = 1.8, shape = 16,
      inherit.aes = FALSE
    )
  }

  p <- p +
    ggplot2::geom_vline(xintercept = meta$menses, color = "red") +
    ggplot2::geom_vline(xintercept = meta$ov,     color = "forestgreen")

  if (!is_no_gam) {
    p <- p +
      ggplot2::geom_line(linewidth = 1, color = line_col) +
      ggplot2::geom_point(data = max_pt, size = 2.5, color = line_col) +
      ggplot2::geom_point(data = min_pt, size = 2.5, color = line_col)
  }

  p <- p +
    ggplot2::annotate("text", x = -1, y = Inf, label = ann,
                      hjust = -0.05, vjust = 1.5, size = 2.5, fontface = "bold") +
    ggplot2::labs(title = dplyr::first(plot_df$outcome_label), x = "Cycle Phase", y = NULL) +
    ggplot2::scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5),
                                labels = meta$labels) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5),
      plot.margin = ggplot2::margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"),
      axis.text = ggplot2::element_text(size = 7)
    )

  if (mode == "fixed" && !is.na(y_fix_min) && !is.na(y_fix_max)) {
    p <- p + ggplot2::scale_y_continuous(breaks = seq(y_fix_min, y_fix_max, by = 1))
  }

  if (!is.null(y_limits)) {
    p <- p + ggplot2::coord_cartesian(ylim = y_limits, clip = "off")
  } else {
    p <- p + ggplot2::coord_cartesian(clip = "off")
  }

  is_drsp <- grepl(DRSP_VAR_PATTERN, outcome_var)
  if (is_drsp && cpass_flag) {
    p <- p + ggplot2::theme(
      panel.border = ggplot2::element_rect(color = "red", linewidth = 1.5, fill = NA)
    )
  }

  p
}

# =============================================================================
# Overlay page helper
# =============================================================================
plot_overlay_page <- function(sig_tv, pred_tv, tv, meta, outcomes,
                               drsp_color_map, norm_scale_ranges = NULL,
                               range_thresh, title_prefix, subtitle_label,
                               current_id = "",
                               var_pattern = NULL, var_list = NULL,
                               ylim_strategy = "fixed", fixed_ylim = c(1, 6),
                               show_hline = TRUE, hline_y = 4,
                               legend_ncol = 3,
                               show_range_in_labels = FALSE,
                               normalize_01 = FALSE) {
  if (!is.null(var_list)) {
    qualifying <- sig_tv %>% dplyr::filter(variable %in% var_list)
  } else if (!is.null(var_pattern)) {
    qualifying <- sig_tv %>% dplyr::filter(grepl(var_pattern, variable))
  } else {
    return(invisible(NULL))
  }
  qualifying <- qualifying %>%
    dplyr::filter(
      (is.finite(p.value) & p.value < 0.05) |
      (is.finite(range) & range >= range_thresh)
    )
  if (nrow(qualifying) == 0) return(invisible(NULL))

  ov_vars <- unique(qualifying$variable)
  ov_data <- pred_tv %>% dplyr::filter(outcome_var %in% ov_vars)
  if (nrow(ov_data) == 0) return(invisible(NULL))

  n_curves <- length(ov_vars)
  ov_colors <- drsp_color_map[ov_vars]
  ov_colors[is.na(ov_colors)] <- "#0072B2"
  names(ov_colors) <- ov_vars

  range_lookup <- qualifying %>%
    dplyr::distinct(variable, .keep_all = TRUE) %>%
    dplyr::select(variable, range)

  if (show_range_in_labels) {
    ov_labels <- sapply(ov_vars, function(v) {
      r <- range_lookup$range[range_lookup$variable == v]
      if (length(r) > 0 && is.finite(r)) {
        paste0(outcomes[v], " (", sprintf("%.2f", r), ")")
      } else {
        outcomes[v]
      }
    })
  } else {
    ov_labels <- outcomes[ov_vars]
  }

  if (normalize_01 && !is.null(norm_scale_ranges)) {
    ov_data <- ov_data %>%
      dplyr::left_join(norm_scale_ranges, by = c("outcome_var" = "variable")) %>%
      dplyr::group_by(outcome_var) %>%
      dplyr::mutate(
        scale_min = dplyr::coalesce(norm_min, min(estimate, na.rm = TRUE)),
        scale_max = dplyr::coalesce(norm_max, max(estimate, na.rm = TRUE)),
        scale_range = scale_max - scale_min,
        estimate  = ifelse(scale_range > 0, (estimate  - scale_min) / scale_range, 0.5),
        conf.low  = ifelse(scale_range > 0, (conf.low  - scale_min) / scale_range, 0.5),
        conf.high = ifelse(scale_range > 0, (conf.high - scale_min) / scale_range, 0.5)
      ) %>%
      dplyr::select(-norm_min, -norm_max, -scale_min, -scale_max, -scale_range) %>%
      dplyr::ungroup()
    ylim_strategy <- "fixed"
    fixed_ylim <- c(0, 1)
  }

  if (ylim_strategy == "data") {
    y_min <- min(ov_data$conf.low, na.rm = TRUE)
    y_max <- max(ov_data$conf.high, na.rm = TRUE)
    y_pad <- (y_max - y_min) * 0.05
    plot_ylim <- c(max(0, y_min - y_pad), y_max + y_pad)
  } else {
    plot_ylim <- fixed_ylim
    if (!normalize_01 && any(is.finite(ov_data$estimate))) {
      max_est <- max(ov_data$estimate, na.rm = TRUE)
      expanded_ymax <- expand_ymax(plot_ylim[2], max_est)
      if (expanded_ymax > plot_ylim[2]) plot_ylim[2] <- expanded_ymax
    }
  }

  range_label <- format(range_thresh, nsmall = 2)
  y_axis_label <- if (normalize_01) "Normalized Score (0\u20131)" else "Predicted Score"

  if (is.infinite(range_thresh) || is.na(range_thresh)) {
    filter_label <- "p < .05"
  } else {
    filter_label <- paste0("p < .05 or range \u2265 benchmark (", range_label, ")")
  }

  id_suffix <- if (nchar(current_id) > 0) paste0(" \u2014 ID: ", current_id) else ""

  p <- ggplot2::ggplot(ov_data,
         ggplot2::aes(x = .data[[tv]], y = estimate, color = outcome_var))

  if (!normalize_01 && plot_ylim[2] > 4.5) {
    p <- p + ggplot2::annotate(
      "rect", xmin = -Inf, xmax = Inf,
      ymin = 4.5, ymax = plot_ylim[2],
      fill = "pink", alpha = 0.25
    )
  }

  if (show_hline) {
    p <- p + ggplot2::geom_hline(yintercept = hline_y, color = "red",
                                  linewidth = 0.4, alpha = 0.4)
  }

  p <- p +
    ggplot2::geom_vline(xintercept = meta$menses, color = "red",
                        linewidth = 0.6, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = meta$ov, color = "forestgreen",
                        linewidth = 0.6, linetype = "dashed") +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_color_manual(
      values = ov_colors, breaks = ov_vars, labels = ov_labels
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5), labels = meta$labels
    ) +
    ggplot2::coord_cartesian(ylim = plot_ylim) +
    ggplot2::labs(
      title = paste0(title_prefix, id_suffix),
      subtitle = paste0(
        subtitle_label, " with ", filter_label,
        "  |  N curves = ", n_curves, "  |  Time variable: ", tv
      ),
      x = "Cycle Phase", y = y_axis_label, color = "Outcome"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 6),
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      plot.subtitle = ggplot2::element_text(size = 8),
      plot.margin = ggplot2::margin(t = 8, r = 8, b = 8, l = 8, unit = "pt")
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = legend_ncol))

  print(p)
}
