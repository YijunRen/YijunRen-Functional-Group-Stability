
###### plot for field effect

myplot.field.3 <- function(
    data, xvar, yvar,
    groups = c("all", "grass", "forb"),
    facet_by = "Group",
    facet_labels = NULL,
    xlab = "your,x",
    ylab = "your.y",
    ncol = 3,
    nrow = NULL,
    mycex = 1,
    point.hide = FALSE,
    degree = 1,
    label_type = "coefficient",
    facet_scales = "free_y"
){
  
  # 1. Grouping and colours
  stopifnot(facet_by %in% c("Group", "Field"))
  stopifnot(degree %in% c(1, 2))
  stopifnot(label_type %in% c(
    "coefficient", "p.value", "none"))
  
  line_by <- if(facet_by == "Group") "Field" else "Group"
  
  field_cols <- c(
    "Duolun" = "#983451",
    "CDR B"  = "#D1A060",
    "CDR C"  = "#58688F")

  cols <- if(facet_by == "Group") {
    field_cols
  } else {
    group_cols
  }
  
  # 2. Prepare data
  D <- data %>%
    filter(Group %in% groups) %>%
    transmute(
      y = .data[[yvar]],
      x = .data[[xvar]],
      Field = factor(
        Field, levels = names(field_cols)),
      Group = factor(
        Group, levels = groups)) %>%
    filter(
      is.finite(x), 
      is.finite(y),
      !is.na(Field))
  
  # 3. Model formula
  formula_use <- if(degree == 1) {
    y ~ x
  } else {
    y ~ x + I(x^2)
  }
  
  # 4. Separate regressions
  fits <- D %>%
    group_by(
      across(all_of(c(facet_by, line_by)))) %>%
    nest() %>%
    mutate(
      
      model = map(
        data, ~ lm(formula_use, data = .x)),
      
      stats = map(
        model, ~ broom::tidy(.x)),
      
      model_stats = map(
        model, ~ broom::glance(.x)),
      
      pred = map2(
        data, model,
        ~ {
          
          newdata <- data.frame(
            x = seq(
              min(.x$x),
              max(.x$x),
              length.out = 100
            )
          )
          
          bind_cols(
            newdata,
            as.data.frame(
              predict(
                .y,
                newdata = newdata,
                interval = "confidence"
              )
            )
          )
        }
      )
    )
  
  
  # 5. Extract coefficients and P values
  fit_each <- fits %>%
    mutate(
      
      coefficient = map_dbl(
        stats,
        ~ {
          term_use <- if(degree == 1) {
            "x"
          } else {
            "I(x^2)"
          }
          
          .x$estimate[.x$term == term_use]
        }
      ),
      
      p_coefficient = map_dbl(
        stats,
        ~ {
          term_use <- if(degree == 1) {
            "x"
          } else {
            "I(x^2)"
          }
          
          .x$p.value[.x$term == term_use]
        }
      ),
      
      p_model = map_dbl(
        model_stats,
        ~ .x$p.value),
      
      r_squared = map_dbl(
        model_stats,
        ~ .x$r.squared)) %>%
    select(
      all_of(c(facet_by, line_by)),
      coefficient,
      p_coefficient,
      p_model,
      r_squared) %>%
    ungroup() %>%
    mutate(
      
      # Line type based on significance
      lty = case_when(
        is.na(p_coefficient) ~ "dashed",
        p_coefficient < 0.05 ~ "solid",
        TRUE ~ "dashed"),
      
      # Significance stars
      stars = vapply(
        p_coefficient,
        function(p) {
          if(is.na(p)) return("")
          if(p < 0.001) return("***")
          if(p < 0.01)  return("**")
          if(p < 0.05)  return("*")
          return("")
        },
        character(1)),
      
      # Mathematical labels for plotmath
      label = case_when(
        
        label_type == "none" ~ "",
        
        label_type == "p.value" ~ paste0(
          "italic(P) == ",
          ifelse(
            is.finite(p_model) & p_model < 0.001,
            "'<0.001'",
            ifelse(
              is.finite(p_model),
              sprintf("%.3f", p_model),
              "'NA'"
            )
          )
        ),
        
        degree == 1 ~ paste0(
          "bold('Slope = ",
          sprintf("%.3f", coefficient),
          ifelse(stars == "", "", paste0(" ", stars)),
          "')"),
        
        degree == 2 ~ paste0(
          "bold('β₂ = ",
          sprintf("%.3f", coefficient),
          ifelse(stars == "", "", paste0(" ", stars)),
          "')"
        )
      )
    )
  
  
  # 6. Predictions and 95% CI
  pred <- fits %>%
    select(
      all_of(c(facet_by, line_by)),
      pred) %>%
    unnest(pred) %>%
    rename(
      predicted = fit,
      conf.low = lwr,
      conf.high = upr) %>%
    left_join(
      fit_each %>%
        select(
          all_of(c(facet_by, line_by)),
          lty),
      by = c(facet_by, line_by))
  
  
  # 7. Automatically select upper-left or upper-right
  label_pos <- D %>%
    group_by(.data[[facet_by]]) %>%
    group_modify(~ {
      
      dat <- .x
      facet_value <- .y[[facet_by]][1]
      
      pp <- pred[
        as.character(pred[[facet_by]]) ==
          as.character(facet_value),
      ]
      
      xr <- range(dat$x)
      
      yr <- range(
        c(dat$y,
          pp$conf.low,
          pp$conf.high),
        na.rm = TRUE)
      
      xs <- diff(xr)
      ys <- diff(yr)
      
      if(xs == 0) xs <- 1
      if(ys == 0) ys <- 1
      
      xmid <- mean(xr)
      ycut <- yr[1] + 0.60 * ys
      
      left_score <-
        sum(dat$x <= xmid & dat$y >= ycut) +
        sum(pp$x <= xmid &
              pp$conf.high >= ycut,
            na.rm = TRUE)
      
      right_score <-
        sum(dat$x > xmid & dat$y >= ycut) +
        sum(pp$x > xmid &
              pp$conf.high >= ycut,
            na.rm = TRUE)
      
      tibble(
        side = ifelse(
          left_score <= right_score,
          "left", "right" ),
        xmin = xr[1],
        xmax = xr[2])
    }) %>%
    ungroup()
  
  
  # 8. Label positions
  fit_each <- fit_each %>%
    left_join(
      label_pos,
      by = facet_by) %>%
    group_by(.data[[facet_by]]) %>%
    arrange(
      .data[[line_by]],
      .by_group = TRUE) %>%
    mutate(
      x.label = ifelse(
        side == "left",
        xmin + 0.08 * (xmax - xmin),
        xmax - 0.50 * (xmax - xmin)),
      y.label = Inf,
      vjust.label = 1.5 + (row_number() - 1) * 1.1) %>%
    ungroup()
  
  
  # 9. Plot
  p <- ggplot() +
    
    # 95% confidence intervals
    geom_ribbon(
      data = pred,
      aes(x = x,
          ymin = conf.low,
          ymax = conf.high,
          fill = .data[[line_by]],
          group = .data[[line_by]]),
      alpha = 0.10, colour = NA,
      show.legend = FALSE) +
        
    # Regression lines
    geom_line(
      data = pred,
      aes(x = x, y = predicted,
          colour = .data[[line_by]],
          linetype = lty,
          group = .data[[line_by]]),
      linewidth = 1.3) +
    scale_linetype_identity() +
        
    # Original points
    {
      if(!point.hide) {
        geom_point(
          data = D,
          aes(x = x, y = y,
              colour = .data[[line_by]]),
          alpha = 0.35, size = 2)
      }
    } +
    
    # Mathematical labels
    geom_text(
      data = fit_each %>%
        filter(label != ""),
      aes(x = x.label, y = y.label, label = label,
          colour = .data[[line_by]],
          vjust = vjust.label),
      parse = TRUE, hjust = 0, size = 5 * mycex,
      fontface = "bold", show.legend = FALSE) +
        
    # Facet
    facet_wrap(
      vars(.data[[facet_by]]),
      ncol = ncol,
      nrow = nrow,
      scales = facet_scales,
      labeller = if(is.null(facet_labels)) {
        "label_value"
      } else {
        ggplot2::as_labeller(facet_labels)
      }) +
    scale_colour_manual(values = cols, drop = FALSE ) +
    scale_fill_manual(values = cols, drop = FALSE) +
    labs(x = xlab, y = ylab, colour = NULL) +
    scale_y_continuous(
      labels = scales::label_number(accuracy = 0.1)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 17 * mycex),
      axis.title = element_text(face = "bold"),
      text = element_text(size = 20 * mycex, face = "bold"),
      legend.position = "bottom",
      panel.spacing = grid::unit(0.5, "lines"))
  
  return(p)
}








