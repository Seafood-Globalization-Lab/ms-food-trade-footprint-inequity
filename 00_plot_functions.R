# Plot functions

# Ranked bar charts
plot_top_countries <- function(stresssor_data,
                               type = "d", stressor = "tonnes_traded", top_n = 25,
                               title_lab = ""){
  
  iso3_col <- paste("iso3", type, sep = "_") 
  country_col <- paste("country", type, sep = "_") 
  
  top_countries_df <- stresssor_data %>%
    # Rename for focal origin/destination 
    mutate(iso3 = .data[[iso3_col]], country = .data[[country_col]]) %>%
    # Calculate total by food item for selected pressure
    group_by(iso3, country, food_group) %>%
    summarise(pressure = sum(.data[[stressor]], na.rm = TRUE)) %>%
    # Calculate country total across food items for ranking
    group_by(iso3, country) %>%
    mutate(total_pressure = sum(pressure)) %>%
    # Ungroup to identify max across all countries
    ungroup()
  
  top_n_iso3c <- top_countries_df %>%
    select(iso3, total_pressure) %>%
    distinct() %>%
    # Select top n countries by total pressure
    slice_max(order_by = total_pressure, n = top_n) %>%
    pull(iso3)
  
  g <- top_countries_df %>% 
    filter(iso3 %in% top_n_iso3c) %>%
    mutate(country = fct_reorder(country, total_pressure)) %>%
    ggplot(aes(x = pressure, y = country, fill = food_group)) +
    geom_bar(stat = "identity", position = "stack") + 
    scale_fill_brewer(palette = "Spectral") +
    labs(title = title_lab, y = "", fill = "") +
    theme_bw()
  
  return(g)
}

# Inequality figure - gini and map
# custom presets
base_size <- 10
base_family <- "Helvetica Neue"


plot.dist.log <- function(dat.col, variable.title = "", log.dat.col = NULL, log.variable.title = NULL, 
                          main.title = "", color_high = "#cbeef0", color_low = "#1e666a"){
  
  # Calculate Gini
  df.world <- as.data.frame(map.world)
  df.world <- data.frame(gini.col = c(df.world[[dat.col]]), 
                         pop = df.world$pop_d)
  df.world <- df.world %>%
    drop_na()
  # Can add population weighting, but https://doi.org/10.1080/17421772.2017.1343491 argues against
  gini <- gini(x = df.world$gini.col, weights = df.world$pop) 
  gini_annotate <- gini(x = df.world$gini.col) 
  
  # Create histogram
  g_hist <- ggplot(df.world, aes(x = gini.col)) +
    geom_histogram(bins = 50) +
    labs(x = paste(variable.title), y = "No. of countries") + 
    theme(
      axis.line.x = element_line(colour = "black", linewidth = 0.5, linetype = "solid"),
      axis.line.y = element_line(colour = "black", linewidth = 0.5, linetype = "solid"),
      axis.text = element_text(size = ceiling(base_size*0.7), colour = "black"),
      axis.title = element_text(size = ceiling(base_size*0.8)),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(), panel.border = element_blank(),
      strip.background = element_rect(linetype = 0), strip.text = element_text(),
      strip.text.x = element_text(vjust = 0.5), strip.text.y = element_text(angle = -90),
      legend.text = element_text(size = ceiling(base_size*0.9), family = "sans"),
      legend.title = element_blank(), 
      legend.key = element_rect(fill = "white", colour = NA), 
      legend.position="bottom",
      plot.title = element_text(size = ceiling(base_size*1.1), face = "bold"), 
      plot.subtitle = element_text(size = ceiling(base_size*1.05))
    )
  
  # Create unweighted Lorenz
  g_lorenz <- ggplot(df.world, aes(gini.col)) +
    stat_lorenz() +
    #annotate_ineq(df.world$gini.col, x = .15, size = 2.5) + # this version of annotate drops the 0 in "0.90"
    annotate("text", label = paste("Gini: ", substr(as.character(gini_annotate), 1, 4), sep = ""), x = .15, y = 0.95, size = 2.5) + # set the number of digits with substr
    geom_abline(linetype = "dashed") +
    labs(x = paste(variable.title), y = "Proportion of pressure") + 
    theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"), 
          axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"), 
          axis.text = element_text(size = ceiling(base_size*0.7), colour = "black"),
          axis.title = element_text(size = ceiling(base_size*0.8)), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.major.x = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          strip.background = element_rect(linetype = 0), strip.text = element_text(), 
          strip.text.x = element_text(vjust = 0.5), strip.text.y = element_text(angle = -90), 
          legend.text = element_text(size = ceiling(base_size*0.9), family = "sans"), 
          legend.title = element_blank(), 
          legend.key = element_rect(fill = "white", colour = NA), 
          legend.position="bottom",
          plot.title = element_text(size = ceiling(base_size*1.1), face = "bold"), 
          plot.subtitle = element_text(size = ceiling(base_size*1.05)))
  
  
  
  map.dat.col <- ifelse(is.null(log.dat.col) == TRUE, paste(dat.col), paste(log.dat.col))
  map.lab <- ifelse(is.null(log.variable.title) == TRUE, paste(variable.title), paste(log.variable.title))
  
  PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
  map.world.reproj <- st_transform(map.world,PROJ) %>%
    filter(!subregion %in% c("Micronesia","Polynesia"))
  map.islands.reproj<- st_transform(map.islands,PROJ)
  poly_points <- rbind(map.world.reproj,map.islands.reproj) %>%
    filter(continent !="Antarctica")
  
  
  # Create map
  g_map <- ggplot(data = poly_points) +
    geom_sf(aes_string(fill = map.dat.col,color=map.dat.col), size = .1) +
    scale_fill_gradient(low = color_low, high = color_high,trans="log10",na.value = "#dfd9c9") +
    scale_color_gradient(low = color_low, high = color_high,trans="log10",na.value = "#dfd9c9") +
    labs(fill = paste(map.lab),color=paste(map.lab)) +
    # coord_sf(ylim = c(-50, 90), datum = NA) +
    guides(
      fill = guide_colourbar(barwidth = 10, barheight = 0.5),
      color = guide_colourbar(barwidth = 10, barheight = 0.5)) +
    theme(
      axis.line = element_blank(), axis.text = element_blank(), 
      axis.ticks = element_blank(), axis.title = element_blank(), 
      panel.background = element_blank(),
      # panel.background = element_rect(fill = "black"),
      panel.border = element_blank(), 
      panel.grid = element_blank(), panel.spacing = unit(0, "lines"), plot.background = element_blank(), 
      legend.justification = c(0.5, 0), legend.position = "bottom",
      legend.title= element_text(size = ceiling(base_size*0.8)),
      legend.text = element_text(size = ceiling(base_size*0.7))
    )
  
  # Layout without histogram
  ggdraw() +
    draw_plot(
      g_lorenz, x=0.01, y = .25, height = .4, width = .25, hjust = 0, vjust=0.55
    ) + 
    draw_plot(
      g_map, x=0.05, y=0.5, hjust = 0, vjust=0.5,
    ) +
    geom_text(data = data.frame(x = 0.05, y = .86, label = paste(main.title)),
              aes(x, y, label = label),
              hjust = 0, vjust = 0, angle = 0, size = .5*base_size, fontface="bold",
              color = "black",
              inherit.aes = FALSE,
              family= base_family)
  
}

# Gravity model fit
fit_gravity <- function(trade_data, food_item = NA, response, regressors, model_name){
  
  # Filter to food group, if provided
  trade_data <- trade_data %>%
    {if (sum(is.na(food_item)) == 0)
      filter(., food_group %in% food_item)
      else .} 
  
  fit <- ppml(
    dependent_variable = response,
    distance = "dist", 
    additional_regressors = regressors,
    data = trade_data,
    robust = TRUE
  )
  
  # Calculate r^2
  r2 <- (cor(trade_data %>% 
               select(response, all_of(regressors)) %>%
               drop_na() %>% 
               pull(response), 
             fit$fitted.values))^2
  
  # Initiate data frame for parameter estimates
  gravity_summary <- data.frame("model_name" = rep(model_name, (length(regressors)+2)), 
                                "regressor" = c("intercept", "dist_log", regressors), 
                                "estimate" = c(fit$coefficients[,1]), 
                                "std_error" = c(fit$coefficients[,2]),
                                "p_value" = c(fit$coefficients[,4]),
                                "r2" = rep(r2, (length(regressors)+2)))
  
  gravity_summary <- gravity_summary %>%
    mutate(significance = case_when(
      p_value <= 0.05 ~ "significant",
      p_value > 0.05 ~ "not significant"
    ))
  
  return(gravity_summary)
}

# Plot gravity model estimates
plot_gravity_est <- function(model_fit_summary){
  model_fit_summary %>% 
    filter(regressor != "intercept") %>%
    ggplot() +
    geom_segment(aes(x = estimate, xend=0, y = regressor, yend = regressor, 
                     color = significance)) +
    geom_point(aes(x = estimate, y = regressor, 
                   color = significance)) +
    geom_text(aes(x = Inf, y = Inf, hjust = 1.5, vjust = 1.5, 
                  label = paste("r2 = ", round(unique(r2), 2)))) + 
    scale_color_manual(values = c("#A5D4EB", "#2E84C5")) +
    labs(title = paste(unique(model_fit_summary$model_name)), y = "") +
    theme_bw()
}


# Set variables for mapping function
country_centroids <- countryref %>%
  filter(str_count(as.character(adm1_code))==3) %>%
  group_by(iso3) %>%
  summarise(centroid.lon = mean(centroid.lon),
            centroid.lat = mean(centroid.lat)) %>%
  ungroup() %>%
  mutate(
    centroid.lon = case_when(
      iso3 == "KIR" ~ 172.9717,
      TRUE ~ centroid.lon
    ),
    centroid.lat = case_when(
      iso3 == "KIR" ~ 1.4518,
      TRUE ~ centroid.lat
    )
  )

countryref <- countryref

# Map plotting function with arrows
plot_map <- function(data, pressure,
                     country_fill = NA, flow_arrows = TRUE, n_flows = 10,
                     arrow_label = NA, fill_label = NA, caption_label = NA){
  # Rename data columns to align with original code
  data <- data %>%
    rename("exporter_iso3c" = "iso3_o",
           "importer_iso3c" = "iso3_d",
           "quantity" = all_of(pressure))
  
  # Load world map data
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(iso_a3 != "ATA")
  
  if (is.na(country_fill)) {
    trade_map <- ggplot(world) +
      geom_sf(size = 0.1, color = "white", fill = "#97acb7", alpha = 1)
  } else {
    trade_map <- ggplot(world) +
      geom_sf(size = 0.1, color = "white", fill = "grey", alpha = 0.5)
  }
  
  # If country_fill is provided, create import or export value to fill by
  if(is.na(country_fill) == FALSE){
    
    country_fill_col <- country_fill
    
    # Summarize total export or import data
    chloropleth_df <- data %>%
      group_by(.data[[country_fill_col]]) %>% 
      summarize(quantity = sum(quantity, na.rm=TRUE)) %>%
      rename("iso_a3" = paste(country_fill_col))
    
    
    chloropleth_df <- chloropleth_df %>%
      left_join(world, by = "iso_a3")
    
    trade_map <- trade_map +
      geom_sf(data = chloropleth_df, 
              aes(fill = quantity, geometry = geometry), color = "white", size = 0.1) +
      scale_fill_gradient(low = "#86ADA7", high = "#0F2D59") +
      labs(fill = fill_label)
    
    
  }else{
    trade_map <- trade_map
  }
  
  if(flow_arrows == TRUE){
    
    flows_df <- data %>%
      group_by(importer_iso3c, exporter_iso3c) %>% 
      summarize(quantity = sum(quantity, na.rm=TRUE)) %>%
      ungroup()
    
    
    flows_df <- flows_df %>%
      slice_max(n = n_flows, order_by = quantity) %>%
      # Join with lat/long data for centroids
      left_join(country_centroids, by = c("exporter_iso3c" = "iso3")) %>%
      left_join(country_centroids, by = c("importer_iso3c" = "iso3"))
    
    flows_df <- flows_df %>%
      mutate(
        centroid.lat.x = case_when(
          exporter_iso3c %in% c("USA", "CHN", "RUS") ~ 1.05 * centroid.lat.x,
          TRUE ~ centroid.lat.x),
        centroid.lat.y = case_when(
          importer_iso3c %in% c("USA", "CHN", "RUS") ~ 0.95 * centroid.lat.y,
          TRUE ~ centroid.lat.y
        ))
    
    
    trade_map <- trade_map +
      geom_curve(data = flows_df, 
                 aes(x = centroid.lon.x, y = centroid.lat.x, 
                     xend = centroid.lon.y, yend = centroid.lat.y,
                     color = quantity),
                 linewidth = 1,
                 alpha = 0.75,
                 curvature = -0.35, arrow = arrow(length = unit(3, "mm"), angle = 20)) +
      scale_colour_gradient(low = "#F7AF75", high = "#E24027") +
      labs(color = arrow_label)
  }
  
  trade_map <- trade_map + 
    theme_map() + 
    theme(legend.position = "bottom", legend.box="vertical", 
          plot.background = element_blank(), panel.border = element_blank(),
          legend.text = element_text(size=10),
          legend.title = element_text(size=11)) +
    guides(size = "none", fill = guide_colorbar(barwidth = 10, barheight = 0.75), 
           color = guide_colorbar(barwidth = 10, barheight = 0.75))
  
  if (!is.na(caption_label)) {
    trade_map <- trade_map +
      labs(caption = caption_label) +
      theme(plot.caption = element_text(face = "italic"))
  }
  
  return(trade_map)
}
