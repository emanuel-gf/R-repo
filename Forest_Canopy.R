

# %%

## Load the files
raster_files <- 
                list.files(
                  path = "D:/R_download/Canopy_Height/Portugal",
                  pattern = "ETH",
                  full.names = T
                )

## Portugal Polygon
folder_path <- "D:/R_download/Canopy_Height/Portugal"

get_country_borders <- function(){
  main_path <- folder_path 
  country_borders <- geodata::gadm(
    country = "PRT",
    level=1,
    path = main_path 
  ) |>
    sf::st_as_sf() ##convert to shapefile

return (country_borders)
}

country_borders <- get_country_borders()


# %%
###### Filtering Portugal ###############
portugal_sf <- country_borders |>
  dplyr::filter( ##Filters all regions that are not azores and madeira 
    !NAME_1 %in% c(
      "Azores",
      "Madeira"
    )
  ) |>
  sf::st_union() ## colapse all small regions of portugal

# %%
###### LOAD FOREST HEIGHT ###############

## transform all files into terra.raster
forest_height_list <- lapply(
  raster_files, terra::rast
)

forest_height_rasters <- lapply(
  forest_height_list,
  function(x){
    terra::crop(
      x,
      terra::vect(portugal_sf)
    ,
    snap = 'in',  ## all that is outside the polygon is declared as NAN value and not being seen
    mask=T
    )
  }
)

##Create the mosaic
forest_height_mosaic <- do.call(
  terra::mosaic,
  forest_height_rasters
)


# %%
### Aggregate the mosaic
### Rayshader has a maximum value acceptable for pixel as 6k to 8k.
### The current mosaic has over the 40k
forest_height_portugal <- forest_height_mosaic |>
  terra::aggregate(
    fact = 25
  )

terra::writeRaster(
  forest_height_portugal,
  "D:/R_download/Canopy_Height/Portugal/forest_height_pt_250m"
)
# %%
## Read raster
forest_height_portugal_100m <- terra::rast(
  "D:/R_download/Canopy_Height/Portugal/forest_height_pt_100m.tif"
)
# %%
forest_height_portugal_200 <- forest_height_portugal_100m |>
  terra::aggregate(
    fact = 2.5
  )

# %%
print(forest_height_portugal_200)
# %%
### RASTER TO DATAFRAME
forest_height_portugal_df <- forest_height_portugal_200 |>
  as.data.frame(
    xy=T
  )
# changing the names of the third column
names(forest_height_portugal_df)[3]<-"height"


# %%
### BREAKS
#--------------
breaks <- classInt::classIntervals(
  forest_height_portugal_df$height,
  n=5,
  style = "fisher",
)$brks 


# %%
## COLOR PALETTS
cols <- c(
  "#ffffff", "#ffd3af","#fab57c", "#8dd86f",
        "#6daa55", "#205544"
)

texture <- colorRampPalette(
  cols,
  bias = 3 # color at the end of the spectrum
)(length(breaks))

# %%

library(ggplot2)
library(grid)


p <- ggplot2::ggplot(
  forest_height_portugal_df
) +
  ggplot2::geom_raster(
    ggplot2::aes(
      x = x,
      y = y,
      fill = height
  )
) +
  ggplot2::scale_fill_gradientn(
  name = "height (m)",
  colors = texture,
  breaks = round(breaks, 0)
) +
  ggplot2::coord_sf(crs = 4326) +
    ggplot2::guides(
  fill = ggplot2::guide_legend(
      direction = "vertical",
      keyheight = grid::unit(5, "mm"),
      keywidth = grid::unit(5, "mm"),
      title.position = "top",
      label.position = "right",
      title.hjust = .5,
      label.hjust = .5,
      ncol = 1,
      byrow = F
  )
) +
  ggplot2::theme_minimal() +
    ggplot2::theme(
  axis.line = ggplot2::element_blank(),
  axis.title.x = ggplot2::element_blank(),
  axis.title.y = ggplot2::element_blank(),
  axis.text.x = ggplot2::element_blank(),
  axis.text.y = ggplot2::element_blank(),
  legend.position = "right",
  legend.title = ggplot2::element_text(
      size = 11, color = "grey10"
  ),
  legend.text = ggplot2::element_text(
      size = 10, color = "grey10"
  ),
  panel.grid.major = ggplot2::element_line(
      color = "white"
  ),
   panel.grid.minor = ggplot2::element_line(
      color = "white"
  ),
  plot.background = ggplot2::element_rect(
      fill = "white", color = NA
  ),
  legend.background = ggplot2::element_rect(
      fill = "white", color = NA
  ),
  panel.border = ggplot2::element_rect(
      fill = NA, color = "white"
  ),
  plot.margin = grid::unit(
      c(
          t = 0, r = 0,
          b = 0, l = 0
      ), "lines"
  )
)

p
# %%
p <- ggplot2::ggplot(
  forest_height_portugal_df
) +
  ggplot2::geom_tile(
    ggplot2::aes(
      x = x,
      y = y,
      fill = height
    )
  ) +
  ggplot2::scale_fill_gradientn(
    name = "height (m)",
    colors = texture,
    breaks = c(round(breaks,0))
  ) +
  ggplot2::coord_sf(crs = 4326) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      direction = "vertical",
      keyheight = grid::unit(5, "mm"),
      keywidth = grid::unit(5, "mm"),
      title.position = "top",
      label.position = "right",
      title.hjust = .5,
      label.hjust = .5,
      ncol = 1,
      byrow = FALSE
    )
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    legend.position = "right",
    legend.title = ggplot2::element_text(
      size = 11, color = "grey10"
    ),
    legend.text = ggplot2::element_text(
      size = 10, color = "grey10"
    ),
    panel.grid.major = ggplot2::element_line(
      color = "white"
    ),
    panel.grid.minor = ggplot2::element_line(
      color = "white"
    ),
    plot.background = ggplot2::element_rect(
      fill = "white", color = NA
    ),
    legend.background = ggplot2::element_rect(
      fill = "white", color = NA
    ),
    panel.border = ggplot2::element_rect(
      fill = NA, color = "white"
    ),
    plot.margin = grid::unit(
      c(t = 0, r = 0, b = 0, l = 0), "lines"
    )
  )


# %%
# Make sure these are defined correctly
print(c(round(breaks,0)))