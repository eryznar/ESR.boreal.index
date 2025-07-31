  # produce ice cover time series for Bering (55-64N, 180-160W)
  
source("./Scripts/load_libs_params.R")
  ## load and process ------------------------
  
  # note that there are separate time series for 1950-1978 and 1979-present
  
  nc1 <- nc_open("./Data/ERA5_ice_1950-1978.nc")
  
  # process
  
  ncvar_get(nc1, "time")   # hours since 1-1-1900
  raw <- ncvar_get(nc1, "time")
  h <- raw/24
  d1 <- dates(h, origin = c(1,1,1900))
  m1 <- months(d1)
  yr1 <- years(d1)
  
  x1 <- ncvar_get(nc1, "longitude")
  y1 <- ncvar_get(nc1, "latitude")
  
  ice1 <- ncvar_get(nc1, "siconc", verbose = F)
  dim(ice1) # 87 long, 37 lat, 203 months
  
  # reverse lat for plotting
  ice1 <- ice1[,37:1,]
  
  # reverse y too
  y1 <- rev(y1)
  
  ice1 <- aperm(ice1, 3:1)
  
  ice1 <- matrix(ice1, nrow=dim(ice1)[1], ncol=prod(dim(ice1)[2:3]))
  
  # plot to check
  
  ice.mean <- colMeans(ice1, na.rm=T)
  z <- t(matrix(ice.mean,length(y1)))
  image.plot(x1,y1,z, col=oceColorsPalette(64), xlab = "", ylab = "")
  contour(x1, y1, z, add=T)
  map('world2Hires', c('usa', 'USSR'),  fill=T,add=T, lwd=1, col="lightyellow3")  
  
  # now the second time series
  
  nc2 <- nc_open("./Data/ERA5_ice_1979-2022.nc")
  
  # process
  
  ncvar_get(nc2, "time")   # hours since 1-1-1900
  raw <- ncvar_get(nc2, "time")
  h <- raw/24
  d2 <- dates(h, origin = c(1,1,1900))
  m2 <- months(d2)
  yr2 <- years(d2)
  
  x2 <- ncvar_get(nc2, "longitude")
  y2 <- ncvar_get(nc2, "latitude")
  
  # expver <-  ncvar_get(nc2, "expver", verbose = F)
  # expver # 1 and 5??
  
  
  ice2 <- ncvar_get(nc2, "siconc", verbose = F)
  dim(ice2) # 87 long, 37 lat, 2 expver, 203 months
  
  # expver1 - this is ERA5
  
  # ice2 <- ice2[,,1,]
  
  # reverse lat for plotting
  ice2 <- ice2[,37:1,]
  
  # reverse y too
  y2 <- rev(y2)
  
  ice2 <- aperm(ice2, 3:1)
  
  ice2 <- matrix(ice2, nrow=dim(ice2)[1], ncol=prod(dim(ice2)[2:3]))
  
  
  # plot to check
  
  ice.mean <- colMeans(ice2, na.rm=T)
  z <- t(matrix(ice.mean,length(y2)))
  image.plot(x2,y2,z, col=oceColorsPalette(64), xlab = "", ylab = "")
  contour(x2, y2, z, add=T)
  map('world2Hires', c('usa', 'USSR'),  fill=T,add=T, lwd=1, col="lightyellow3")  
  
  
  # check dimensions
  identical(x1, x2)
  identical(y1,y2)
  
  
  # Keep track of corresponding latitudes and longitudes of each column:
  lat <- rep(y1, length(x1))
  lon <- rep(x1, each = length(y1))
  
  ice <- rbind(ice1, ice2)
  
  # drop E of 165 and N of 63
  drop <- lon > -165 | lat > 63
  ice[,drop] <- NA
  
  # plot to check
  ice.mean <- colMeans(ice, na.rm=T)
  z <- t(matrix(ice.mean,length(y1)))
  image.plot(x1,y1,z, col=oceColorsPalette(64), xlab = "", ylab = "")
  contour(x1, y1, z, add=T)
  map('world2Hires', c('usa', 'USSR'),  fill=T,add=T, lwd=1, col="lightyellow3") # perfecto
  
  dimnames(ice) <- list(as.character(c(d1, d2)), paste("N", lat, "E", lon, sep=""))
  
  f <- function(x) colMeans(x, na.rm = T)
  
  m <- c(as.character(m1), as.character(m2))
  
  yr <- c(as.numeric(as.character(yr1)), as.numeric(as.character(yr2)))
  
  means <- data.frame(month = m,
                      year = as.numeric(as.character(yr)),
                      ice = rowMeans(ice, na.rm = T)) 
  
  
  ggplot(means, aes(year, ice, color = month)) +
    geom_line()
  
  # drop Oct - Dec
  means <- means %>%
    filter(!month %in% c("Oct", "Nov", "Dec"))
  
  
  # pivot wider
  means <- means %>% 
    pivot_wider(values_from = ice, names_from = month) %>%
    filter(year %in% 1953:2022) 

 # 2023-2024 EMILY CODE BELOW

    nc <- nc_open("./Data/ERA5_ice_2023-2024.nc")
    
    ice <- ncvar_get(nc, "siconc", verbose = F) 
    
    ice.1 <- ice[,,1,]
    ice.5 <- ice[,,2,]
    
    dim(ice.1) #1440 lon, 721 lat, 19 months
    dim(ice.5) #1440 lon, 721 lat, 19 months
    
    
    #Process
    h <- (ncvar_get(nc, "time")/24)
    d <- dates(h, origin = c(1, 1, 1900))  
    m <- months(d)
    yr <- chron::years(d)
    
    x <- ncvar_get(nc, "longitude")
    y <- ncvar_get(nc, "latitude")
    
    # Keep track of corresponding latitudes and longitudes of each column:
    lat <- rep(y, length(x))
    lon <- rep(x, each = length(y))
    
    #Create sst data matrix
    ice.1 <- aperm(ice.1, 3:1)
    ice.5 <- aperm(ice.5, 3:1)
    
    mat_ice.1 <- t(matrix(ice.1, nrow = dim(ice.1)[1], ncol = prod(dim(ice.1)[2:3])))
    mat_ice.5 <- t(matrix(ice.5, nrow = dim(ice.5)[1], ncol = prod(dim(ice.5)[2:3])))
    
    
    data.frame(lon = lon, lat = lat,  mat_ice.1) %>%
      pivot_longer(cols = c(3:ncol(.)), names_to = "month", values_to = "ice") %>%
      mutate(month = rep(m, nrow(.)/length(m)), year = rep(yr, nrow(.)/length(yr)), ice = ice) %>%
      na.omit()-> ice_latlon_2023.2024a ## all 2023, 2024 Jan-June
    
    data.frame(lon = lon, lat = lat,  mat_ice.5) %>%
      pivot_longer(cols = c(3:ncol(.)), names_to = "month", values_to = "ice") %>%
      mutate(month = rep(m, nrow(.)/length(m)), year = rep(yr, nrow(.)/length(yr)), ice = ice) %>%
      na.omit()-> ice_latlon_2023.2024b ## 2024 July
    
    
    #bind with ice datasets, subset to study area, calculate mean by month
    rbind(ice_latlon_2023.2024a, ice_latlon_2023.2024b) %>%
      filter(lon >= -180 & lon <= -165,
             lat >= 55 & lat <= 63) %>% #drop E of 165 and N of 63
      filter(month %in% c("Jan", "Feb", "Mar", "Apr")) %>% #months != Jan-Apr
      group_by(year, month) %>%
      reframe(value= mean(ice)) -> ice_2023.2024
    
    # Bind with data < 2023
    rbind(means, pivot_wider(ice_2023.2024, names_from = month)) %>%
      mutate(year = as.numeric(year)) -> ice.means
    
    # Scale
    ice.means[,2:5] <- apply(ice.means[,2:5], 2, scale)
    
    # Pivot longer and plot
    plot <- ice.means %>%
      pivot_longer(cols = -year)
    
    ggplot(plot, aes(year, value, color = name)) +
      geom_line() 
    
    # generate Jan-Feb and Mar-Apr means
    ice.means$JanFeb_ice <- apply(ice.means[,2:3], 1, mean)
    ice.means$MarApr_ice <- apply(ice.means[,4:5], 1, mean)
    
    # clean up
    ice.means2 <- ice.means %>%
      dplyr::select(year, JanFeb_ice, MarApr_ice) %>%
      rename(`Jan-Feb ice cover` = JanFeb_ice,
             `Mar-Apr ice cover` = MarApr_ice) %>%
      filter(year >= 1972) %>%
      pivot_longer(cols = -year)
      
    
    # save ERA5
    write.csv(ice.means2, "./Output/ice.csv", row.names = F)
  
## NEW CODE ----
    nc <- nc_open("./Data/ERA5_ice_2024.nc")
    
    ice <- ncvar_get(nc, "siconc", verbose = F) 
    
    # load and process 1950-1978
    tidync("./Data/ERA5_ice_1950-1978.nc") %>%
      hyper_filter(longitude = longitude >= -180 & longitude <= -165,
                   latitude = latitude >= 55 & latitude <= 63) %>%
      activate("siconc") %>%
      hyper_tibble() %>%
      mutate(year = lubridate::year(time),
             month = lubridate::month(time),
             latitude = as.numeric(as.character(latitude)),
             longitude = as.numeric(as.character(longitude))) %>%
      filter(month %in% c(1:4)) %>% #months != Jan-Apr
      group_by(year,  month)  %>%
      reframe(value= mean(siconc)) -> ice.1950_1978
    
    # load and process 1979-2022
    tidync("./Data/ERA5_ice_1979-2022.nc") %>%
      hyper_filter(longitude = longitude >= -180 & longitude <= -165,
                   latitude = latitude >= 55 & latitude <= 63) %>%
      activate("siconc") %>%
      hyper_tibble() %>%
      mutate(year = lubridate::year(time),
             month = lubridate::month(time),
             latitude = as.numeric(as.character(latitude)),
             longitude = as.numeric(as.character(longitude))) %>%
      filter(month %in% c(1:4)) %>% #months != Jan-Apr
      # mutate(name = case_when((month %in% 1:2) ~ "Jan-Feb",
      #                           TRUE ~ "Mar-Apr")) %>%
      group_by(year,  month)  %>%
      reframe(value= mean(siconc)) -> ice.1979_2022
    
    
    # load and process 2023-2024
    tidync("./Data/ERA5_ice_2023-2024.nc") %>%
      hyper_filter(longitude = longitude >= -180 & longitude <= -165,
                   latitude = latitude >= 55 & latitude <= 63) %>%
      activate("siconc") %>%
      hyper_tibble() %>%
      mutate(year = lubridate::year(time),
             month = lubridate::month(time),
             latitude = as.numeric(as.character(latitude)),
             longitude = as.numeric(as.character(longitude))) %>%
      filter(month %in% c(1:4)) %>% #months != Jan-Apr
      group_by(year,  month)  %>%
      reframe(value= mean(siconc)) -> ice.2023_2024
      
    # load and process new data
    old.ice <- read.csv("./Output/ice.csv")
     
    
 