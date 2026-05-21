## Setting the repositories
setRepositories()
## Creating packages.txt file; we can download packages manually by using install.package()
packages=scan("packages.txt",what=character())
install.packages("packages",dependencies = TRUE)
## Using lapply to loop through all packages in the file.
lapply(packages, library, character.only = TRUE)
## Making the plots
world= ne_countries(scale="medium",returnclass = "sf")
view(world)
## Practice plot 1
# aes-aesthetic plot,geom_sf-for plotting, theme_map-for theme,scale_fill_viridis_d-for colors
ggplot(data=world,aes(fill = income_grp)) +geom_sf() +theme_map() +scale_fill_viridis_d()
world %>%
  ggplot(aes(fill = income_grp)) +geom_sf() +theme_map() +scale_fill_viridis_d()
## Practice plot 2
world %>%
  filter(region_un=="Africa") %>%
  ggplot() + geom_sf(aes(fill=pop_est/1e6), color="green", lwd=0.3) + theme_map()
## Practice plot 3
# Getting the administration levels
gisco_get_nuts(country='Germany') %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  count(levl_code)
# Getting to Administration level 3
gisco_get_nuts(country = 'Germany',nuts_level = 3,year='2021') %>%
  as_tibble() %>%
  janitor::clean_names()
# Assigning variable to administrative level 3
germany_distr=gisco_get_nuts(
  country = 'Germany',nuts_level = 3,year='2021',epsg = 3035
  ) %>%
  as_tibble() %>%
  janitor::clean_names()
# Assigning variable to administration level 1
germany_states=gisco_get_nuts(
  country = 'Germany',nuts_level = 1,year='2021',epsg = 3035
) %>%
  as_tibble() %>%
  janitor::clean_names()
# Plotting level 3
germany_distr %>%
  ggplot(aes(geometry=geometry)) +geom_sf()
# Making the plot interactive using ggiraph
germany_distr %>%
  ggplot(aes(geometry=geometry)) +geom_sf(data = germany_states,aes(fill=nuts_name,color='black',lwd=0.5))+ geom_sf_interactive(fill=NA,aes(data_id=nuts_id,tooltip=nuts_name),color='black',lwd=0.1)+theme_void()
#  Identifying which German state contains a given district geometry.
map_lgl(
  germany_states$geometry, 
  \ (y) {
    st_within(
      germany_distr$geometry[[1]],
      y
    ) %>% as.logical()
  }
) %>% which()
# Mapping each district to the state index that contains it.
map_dbl(
  germany_distr$geometry, 
  \(x) {map_lgl(
    germany_states$geometry, 
    \ (y) {
      st_within(x,y) %>% as.logical()
    }
  ) %>% which()
  }
)
# Mapping each district to its parent state numbers
state_nmbrs=  
  map_dbl(
    germany_distr$geometry , 
    \(x) {
      map_lgl(
        germany_states$geometry,
        \(y) {
          st_within(
            x,
            y
          ) %>% as.logical() 
        }
      ) %>% which()
    }
  )
# Creating a dataset of German districts with their corresponding state names
germany_districts_w_state <- germany_distr %>% 
  mutate(
    state = germany_states$nuts_name[state_nmbrs]
  )
# Plotting German states with interactive district boundaries and tooltips
gg_plt = germany_districts_w_state %>% 
  ggplot(aes(geometry = geometry))+ geom_sf( data = germany_states, 
                                             aes(fill= nuts_name, 
                                                 color = "black", 
                                                 lwd = 0.5)) + geom_sf_interactive( fill = NA, 
                                                                                    aes(
                                                                                      data_id = nuts_id, 
                                                                                      tooltip = nuts_name
                                                                                    ), 
                                                                                    color= 'black', 
                                                                                    lwd  = 0.1) +
  theme_void()
girafe(ggobj = gg_plt)
# Generating an interactive map of German states and districts with custom colors and tooltips
gg_plt = germany_districts_w_state %>% 
  ggplot(aes(geometry = geometry))+ geom_sf( data = germany_states, 
                                             aes(fill= nuts_name, 
                                                 color = "black", 
                                                 lwd = 0.5)) + geom_sf_interactive( fill = NA, 
                                                                                    aes(
                                                                                      data_id = nuts_id, 
                                                                                      tooltip = glue::glue(
                                                                                        '{nuts_name}<br>{state}'
                                                                                      )
                                                                                    ), 
                                                                                    color= 'black', 
                                                                                    lwd  = 0.1) +
  theme_void() + theme(legend.position = 'none') +
  scale_fill_manual(
    values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
               "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", 
               "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", 
               "#98df8a", "#ff9896", "#c5b0d5", "#c49c94")
  )
girafe(ggobj = gg_plt)

# Creating styled labels and render an interactive map of German states and districts
make_nice_label = function(nuts_name, state) {
  nuts_name_label = htmltools::span(
    nuts_name, 
    style = htmltools::css(
      fontweight = 600, 
      font_family = 'Source Sans Pro', 
      font_size = '32px'
    )
  )
  state_label <- htmltools::span(
    state, 
    style = htmltools::css(
      font_family ='Source Sans Pro', 
      font_size = '20px'
    )
  )
  glue::glue('{nuts_name_label}<br>{state_label}')
}
gg_plt = germany_districts_w_state %>% 
  mutate(
    nice_label = map2_chr(
      nuts_name, 
      state, 
      make_nice_label
    )
  ) %>% 
  ggplot(aes(geometry = geometry))+ geom_sf( data = germany_states, 
                                             aes(fill= nuts_name, 
                                                 color = "black", 
                                                 lwd = 0.5)) + geom_sf_interactive( fill = NA, 
                                                                                    aes(
                                                                                      data_id = nuts_id, 
                                                                                      tooltip = nice_label
                                                                                    ), 
                                                                                    color= 'black', 
                                                                                    lwd  = 0.1) +
  theme_void() + theme(legend.position = 'none') +
  scale_fill_manual(
    values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
               "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", 
               "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", 
               "#98df8a", "#ff9896", "#c5b0d5", "#c49c94")
  )
girafe(ggobj = gg_plt)
