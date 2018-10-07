library(shiny)
library(fontawesome)
library(shinyWidgets)
library(bs4Dash)
library(plotly)
#library(mapview)
library(r2d3)
library(r2d3maps)
library( rnaturalearth )
#library(rnaturalearthhires)
library(magick)
#
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(DT)
#
library(recipes)
library(highcharter)
library(tidyverse)
library(rdhs)
library(billboarder)

library(echarts4r)
library(countrycode) # converstions
Targetcountries  <- c("Chad","Niger","Mali","Burkina Faso","Mauritania")

library(flexdashboard)
library(sf)

# 
library(janitor)
library(readxl)
# 
library(shinyWidgets)

# DHS: Getting Data on Education : Youth employability
d1_variables <-c("ED_GARS_B_BTH","ED_GARP_B_BTH")
d2_variables <-c("ED_LITR_M_LIT","ED_LITR_W_LIT")
d3_variables <-"EIP_NEET_SEX_NB"
d4_variables <-c("ED_GARS_B_GPI","ED_GARP_B_GPI")
d5_variables <-c("HC_WIXQ_P_4TH","HC_WIXQ_P_MID","HC_WIXQ_P_HGH","HC_WIXQ_P_LOW","HC_WIXQ_P_2ND")
d6_variables <-"ED_EDAT_B_MYR"

variables<- c(d1_variables,d2_variables,d3_variables,d4_variables,d5_variables,d6_variables)


# dhs
df_dhs <- tbl_df(read_csv('./inputs/df_dhs.csv'))%>%
  mutate(
    CharacteristicLabel = stringr::str_replace_all( # remove leading points (ex: "..Kanem") in labels
      string = CharacteristicLabel,
      pattern = "^\\.+", replacement = ""
    )
  )%>%
  mutate(location=paste0(DHS_CountryCode,sep='-',CharacteristicLabel))%>%
  mutate_if(is.character, factor, ordered = FALSE)%>%
  mutate('iso3c' = countrycode(CountryName, 'country.name', 'iso3c'))

# sdg4 indicators
school_enrol_funnel <- tbl_df(read_csv('./inputs/school_enrol_funnel.csv'))%>%mutate_if(is.character, factor, ordered = FALSE)
enrol_by_income <- tbl_df(read_csv('./inputs/enrol_by_income.csv'))%>%mutate_if(is.character, factor, ordered = FALSE)
pre_primary_enrol <- tbl_df(read_csv('./inputs/pre_primary_enrol.csv'))%>%mutate_if(is.character, factor, ordered = FALSE)
edu_spending <- tbl_df(read_csv('./inputs/edu_spending.csv'))%>%mutate_if(is.character, factor, ordered = FALSE)
pupil_teacher_ratio <- tbl_df(read_csv('./inputs/pupil_teacher_ratio.csv'))%>%mutate_if(is.character, factor, ordered = FALSE)
completion_by_gend_income <- tbl_df(read_csv('./inputs/completion_by_gend_income.csv'))%>%mutate_if(is.character, factor, ordered = FALSE)
gend_parity <- tbl_df(read_csv('./inputs/gend_parity.csv'))%>%mutate_if(is.character, factor, ordered = FALSE)
enrol_fertility <- tbl_df(read_csv('./inputs/enrol_fertility.csv'))%>%mutate_if(is.character, factor, ordered = FALSE)
out_of_school_primary <- tbl_df(read_csv('./inputs/out_of_school_primary.csv'))%>%mutate_if(is.character, factor, ordered = FALSE)

# Données de l'ACLED

acled_tiwara <- read_csv("./inputs/acled_data/2010-01-01-2018-06-30.csv")
#acled_tiwara <- read_csv("./inputs/acled_data/2010-01-01-2018-06-30-Eastern_Africa-Middle_Africa-Middle_East-Western_Africa-Burkina_Faso-Chad-Mali-Mauritania-Niger.csv")

acled_tiwara <- acled_tiwara %>%mutate(event_date = dmy(event_date))%>%mutate_if(is.character, factor, ordered = FALSE)


# data for map --------------------------------------------------------------------

df_dhs_sf <- df_dhs %>%
  filter(!is.na(Coordinates)) %>% # remove missing values
  select(IndicatorId, CountryName, CharacteristicLabel, Value, Coordinates) %>%
  distinct(IndicatorId, CharacteristicLabel,CountryName, .keep_all = TRUE) %>% # <<<---- deduplicated (you need one row by country at the end)
  tidyr::spread(IndicatorId, Value) %>% # <<<---- here transpose data
  mutate(
    Coordinates = st_as_sfc(Coordinates) # convert to sf geometries
  )%>%
  st_as_sf()


#### Donnee INform Risk #####@


# Inform Indicators

INFORM_SAHEL_2017_v102_raw <- read_excel("./inputs/inform_data/INFORM_SAHEL_2017_v102.xlsx",
                                         sheet = "INFORM SAHEL June 2017 (a-z)", na = c("(0-10)","x","(0-50)","(0-100%)","()","(1-191)"))
INFORM_SAHEL_2017_v102 <- INFORM_SAHEL_2017_v102_raw %>% slice(-1L)%>%janitor::clean_names() #remove 1st row, which is simple the scale

sahel_tiwara <- c("Burkina Faso","Chad","Mali","Mauritania","Niger")
sahel_tiwara_iso3 <- c("BFA","TCD","MLI","MRT","NER")

inform_sahel_tiwara <- INFORM_SAHEL_2017_v102 %>%filter(iso3 %in% sahel_tiwara_iso3)

##-------get geodata on Sahel states 
#sahel <- ne_states(country=sahel_tiwara, returnclass = "sf") # filter on states in target countries
#st_write(st_as_sf(sahel), './inputs/sahel.shp')
sahel <- st_read('./inputs/sahel.shp')
sahel %>%select(admin,adm0_a3,name, name_fr,region_sub,geometry,latitude, longitude)

sahel_tiwara_geo <- left_join(
  x = sahel %>%select(admin,adm0_a3,name, name_fr,region_sub,geometry,latitude, longitude),
  y = inform_sahel_tiwara,
  by = c("name" = "admin1")
)




# ----------------------------------------------

# ----------------------------

shiny::shinyApp(
  ui = bs4DashPage(
   # old_school = TRUE,
    navbar = bs4DashNavbar(
      status = "white",
      "TIWARA - L'initiative Française Portée par L'AFD",
      rightUi = bs4DropdownMenu(
        show = FALSE,
        labelText = "!",
        status = "danger",
        src = "http://www.google.fr",
        bs4DropdownMenuItem(
          text = "Survey Year",
          date = "today"
        ),
        bs4DropdownMenuItem(
          text = "Last Data Update",
          date = "yesterday"
        )
      )
    ),

    sidebar = bs4DashSidebar(
      skin = "light",
      status = "primary",
      title = "Tiwara Dash",
      brandColor = "primary",
      url = "http://www.afd.fr",
      src = "https://upload.wikimedia.org/wikipedia/fr/5/58/Logo-AFD.jpg",
      #src = "https://pbs.twimg.com/profile_images/819938214853148673/5X2_8VLs_400x400.jpg", #brainy pic
      elevation = 4,
      opacity = 0.8,
      bs4SidebarMenu(
        selectInput('CountryName', "Pays",
                    c("Sahel G5",levels(df_dhs$CountryName)),selected = "Sahel G5"),
        bs4SidebarHeader("Insertion socio éco des jeunes"),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "General Outlook",
              bs4Badge(
                "new",
                position = "right",
                status = "success"
              )
            )
          ),
          tabName = "general",
          icon = "laptop"  #"desktop"
        ),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "Indicator View",
              bs4Badge(
                "new",
                position = "right",
                status = "success"
              )
            )
          ),
          tabName = "indicatorMap",
          icon = "gears" #"sliders"
        ),
      #   bs4SidebarMenuItem(
      #   HTML(
      #     paste(
      #       "ODD 4 View",
      #       bs4Badge(
      #         "todo!",
      #         position = "right",
      #         status = "warning"
      #       )
      #     )
      #   ),
      #   tabName = "odd4",
      #   icon = "suitcase"
      # ),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "Carto Bar",
              bs4Badge(
                "beta!",
                position = "right",
                status = "warning"
              )
            )
          ),
          tabName = "carto",
          icon = "map"
        ),
      bs4SidebarMenuItem(
        HTML(
          paste(
            "Data Sources",
            bs4Badge(
              "new",
              position = "right",
              status = "success"
            )
          )
        ),
        tabName = "dbsource",
        icon = "database"
      ),
      bs4SidebarHeader("Crisis and Conflict Trends"),
      bs4SidebarMenuItem(
        HTML(
          paste(
            "INFORM Risks",
            bs4Badge(
              "new",
              position = "right",
              status = "success"
            )
          )
        ),
        tabName = "inform_risk_data",
        icon = "map"
      ),
      bs4SidebarMenuItem(
        HTML(
          paste(
            "ACLED Reports",
            bs4Badge(
              "new",
              position = "right",
              status = "success"
            )
          )
        ),
        tabName = "crisis",
        icon = "desktop"
      ),
      bs4SidebarHeader("About Project"),
      bs4SidebarMenuItem(
        HTML(
          paste(
            "About Tiwara",
            bs4Badge(
              "to do",
              position = "right",
              status = "warning"
            )
          )
        ),
        tabName = "description",
        icon = "picture-o"
      ),
      bs4SidebarHeader("Enjeux démographiques"),
      bs4SidebarMenuItem(
        "Démographiques",
        tabName = "démograph",
        icon = "object-ungroup"
      ),
      bs4SidebarHeader("Developpement local"),
      bs4SidebarMenuItem(
        HTML(
          paste(
            "Infrastructures",
            bs4Badge(
              "todo",
              position = "right",
              status = "warning"
            )
          )
        ),
        tabName = "backlog",
        icon = "globe"
      )
      
      )
    ),


    body = bs4DashBody(
      ## ------------------ add the script below to work with shinyapp.io -------------------------
      shiny::tags$head(
        shiny::tags$script(
          "// handle shinyapps.io: we need to extract the worker id and
          // paste it in the url so that the apps works correctly
          // get the shiny app.io workerId
          var workerId = $('base').attr('href');
          // ensure that this code does not run on shiny server/pro and locally
          if (typeof workerId != 'undefined') {
          // get the initial page url
          var url = window.location.href;
          // get the name of the first selected tab
          // replace the url by the url for shinyapp.io
          window.location.replace(url + workerId);
          }
          "
        )
        ),
      ## ------------------ added to work with shinyapp.io -------------------------
      bs4TabItems(
        bs4TabItem(
          tabName = "general",
          fluidRow(
            bs4InfoBoxOutput("ibox1"),
            bs4InfoBoxOutput("ibox2"),
            bs4InfoBoxOutput("ibox3")
          ),
          fluidRow(
            align = "center",
            column(3,billboarderOutput("gplt_w_literacy", height = "auto")), #column(6,echarts4rOutput("picto_rate")),
            column(3,billboarderOutput("gplt_m_literacy", height = "auto")),
            column(3,billboarderOutput("gplt_seco", height = "auto")),
            column(3,billboarderOutput("gplt_prim", height = "auto")) #column(6,echarts4rOutput("picto_rate_mont"))
          ),
          fluidRow(
            bs4Card(
              title = "Education Indicator Map",
              elevation = 4,
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              selectInput('varia', "Select Education Indicator",choices = list(
                "Women who are literate"="ED_LITR_W_LIT",
                "Men who are literate"="ED_LITR_M_LIT",
                "Number of years of education"="ED_EDAT_B_MYR",
                "Primary school attendance rate"="ED_GARP_B_BTH",
                "Secondary school attendance rate"="ED_GARS_B_BTH",
                "Parity index for primary school attendance"="ED_GARP_B_GPI",
                "Parity index for secondary school attendance"="ED_GARS_B_GPI",
                "Population in the lowest wealth quintile"="HC_WIXQ_P_LOW",
                "Population in the second wealth quintile"="HC_WIXQ_P_2ND",
                "Population in the middle wealth quintile"="HC_WIXQ_P_MID",
                "Population in the fourth wealth quintile"="HC_WIXQ_P_4TH",
                "Population in the highest wealth quintile"="HC_WIXQ_P_HGH"), selected = "ED_LITR_W_LIT"),  
              d3Output('mymap'), # , width = "1000px" #leafletOutput('indicatormap'),
              footer = column(
                width = 12,
                align = "center",
                radioButtons(
                   inputId = "palette",
                    label = "Change color",
                    choices = c("viridis", "magma", "plasma", "Blues", "Greens", "Reds"), selected = "Blues",
                      inline = TRUE
              )
              ) #end footer
            )# end bs4Card
            ), 
          fluidRow(
            bs4Card(
              title = "Vulnerable States by Indicator",
              elevation = 4,
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              selectInput('Indicator', NULL,
                          choices = c(
                            "Men who are literate",
                            "Women who are literate",
                            "Number of years of education" = "Median number of years of education: Both sexes",
                            "Primary school attendance rate" = "Gross primary school attendance rate: Total",
                            "Secondary school attendance rate" = "Gross secondary school attendance rate: Total",
                            "Parity index for primary school attendance" = "Gross parity index for gross primary school attendance",
                            "Parity index for secondary school attendance" = "Gross parity index for gross secondary school attendance"
                          ),
                          #levels(df_dhs$Indicator),
                          selected = "Women who are literate"),
              billboarderOutput("barplot_lol"),
              footer = column(
                width = 12,
                align = "center",
                sliderInput("vuls", "Select number of Vulnerable States:",
                            min = 0, max = 15, value = 10
                )
              )# end footer
            )
          )
        ),
        bs4TabItem(
          tabName = "indicatorMap",
          fluidRow(
           bs4ValueBoxOutput('primary_pupil_teacher_ratio_reat_vbox'),
           bs4ValueBoxOutput('lower_sec_pupil_teacher_ratio_reat_vbox'),
           bs4ValueBoxOutput('upper_sec_pupil_teacher_ratio_reat_vbox')
          ),
          fluidRow(
            bs4Card(
              title = "Gender Parity Index and School Enrolment (Primary, Secondary & Tertiary)",
              closable = FALSE,
              width = 12,
              #status = "warning",
              solidHeader = FALSE,
              status = "primary",
              #gradientColor = "primary", #"success",
              collapsible = TRUE,
              fluidRow(
                column(6,billboarderOutput("gend_parity_plt")), #column(6,echarts4rOutput("picto_rate")),
                column(6,billboarderOutput("enrol_funnel_plt")) #column(6,echarts4rOutput("picto_rate_mont"))
              ),
              footer = column(
                width = 12,
                align = "center",
                sliderInput("period", "Period in years", min = 2010, max = year(today()), value = c(min,max),step = 1)
              )# end footer
            ), # end bs4Card
          bs4Card(
            title = "School Attendance with Literacy Rate",
            elevation = 4,
            closable = FALSE,
            width = 12,
            status = "primary", # "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("bubble"),
            footer = column(
              width = 12,
              align = "center",
              radioButtons(
                "literacy", label = NULL,
                choices=c("Women Literacy Rate" = 'ED_LITR_W_LIT',
                          "Men Literacy Rate" = 'ED_LITR_M_LIT'
                )
              )
            )# end footer
          ),
            bs4Card(
              title = "Primary Completion Rate by Sex",
              closable = FALSE,
              elevation = 4,
              width = 6,
              #status = "warning",
              solidHeader = TRUE,
              status = "primary",
              #gradientColor = "primary", #"success",
              collapsible = TRUE,
              billboarderOutput("prim_completion_by_gend_income_plt")
            ), # end bs4Card
            bs4Card(
              title = "Lower Secondary Completion Rate by Sex",
              elevation = 4,
              closable = FALSE,
              width = 6,
              #status = "warning",
              solidHeader = TRUE,
              status = "primary",
              #gradientColor = "primary", #"success",
              collapsible = TRUE,
              billboarderOutput("seco_completion_by_gend_income_plt")
            )
          ),
          fluidRow(
            bs4Card(
              title = "Out-of-School Children and Youth",
              elevation = 4,
              closable = FALSE,
              width = 12,
              #status = "warning",
              solidHeader = TRUE,
              status = "primary",
              #gradientColor = "primary", #"success",
              collapsible = TRUE,
              billboarderOutput("out_of_school_primary_plt")
            )
          )
        ),

        bs4TabItem(
          tabName = "carto",
          fluidRow(
            tags$iframe(seamless="seamless",src="https://kepler.gl/#/demo", height=800, width=1400)
          )
          ),

        bs4TabItem(
          tabName = "description",
          fluidRow(
            bs4Card(
              title = "TIWARA - L'initiative Française Portée par L'AFD",
              width = 12,
              bs4Carousel(
                id = "mycarousel",
                width = 12,
                bs4CarouselItem(
                  active = TRUE,
                  src = "https://upload.wikimedia.org/wikipedia/fr/5/58/Logo-AFD.jpg"
                ),
                bs4CarouselItem(
                  active = FALSE,
                  src = "https://raw.githubusercontent.com/brainy749/Tiwara/master/g5_sahel.jpg"
                ),
                bs4CarouselItem(
                  active = FALSE,
                  src = "https://raw.githubusercontent.com/brainy749/Tiwara/master/g5_sahel_en_3_cle8243c4.jpg"
                )
              )
            )
          ),
          fluidRow(
            bs4Card(
              width = 6,
              title = "3 Dimension of the Tiwara Initiative",
              closable = FALSE,
              status = "primary",
              footer = tagList(
               # h4("Le nom « Tiwara » désigne une statuette traditionnelle dans tous le Sahel en forme de gazelle stylisée qui symbolise la fertilité des terres et la créativité de ces régions."),
                bs4Accordion(
                  bs4AccordionItem(
                    id = "item1",
                    title = "Insertion socio éco des jeunes",
                    status = "primary",
                    "L’initiative Tiwara au Sahel vise à lutter contre les fragilités profondes qui
                    sous-tendent les crises pour accroître la résilience des régions du Sahel.
                    Ces interventions cibleront les populations dans les zones fragilisées à travers un
                    effort financier additionnel, financé par une part du produit de la taxe sur les
                    transactions financières."
                  ),
                  bs4AccordionItem(
                    id = "item2",
                    title = "Enjeux démographiques",
                    status = "warning",
                    "info sur enjeux demographiques..."
                  ),
                  bs4AccordionItem(
                    id = "item3",
                    title = "Developpement local",
                    status = "warning",
                    "info sur enjeux developpement local..."
                  )
                  )
                  )
                  ),
            bs4UserCard(
              width = 6,
              type = 2,
              src = "https://www.afd.fr/sites/afd/files/styles/1120_x_750/public/2017-07/Remy-Rioux-par-Alain-Buu.jpg?itok=9vbKvJam",
              status = "info",
              title = "Rémy Rioux",
              subtitle = "Directeur général de l'AFD",
              elevation = 4,
              "Nous allons intervenir plus et mieux au Sahel, avec nos partenaires africains, européens et internationaux. L’AFD contribuera à ce plan d’action d’envergure centré sur l’éducation, l’emploi des jeunes, l’agriculture, les énergies vertes et la gouvernance."
            )
          )
          ),
        bs4TabItem(
          tabName = "dbsource",
          fluidRow(
            bs4Card(
              title = "Data Sources Providers",
              width = 12,
              closable = FALSE,
              footer = tagList(
                bs4Accordion(
                  bs4AccordionItem(
                    id = "wb",
                    title = "World Bank",
                    status = "info",
                    "We make use of world bank development indicators and microdata. More Info: https://data.worldbank.org "
                  ),
                  bs4AccordionItem(
                    id = "dhs",
                    title = "USAID - DHS Program",
                    status = "info",
                    "The Demographic and Health Surveys (DHS) Program has collected, analyzed, and disseminated accurate and representative data on population, 
                    health, HIV, and nutrition through more than 300 surveys in over 90 countries. 
                    We make use of some education indicators. More info: https://www.dhsprogram.com/"
                  ),
                  bs4AccordionItem(
                    id = "unesco",
                    title = "UNESCO",
                    status = "info",
                    "UNESCO has a rich source of data for sustainable development goals. 
                    We make use of some indicators on education and literacy. More Info: http://uis.unesco.org"
                  ),
                  bs4AccordionItem(
                    id = "acled",
                    title = "ACLED",
                    status = "info",
                    "The Armed Conflict Location & Event Data Project (ACLED) is a disaggregated conflict analysis and crisis mapping project. ACLED is the highest quality, most widely used, realtime data and analysis source on political violence and protest in the developing world. Practitioners, researchers and governments depend on ACLED for the latest reliable information on current conflict and disorder patterns.
                    More Info: https://www.acleddata.com"
                  ),
                  bs4AccordionItem(
                    id = "inform",
                    title = "INFORM Risk",
                    status = "info",
                    "INFORM is a global, open-source risk assessment for humanitarian crises and disasters. 
                    It can support decisions about prevention, preparedness and response. 
                     More Info: http://www.inform-index.org"
                  )
                  )
              )
            ),
            bs4Card(
              title = "Data API Clients",
              width = 12,
              closable = FALSE,
              footer = tagList(
                bs4Accordion(
                  bs4AccordionItem(
                    id = "dbnomics",
                    title = "DBnomics",
                    status = "info",
                    "We used rdbnomics, which is the R Client for DBNomics. More Info: https://db.nomics.world"
                  ),
                  bs4AccordionItem(
                    id = "rdhs",
                    title = "R Client DHS",
                    status = "info",
                    "We used rdhs, which is a package for management and analysis of Demographic and Health Survey (DHS) data. More Info on Source: https://ojwatson.github.io/rdhs/articles/client.html"
                  )
                  )
                  )
                  )
          )
        ),
        bs4TabItem(
          tabName = "odd4",
          h4("SDG 4 Education"),
          fluidRow(
            bs4Card(
              title = "ODD",
              elevation = 4,
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              billboarderOutput('')

            )
          )
        ),

        bs4TabItem(
          tabName = "démograph",
          h4("Under construction")
          # , fluidRow(
          #   imageOutput("construction")
          # )
        ),
        bs4TabItem(
          tabName = "backlog",
          h4("Under construction")
          ),
        bs4TabItem(
          tabName = "crisis",
          fluidRow(
            bs4Card(
              title = "Crisis and Conflict trends",
              closable = FALSE,
              width = 12,
              #status = "warning",
              solidHeader = FALSE,
              status = "primary",
              #gradientColor = "primary", #"success",
              collapsible = TRUE,
              fluidRow(
                column(6,billboarderOutput("chart_arc_trends")), #column(6,echarts4rOutput("picto_rate")),
                column(6,billboarderOutput("chart_arc_donut")) #column(6,echarts4rOutput("picto_rate_mont"))
              ),
              footer = column(
                width = 12,
                align = "center",
                radioButtons('country', NULL,
                             c("Sahel G5",levels(acled_tiwara$country)),selected = "Sahel G5",inline = TRUE) 
              )# end footer
            )
            
          ),
          fluidRow(
            bs4Card(
              title = "Crisis & Conflict Map",
              elevation = 4,
              closable = FALSE,
              width = 6,
              solidHeader = TRUE,
              status = "warning",
              collapsible = TRUE,
              leafletOutput('map2')
              # ,footer = column(
              #   width = 12,
              #   align = "center",
              #   DTOutput("responses")
              # )
            ),
            bs4Card(
              title = "Fragile Location in terms of fatalities",
              elevation = 4,
              closable = FALSE,
              width = 6,
              solidHeader = TRUE,
              status = "danger",
              collapsible = TRUE,
              DTOutput("responses")
            )
          )
        ),
        bs4TabItem(
          tabName = "inform_risk_data",
          fluidRow(
            column(
              width = 12, offset = 1,
              selectInput('inform_indicator', "Select Risk Indicator",choices = list(
                "Global Risk"="risk",
                "Vulnerability"="vulnerability",
                "Socio Economic Vulnerability"="socio_economic_vulnerability",
                "Inequality"="inequality",
                "Health conditions"="health_conditions",
                "Lack of coping capacity"="lack_of_coping_capacity",
                "Food insecurity probability"="food_insecurity_probability",
                "Physical exposure to flood"="physical_exposure_to_flood",
                "Land degradation"="land_degradation",
                "Droughts impact"="droughts_probability_and_historical_impact",
                "Political violence"="political_violence",
                "Access to health care"="access_to_health_care"), selected = "risk")
             
            ),
            bs4Card(
              title = "Risk Indicators for G5 Sahel",
              elevation = 4,
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = FALSE,
              d3Output('informmap')
              )
            
          )
          
          
        )
            )
              ),

    controlbar = bs4DashControlbar(
      skin = "light",
      title = "Crisis and Fragility Action",
      setSliderColor(sliderId = 1, "black"),
      selectInput('event_type', 'Reported Event Type',
                  c('All',levels(acled_tiwara$event_type))),
      sliderInput('fatalities', label = 'Number of Fatalities', min = min(acled_tiwara$fatalities,na.rm = TRUE), max = max(acled_tiwara$fatalities,na.rm = TRUE), value = c(min,max),
                  step = 1),
      sliderInput("year", "Year", min = min(acled_tiwara$year,na.rm = TRUE), max = max(acled_tiwara$year,na.rm = TRUE), value = c(min,max),step = 1)
    ),

    footer = bs4DashFooter(
      copyrights = "@AFD Data Team",
      right_text = "2018"
    ),
    title = "AFD Tiwara App"
        ),
  server = function(input, output, session) {

### load datasets in reactive mode :)
    dhs_data <- reactive({

      select_country <- input$CountryName

      dhs_data_filtered <- df_dhs

      if (input$CountryName != 'Sahel G5') {
        dhs_data_filtered <- dhs_data_filtered%>% filter(CountryName == select_country)
      }

      dhs_data_filtered


    })

    dhs_data2 <- reactive({

      select_country <- input$CountryName
      select_indicator <- input$Indicator

      dhs_data_filtered <- df_dhs%>%filter(IndicatorId==select_indicator)

      if (input$CountryName != 'Sahel G5') {
        dhs_data_filtered <- dhs_data_filtered%>% filter(CountryName == select_country)%>%filter(Indicator==select_indicator)
      }

      dhs_data_filtered


    })


    arc_analysis_data <- reactive({

      minyear <- input$year[1]
      maxyear <- input$year[2]
      min_fatalities <- input$fatalities[1]
      max_fatalities <- input$fatalities[2]

      arc_data_filtered <- acled_tiwara %>% filter(
        year >= minyear,
        year <= maxyear,
        fatalities >= min_fatalities,
        fatalities <= max_fatalities
      )

      if (input$event_type != 'All') {
        arc_data_filtered <- filter(arc_data_filtered, event_type == input$event_type)
      }
      arc_data_filtered



    })
    
    
    arc_analysis_data2 <- reactive({
      
      select_country <- input$country
      
      arc_data_filtered <- acled_tiwara 
      
      if (input$country != 'Sahel G5') {
        arc_data_filtered <- filter(arc_data_filtered, country == input$country)
      }
      arc_data_filtered
      
      
      
    })
    
    
    output$chart_arc_trends <- renderBillboarder({
      acled <- arc_analysis_data2()
      acled_event_line <- acled %>% 
        dplyr::select(event_type,year,fatalities) %>%
        group_by(year, event_type)%>%
        summarise_all(funs(sum(., na.rm = TRUE)))%>% 
        ungroup()%>%
        spread(event_type, fatalities)%>% 
        arrange(year)
      
      billboarder() %>% 
        bb_linechart(data = acled_event_line, type = "spline") %>% 
        bb_data(x = "year") %>% 
        bb_data(color = htmlwidgets::JS("function(color, d) {return d3.rgb(color).brighter().toString();}")) %>% 
        bb_y_grid(show = TRUE) %>% 
        bb_x_grid(show = TRUE) %>% 
        bb_x_axis(tick = list(fit = FALSE)) %>% 
        # bb_legend(position = "inset") %>% # , inset = list(anchor = "top-right")
        bb_labs(title = "Conflict and Crisis Events", 
                y = "Official Number of Fatalities", 
                caption = "Data source: ACLED")
    })
    
    
    output$chart_arc_donut <- renderBillboarder({
      acled <- arc_analysis_data2()
      acled_event_donut <- acled %>% 
        dplyr::select(event_type,fatalities) %>%
        group_by(event_type)%>%
        summarise_all(funs(sum(., na.rm = TRUE)))%>% 
        arrange(desc(fatalities))%>%
        ungroup()
      
      billboarder() %>% 
        bb_donutchart(data = acled_event_donut) %>% 
        bb_donut(title = "Fatalities") %>%
        bb_labs(title = "Fatality distribution", 
                caption = "Data source: ACLED")
    })
    
    
### Map

    output$map2 <- renderLeaflet({
      acled <- arc_analysis_data()
      Icon <- makeIcon(
        iconUrl = "http://12zc4845uhr73vbfjp3ubgkz.wpengine.netdna-cdn.com/wp-content/themes/MTIRedesign/assets/img/crisisIcon.png",
        iconWidth = 35*215/230, iconHeight = 35,
        iconAnchorX = 35*215/230/2, iconAnchorY = 35) # retrieve AFD logo to add on the map at each Beneficiaire location
      AFDCoords <- data.frame(
        lat = acled$latitude,
        lng = acled$longitude)  # prepare a dataframe with GPS coordinates
      popup <-
        paste0(
          "<br><strong>Fatalities: </strong>", acled$fatalities,
          "<br><strong>Source Notes: </strong>",acled$notes,
          "<br><strong>Location: </strong>",acled$location,
          "<br><strong>Country: </strong>",acled$country,
          "<br><strong>Region: </strong>",acled$region,
          "<br><strong>Source: </strong>",paste("<a href='", "'>", acled$source,"</a>" ,sep = ""),
          "<br><strong>Admin1: </strong>", acled$admin1,
          "<br><strong>Admin2: </strong>", acled$admin2,
          "<br><strong>Admin3: </strong>", acled$admin3
        )

      InteractiveMap <- AFDCoords %>% leaflet() %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions, icon=Icon, popup = popup)
      InteractiveMap
    })

    output$responses <- renderDT({
      acled <- arc_analysis_data()
      acled_tbl <- acled %>%
        select(country,region,location,fatalities)%>%
        arrange(desc(fatalities)) %>% top_n(10)
      DT::datatable(acled_tbl)
      #DT::datatable(acled_tbl, options = list(bPaginate = TRUE), style = 'bootstrap')
    })


###♦ indicatormap
    dhs_data_sf <- reactive({

      select_country <- input$CountryName

      dhs_data_filtered <- df_dhs_sf

      if (input$CountryName != 'Sahel G5') {
        dhs_data_filtered <- dhs_data_filtered%>% filter(CountryName == select_country)
      }

      dhs_data_filtered


    })

    # output$indicatormap <- renderLeaflet({
    #   mapdata <- dhs_data_sf() %>%filter(IndicatorId==input$Indicator)
    #
    #   #AFDCoords = mapdata$Coordinates
    #   popup <-
    #     paste0(
    #       "<br><strong>Indicator: </strong>", mapdata$Indicator,
    #       "<br><strong>Value: </strong>",mapdata$Value,
    #       "<br><strong>Location: </strong>",mapdata$CharacteristicLabel,
    #       "<br><strong>Country: </strong>",mapdata$CountryName
    #     )
    #
    #   InteractiveMap <- leaflet() %>%
    #     addTiles() %>%
    #     addProviderTiles("OpenStreetMap")%>%
    #     addFeatures(mapdata, weight = 1, fillColor = "Reds")
    #   InteractiveMap
    #
    #   # mapdata%>%mapview(zcol='Value', legend=TRUE)
    # })

###
    output$gplt4a <- renderValueBox({
      dhs_sub <- dhs_data()
      dhs_sub %>%
        select(IndicatorId,Value)%>%
        group_by(IndicatorId)%>%
        summarise(average_Value = max(Value[Value != 0], na.rm = TRUE))%>%
        ungroup()#%>% filter(IndicatorId=='ED_EDAT_B_MYR')
    })



###
    output$barplot <- renderBillboarder({
      dhs_sub <- dhs_data()

      dhs_sub  %>% select(location,Indicator,IndicatorId,Value) %>% filter(IndicatorId %in% variables) %>% group_by(location,Indicator,IndicatorId)%>%summarise_all(funs(mean(.[. != 0], na.rm = TRUE))) %>%ungroup()%>%filter(Indicator==input$Indicator) %>% select(-Indicator,-IndicatorId) %>% arrange(desc(Value)) %>% top_n(-10) -> hehe  # bottom 10

      billboarder() %>%
        bb_barchart(data = hehe) %>%
        bb_data(labels = FALSE) %>%
        bb_y_grid(show = TRUE) %>%
        bb_legend(position = "right") %>%
        bb_labs(title = "Indicator against subnational (bottom 10)",
                y = "Value",
                caption = "Data source: DHS")
    })

###
    output$barplot_lol <- renderBillboarder({
      dhs_sub <- dhs_data()

      dhs_sub  %>% select(location,Indicator,IndicatorId,Value) %>%
        filter(IndicatorId %in% variables) %>%
        group_by(location,Indicator,IndicatorId)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE))) %>%
        ungroup()%>%filter(Indicator==input$Indicator) %>%
        select(-Indicator,-IndicatorId) %>%
        arrange(desc(Value)) %>%
        top_n(-input$vuls) -> hehe  # bottom 10

      billboarder() %>%
        bb_lollipop(data = arrange(hehe, -desc(Value)), rotated = FALSE)%>%
        bb_y_grid(show = TRUE) %>%
        bb_x_axis(tick = list(centered = TRUE)) %>%
        bb_labs(
          #title = "Indicator against subnational",
          caption = "Data source: DHS program"
        )
    })


    ###
    output$radarchart <- renderBillboarder({
      dhs_sub <- dhs_data()

      df_dhs %>% select(CountryName,Indicator,IndicatorId,Value) %>% filter(IndicatorId %in% variables) %>% group_by(CountryName,Indicator,IndicatorId)%>%summarise_all(funs(mean(.[. != 0], na.rm = TRUE))) ->hehehe

      billboarder() %>%
        bb_radarchart(
          data = hehehe,
          mapping = bbaes(x = Indicator, y = Value, group = CountryName)
        )%>%
        bb_labs(#title = "Indicator by location",
                y = "Value")
    })


    ###
    output$gplt_w_literacy <- renderBillboarder({

      dhs_sub <- dhs_data()

      df_prim <- dhs_sub  %>%
        select(Indicator,IndicatorId,Value) %>%
        filter(IndicatorId %in% variables) %>%
        group_by(Indicator,IndicatorId)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))

      df_prim%>%filter(IndicatorId=="ED_LITR_W_LIT") ->prim

      billboarder() %>%
        bb_gaugechart(
          value = round(prim$Value,2),
          name = "Average Literacy Women",
          steps_color = (c("#FF0000", "#F97600", "#F6C600", "#50B044")) #rev() #see color-hex.com
        ) %>% bb_gauge(
          label = list(format = suffix("%")),units = "Women Literacy Rate", width = 10,
          min = 0, max = 100, label = list(format = htmlwidgets::JS("function(value) {return value;}"))
        )
    })

    output$gplt_m_literacy <- renderBillboarder({

      dhs_sub <- dhs_data()

      df_seco <- dhs_sub  %>%
        select(Indicator,IndicatorId,Value) %>%
        filter(IndicatorId %in% variables) %>%
        group_by(Indicator,IndicatorId)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))

      df_seco%>%filter(IndicatorId=="ED_LITR_M_LIT") ->seco

      billboarder() %>%
        bb_gaugechart(
          value = round(seco$Value,2),
          name = "Average Literacy Men",
          steps_color = (c("#FF0000", "#F97600", "#F6C600", "#50B044"))  #see color-hex.com
        ) %>% bb_gauge(
          label = list(format = suffix("%")),units = "Men Literacy Rate", width = 10,
          min = 0, max = 100, label = list(format = htmlwidgets::JS("function(value) {return value;}"))
        )
    })

    output$gplt_seco <- renderBillboarder({

      dhs_sub <- dhs_data()

      df_seco <- dhs_sub  %>%
        select(Indicator,IndicatorId,Value) %>%
        filter(IndicatorId %in% variables) %>%
        group_by(Indicator,IndicatorId)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))

      df_seco%>%filter(IndicatorId=="ED_GARS_B_BTH") ->seco

      billboarder() %>%
        bb_gaugechart(
          value = round(seco$Value,2),
          name = "Secondary school attendance rate",
          steps_color = (c("#FF0000", "#F97600", "#F6C600", "#50B044"))  #see color-hex.com
        ) %>% bb_gauge(
          label = list(format = suffix("%")),units = "Secondary school", width = 10,
          min = 0, max = 100, label = list(format = htmlwidgets::JS("function(value) {return value;}"))
        )
    })

    output$gplt_prim <- renderBillboarder({

      dhs_sub <- dhs_data()

      df_seco <- dhs_sub  %>%
        select(Indicator,IndicatorId,Value) %>%
        filter(IndicatorId %in% variables) %>%
        group_by(Indicator,IndicatorId)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))

      df_seco%>%filter(IndicatorId=="ED_GARP_B_BTH") ->seco

      billboarder() %>%
        bb_gaugechart(
          value = round(seco$Value,2),
          name = "Primary school attendance rate",
          steps_color = (c("#FF0000", "#F97600", "#F6C600", "#50B044"))  #see color-hex.com
        ) %>% bb_gauge(
          label = list(format = suffix("%")),units = "Primary school", width = 10,
          min = 0, max = 100, label = list(format = htmlwidgets::JS("function(value) {return value;}"))
        )
    })
    ###




    output$bubble <- renderPlotly({

      dhs_sub <- dhs_data()
      bubble <- dhs_sub %>%
        select(IndicatorId,CountryName, location,Value)%>%
        group_by(IndicatorId,CountryName, location)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%
        ungroup()%>%spread(IndicatorId, Value)

       literacyRate <- switch(input$literacy,
                     'ED_LITR_W_LIT' = bubble$ED_LITR_W_LIT,
                     'ED_LITR_M_LIT' = bubble$ED_LITR_M_LIT,
                     'ED_LITR_W_LIT' = bubble$ED_LITR_W_LIT)
      
      if (sum(!is.na(bubble$ED_GARP_B_GPI | bubble$ED_GARS_B_GPI | bubble$ED_LITR_W_LIT | bubble$ED_LITR_M_LIT ))==0)
        plotly_empty() else 
      plot_ly(bubble, x = ~ED_GARP_B_GPI, y = ~ED_GARS_B_GPI,
              text = ~paste('Location:', location, '<br>Country Name:', CountryName,
                            '<br>Women who are literate:', literacyRate),
              mode = "markers", color = ~CountryName, size = ~literacyRate,
              marker = list(symbol = 'circle', sizemode = 'diameter',
                    line = list(width = 2, color = '#FFFFFF'))) %>%
        layout(#title = 'Gross Parity Index: Primary v. Secondary school attendance',
               xaxis = list(title = 'Primary school attendance'),
               yaxis = list(title = 'Secondary school attendance'),
               plot_bgcolor = 'rgb(243, 243, 243)')  # parity index Primary vs secondary. size is women literate, color is country

    })


    ### ODD

    # dhs_data <- reactive({
    #
    #   select_country <- input$CountryName
    #
    #   dhs_data_filtered <- df_dhs
    #
    #   if (input$CountryName != 'Sahel G5') {
    #     dhs_data_filtered <- dhs_data_filtered%>% filter(CountryName == select_country)
    #   }
    #
    #   dhs_data_filtered
    #
    #
    # })

    # output$enrol_funnel <- renderBillboarder({
    #
    #
    #   school_enrol_funnel%>%filter(country==input$CountryName)%>%
    #     select(country_label,period,indicator_label,value)%>%
    #     spread(indicator_label,value) %>% select(-country_label)%>%
    #     mutate(year=year(period))%>% filter(between(year,2010,2017))%>%
    #     select(-period, -year)%>%summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%
    #     gather(Indicator)->hehehe
    #
    #
    #
    #   billboarder() %>%
    #     bb_lollipop(hehehe, rotated = TRUE)%>%
    #     bb_y_grid(show = TRUE) %>%
    #     bb_x_axis(tick = list(centered = TRUE)) %>%
    #     bb_labs(
    #       #title = "Indicator against subnational",
    #       caption = "Data source: UNESCO Institute for Statistics"
    #     )
    #
    # })

    ###
    output$bubble_lol <- renderBillboarder({
      dhs_sub <- dhs_data()
      bubble <- dhs_sub %>%
        select(IndicatorId,CountryName, location,Value)%>%
        group_by(IndicatorId,CountryName, location)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%
        ungroup()%>%spread(IndicatorId, Value)

      billboarder() %>%
        bb_scatterplot(
          data = bubble,
          mapping = bbaes(ED_GARP_B_GPI, ED_GARS_B_GPI, group = CountryName)
        ) %>%
        bb_x_axis(tick = list(fit = FALSE))

    })
    

    # output$picto_rate <- renderEcharts4r({
    # 
    #   hehe <- dhs_data()%>%filter(IndicatorId %in%c('ED_LITR_W_LIT','ED_LITR_M_LIT'))
    # 
    #   path <- "path://M0,10 L10,10 C5.5,10 5.5,5 5,0 C4.5,5 4.5,10 0,10 z"
    # 
    #   style <- list(
    #     normal = list(opacity = 0.5), # normal
    #     emphasis = list(opacity = 1) # on hover
    #   )
    # 
    #   hehe %>%
    #     e_charts(Indicator) %>%
    #     e_pictorial(Value, symbol = path,name = "Literacy Rate",
    #                 barCategoryGap = "-10%",
    #                 itemStyle = style) %>%
    #     e_legend(FALSE)%>%
    #     e_tooltip()%>%
    #     e_title("")
    # 
    # })
    # 
    # output$picto_rate_mont <- renderEcharts4r({
    # 
    #   hehe <- dhs_data()%>%filter(IndicatorId %in%c('ED_LITR_W_LIT','ED_LITR_M_LIT'))
    # 
    #   qomo <- paste0(
    #     "https://ecomfe.github.io/echarts-examples/public/",
    #     "data/asset/img/hill-Qomolangma.png"
    #   )
    # 
    #   kili <- paste0(
    #     "https://ecomfe.github.io/echarts-examples/public/",
    #     "data/asset/img/hill-Kilimanjaro.png"
    #   )
    # 
    #   hehe%>%mutate(images= case_when(IndicatorId=='ED_LITR_W_LIT' ~ paste0("image://", qomo), IndicatorId=='ED_LITR_M_LIT' ~ paste0("image://", kili))) -> hehehe
    # 
    #   style <- list(
    #     normal = list(opacity = 0.5), # normal
    #     emphasis = list(opacity = 1) # on hover
    #   )
    # 
    #   hehehe %>%
    #     e_charts(Indicator) %>%
    #     e_pictorial(Value, images, name = "Literacy Rate") %>%
    #     e_legend(FALSE) %>%
    #     e_tooltip()%>%
    #     #e_theme("westeros")%>%
    #     e_title("")
    # 
    # })
    
    ## ODD
    enrol_funnel_reat <- reactive({
      
      select_country <- input$CountryName
      
      enrol_funnel_filtered <- school_enrol_funnel
      
      if (input$CountryName != 'Sahel G5') {
        enrol_funnel_filtered <- enrol_funnel_filtered%>% filter(country_label == select_country)
      }
      
      enrol_funnel_filtered
      
      
    })
    
    gend_parity_reat <- reactive({
      
      select_country <- input$CountryName
      
      gend_parity_filtered <- gend_parity
      
      if (input$CountryName != 'Sahel G5') {
        gend_parity_filtered <- gend_parity_filtered%>% filter(country_label == select_country)
      }
      
      gend_parity_filtered
      
      
    })
    
    completion_by_gend_income_reat <- reactive({
      
      select_country <- input$CountryName
      
      completion_by_gend_income_filtered <- completion_by_gend_income
      
      if (input$CountryName != 'Sahel G5') {
        completion_by_gend_income_filtered <- completion_by_gend_income_filtered%>% filter(country_label == select_country)
      }
      
      completion_by_gend_income_filtered
      
      
    })
    
    
    pupil_teacher_ratio_reat <- reactive({
      
      select_country <- input$CountryName
      
      pupil_teacher_ratio_filtered <- pupil_teacher_ratio
      
      if (input$CountryName != 'Sahel G5') {
        pupil_teacher_ratio_filtered <- pupil_teacher_ratio_filtered%>% filter(country_label == select_country)
      }
      
      pupil_teacher_ratio_filtered
      
      
    })
    
    
    out_of_school_primary_reat <- reactive({
      
      select_country <- input$CountryName
      
      out_of_school_primary_filtered <- out_of_school_primary
      
      if (input$CountryName != 'Sahel G5') {
        out_of_school_primary_filtered <- out_of_school_primary_filtered%>% filter(country_label == select_country)
      }
      
      out_of_school_primary_filtered
      
      
    })
    
    
    output$out_of_school_primary_plt <- renderBillboarder({
      
      out_of_school_primary_reat()%>%
        filter(indicator %in% c('SE.PRM.UNER.MA.ZS','SE.PRM.UNER.FE.ZS'))%>%
        select(country_label,period,indicator_label,value)%>%spread(indicator_label,value) %>% 
        select(-country_label) %>%
        rename("Female (% of female primary school age)"="Children out of school, female (% of female primary school age)",
               "Male (% of male primary school age)"="Children out of school, male (% of male primary school age)")->hehe
      
      hehe_out <- hehe
      if (input$CountryName == 'Sahel G5') {
        hehe_out <- hehe%>% group_by(period)%>%summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%ungroup()
      }
      
      billboarder() %>% 
        bb_linechart(
          data = hehe_out, 
          type = "spline"
        ) %>%   
        bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE))%>%
        bb_y_axis(tick = list(format = suffix("%"))) %>%
        bb_labs(
          #title = "Indicator against subnational",
          caption = "Data source: UNESCO Institute for Statistics"
        )
      
      
    })
    
    output$prim_completion_by_gend_income_plt <- renderBillboarder({
      
      completion_by_gend_income_reat()%>%
        filter(indicator %in% c('SE.PRM.CMPT.FE.ZS','SE.PRM.CMPT.MA.ZS'))%>%
        select(country_label,period,indicator_label,value)%>%spread(indicator_label,value) %>% 
        select(-country_label)%>%
        rename("Female (% of relevant age group)"="Primary completion rate, female (% of relevant age group)",
               "Male (% of relevant age group)"="Primary completion rate, male (% of relevant age group)")->hehe
     
      hehe_out <- hehe
      if (input$CountryName == 'Sahel G5') {
        hehe_out <- hehe%>% group_by(period)%>%summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%ungroup()
      }
      
      billboarder() %>% 
        bb_linechart(
          data = hehe_out, 
          type = "spline"
        ) %>%   
        bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE))%>%
        bb_y_axis(tick = list(format = suffix("%"))) %>%
        bb_data(names =list("value"="Primary completion rate (% of relevant age group)"))%>%
        bb_labs(
          #title = "Indicator against subnational",
          caption = "Data source: UNESCO Institute for Statistics"
        )
    
      
    })
    
    output$seco_completion_by_gend_income_plt <- renderBillboarder({
      
      completion_by_gend_income_reat()%>%
        filter(indicator %in% c('SE.SEC.CMPT.LO.FE.ZS','SE.SEC.CMPT.LO.MA.ZS'))%>%
        select(country_label,period,indicator_label,value)%>%spread(indicator_label,value) %>% 
        select(-country_label)%>%
        rename("Female (% of relevant age group)"="Lower secondary completion rate, female (% of relevant age group)",
               "Male (% of relevant age group)"="Lower secondary completion rate, male (% of relevant age group)")->hehe
      
      hehe_out <- hehe
      if (input$CountryName == 'Sahel G5') {
        hehe_out <- hehe%>% group_by(period)%>%summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%ungroup()
      }
      
      billboarder() %>% 
        bb_linechart(
          data = hehe_out, 
          type = "spline"
        ) %>%   
        bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE))%>%
        bb_y_axis(tick = list(format = suffix("%"))) %>%
        bb_data(names =list("value"="Lower secondary completion rate (% of relevant age group)"))%>%
        bb_labs(
          #title = "Indicator against subnational",
          #y = "(% of relevant age group)",
          caption = "Data source: UNESCO Institute for Statistics"
        )
      
      
    })
    
    
    output$gend_parity_plt <- renderBillboarder({
      
      minyear <- input$period[1]
      maxyear <- input$period[2]
      
      gend_parity_reat()%>%
        select(country_label,period,indicator_label,value)%>%
        spread(indicator_label,value) %>% select(-country_label)%>%
        mutate(year=year(period))%>%
        filter(between(year,minyear,maxyear))%>%select(-period, -year)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%
        gather(Indicator)%>%mutate(Indicator = case_when(Indicator=='School enrollment, primary (gross), gender parity index (GPI)' ~ 'Primary', 
                                                         Indicator=='School enrollment, secondary (gross), gender parity index (GPI)' ~ 'Secondary', 
                                                         Indicator =='School enrollment, tertiary (gross), gender parity index (GPI)' ~'Tertiary'))%>%
        mutate(value=round(value,2)) ->hehehe
      
      billboarder() %>%
        bb_barchart(hehehe, rotated = TRUE)%>%
        #bb_lollipop(hehehe, rotated = TRUE)%>%
        bb_data(names =list("value"="Gender Parity Index (GPI) in school enrollment (gross)"))%>%
        #bb_legend(show=FALSE)%>%
        bb_y_grid(show = TRUE) %>%
        bb_x_axis(tick = list(centered = TRUE)) %>%
        bb_labs(
          title = "Gender Parity Index",
          caption = " "
        )
      
    })
    
    output$enrol_funnel_plt <- renderBillboarder({

      minyear <- input$period[1]
      maxyear <- input$period[2]
      
      enrol_funnel_reat()%>%
        select(country_label,period,indicator_label,value)%>%
        spread(indicator_label,value) %>% select(-country_label)%>%
        mutate(year=year(period))%>%
        filter(between(year,minyear,maxyear))%>%select(-period, -year)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%
        gather(Indicator)%>%mutate(Indicator = case_when(Indicator=='School enrollment, primary (% gross)' ~ 'Primary', 
                                                         Indicator=='School enrollment, secondary (% gross)' ~ 'Secondary', 
                                                         Indicator =='School enrollment, tertiary (% gross)' ~'Tertiary'))%>%
        mutate(value=round(value,2)) ->hehehe
      
      billboarder() %>%
        bb_barchart(hehehe, rotated = TRUE)%>%
        #bb_lollipop(hehehe, rotated = TRUE)%>%
        bb_data(names =list("value"="School enrollment (% gross)"))%>%
       # bb_legend(show=FALSE)%>%
        bb_y_grid(show = TRUE) %>%
        bb_x_axis(tick = list(centered = TRUE)) %>%
        bb_y_axis(tick = list(format = suffix("%"))) %>%
        bb_labs(
          title = "School enrollment",
          y = "% gross",
          caption = "Data source: UNESCO Institute for Statistics"
        )
      
    })

    output$treemap <- renderEcharts4r({

      dhs_sub <- dhs_data()

      dhs_sub  %>%
        filter(Indicator==input$Indicator)%>%
        e_charts() %>%
        e_treemap(CountryName, location, Value)

    })

    #----------------- Map
    
    output$mymap <- renderD3({
      mapdata <- dhs_data_sf()%>%select(input$varia, CharacteristicLabel)
     # # # -----working-------
     #  d3_map(shape = mapdata) %>%
     #    add_continuous_breaks(var = input$varia,palette = input$palette) %>%
     #    add_tooltip(value = sprintf("<b>{CharacteristicLabel}</b>: {round(%s, 1)}%%", input$varia))%>%
     #    add_legend(title = "Value of Indicator", suffix = "")
     # #  # ------------
      
 
      
      
      if (sum(!is.na(mapdata[[input$varia]])) == 0) {
        he <- d3_map(shape = mapdata)%>%
          add_tooltip(value = sprintf("<b>{CharacteristicLabel}</b>: {round(%s, 1)}", input$varia))
      }
       else {
         he <- d3_map(shape = mapdata) %>%
           add_continuous_breaks(var = input$varia,palette = input$palette) %>%
           add_tooltip(value = sprintf("<b>{CharacteristicLabel}</b>: {round(%s, 1)}", input$varia))%>%
           add_legend(title = "Value of Indicator", suffix = "")

       }
     he
      
      
    })
    
    
    
    sahel_tiwara_geo_reat <- reactive({
      
      select_indicator <- input$inform_indicator
      #select_country <- input$CountryName
      
      sahel_tiwara_geo_filtered <- sahel_tiwara_geo%>% select(select_indicator,name_fr,name,admin)
      
      # if (input$CountryName != 'Sahel G5') {
      #   sahel_tiwara_geo_filtered <- sahel_tiwara_geo_filtered%>% filter(admin == select_country)
      # }
      
      sahel_tiwara_geo_filtered
      
      
    })
    
    
    
    
    
    output$informmap <- renderD3({
      mapdata <- sahel_tiwara_geo_reat()
       d3_map(shape = mapdata) %>%
          add_continuous_breaks(var = input$inform_indicator, palette = "Reds") %>%
          add_tooltip(value = sprintf("<b>{name_fr}</b>: {round(%s, 1)}", input$inform_indicator))%>%
          add_legend(title = "Range", suffix = "")
      
    })
    
    
    image <- image_read("./images/construction2.gif")
    
    output$construction <- renderImage({
      # Numeric operators
      tmpfile <- image%>%
        image_write(tempfile(fileext='gif'), format = 'gif')
      # Return a list
      list(src = tmpfile, contentType = "image/gif")
    })
    
    
    
    output$ibox1 <- renderbs4InfoBox({
      no_events <- acled_tiwara %>%distinct(data_id) %>% tally()
      bs4InfoBox(
        title = "Number of ACLED Events ",
        width = 3,
        value = no_events,
        status = "info",
        icon = "newspaper"
      )
    })
    
  output$ibox2 <- renderbs4InfoBox({
    repo_fatalities <-sum(acled_tiwara$fatalities,na.rm = TRUE)
    bs4InfoBox(
      title = "Reported Fatalities",
      status = "info",
      width = 3,
      value = repo_fatalities,
      gradientColor = ifelse(repo_fatalities > 10, "danger", "warning"),
      icon = "bullhorn"
    )
    })
      
  output$ibox3 <- renderbs4InfoBox({
    no_violence <- acled_tiwara %>% filter(event_type == 'Violence against civilians')%>%tally()
    bs4InfoBox(
      title = "Number of Violence against civilians",
      gradientColor = "warning",
      width = 3,
      value = no_violence,
      icon = "ambulance" #icon = "sliders"
    )
    })
    
    
    output$primary_pupil_teacher_ratio_reat_vbox <- renderbs4ValueBox({
      minyear <- input$period[1]
      maxyear <- input$period[2]
      
      pupil_teacher_ratio_reat()%>%
        select(country_label,period,indicator_label,value)%>%
        spread(indicator_label,value) %>% select(-country_label)%>%
        mutate(year=year(period))%>%
        filter(between(year,minyear,maxyear))%>%select(-period, -year)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%
        gather(Indicator)%>%
        filter(Indicator == 'Pupil-teacher ratio, primary')%>%
        select(value) ->primary_pt_ratio
      
      bs4ValueBox(
        value = round(primary_pt_ratio,digits = 0), #(gplt4a %>%filter(IndicatorId=='ED_EDAT_B_MYR'))$average_Value,
        subtitle = "Primary pupil-teacher ratio",
        status = ifelse(round(primary_pt_ratio,2) > 40, "danger", ifelse(round(primary_pt_ratio,2)> 20, "warning", "primary")),
        icon = "chalkboard-teacher",
        href = "#"  #http://uis.unesco.org/en/glossary
      )
      
    })
    
    
    output$lower_sec_pupil_teacher_ratio_reat_vbox <- renderbs4ValueBox({
      minyear <- input$period[1]
      maxyear <- input$period[2]
      
      pupil_teacher_ratio_reat()%>%
        select(country_label,period,indicator_label,value)%>%
        spread(indicator_label,value) %>% select(-country_label)%>%
        mutate(year=year(period))%>%
        filter(between(year,minyear,maxyear))%>%select(-period, -year)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%
        gather(Indicator)%>%
        filter(Indicator == 'Pupil-teacher ratio, lower secondary')%>%
        select(value) ->lower_seco_pt_ratio
      
      bs4ValueBox(
        value = round(lower_seco_pt_ratio,digits = 0), #(gplt4a %>%filter(IndicatorId=='ED_EDAT_B_MYR'))$average_Value,
        subtitle = "Lower Secondary pupil-teacher ratio",
        status = ifelse(round(lower_seco_pt_ratio,2) > 40, "danger", ifelse(round(lower_seco_pt_ratio,2)> 20, "warning", "primary")),
        icon = "users",
        href = "#"
      )
      
    })
    
    output$upper_sec_pupil_teacher_ratio_reat_vbox <- renderbs4ValueBox({
      minyear <- input$period[1]
      maxyear <- input$period[2]
      
      pupil_teacher_ratio_reat()%>%
        select(country_label,period,indicator_label,value)%>%
        spread(indicator_label,value) %>% select(-country_label)%>%
        mutate(year=year(period))%>%
        filter(between(year,minyear,maxyear))%>%select(-period, -year)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%
        gather(Indicator)%>%
        filter(Indicator == 'Pupil-teacher ratio, upper secondary')%>%
        select(value) ->upper_seco_pt_ratio
      
      bs4ValueBox(
        value = round(upper_seco_pt_ratio,digits = 0), #(gplt4a %>%filter(IndicatorId=='ED_EDAT_B_MYR'))$average_Value,
        subtitle = "Upper Secondary pupil-teacher ratio",
        status = ifelse(round(upper_seco_pt_ratio,2) > 40, "danger", ifelse(round(upper_seco_pt_ratio,2)> 20, "warning", "primary")),
        icon = "users-cog"
      )
      
    })
  



  }
          )
