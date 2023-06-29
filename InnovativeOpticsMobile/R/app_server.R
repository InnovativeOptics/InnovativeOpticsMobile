#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import dplyr
#' @import purrr
#' @noRd
our_data <- readxl::read_excel("data/Master_2.xlsx",
                               sheet = "Lens_details") %>%
  mutate(VLT = scales::percent(as.numeric(VLT)),
         `Price(from)` = scales::dollar(as.numeric(`Price(from)`)))
oem_data <- read.csv("data/oemDataSearch1.csv")
search_data <- read.csv("data/wl_search_data_full.csv") %>%
  select(2:4)

app_server <- function(input, output, session) {
  # Your application server logic
  fullData <- eventReactive(as.numeric(input$wl) | as.numeric(input$od), {
    req(as.numeric(input$wl) >= 200 & as.numeric(input$wl) <= 11000)
    fullData <- search_data %>%
      filter(Wavelength == round(as.numeric(input$wl), digits=0),
             OD >= as.numeric(input$od)) %>%
      arrange(desc(OD))
    req(as.numeric(input$od) <= max(fullData$OD))
    updateNumericInput(inputId = "od",max = max(fullData$OD))
    map(unique(fullData$Lens), ~tibble(filter(our_data, Lens == .x)))
  })
  output$links <- renderUI({
    req(fullData())
    map(1:length(fullData()), ~f7Shadow(intensity = 5,
          f7Card(
      f7Row(
        f7Col(f7Link(
      href = fullData()[[.x]]$Website,
      label = h3(style = "
              text-shadow: 1px 1px 1px #ababab;",
                 glue::glue_safe(fullData()[[.x]]$Lens, " - Click here to shop"))
    )),
    f7Col(img(src = fullData()[[.x]]$Image, width = "144px"))),
    f7Card(
      f7Align(side = 'center',
              img(src = fullData()[[.x]]$Graph, width = "100%"))),
    f7Table(card = T,
      {tibble("Qualities/Specifications" = c("Optical Density", "% VLT", "Price (from)"),
        "Values" = c(fullData()[[.x]]$OD,
                    scales::percent(as.numeric(fullData()[[.x]]$VLT)),
                    scales::dollar(fullData()[[.x]]$`Price(from)`)))
    }))
    ))
  })
  observeEvent(input$mfg, {
    # filter oem data to select mfg
    mfg_filtered_oem_data <- oem_data %>%
      filter(`Mfg` == input$mfg)
    # update select input - laser model
    updateF7SmartSelect(inputId = "mod",
                   choices = sort(unique(mfg_filtered_oem_data$`Mod`)))
  })
  output$mod <- renderUI({
    req(input$mfg)
    mfg_filtered_oem_data <- oem_data %>%
      filter(`Mfg` == input$mfg)
    f7Shadow(intensity = 3,f7Card(f7SmartSelect(inputId = "modui",
                  label = h2(strong("Model")),
                  choices = sort(unique(mfg_filtered_oem_data$`Mod`)),
                  openIn = "popup")))
  })
  selected_data_oem <- eventReactive(input$modui,{
    req(input$mfg)
    oem_data %>%
      filter(`Mfg` == input$mfg,
             `Mod` == input$modui)
  })
  selected_data <- eventReactive(input$modui,{
    req(input$mfg)
    map(unique(selected_data_oem()$Lens), ~tibble(filter(our_data, Lens == .x)))
  })
  output$laser_specs <- renderUI({
      f7Table(card = T,
        select(selected_data_oem(), `Eyewear.Requirement`) %>%
              unique() %>%
              mutate("Selected Laser Specifications (nm)" = Eyewear.Requirement, .keep = "none") %>%
              tibble())
  })
  output$linksOEM <- renderUI({
    req(selected_data())
    map(1:length(selected_data()), ~f7Shadow(intensity = 5,
          f7Card(
      f7Row(
        f7Col(
          f7Link(
        href = selected_data()[[.x]]$Website,
        label = h3(style = "
              text-shadow: 1px 1px 1px #ababab;",
          glue::glue_safe(selected_data()[[.x]]$Lens, " - Click here to shop"))
      )),
      f7Col(img(src =selected_data()[[.x]]$Image, width = "144px"))),
      f7Card(
        f7Align(side = 'center',
        img(src = selected_data()[[.x]]$Graph, width = "100%"))),
      f7Table(card =T,
              tibble("Qualities/Specifications" = c("Optical Density", "% VLT", "Price (from)"),
                     "Values" = c(selected_data()[[.x]]$OD,
                                  scales::percent(as.numeric(selected_data()[[.x]]$VLT)),
                                  scales::dollar(selected_data()[[.x]]$`Price(from)`)))
      )
      )
      )
      )

  })


}
