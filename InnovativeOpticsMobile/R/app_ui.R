#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import dplyr
#' @import purrr
#' @noRd
oem_data <- read.csv("data/oemDataSearch1.csv")
our_data <- readxl::read_excel("data/Master_2.xlsx",
                               sheet = "Lens_details")
web_data <- read.csv("data/invo_products_6.19.23.csv")
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    f7Page(
      options = list(
        dark = F,
        color = "#00a033",
        filled = F
      ),
      title = "Innovative Optics Mobile",
      allowPWA = TRUE,
      f7TabLayout(
        navbar = f7Navbar(
          title = h1(style =
                       {
                         "color: darkblue;
                         text-shadow: 3px 3px 3px #ababab;"
                       },
                     "Innovative Optics"),
          shadow = T,
          hairline = T,
          bigger = F
        ),
        f7Tabs(
          style = "toolbar",
          swipeable = TRUE,
          animated = F,
          f7Tab(
            active = T,
            tabName = "Home",
            icon = f7Icon("house"),
            f7Row(img(src = "flair/physician_patient.jpg",
                      width = "100%")),
            f7Block(
              strong = T,
              inset = T,
              f7Align(
                side = 'center',
                h1(style =
                     "color: #ec323b;
              text-shadow: 2px 2px 2px #ababab;",
                   "Deals")
              )
            ),
            f7Swiper(id = "Deals",
                     f7Slide(f7Shadow(
                       intensity = 7,
                       f7Card(
                         title = f7Link(
                           label = h3(
                             style = "
              text-shadow: 1px 1px 1px #ababab;",
                             "PTCE-III Patient Goggle"
                           ),
                           href = "https://innovativeoptics.com/product/ptceiii-blue-adjustable-patient-goggle/"
                         ),
                         footer = tagList(
                           f7Badge("BOGO", color = "red"),
                           f7Badge("Patient Protection", color = "black")
                         ),
                         img(src = "https://innovativeoptics.com/wp-content/uploads/2019/07/PTCEIII-Blue-NEW-400x400.jpg", width = "100%")
                       )
                     )),
                     f7Slide(f7Shadow(
                       intensity = 7,
                       f7Card(
                         title = f7Link(
                           label = h3(
                             style = "
              text-shadow: 1px 1px 1px #ababab;",
                             "BBCE Patient Goggles"
                           ),
                           href = "https://innovativeoptics.com/product/bbce/"
                         ),
                         footer = tagList(
                           f7Badge("BOGO", color = "red"),
                           f7Badge("Patient Protection", color = "black")
                         ),
                         img(src = "https://innovativeoptics.com/wp-content/uploads/2021/04/BBCE-white-silicone-straps.jpg", width = "100%")
                       )
                     ))),
            f7Block(
              strong = T,
              inset = T,
              f7Align(
                side = 'center',
                h1(style =
                     "color: #ec323b;
              text-shadow: 2px 2px 2px #ababab;",
                   "Featured Items")
              )
            ),
            f7Swiper(id = "Featured",
                     f7Slide(f7Shadow(
                       intensity = 7,
                       f7Card(
                         title = f7Link(
                           label = h3(
                             style = "
              text-shadow: 1px 1px 1px #ababab;",
                             "Renegade Loupe Insert"
                           ),
                           href = "https://innovativeoptics.com/laser-inserts/"
                         ),
                         footer = tagList(
                           f7Badge("Insert", color = "red"),
                           f7Badge("Loupe Compatible", color = "black"),
                           f7Badge("Laser", color = "yellow")
                         ),
                         img(src = "https://innovativeoptics.com/wp-content/uploads/2022/04/Renegade-Pi10-Lens.png", width = "100%")
                       )
                     )),
                     f7Slide(f7Shadow(
                       intensity = 7,
                       f7Card(
                         title = f7Link(
                           label = h3(style = "
              text-shadow: 1px 1px 1px #ababab;",
                                      "Primo Loupe Insert"),
                           href = "https://innovativeoptics.com/primo-laser-inserts/"
                         ),
                         footer = tagList(
                           f7Badge("Insert", color = "red"),
                           f7Badge("Loupe Compatible", color = "black"),
                           f7Badge("Laser", color = "yellow")
                         ),
                         img(src = "https://innovativeoptics.com/wp-content/uploads/2020/11/banner-primo-2.png", width = "100%")
                       )
                     ))),
            f7Block(
              strong = T,
              inset = T,
              f7Align(
                side = 'center',
                h1(style =
                     "color: #ec323b;
              text-shadow: 2px 2px 2px #ababab;",
                   "Advertisements")
              )
            ),
            f7Block(f7Row(
              img(src = "icons/INVO_978.png",
                  width = "72px")
            )),
          ),
          f7Tab(
            tabName = "Search",
            icon = f7Icon("search"),
            img(src = "flair/cool_guy.jpg",
                width = "100%"),
            f7Block(
              h1("Use this tab to search for compatible lenses"),
              h1(
                "Have an LSO confirm the specifications of our lenses
                   are sufficient for your application"
              ),
              h1("Contact us if you have any questions/comments")
            ),
            f7Accordion(
              f7AccordionItem(
                title = h1(
                  style = "color: #ec323b;
              text-shadow: 2px 2px 2px #ababab;",
                  strong("Search by Laser Type")
                ),
                f7Shadow(intensity = 3,
                         f7Card(
                           f7SmartSelect(
                             inputId = "mfg",
                             label = h2(strong("Manufacturer")),
                             choices = sort(unique(oem_data$`Mfg`)),
                             openIn = "popup",
                             virtualList = TRUE
                           )
                         )),
                uiOutput(outputId = "mod"),
                h1(uiOutput("laser_specs")),
                uiOutput("linksOEM")
              ),
              f7AccordionItem(
                title =
                  h1(
                    style = "color: #ec323b;
              text-shadow: 2px 2px 2px #ababab;",
                    strong("Search by Wavelength")
                  ),
                f7Block(f7Row(f7Col(
                  f7Shadow(intensity = 3,
                           f7Card(
                             f7Stepper(
                               inputId = "wl",
                               label = h2(strong("Wavelength")),
                               min = 200,
                               max = 11000,
                               value = 580,
                               manual = T,
                               autorepeat = T,
                               raised = T,
                               fill = T
                             )
                           ))
                ),
                f7Col(
                  f7Shadow(intensity = 3,
                           f7Card(
                             f7Stepper(
                               inputId = "od",
                               label = h2(strong("Optical Density")),
                               min = 0,
                               max = 10,
                               value = 6,
                               manual = T,
                               autorepeat = T,
                               raised = T,
                               fill = T
                             )
                           ))
                ))),
                uiOutput(outputId = "links")
              )
            ),
            f7Block(f7Row(
              img(src = "icons/INVO_978.png",
                  width = "72px")
            ))
          ),
          f7Tab(
            tabName = "Catalog",
            icon = f7Icon("book"),
            f7Searchbar(id = "prod-search",
                        placeholder = "Search"),
            f7List(map(
              seq_len(nrow(web_data)),
              ~ f7ListItem(href = web_data$Website[[.x]],
                           web_data$Product[[.x]])
            ))
            ,
            f7Block(f7Row(
              img(src = "icons/INVO_978.png",
                  width = "72px")
            ))
          ),
          f7Tab(
            tabName = "Share",
            icon = f7Icon("hare"),
            f7Block(f7Row(
              img(src = "icons/INVO_978.png",
                  width = "72px")
            ))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www",
                    system.file("app/www", package = "InnovativeOpticsMobile"))

  tags$head(tags$style('body {font-family: Arial;}'),
            favicon(ext = 'png')
            # Add here other external resources
            # for example, you can add shinyalert::useShinyalert()
            )
}
