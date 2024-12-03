#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  df_select <- reactive({
    "Data/92st_RData/" |> 
      paste0(input$st_sel, ".RData") |> 
      load() 
    df$Year <- df$Date |> 
      format("%Y")
    
    df <- df |> 
      dplyr::filter(Temp > -100)
    l <- length(input$yr_sel)
    
    if(l == 1) {
      df |> 
        dplyr::filter(Year > 1980) |> 
        dplyr::filter(Year < 2012)
    } else{
      inp <- input$yr_sel[-1] |> as.numeric()
      df_yr <- matrix(ncol = 4, nrow = 0) |> 
        data.frame()
      colnames(df_yr) <- c("V1", "Date", "Temp", "Year")
      
      for (i in 1:(l-1)) {
        df_yr <- df_yr |> 
          rbind(df |> dplyr::filter(Year == as.numeric(inp[i])))
      }
      df_yr
      
    }
  })
  
  hist_plt <- reactive({
    
    silv <- find_modes(df_select()$Temp, input$bins_sel, get_data = TRUE)
    silv_modes <- silv$modes
    silv_data <- silv$data
    
    # gauss <- mixt_modes(
    #   df_select()$Temp,  
    #   g = 3, 
    #   n_modes = length(silv_modes), 
    #   get_data = TRUE
    # )
    # gauss_modes <- gauss$modes
    # gauss_data <- gauss$data
    
    plot_hist(
      data = df_select(),
      bw = input$bins_sel
    ) + labs(
      title = paste0("Station: ", input$st_sel),
      x = "Temperature",
      y = "Density"
    ) +
      geom_line(aes(x, y), data = silv_data, color = "#2166ac", linewidth = 1.2) +
      geom_vline(xintercept = silv_modes, color = "#2166ac", linewidth = 1.2)
    
  })
  
  
  
  ## Render Tab1
  output$Tab1 <- renderUI({
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "st_sel",
            label = "Select a station",
            choices = St_names,
            multiple = TRUE,
            selectize = FALSE,
            selected = St_names[1]
          ),
          
          selectInput(
            inputId = "yr_sel",
            label = "Year",
            choices = c("All", as.character(seq(1981, 2011))),
            multiple = TRUE,
            selectize = TRUE,
            selected = "All"
          ),
          
          sliderInput("bins_sel", "Bins",
                      min = .25, max = 5,
                      value = 4, step = .1,
                      animate =
                        animationOptions(interval = 2000, loop = TRUE)
          )
        ),
        mainPanel(
          h4("Selected years :"),
          renderPrint({input$yr_sel[-1]}),
          renderPlot({
            hist_plt()
          })
        )
      )
    )
  })
  
  df_select2 <- reactive({
    "Data/92st_RData/" |> 
      paste0(input$st_sel2, ".RData") |> 
      load() 
    df$Year <- df$Date |> 
      format("%Y")
    
    df <- df |> 
      dplyr::filter(Temp > -100)
    l <- length(input$yr_sel2)
    
    if(l == 1) {
      df |> 
        dplyr::filter(Year > 1980) |> 
        dplyr::filter(Year < 2012)
    } else{
      inp <- input$yr_sel2[-1] |> as.numeric()
      df_yr <- matrix(ncol = 4, nrow = 0) |> 
        data.frame()
      colnames(df_yr) <- c("V1", "Date", "Temp", "Year")
      
      for (i in 1:(l-1)) {
        df_yr <- df_yr |> 
          rbind(df |> dplyr::filter(Year == as.numeric(inp[i])))
      }
      df_yr
      
    }
  })
  
  hist_plt2 <- reactive({
    
    gauss <- mixt_modes(
      df_select2()$Temp,
      g = as.numeric(input$g_sel),
      n_modes = as.numeric(input$nmodes_sel),
      get_data = TRUE
    )
    gauss_modes <- gauss$modes
    gauss_data <- gauss$data
    
    plot_hist(
      data = df_select(),
      bw = input$bins_sel2
    ) + labs(
      title = paste0("Station: ", input$st_sel2),
      x = "Temperature",
      y = "Density"
    ) +
      geom_line(aes(x, Y), data = gauss_data, color = "#2166ac", linewidth = 1.2) +
      geom_vline(xintercept = gauss_modes, color = "#2166ac", linewidth = 1.2)
    
  })
  
  ## Render Tab2
  output$Tab2 <- renderUI({
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "st_sel2",
            label = "Select a station",
            choices = St_names,
            multiple = TRUE,
            selectize = FALSE,
            selected = St_names[1]
          ),
          
          selectInput(
            inputId = "yr_sel2",
            label = "Year",
            choices = c("All", as.character(seq(1981, 2011))),
            multiple = TRUE,
            selectize = TRUE,
            selected = "All"
          ),
          
          sliderInput("bins_sel2", "Bins",
                      min = .25, max = 5,
                      value = 4, step = .1,
                      animate =
                        animationOptions(interval = 2000, loop = TRUE)
          ),
          
          selectInput(
            inputId = "g_sel",
            label = "Number of components",
            choices = seq(2, 6),
            multiple = TRUE,
            selectize = FALSE,
            selected = 4
          ),
          
          selectInput(
            inputId = "nmodes_sel",
            label = "Number of pics:",
            choices = c(1, 2, 3),
            multiple = TRUE,
            selectize = FALSE,
            selected = 1
          )
          
        ),
        mainPanel(
          h4("Selected years :"),
          renderPrint({input$yr_sel2[-1]}),
          renderPlot({
            hist_plt2()
          })
        )
      )
    )
  })
  
  
  observe({
    lx <- which(est_methods == input$meth1_sel)
    updateSelectInput(
      session,
      inputId = "meth2_sel",
      choices = est_methods[-lx]
    )
  })
  
  # yearly_select <- reactive({
  #   j <- 0
  #   if(input$estim_sel == "Yearly estimations"){
  #     j + 1
  #   }else {
  #     j
  #   }
  # })
  
  observeEvent(input$estim_sel, {
    if(input$estim_sel == "Yearly estimations"){
      updateSelectInput(
        session,
        inputId = "yearly",
        #label = "Year",
        choices = c(seq(1981, 2011)),
        #multiple = TRUE,
        #selectize = FALSE,
        selected = 1981
      )
    }
  })
  
  compar_plot1 <- reactive({
    l <- input$estim_sel
    if(l == "From 1981 to 2011") {
      comparison_plot(estimates, input$meth1_sel, input$meth2_sel, "Pic1")
    }else{
      if(l == "Mean of yearly estimations"){
        comparison_plot(estimates_mean,
                        input$meth1_sel, input$meth2_sel, 
                        "Pic1")
      }else{
        comparison_plot(estimates_yearly,
                        input$meth1_sel, input$meth2_sel, "Pic1",
                        yearly = input$yearly)
      }
    }
  })
  
  compar_plot2 <- reactive({
    l <- input$estim_sel
    if(l == "From 1981 to 2011") {
      comparison_plot(estimates, input$meth1_sel, input$meth2_sel, "Pic2")
    }else{
      if(l == "Mean of yearly estimations"){
        comparison_plot(estimates_mean,
                        input$meth1_sel, input$meth2_sel, 
                        "Pic2")
      }else{
        comparison_plot(estimates_yearly,
                        input$meth1_sel, input$meth2_sel, "Pic2",
                        yearly = input$yearly)
      }
    }
  })
  
  ## Render Tab3
  output$Tab3 <- renderUI({
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "meth1_sel",
            label = "Select x",
            choices = est_methods,
            selected = est_methods[1]
          ),
          
          selectInput(
            inputId = "meth2_sel",
            label = "Select y",
            choices = est_methods,
            selected = est_methods[2]
          ),
          
          radioButtons(
            inputId = "estim_sel",
            label = "Estimation",
            choices = c(
              "From 1981 to 2011",
              "Mean of yearly estimations",
              "Yearly estimations"
            )
          ),
          
          selectInput(
            inputId = "yearly",
            label = "Year",
            choices = c()
          ),
          h2(strong("Methods :")),
          p(strong("Method 1:"), "Silverman's kernel density estimation"),
          p(strong("Method 2:"), "Seasonal separation"),
          p(strong("Method 3:"), "Gaussian mixture with 2 components"),
          p(strong("Method 4:"), "Gaussian mixture with 3 components"),
          p(strong("Method 5:"), "Gaussian mixture with 4 components")
          # h4(strong("Method 1")),
          # h4(strong("Method 1")),
          # h4(strong("Method 1")),
          # h4(strong("Method 1")),
          # h4(strong("Method 1"))
          
          
          
          # radioButtons(
          #   inputId = "pic_sel",
          #   label = "Select a pic",
          #   choices = c("Pic1", "Pic2"),
          #   selected = "Pic1"
          # )
          
        ),
        mainPanel(
          h3(strong("Pic 1")),
          renderPlotly({
            compar_plot1()
          }),
          
          h3(strong("Pic 2")),
          renderPlotly({
            compar_plot2()
          })
        )
      )
    )
  })
  
  
  df_select3 <- reactive({
    "Data/92st_RData/" |> 
      paste0(input$st_sel3, ".RData") |> 
      load() 
    df$Year <- df$Date |> 
      format("%Y")
    
    df <- df |> 
      dplyr::filter(Temp > -100)
    l <- length(input$yr_sel3)
    
    if(l == 1) {
      df |> 
        dplyr::filter(Year > 1980) |> 
        dplyr::filter(Year < 2012)
    } else{
      inp <- input$yr_sel3[-1] |> as.numeric()
      df_yr <- matrix(ncol = 4, nrow = 0) |> 
        data.frame()
      colnames(df_yr) <- c("V1", "Date", "Temp", "Year")
      
      for (i in 1:(l-1)) {
        df_yr <- df_yr |> 
          rbind(df |> dplyr::filter(Year == as.numeric(inp[i])))
      }
      df_yr
      
    }
  })
  
  hist_plt3 <- reactive({
    
    seas <- sep_modes(
      df_select3(),
      sep_dates = sepd2,
      get_data = TRUE
    )
    seas_modes <- seas$modes
    seas_data <- seas$data |> as.data.frame()
    
    plot_hist(
      data = df_select3(),
      bw = input$bins_sel3
    ) + labs(
      title = paste0("Station: ", input$st_sel3),
      x = "Temperature",
      y = "Density"
    ) +
      geom_line(aes(x, y1), data = seas_data, color = "#b2182b", linewidth = 1.2) +
      geom_line(aes(x, y2), data = seas_data, color = "#2166ac", linewidth = 1.2) +
      geom_vline(xintercept = seas_modes, color = c("#2166ac", "#b2182b"), linewidth = 1.2)

  })
  
  ## Render Tab2
  output$Tab4 <- renderUI({
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "st_sel3",
            label = "Select a station",
            choices = St_names,
            multiple = TRUE,
            selectize = FALSE,
            selected = St_names[1]
          ),
          
          selectInput(
            inputId = "yr_sel3",
            label = "Year",
            choices = c("All", as.character(seq(1981, 2011))),
            multiple = TRUE,
            selectize = TRUE,
            selected = "All"
          ),
          
          sliderInput("bins_sel3", "Bins",
                      min = .25, max = 5,
                      value = 4, step = .1,
                      animate =
                        animationOptions(interval = 2000, loop = TRUE)
          )
          
        ),
        mainPanel(
          h4("Selected years :"),
          renderPrint({input$yr_sel3[-1]}),
          renderPlot({
            hist_plt3()
          })
        )
      )
    )
  })
  
  df_select5 <- reactive({
    "Data/92st_RData/" |> 
      paste0(input$st_sel5, ".RData") |> 
      load() 
    df$Year <- df$Date |> 
      format("%Y")
    
    df <- df |> 
      dplyr::filter(Temp > -100)
    l <- length(input$yr_sel5)
    
    if(l == 1) {
      df |> 
        dplyr::filter(Year > 1980) |> 
        dplyr::filter(Year < 2012)
    } else{
      inp <- input$yr_sel5[-1] |> as.numeric()
      df_yr <- matrix(ncol = 4, nrow = 0) |> 
        data.frame()
      colnames(df_yr) <- c("V1", "Date", "Temp", "Year")
      
      for (i in 1:(l-1)) {
        df_yr <- df_yr |> 
          rbind(df |> dplyr::filter(Year == as.numeric(inp[i])))
      }
      df_yr
      
    }
  })
  
  observeEvent(input$st_sel5, {
    if(input$st_sel5 != St_names[1]){
      n <- df_select5()$Temp |> 
        multimode::bw.crit(mod0 = 1) + .15
      updateSliderInput(
        session,
        inputId = "bins_sel3",
        value = c(.25, n)
      )
    }
  })
  
  mode_tree_plot <- reactive({
    #n <- multimode::bw.crit(mod0 = 1) + .15
    m <- df_select5()$Temp |> 
      multimode::modetree(bws = input$bins_sel3)
      
    plot(m)
      
  })
  
  ## Render Tab5
  output$Tab5 <- renderUI({
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "st_sel5",
            label = "Select a station",
            choices = St_names,
            multiple = TRUE,
            selectize = FALSE,
            selected = St_names[1]
          ),
          
          selectInput(
            inputId = "yr_sel5",
            label = "Year",
            choices = c("All", as.character(seq(1981, 2011))),
            multiple = TRUE,
            selectize = TRUE,
            selected = "All"
          ),
          
          sliderInput("bins_sel3", "Bins",
                      min = .01, max = 4,
                      value = c(.25, 2.5)
          )
          
        ),
        mainPanel(
          renderPlot({
            mode_tree_plot()
          })
        )
      )
    )
  })
  
  N_modes <- reactive({
    nmodes |> as.tibble()
  })
  
  output$download1 <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0('table1', ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(N_modes(), file)
    }
  )
  
  df_Pics <- reactive({
    n <- input$n_pics
    file <- 'Data/Pic_' |> paste0(n, '.csv')
    read.csv(file = file)
  })
  
  output$download2 <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0('table2', ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(df_Pics(), file)
    }
  )
  
  ## Render Tab6
  output$Tab6 <- renderUI({
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          downloadButton("download1", "Download table 1"),
          selectInput(
            'n_pics',
            label = "Select a number of pics",
            choices = c(1, 2, 3, 4, 5),
            multiple = TRUE,
            selectize = FALSE,
            selected = 1
          ),
          downloadButton("download2", "Download table 2")
        ),
        mainPanel(
          h3(strong('Table 1')),
          renderDataTable({
            N_modes() |> head()
          }),
          h3(strong('Table 2')),
          renderDataTable({
            df_Pics()
          })
        )
      )
    )
  })
  
  # pics <- reactive({
  #   n <- input$n_pics_tab7
  #   if(n == 1){
  #     paste0(n, ' pic')
  #   }else{
  #     paste0(n, ' pics')
  #   }
  # })
  output$map_plot7 <- renderPlotly({
    
    plot_map(nmodes,
             pic = input$n_pics_tab7, title = '')
  })
  
  output$st_gap7 <- renderPlotly({
    if(input$max_bw == 'max'){
      x <- get_bw_diff()
    }else{
      limit <- input$max_bw |> as.numeric()
      x <- get_bw_diff(limit)
    }
    
    x <- x |> 
      dplyr::filter(Station == input$st_tab7) |> 
      as.data.table() |> 
      melt.data.table(id.vars = c("Station"),
                      measure.vars = c("1 pic", "2 pics", '3 pics', '4 pics', '5 pics'),
                      variable.name = "N_pics",
                      value.name = "Gap")
    
    
    x |>
      ggplot(aes(x = N_pics, y = Gap)) + 
      geom_point(size = 3, shape=16)
  })
  
  output$hist_tab7 <- renderPlotly({
    "Data/92st_RData/" |> 
      paste0(input$st_tab7, ".RData") |> 
      load() 
    df$Year <- df$Date |> 
      format("%Y")
    
    df <- df |> 
      dplyr::filter(Temp > -100) |>
      dplyr::filter(Year > 1980) |> 
      dplyr::filter(Year < 2012)
    
    n_m <- input$n_pics_tab7 |> substr(1,1) |> as.numeric()
    bw_d <- df$Temp |> multimode::bw.crit(mod0 = n_m)
    
    if(input$max_bw == 'max'){
      bw_h <- 11
    }else{
      bw_h <- input$max_bw |> as.numeric()
      bw_h <- bw_h*2
    }
    
    plot_hist(df, 2) 
      # geom_density(adjust=bw_d)
  })
  
  output$Tab7 <- renderUI({
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          #downloadButton("download1", "Download table 1"),
          selectInput(
            'n_pics_tab7',
            label = "Select a number of pics",
            choices = c('1 pic', '2 pics', '3 pics', '4 pics', '5 pics'),
            multiple = TRUE,
            selectize = FALSE,
            selected = '2 pics'
          ),
          selectInput(
            'st_tab7',
            label = "Select a station",
            choices = St_names,
            multiple = TRUE,
            selectize = FALSE,
            selected = St_names[16]
          ),
          selectInput(
            'max_bw',
            label = "Select maximum value of the bandwith:",
            choices = c('max', '1', '2', '3'),
            multiple = TRUE,
            selectize = FALSE,
            selected = '3'
          )
          # downloadButton("download2", "Download table 2")
        ),
        mainPanel(
          plotlyOutput("map_plot7", height = '500px', width = '900px'),
          #renderPrint(input$plot_click),
          #nearPoints(X92st_list[c('St_Name', 'x', 'y')], input$plot_click),
          plotlyOutput('st_gap7', height = '300px', width = '900px'),
          plotlyOutput('hist_tab7', height = '300px', width = '900px')
        )
      )
    )
  })
  
  
  # df_select8 <- reactive({
  #   "Data/92st_RData/" |> 
  #     paste0(input$st_tab8, ".RData") |> 
  #     load() 
  #   df$Year <- df$Date |> 
  #     format("%Y")
  #   
  #   df <- df |> 
  #     dplyr::filter(Temp > -100)
  # 
  #   df |> 
  #     dplyr::filter(Year > 1980) |> 
  #     dplyr::filter(Year < 2012)
  #   
  # })
  
  output$map_plot8 <- renderPlotly({
    
    gap <- get_bw_diff(lim1 = as.numeric(input$n_bins8), by_bw = FALSE)
    
    gap_modes <- opt_n_modes(gap)
    wmap <- map_data("world")
    ca <- wmap |> dplyr::filter(region == "Canada")
    mp92 <- ca |> ggplot(aes(long, lat)) +
      geom_path(aes(x = long, y = lat, group = group),
                color = "#737373") +
      geom_polygon(aes(group = group, subgroup = subregion),
                   fill = "#f0f0f0", color = "#525252") +
      scale_color_brewer(palette = "Dark2") +
      theme_void() +
      geom_point(
        data = gap_modes, 
        aes(x = Long, y = Lat,# shape = N_modes,
            fill = N_modes, label = St_Name),
        size = 3, shape = 21
      ) +
      labs(
        color = 'Number of pics'
        # size = 'Bandwith'
      )
  })
  
  output$st_gap8 <- renderPlotly({
    
    df <- nmodes[nmodes$Station == input$st_tab8,]
    minT <- df$min
    maxT <- df$max
    
    limit <- (
      maxT - minT
    )/as.numeric(input$n_bins8)
    
    x <- get_bw_diff(limit)
    x <- x |>
      dplyr::filter(Station == input$st_tab8) |>
      as.data.table() |>
      melt.data.table(id.vars = c("Station"),
                      measure.vars = c("1 pic", "2 pics", '3 pics', '4 pics', '5 pics'),
                      variable.name = "N_pics",
                      value.name = "Gap")


    x |>
      ggplot(aes(x = N_pics, y = Gap)) +
      geom_point(size = 3, shape=16)
  })
  
  
  output$hist_tab8 <- renderPlotly({
    "Data/92st_RData/" |> 
      paste0(input$st_tab8, ".RData") |> 
      load() 
    df$Year <- df$Date |> 
      format("%Y")
    
    df <- df |> 
      dplyr::filter(Temp > -100) |>
      dplyr::filter(Year > 1980) |> 
      dplyr::filter(Year < 2012)
    
    
    df |> 
      ggplot(aes(x = Temp)) +
      geom_histogram(aes(y=..density..), bins = input$n_bins8,
                     colour="black", fill="grey") +
      #geom_density(alpha=.2, fill="#FF6666")
      #  geom_line(aes(x, y), linewidth = 1.2, color = "red",
      #            data = data.frame(x = dens$x, y = dens$y)) +
      theme_classic()
    
  })
  
  output$Tab8 <- renderUI({
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(
            'st_tab8',
            label = "Select a station",
            choices = St_names,
            multiple = TRUE,
            selectize = FALSE,
            selected = St_names[16]
          ),
          selectInput(
            'n_bins8',
            label = "Select a number of bins",
            choices = seq.int(10, 100, 5),
            multiple = TRUE,
            selectize = FALSE,
            selected = 10
          )
        ),
        mainPanel(
          plotlyOutput("map_plot8", height = '500px', width = '900px'),
          plotlyOutput('st_gap8', height = '300px', width = '900px'),
          plotlyOutput('hist_tab8', height = '300px', width = '900px')
        )
      )
    )
  })
  
  
  output$plot9 <- renderPlotly({
    
    df <- data.frame(
      N_bins = seq(5, 100, 1)
    )
    df['1 pic'] <- NA
    df['2 pics'] <- NA
    df['3 pics'] <- NA
    
    for (x in df$N_bins){
      
      y <- x |> 
        get_bw_diff(by_bw = FALSE) |> 
        opt_n_modes() |> 
        dplyr::select(N_modes) |> 
        table()
      
      if(length(y) == 2){
        yy <- c(y, 0)
      }else{
        yy <- y
      }
      
      df[
        df$N_bins == x,
        c('1 pic', '2 pics', '3 pics')
      ] <- yy
    }
    
    df <- df |> 
      melt(id.vars = c("N_bins"),
           variable.name = "N_pics")
    colnames(df) <- c("N_bins", "N_pics", "N_stations")
    
    df |> 
      ggplot(aes(N_bins, N_stations)) +
      geom_line(aes(color = N_pics)) +
      theme_classic() +
      scale_color_brewer(palette = "Dark2") +
      labs(
        x = "Number of bins",
        y = "Number of stations",
        color = "Number of pics"
      )
  })
  
  output$Tab9 <- renderUI({
    fluidPage(
      plotlyOutput("plot9", height = '700px', width = '1500px')
    )
  })
  
  output$fig3_1_a <- renderPlotly({
    xx <- 65 |> 
      get_bw_diff(by_bw = FALSE) |> 
      opt_n_modes()
  })
  
  output$chap_3_1 <- renderUI({
    fluidPage(
      fluidRow(
        box(
          title = ""
        )
      ),
      
      fluidRow(
        
      ),
      
      fluidRow(
        
      )
    )
  })
  
}
