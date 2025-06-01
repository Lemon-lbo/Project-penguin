# ---
# title: "My Shiny App"
# output: html_document
# runtime: shiny
# ---
# ```{r setup, include=FALSE}
# 
# required_packages <- c("shinydashboard", "shiny", "tidyverse", "ggplot2", "DT", "ggcorrplot", "gridExtra")
# missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
# if(length(missing_packages)) install.packages(missing_packages)

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(DT) # For interactive tables
library(ggcorrplot) # For correlation matrix plot
library(gridExtra)
library(GGally)
library(forcats)
library(stats)
# Read and preprocess the data
p_lter <- read.csv("penguins_lter.csv")
  #read.csv("C:\\Users\\trish\\Desktop\\Sem1\\Visualization\\Penguin Project\\myApp\\penguins_lter.csv")
p_size1 <- read.csv("penguins_size.csv")
  #read.csv("C:\\Users\\trish\\Desktop\\Sem1\\Visualization\\Penguin Project\\myApp\\penguins_size.csv")

# Clean the p_size1 data
p_size <- p_size1 %>%    
  drop_na(sex) %>% 
  filter(sex %in% c("MALE", "FEMALE")) # Keep only 'MALE' and 'FEMALE'

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Palmer Penguins Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("book")),
      menuItem("Data Description", tabName = "data_description", icon = icon("info-circle")),
      menuItem("Data Overview", tabName = "data_overview", icon = icon("table")),
      menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Conclusions", tabName = "results", icon = icon("clipboard")),
      menuItem("About Me", tabName = "about", icon = icon("user"))
    )
  ),
  dashboardBody(
    
    tabItems(
      # Introduction tab
      tabItem(tabName = "intro",
              fluidRow(
                box(title = "Welcome to the Penguin Explorer!", width = 12, status = "primary", solidHeader = TRUE,
                    p("Welcome to the Palmer Penguins data visualization app! Dive into the fascinating world of Adélie, Chinstrap, and Gentoo penguins, as we explore their physical characteristics, distributions, and correlations."),
                    p("This app is designed to take you on an analytical journey through interactive plots and insights, showcasing the biological and geographical nuances of these remarkable birds. Whether you're here to uncover patterns or simply admire the beauty of data, this app has something for everyone."),
                    p("Get ready to waddle through tabs covering everything from data descriptions and exploratory analyses to conclusions. Let’s get started!"),
                    # add images
                    HTML('
                    <div style="display: flex; justify-content: center;align-items: flex-end; gap: 20px;">
                      <img src="3_pen_2.jpg" style="height: 100px; width: auto; border-radius: 8px;" />
                      <img src="alice.jpg" style="height: 200px; width: auto; border-radius: 8px;" />
                      <img src="3_pen_1.jpg" style="height: 100px; width: auto; border-radius: 8px;" />
                    </div>
                  ')
                )
              )
      ),
      # Data Description tab
      tabItem(tabName = "data_description",
              fluidRow(
                box(title = "Data Description", width = 12,
                    p("The Palmer Penguins dataset provides a set of body size measurements for three penguin species—Adélie,
Gentoo, and Chinstrap—collected from 2007 to 2009 in the Palmer Archipelago, Antarctica. 
                      It includes 17 columns and 344 observations, focusing on 7 variables (3 categorical and 4 numerical) corresponding
to 333 observations after excluding missing values. These variables provide insights into the physical and biological characteristics of penguins, enabling meaningful analysis."),
                    tags$ul(
                      tags$li("Species: Identifies the penguin species (Adélie, Chinstrap, or Gentoo)."),
                      tags$li("Island: Indicates the penguin's specific island (Biscoe, Dream, or Torgersen)."),
                      tags$li("Sex: Denotes the penguin's sex (Male or Female)."),
                      tags$li("Culmen Length (mm): Represents the penguin's bill length."),
                      tags$li("Culmen Depth (mm): Represents the penguin's bill depth."),
                      tags$li("Flipper Length (mm): Measures the penguin's flipper length."),
                      tags$li("Body Mass (grams): Indicates the penguin's weight.")
                    ),
                    p("The selected attributes were chosen because they are biologically meaningful and straightforward to interpret. The numerical variables, such as Culmen Length, Culmen Depth, Flipper Length, and Body Mass, are strongly correlated and critical for understanding physical characteristics. Meanwhile, categorical variables like Species, Island, and Sex help in segmentation, grouping, and deriving meaningful biological insights about the penguins.")
                )
              )
      ),
      # Data Overview tab
      tabItem(tabName = "data_overview",
              fluidRow(
                box(title = "Data Overview", width = 12,
                    p("This dataset includes information about penguins in the Palmer Archipelago, Antarctica."),
                    br(),
                    DTOutput("data_table")
                )
              )
      ),
      
      # Exploratory Data Analysis tab
      tabItem(tabName = "eda",
              tabsetPanel(
                # Section 1: Distribution of Categorical Variables
                tabPanel("Distribution of Categorical Variables", 
                         fluidRow(
                           box(title = "Distribution of Categorical Variables", width = 12, collapsible = TRUE, collapsed = FALSE,
                               selectInput("cat_var", "Select Categorical Variable:", 
                                           choices = c("Sex", "Species", "Island"), 
                                           selected = "Sex"),
                               plotOutput("cat_plot"),
                               h4("Interpretation:"),
                               textOutput("cat_interpretation") # dynamic interpretation text
                           )
                         )
                ),
                # Section 2: Distribution of Numerical Variables
                tabPanel("Distribution of Numerical Variables", 
                         fluidRow(
                           box(title = "Distribution of Numerical Variables", width = 12, collapsible = TRUE, collapsed = FALSE,
                               selectInput("num_var", "Select Numerical Variable:", 
                                           choices = c("Body Mass (g)", "Culmen Length (mm)", "Culmen Depth (mm)", "Flipper Length (mm)"), 
                                           selected = "Body Mass (g)"),
                               plotOutput("num_plot"),
                               h4("Interpretation:"),
                               textOutput("num_interpretation") # dynamic interpretation text
                           )
                         )
                ),
                
                # Section 3: Correlation Matrix
                tabPanel("Correlation Matrix", 
                         fluidRow(
                           box(title = "Correlation Matrix of Numerical Variables", width = 12, collapsible = TRUE, collapsed = FALSE,
                               plotOutput("corr_plot"),
                               h4("Interpretation:"),
                               p("The Pearson correlation analysis reveals significant relationships among the selected numerical variables. There are strong positive correlations between flipper length and body mass, flipper length and culmen length, as well as culmen length and body mass, indicating that increases in one variable are associated with proportional increases in the others. Additionally, a notable negative correlation exists between flipper length and culmen depth, suggesting an inverse relationship where an increase in one corresponds to a decrease in the other.") # Placeholder for scatter plot interpretation
                           )
                         )
                ),
                # Section 4: Scatter Plot
                tabPanel("Scatter Plot", 
                         fluidRow(
                           box(title = "Scatter Plot Matrix", width = 12, collapsible = TRUE, collapsed = FALSE,
                               selectInput("matrix_var_x", "Select X Variable:", 
                                           choices = c("Body Mass (g)", "Culmen Length (mm)", "Culmen Depth (mm)", "Flipper Length (mm)"), 
                                           selected = "Body Mass (g)"),
                               selectInput("matrix_var_y", "Select Y Variable:", 
                                           choices = c("Body Mass (g)", "Culmen Length (mm)", "Culmen Depth (mm)", "Flipper Length (mm)"), 
                                           selected = "Culmen Length (mm)"),
                               plotOutput("matrix_scatter_plot"),
                               h4("Comment:"),
                               p("The scatter plot matrix displays the pairwise relationships among the selected variables. Adjust the inputs to explore different variable pairs.")
                           )
                         )
                ),
                
                # Section 5: Simpson's Paradox
                tabPanel("Simpson's Paradox", 
                         fluidRow(
                           box(
                             title = "Simpson's Paradox Analysis", 
                             width = 12, 
                             collapsible = TRUE, 
                             collapsed = FALSE,
                             selectInput(
                               "paradox_choice", 
                               "Choose Analysis:", 
                               choices = c("Culmen Depth vs. Flipper Length", "Body Mass vs. Flipper Length"), 
                               selected = "Culmen Depth vs. Flipper Length"
                             ),
                             plotOutput("paradox_plot"),
                             h4("Interpretation:"),
                             textOutput("paradox_interpretation")
                           )
                         )
                )
              )
      ),
      
      # Results/Conclusion Tab
      tabItem(tabName = "results",
              fluidRow(
                box(title = "Key Findings", width = 12,
                    tags$ul(
                      tags$li("The sex ratio across all penguin species is nearly balanced, with an equal number of male and female penguins in the sample."),
                      tags$li("Adélie penguins are present on all three islands (Biscoe, Dream, and Torgersen)."),
                      tags$li("Chinstrap penguins are found only on Dream and Biscoe islands."),
                      tags$li("Gentoo penguins are only present on Dream and Biscoe islands."),
                      tags$li("The distribution of body size characteristics shows central tendencies and species-specific variations, with one species generally exhibiting larger sizes."),
                      tags$li("Flipper size, bill dimensions, and body mass tend to be higher in some species compared to others, reflecting ecological and biological differences."),
                      tags$li("Sexual dimorphism is consistently observed — males tend to be larger than females in all species across multiple body measurements."),
                      tags$li("A strong positive correlation exists between body size and appendage length, consistent within species."),
                      tags$li("Simpson’s Paradox is observed in the relationship between bill depth and flipper size — while overall trends may appear weak or misleading, stratifying by species and sex reveals strong positive correlations.")
                    )
                ),
                
                # Gap
                tags$br(),
                
                # Conclusion Section
                box(title = "Analytical Insights", width = 12,
                    tags$p("Our exploratory data analysis highlights important insights into the penguin population from the Palmer Archipelago:"),
                    tags$ul(
                      tags$li("Species are distributed unevenly across islands, with some species restricted to specific geographic locations."),
                      tags$li("The dataset reflects an approximately balanced sex ratio across all observed species."),
                      tags$li("Key body measurements display clear central tendencies and variation across species, suggesting distinct physiological characteristics."),
                      tags$li("Consistent sexual dimorphism is observed — within each species, male penguins exhibit higher values across all observed key body measurements."),
                      tags$li("Subgroup analyses reveal important correlations that are not always apparent in aggregate data. In particular, Simpson’s Paradox illustrates how aggregated views can obscure trends visible at the subgroup level, emphasizing the importance of species- and sex-level analysis.")
                    )
                ),
                
                # Gap
                tags$br(),
                
                # References Section
                box(title = "References", width = 12,
                    tags$ol(
                      tags$li(
                        "Gorman, K. B., Williams, T. D., & other collaborators. (2019). Palmer Penguins: A dataset for the study of penguins in the Antarctic. Retrieved from ", 
                        tags$a(href = "https://www.kaggle.com/datasets/parulpandey/palmer-archipelago-antarctica-penguin-data?resource=download", target = "_blank", "here"),"."
                      ),
                      tags$li(
                        "Palmer Archipelago Penguins Data in the ", 
                        tags$a(href = "https://journal.r-project.org/articles/RJ-2022-020/", target = "_blank", "palmerpenguins"),
                        " Palmerpenguins R Package - An Alternative to Anderson’s Irises."
                      )
                    )
                )
              )
      ),
      # About Me tab (now outside EDA tab)
      tabItem(tabName = "about",
              fluidRow(
                box(title = "About Me", width = 12,
                    tags$p(
                      "I am Trishita Patra, a first-year MSc Data Science student at Chennai Mathematical Institute. This app was developed as part of the Visualization Course taught by ",
                      tags$a(href = "https://www.cmi.ac.in/~sourish/", "Dr. Sourish Das"),
                      "."
                    ),
                    p("In this project, I applied data visualization techniques to uncover interesting insights from the Palmer Penguin dataset."),
                    p("Feel free to contact me at: trishitap.mds2024@cmi.ac.in. This dashboard was first made public in December 2024.")
    
                ),
                box(title = "Credits and Copyright", width = 12,
                    p("© 2025 Trishita Patra. All rights reserved."),
                    tags$p("Developed by Trishita Patra using the ",
                           tags$a(href = "https://rstudio.github.io/shinydashboard/", "shinydashboard"),
                    " framework in RStudio."),
                    p("Images on Introduction tab sourced from:"),
                    tags$ul(
                      tags$li(tags$a(href = "https://www.vectorstock.com/royalty-free-vector/elegant-penguin-silhouette-vector-53719788", "Elegant Penguin Silhouette vector image by Shamima84")),
                      tags$li(tags$a(href = "https://images.app.goo.gl/StTPLJz5Rorunhyw7", "Pinterest"))
                    )
                )
                
              )
      )
    )
  )
)


# Server
server <- function(input, output, session) {
  #addResourcePath("images", ".") # when pictures to added are in images
  
  ## data to be used
  species = p_size$species
  island = p_size$island
  sex = p_size$sex
  culmen_length = p_size$culmen_length_mm
  culmen_depth = p_size$culmen_depth_mm
  flipper_length = p_size$flipper_length_mm
  body_mass = p_size$body_mass_g
  
  ## Setting colours | c = combine/concatenate the colour values in the array
  
  male_c = "lightblue"       ##  Sex colours
  female_c = "lightcoral"
  male_c_fit = "lightblue4"  ## Colours for fitting by sex 
  female_c_fit = "coral4"
  ad_c = "wheat"             ## Species colour
  ch_c = "lightgreen"
  gen_c = "plum"
  ad_c_fit = "yellow3"       ## Colours for fitting by species 
  ch_c_fit = "green4"
  gen_c_fit = "red4"
  
  ## Note that Named vector would eliminate any error which may cause from different input sequence of elements of attributes.
  colour_sex <- c(male_c, female_c)      
  colour_sex_fit <- c(male_c_fit, female_c_fit) 
  colour_species <- c(ad_c, ch_c, gen_c) 
  colour_species_fit <- c( ad_c_fit, ch_c_fit, gen_c_fit) 
  

  # Define the bar_plot_fill function | categorical variable.
  bar_plot_fill <- function(data_set, feature, fill_by, colour_list, plot_title, x_label, legend_title){
    unique_fill_by <- unique(fill_by)
    
    if (length(colour_list) != length(unique_fill_by)) {
      stop("Length of colour_list must match the number of unique feature values.")
    }
    colour_fill_values <- setNames(colour_list, unique_fill_by)
    
    result <- ggplot(data_set, aes(x = feature, fill = fill_by)) +
      geom_bar(color = "black", position = "stack") +  
      scale_fill_manual(values = colour_fill_values) +  
      geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 2) +  
      theme_minimal() + 
      ggtitle(plot_title) +
      labs(x = x_label , y = "Count", fill = legend_title) + 
      theme(plot.title = element_text(size = 10, hjust = 0.5),
            axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            axis.text.x = element_text(size = 6),
            axis.text.y = element_text(size = 6),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 6)) +   
      coord_flip()
    
    return(result)
  }
  ## Numerical variable distribution
  num_char_hist <- function(data_set, feature, fill_by, colour_list, colour_list_fit, plot_title, x_label, y_label, legend_title) {
    
    # Ensure feature and fill_by are valid columns in data_set
    if (!any(feature %in% names(data_set))) {
      stop(paste("Feature column", feature, "not found in the dataset."))
    }
    
    if (!any(fill_by %in% names(data_set))) {
      stop(paste("Fill_by column", fill_by, "not found in the dataset."))
    }
    
    unique_fill_by <- unique(data_set[[fill_by]])  
    colour_list_fit <- setNames(colour_list_fit, unique_fill_by)
    
    if (length(colour_list) != length(unique_fill_by)) {
      stop("Length of colour_list must match the number of unique feature values.")
    }
    
    colour_fill_values <- setNames(colour_list, unique_fill_by)
    
    result <- ggplot(data_set, aes_string(x = feature, fill = fill_by)) +
      geom_histogram(aes(y = ..density..), 
                     alpha = 0.5, 
                     position = "identity", 
                     bins = 30) + 
      scale_fill_manual(values = colour_fill_values) + 
      labs(title = plot_title, x = x_label, y = y_label, fill = legend_title) +
      theme_minimal() + 
      theme(plot.title = element_text(size = 10, hjust = 0.5),
            axis.title.x = element_text(size = 8),         
            axis.title.y = element_text(size = 8),         
            axis.text.x = element_text(size = 6),          
            axis.text.y = element_text(size = 6),
            legend.title = element_text(size = 8),         
            legend.text = element_text(size = 6),          
            legend.position = "bottom")                   
    
    for (fill_by_ele in unique_fill_by) {
      filtered_data <- data_set[data_set[[fill_by]] == fill_by_ele, ]
      
      mean_feature <- mean(filtered_data[[feature]], na.rm = TRUE)
      sd_feature <- sd(filtered_data[[feature]], na.rm = TRUE)
      
      result <- result +
        stat_function(
          data = data.frame(feature = seq(min(data_set[[feature]], na.rm = TRUE), 
                                          max(data_set[[feature]], na.rm = TRUE), length.out = 100)),
          aes(x = feature), 
          fun = dnorm, 
          args = list(mean = mean_feature, sd = sd_feature),
          color = colour_list_fit[fill_by_ele],  
          size = 1,
          inherit.aes = FALSE)  # Prevent inheritance of fill aesthetic
    }
    return(result)
  }
  # Create the scatter plot for Flipper Length vs. Body Mass | Positive correlation
  var1_scatter_var2_facet <- function(data_set, var1, var2, colour_by, facet_by, colour_list, colour_list_fit, plot_title, x_label, y_label, legend_title) {
    
    # Ensure unique values of the colour_by variable
    unique_fill_by <- unique(data_set[[colour_by]])  
    
    # Name the color list based on unique_fill_by categories
    colour_list_fit <- setNames(colour_list_fit, unique_fill_by)
    colour_fill_values <- setNames(colour_list, unique_fill_by)  
    
    # Check length of colour_list
    if (length(colour_list) != length(unique_fill_by)) {
      stop("Length of colour_list must match the number of unique feature values.")
    }
    
    # Base plot with scatter points
    result <- ggplot(data_set, aes(x = .data[[var1]], y = .data[[var2]], color = .data[[colour_by]])) +
      geom_point() +  # Scatter points
      scale_color_manual(values = colour_fill_values) +  # Custom color mapping for points
      
      # Adding smooth lines for each unique category in colour_by
      lapply(unique_fill_by, function(fill_by_ele) {
        geom_smooth(data = data_set[data_set[[colour_by]] == fill_by_ele, ], 
                    aes(group = .data[[colour_by]]), 
                    method = "lm", se = FALSE, 
                    color = colour_list_fit[fill_by_ele], show.legend = FALSE)
      }) + 
      facet_wrap(as.formula(paste("~", facet_by))) +  # Create separate panels for each facet_by
      labs(title = plot_title,
           x = x_label,
           y = y_label, fill = legend_title) +
      theme_minimal() +  # Minimal theme for the plot
      scale_x_continuous(breaks = seq(170, 230, by = 30)) +  # Set fewer x-axis breaks
      theme(plot.title = element_text(size = 9, hjust = 0.5),
            axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            axis.text.x = element_text(size = 6),
            axis.text.y = element_text(size = 6),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 6),
            legend.position = "bottom") 
    
    return(result)
  }
  
  var1_scatter_var2 <- function(data_set, var1, var2, plot_title, x_label, y_label) {
    
    result <- ggplot(data_set, aes(x = .data[[var1]], y = .data[[var2]])) +
      geom_point() +  # Scatter points
      geom_smooth(method = "lm", se = FALSE) +  # Linear model fit line without confidence interval
      labs(title = plot_title,
           x = x_label,
           y = y_label) +
      theme_minimal()  + # Minimal theme for the plot 
      theme(plot.title = element_text(size = 9,hjust = 0.5),
            axis.title.x = element_text(size = 8),         # Reduce x-axis label font size
            axis.title.y = element_text(size = 8),         # Reduce y-axis label font size
            axis.text.x = element_text(size = 6),          # Reduce x-axis tick mark font size
            axis.text.y = element_text(size = 6))
    
    return(result)
  }
  
  # Distribution of numerical features across sex, faceted by species
  
  num_char_hist_fac <- function(data_set, feature, fill_by, colour_list, colour_list_fit, plot_title, x_label, y_label, legend_title, facet_by) {
    
    # Ensure valid columns
    if (!all(c(feature, fill_by, facet_by) %in% names(data_set))) {
      stop("Ensure 'feature', 'fill_by', and 'facet_by' are valid columns in the dataset.")
    }
    
    # Extract unique values
    unique_fill_by <- unique(data_set[[fill_by]])  
    unique_facets <- unique(data_set[[facet_by]])
    
    # Map colors
    colour_fill_values <- setNames(colour_list, unique_fill_by)
    colour_list_fit <- setNames(colour_list_fit, unique_fill_by)
    
    # Create a container for Gaussian data
    gaussian_data <- list()
    
    # Calculate Gaussian curve data per facet and fill_by
    for (facet in unique_facets) {
      for (fill_by_val in unique_fill_by) {
        filtered_data <- data_set[data_set[[facet_by]] == facet & data_set[[fill_by]] == fill_by_val, ]
        if (nrow(filtered_data) > 1) { # Only add curve if sufficient data
          gaussian_data[[paste(facet, fill_by_val, sep = "_")]] <- list(
            mean = mean(filtered_data[[feature]], na.rm = TRUE),
            sd = sd(filtered_data[[feature]], na.rm = TRUE),
            range = range(filtered_data[[feature]], na.rm = TRUE)
          )
        }
      }
    }
    
    # Initialize plot with histogram
    result <- ggplot(data_set, aes_string(x = feature, fill = fill_by)) +
      geom_histogram(aes(y = ..density..), alpha = 0.75, position = "identity", bins = 30) +
      scale_fill_manual(values = colour_fill_values) +
      facet_wrap(as.formula(paste("~", facet_by))) +
      labs(title = plot_title, x = x_label, y = y_label, fill = legend_title) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.position = "bottom"
      )
    
    # Generate combined Gaussian curve data
    curve_df_all <- data.frame()
    
    for (facet in unique_facets) {
      for (fill_by_val in unique_fill_by) {
        key <- paste(facet, fill_by_val, sep = "_")
        if (!is.null(gaussian_data[[key]])) {
          curve_data <- gaussian_data[[key]]
          x_vals <- seq(curve_data$range[1], curve_data$range[2], length.out = 100)
          y_vals <- dnorm(x_vals, mean = curve_data$mean, sd = curve_data$sd)
          
          temp_df <- data.frame(
            x = x_vals,
            y = y_vals,
            facet_value = facet,
            fill_by_value = fill_by_val
          )
          curve_df_all <- rbind(curve_df_all, temp_df)
        }
      }
    }
    
    return(result)
  }
  #.......................................................................................
  
  # Render the Data Overview table
  output$data_table <- renderDT({
    datatable(p_size, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Numerical variable distribution plot
  output$num_plot <- renderPlot({
    req(input$num_var)
    
    # Variables for general distribution plot
    general_plot <- NULL
    custom_plot <- NULL
    dimorphism_plot <- NULL
    
    # Generate plots based on selected variable
    if (input$num_var == "Body Mass (g)") {
      # General distribution
      general_plot <- ggplot(p_size, aes(x = body_mass_g)) + 
        geom_histogram(bins = 30, fill = "skyblue3", color = "black") + 
        labs(title = "General Distribution of Body Mass", x = "Body Mass (g)", y = "Frequency") +
        theme_minimal()
      
      # Custom visualization 
      custom_plot <- num_char_hist(p_size, "body_mass_g", "species", colour_species, colour_species_fit, 
                                    "Distribution of Body Mass", "Body Mass (g)", "Density", "Species")
      dimorphism_plot <- num_char_hist_fac(
        p_size, 
        "body_mass_g", 
        "sex", 
        colour_sex, 
        colour_sex_fit, 
        "Distribution of Body Mass by Sex and Species", 
        "Body Mass (g)", 
        "Density", 
        "Sex", 
        facet_by = "species"
      )
      
    } else if (input$num_var == "Culmen Length (mm)") {
      # General distribution
      general_plot <- ggplot(p_size, aes(x = culmen_length_mm)) + 
        geom_histogram(bins = 30, fill = "skyblue3", color = "black") + 
        labs(title = "General Distribution of Culmen Length", x = "Culmen Length (mm)", y = "Frequency") +
        theme_minimal()
      
      # Custom visualization
      custom_plot <-  
        num_char_hist(p_size, "culmen_length_mm", "species", colour_species, colour_species_fit, 
                      "Distribution of Culmen Length", "Culmen Length (mm)", "Density", "Species")
      dimorphism_plot <- num_char_hist_fac(
        p_size, 
        "culmen_length_mm", 
        "sex", 
        colour_sex, 
        colour_sex_fit, 
        "Distribution of Culmen Length by Sex and Species", 
        "Culmen Length (mm)", 
        "Density", 
        "Sex", 
        facet_by = "species"
      )
      
    } else if (input$num_var == "Culmen Depth (mm)") {
      # General distribution
      general_plot <- ggplot(p_size, aes(x = culmen_depth_mm)) + 
        geom_histogram(bins = 30, fill = "skyblue3", color = "black") + 
        labs(title = "General Distribution of Culmen Depth", x = "Culmen Depth (mm)", y = "Frequency") +
        theme_minimal()
      
      # Custom visualization 
      custom_plot <- num_char_hist(p_size, "culmen_depth_mm", "species", colour_species, colour_species_fit, 
                                   "Distribution of Culmen Depth", "Culmen Depth (mm)", "Density", "Species")
      dimorphism_plot <- num_char_hist_fac(
        p_size, 
        "culmen_depth_mm", 
        "sex", 
        colour_sex, 
        colour_sex_fit, 
        "Distribution of Culmen Depth by Sex and Species", 
        "Culmen Depth (mm)", 
        "Density", 
        "Sex", 
        facet_by = "species"
      )
      
    } else if (input$num_var == "Flipper Length (mm)") {
      # General distribution
      general_plot <- ggplot(p_size, aes(x = flipper_length_mm)) + 
        geom_histogram(bins = 30, fill = "skyblue3", color = "black") + 
        labs(title = "General Distribution of Flipper Length", x = "Flipper Length (mm)", y = "Frequency") +
        theme_minimal()
      
      # Custom visualization 
      custom_plot <- num_char_hist(p_size, "flipper_length_mm", "species", colour_species, colour_species_fit, 
                                         "Distribution of Flipper Length", "Flipper Length (mm)", "Density", "Species")
      dimorphism_plot <- num_char_hist_fac(
        p_size, 
        "flipper_length_mm", 
        "sex", 
        colour_sex, 
        colour_sex_fit, 
        "Distribution of Flipper Length by Sex and Species", 
        "Flipper Length (mm)", 
        "Density", 
        "Sex", 
        facet_by = "species"
      )
    }
    
    # Final layout
    gridExtra::grid.arrange(
      gridExtra::arrangeGrob(general_plot, custom_plot, ncol = 2),
      dimorphism_plot,
      nrow = 2
    )
  })
                          
  # Update interpretation based on the selected numerical variable
  output$num_interpretation <- renderText({
    req(input$num_var)
    
    if (input$num_var == "Body Mass (g)") {
      "The body mass distribution across all species shows a central tendency but is not strictly Gaussian. When colored by species, we observe distinct clustering. Gentoo penguins tend to have a higher average body mass compared to Adélie and Chinstrap, indicating significant interspecies variation. Average body mass order: Chinstrap < Adélie < Gentoo. Within each species, male penguins exhibit higher average body mass than females, indicating clear sexual dimorphism in this trait."
      
    } else if (input$num_var == "Culmen Length (mm)") {
      "The culmen length distribution reveals a central tendency but is distinctly multimodal, suggesting the presence of multiple dominant groups. When grouped by species, we see that Adélie penguins, on average, have the shortest culmen lengths, Gentoo are intermediate, and Chinstrap have the longest. This highlights species-specific adaptations. Average culmen length order: Adélie < Gentoo < Chinstrap. Within each species, male penguins exhibit higher average culmen lenth than females, indicating clear sexual dimorphism in this trait."
      
    } else if (input$num_var == "Culmen Depth (mm)") {
      "The culmen depth distribution is slightly skewed across the entire dataset. Grouping by species reveals that Gentoo penguins tend to have the shallowest culmen depth, while Adélie and Chinstrap show a almost similar average depth. Average culmen depth order: Gentoo < Adélie < Chinstrap. Within each species, male penguins exhibit higher average culmen depth than females, indicating clear sexual dimorphism in this trait."
      
    } else if (input$num_var == "Flipper Length (mm)") {
      "The flipper length distribution shows a central tendencybut is distinctly multimodal, suggesting the presence of multiple dominant groups. When grouped by species, we observe that Adélie penguins generally have shorter flippers, Chinstrap have slightly longer flippers, and Gentoo have the longest. This is indicative of species-specific ecological or functional adaptations. Average flipper length order: Adélie < Chinstrap < Gentoo. Within each species, male penguins exhibit higher average flipperlength than females, indicating clear sexual dimorphism in this trait."
    }
  })
  
  
  # Categorical variable plot
  
  output$cat_plot <- renderPlot({
    req(input$cat_var)
    if(input$cat_var == "Sex") {
      p3 <- bar_plot_fill(p_size, p_size$sex, p_size$sex, colour_sex, 
                          "Distribution of Sexes", 
                          "Sex", "Sex")
      print(p3)
    } else if(input$cat_var == "Species") {
      p1 <- bar_plot_fill(p_size, p_size$species, p_size$sex, colour_sex, 
                          "Species Distribution With Gender Representation", 
                          "Species", "Sex")
      print(p1)
    } else if(input$cat_var == "Island") {
      p2 <- bar_plot_fill(p_size, p_size$island, p_size$species, colour_species, 
                          "Species Distribution Across Islands", 
                          "Islands", "Species")
      print(p2)
    }
  })
  
  # Update interpretation based on the selected categorical variable
  output$cat_interpretation <- renderText({
    req(input$cat_var)
    if(input$cat_var == "Sex") {
      "The sex distribution is relatively balanced with equal numbers of male and female penguins in sample."
    } else if(input$cat_var == "Species") {
      "The majority of penguins belong to the Adélie species, followed by Gentoo and Chinstrap. 
      The sex ratio across species is nearly 1:1."
    } else if(input$cat_var == "Island") {
      "Most penguins were observed on Biscoe Island, followed by Dream and Torgersen Islands.
      Adélie species is present on all three islands, where Chinstrap and Gentoo penguins are
      only found on Dream and biscoe island as per observation of our sample."
    }
  })
  # Render the Correlation Heatmap
  output$corr_plot <- renderPlot({
    # Assuming `p_lter` is already loaded and contains numeric data
    
    # Calculate the correlation matrix
    cor_matrix <- cor(p_lter[, sapply(p_lter, is.numeric)], use = "pairwise.complete.obs")
    
    ggcorrplot(cor_matrix, 
               lab = TRUE, 
               colors = c("red", "white", "blue"), 
               title = "Correlation Heatmap") + 
      theme(plot.title = element_text(size = 10, hjust = 0.5),
            axis.title.x = element_text(size = 8),         # Reduce x-axis label font size
            axis.title.y = element_text(size = 8),         # Reduce y-axis label font size
            axis.text.x = element_text(size = 8),          # Reduce x-axis tick mark font size
            axis.text.y = element_text(size = 8),
            legend.title = element_text(size = 8),         # Reduce legend title font size
            legend.text = element_text(size = 6),
            panel.grid.major = element_blank(),            # Remove major grid lines
            panel.grid.minor = element_blank(),            # Remove minor grid lines
            panel.background = element_rect(fill = "white", color = NA))  # Optional: Set background to white
    
  })
  # Server code for scatter plot matrix
  output$matrix_scatter_plot <- renderPlot({
    req(input$matrix_var_x, input$matrix_var_y) # Ensure inputs are selected
    
    # Mapping input names to actual column names in the dataset
    var_x <- input$matrix_var_x
    var_y <- input$matrix_var_y
    
    # Function to map input names to actual column names in the dataset
    map_var_name <- function(var) {
      switch(var,
             "Body Mass (g)" = "body_mass_g",
             "Culmen Length (mm)" = "culmen_length_mm",
             "Culmen Depth (mm)" = "culmen_depth_mm",
             "Flipper Length (mm)" = "flipper_length_mm")
    }
    
    # Get the mapped column names
    var_x_col <- map_var_name(var_x)
    var_y_col <- map_var_name(var_y)
    
    # Generate scatter plot using the selected variables
    var1_scatter_var2(p_size, var_x_col, var_y_col,
                      paste(var_x, "vs.", var_y),
                      paste(var_x, "(mm)"), paste(var_y, "(mm)"))
  })
  
  
  # Render Simpson's Paradox Plots (5th Tab)
  output$paradox_plot <- renderPlot({
    req(input$paradox_choice)
    
    if (input$paradox_choice == "Body Mass vs. Flipper Length") {
      # Plot with facets
      flip_len_scatter_body_mass_facet <- var1_scatter_var2_facet(
        p_size, "flipper_length_mm", "body_mass_g", "sex", "species", 
        colour_sex, colour_sex_fit, 
        "Body Mass vs. Flipper Length by Sex and Species", 
        "Flipper Length (mm)", "Body mass (g)", "Sex"
      )
      
      # Overall scatter plot
      flip_len_scatter_body_mass <- var1_scatter_var2(
        p_size, "flipper_length_mm", "body_mass_g", 
        "Body Mass vs. Flipper Length", 
        "Flipper Length (mm)", "Body mass (g)"
      )
      
      # Arrange plots side-by-side
      gridExtra::grid.arrange( flip_len_scatter_body_mass_facet, flip_len_scatter_body_mass, ncol = 2)
      
    } else if (input$paradox_choice == "Culmen Depth vs. Flipper Length") {
      # Plot with facets
      flip_len_scatter_cul_dep_facet <- var1_scatter_var2_facet(
        p_size, "flipper_length_mm", "culmen_depth_mm", "sex", "species", 
        colour_sex, colour_sex_fit, 
        "Culmen Depth vs. Flipper Length by Sex and Species", 
        "Flipper Length (mm)", "Culmen Depth (mm)", "Sex"
      )
      
      # Overall scatter plot
      flip_len_scatter_cul_dep <- var1_scatter_var2(
        p_size, "flipper_length_mm", "culmen_depth_mm", 
        "Culmen Depth vs. Flipper Length", 
        "Flipper Length (mm)", "Culmen Depth (mm)"
      )
      
      # Arrange plots side-by-side
      gridExtra::grid.arrange(flip_len_scatter_cul_dep_facet, flip_len_scatter_cul_dep, ncol = 2)
    }
  })
  
  # Update interpretation based on the selected paradox analysis
  output$paradox_interpretation <- renderText({
    req(input$paradox_choice)
    
    if (input$paradox_choice == "Body Mass vs. Flipper Length") {
      "The relationship between Body Mass and Flipper Length shows a consistent positive correlation across the entire dataset. This positive trend persists even when the data is grouped by sex and species, indicating that the correlation holds across different subgroups."   
    } else if (input$paradox_choice == "Culmen Depth vs. Flipper Length") {
      "When analyzing Culmen Depth versus Flipper Length, the overall trend suggests a negative correlation. However, two distinct clusters become apparent, which, when examined individually, show a positive correlation between the variables. Further grouping by species reveals varying trends within subgroups, demonstrating Simpson's Paradox. This highlights the importance of subgroup analysis in uncovering hidden patterns and complexities in relationships between variables."   }
  })
  
}

# Run the application 
shinyApp(ui, server)
