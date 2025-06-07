# Penguin Pursuits: Alice's Visual Adventure

This project analyzes the **Palmer Penguins** dataset, focusing on body size measurements of *Adélie*, *Gentoo*, and *Chinstrap* penguins. The analysis is performed in **R**, with emphasis on aesthetic data visualization and exploratory analysis *to support understanding of the biological and statistical insights within the dataset*.

Here's the link to interactive R Shiney [Dashboard](https://trishita-patra.shinyapps.io/ProjectPenguin/).
## Highlights

- Examines species-wise and sex-wise differences in physical features
- Investigates correlations between bill length, flipper length, and body mass
- Visualizes patterns using plots such as histograms, scatter plots, and box plots
- Demonstrates statistical phenomena like **Simpson’s Paradox**

## Dataset

- **Source**: [Kaggle – Palmer Archipelago Penguin Data](https://www.kaggle.com/datasets/parulpandey/palmer-archipelago-antarctica-penguin-data?resource=download)
- Originally collected by Dr. Kristen Gorman.

## Tools & Libraries Used

The project was developed using the R programming language along with the following packages:

- **Base R** – Data manipulation and scripting
- **tidyverse** – Data science toolkit (includes `dplyr`, `ggplot2`, `readr`, etc.)
- **dplyr** – Data wrangling and summary statistics
- **ggplot2** – Data visualization
- **plotly** - 3D plotting
- **mgcv** – Generalized Additive Models
- **GGally** – Enhanced pairwise plots and correlation analysis
- **gridExtra** – For arranging multiple plots in custom layouts
- **ggcorrplot** – Correlation matrix visualizations
- **knitr** – Dynamic report generation
- **shiny** – Interactive web application framework
- **shinydashboard** – Dashboard layout and UI components for Shiny apps

## Repository Structure

- `project_penguin.Rmd`: Contains code for data preprocessing, visualization, and statistical analysis.
- `project_penguin.pdf`: A PDF version of the final report generated from the `.Rmd` file.
- `myAPP/app.R`: R code for the interactive Shiny dashboard.
- `myAPP/www/`: Holds images used in the dashboard UI.
- `myAPP/*.csv`: Dataset files corresponding to the Palmer Penguins dataset.

## License
This repository is licensed under the [CC BY-NC 4.0 License](https://creativecommons.org/licenses/by-nc/4.0/).  
You may use, modify, and share this work **non-commercially**, with proper attribution.



