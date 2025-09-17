library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# ---- Population generation ----
set.seed(200200)
generate_population <- function(n_fish = 40, n_jelly = 40, n_whale = 5, n_turtle = 30) {
	
	depth_fish <- runif(n_fish, 0, 10)  
	depth_jelly <- runif(n_jelly, 0, 10)
	depth_whale <- runif(n_whale, 0, 6.9)
	depth_turtle <- runif(n_turtle, 0, 10)
	
	depth_all <- c(depth_fish, depth_jelly, depth_whale, depth_turtle)
	
	data.frame(
		species = c(
			rep("Fish", n_fish),
			rep("Jellyfish", n_jelly),
			rep("Whale", n_whale),
			rep("Turtle", n_turtle)
		),
		x = runif(n_fish + n_jelly + n_whale + n_turtle, 0, 10),
		y = runif(n_fish + n_jelly + n_whale + n_turtle, 0, 10),
		depth = depth_all
	)
}

# ---- Emoji mapping ----
emoji_map <- c(
	"Fish"       = "🐠",
	"Jellyfish"  = "🪼",
	"Whale"      = "🐋",
	"Turtle"     = "🐢"
)

# ---- UI ----
ui <- fluidPage(
	sidebarLayout(
		sidebarPanel(
			selectInput("sample_type", "Sampling Type:",
									choices = c("SRS", "Stratified", "Cluster", "Convenience")),
			
			# Dynamic UI for slider or cluster number
			uiOutput("sampling_options"),
			
			actionButton("draw_sample", "SAMPLE", class = "btn-primary btn-lg"),
			br(), br(),
			
			# Description
			HTML("
Imagine you are acting as a data scientist working with a marine biologist to study sea creatures! 🐠🪼🐋🐢<br><br>
Your goal is to collect a sample of sea creatures and see how well it represents the full population.<br><br>
There are four ways you can sample:<br>
<b>Simple Random Sampling (SRS):</b> randomly selects sea creatures from the entire ocean.<br>
<b>Stratified Sampling:</b> ensures each species is proportionally represented by sampling within each species group.<br>
<b>Cluster Sampling:</b> divides the ocean into spatial quadrants and takes a census within the selected cluster(s).<br>
<b>Convenience Sampling:</b> samples only from shallower areas accessible with your equipment.<br><br>
Adjust the sample size or number of clusters, select a sampling method, and click 'SAMPLE' to see how your sample compares to the population.
")
		),
		
		mainPanel(
			plotOutput("oceanPlot", height = "600px"),
			br(),
			plotOutput("barPlot", height = "300px"),
			br(),
			p("Acknowledgement: Inspired by", 
				a("https://utrecht-university.shinyapps.io/cj_shiny_sampling/", 
					href="https://utrecht-university.shinyapps.io/cj_shiny_sampling/", 
					target="_blank"))
		)
	)
)

# ---- Server ----
server <- function(input, output, session) {
	
	# Dynamic UI for slider or cluster number
	output$sampling_options <- renderUI({
		if (input$sample_type == "Cluster") {
			selectInput("n_clusters", "Number of Clusters:", choices = 1:4, selected = 1)
		} else {
			sliderInput("sample_size", "Sample Size:", min = 1, max = 50, value = 10)
		}
	})
	
	# Generate population once
	population <- generate_population()
	
	# Reactive sampled data
	sampled <- eventReactive(input$draw_sample, {
		type <- input$sample_type
		
		if (type == "SRS") {
			n <- input$sample_size
			sample_n(population, min(n, nrow(population)))
			
		} else if (type == "Stratified") {
			n <- input$sample_size
			population |>
				group_by(species) |>
				sample_frac(n / nrow(population)) |>
				ungroup()
			
		} else if (type == "Cluster") {
			n_clusters <- as.numeric(input$n_clusters)
			
			# assign clusters (quadrants)
			population$cluster <- with(population,
																 paste(ifelse(x <= 5, "Left", "Right"),
																 			ifelse(y <= 5, "Bottom", "Top")))
			clusters <- unique(population$cluster)
			
			# select clusters
			selected_clusters <- sample(clusters, n_clusters)
			population |> filter(cluster %in% selected_clusters)
			
		} else if (type == "Convenience") {
			n <- input$sample_size
			convenience_creatures <- population |> filter(y > 7)
			sample_n(convenience_creatures, min(n, nrow(convenience_creatures)))
		}
	})
	
	# ---- Ocean plot ----
	output$oceanPlot <- renderPlot({
		
		# Wavy ocean top
		wave_x <- seq(0, 10, length.out = 200)
		wave_y <- 12.3 + 0.5*sin(wave_x * pi)
		ocean_poly <- data.frame(
			x = c(0, wave_x, 10),
			y = c(0, wave_y, 0)
		)
		
		p <- ggplot(population, aes(x=x, y=y)) +
			geom_polygon(data=ocean_poly, aes(x=x, y=y), fill="lightblue", color=NA, alpha=0.5) +
			geom_text(aes(label = emoji_map[species]), size=6, alpha = 0.3) +
			theme_minimal() +
			theme(panel.background = element_rect(fill = "white"),
						panel.grid = element_blank(),
						axis.title = element_blank(),
						axis.text = element_blank(),
						axis.ticks = element_blank(),
						legend.text = element_text(size=14),
						legend.title = element_blank()) +
			xlim(0,10) + ylim(0,13)
		
		# Overlay sampled points
		if (input$draw_sample > 0) {
			s <- sampled()
			if (!is.null(s) && nrow(s) > 0) {
				p <- p + geom_text(data=s, aes(label = emoji_map[species]), size=14)
			}
			
			# Cluster quadrant highlighting
			if (input$sample_type == "Cluster") {
				population$cluster <- with(population,
																	 paste(ifelse(x <= 5, "Left", "Right"),
																	 			ifelse(y <= 5, "Bottom", "Top")))
				selected_clusters <- unique(s$cluster)
				
				for (cl in selected_clusters) {
					p <- p + geom_vline(xintercept = 5, linetype="dashed") +
						geom_hline(yintercept = 5, linetype="dashed")
				}
			}
			
			# Convenience line
			if (input$sample_type == "Convenience") {
				cutoff <- 7
				p <- p + geom_hline(yintercept = cutoff, linetype="dashed", color="black")
			}
		}
		
		p
	})
	
	# ---- Side-by-side bar chart ----
	output$barPlot <- renderPlot({
		if (input$draw_sample == 0) return(NULL)
		
		sampled_data <- sampled()
		req(sampled_data)
		
		population_counts <- population |>
			count(species) |>
			mutate(type="Population")
		sample_counts <- sampled_data |>
			count(species) |>
			mutate(type="Sample")
		
		combined <- bind_rows(population_counts, sample_counts)
		
		# Combine emoji and species name
		combined <- combined |>
			mutate(species_label = paste0(emoji_map[species], " ", species))
		
		ggplot(combined, aes(x = species_label, y = n, fill = type)) +
			geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
			coord_flip() +
			theme_minimal() +
			theme(
				axis.title = element_blank(),
				axis.text = element_text(size = 14),
				legend.text = element_text(size = 14),
				legend.title = element_blank()
			) +
			scale_fill_manual(values = c("Population" = "#00C5CD", "Sample" = "#EEAEEE"))

	})
	
}

# ---- Run App ----
shinyApp(ui, server)
