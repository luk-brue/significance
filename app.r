library(shiny)
library(ggplot2)

# User Interface

ui <- fluidPage(
    titlePanel("Signifikanz-Slider"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "alpha",
                label = "Signifikanzniveau (einseitig)",
                min = 0.001,
                max = 0.15,
                value = 0.05,
                step = 0.001
            ),
            sliderInput(
                inputId = "delta",
                label = "effect size",
                min = 0,
                max = 3,
                value = 2.5,
                step = 0.01
            ),
            sliderInput(
                inputId = "sd",
                label = "Standardabweichung",
                min = 0.8,
                max = 3,
                value = 1,
                step = 0.01
            )
        ),
        mainPanel(
            plotOutput(
                outputId = "p"
            ),
            tableOutput(
                outputId = "power"
            )
        ),
    )
)

### Function definitions

# Helper for shading the area under normal dist. curve
.pshade <- function(x, min, max, mean, sd) {
    y <- dnorm(x = x, mean = mean, sd = sd)
    y[x < min  |  x > max] <- NA
    return(y)
}

# Plotting function
.shade_plot <- function(z_krit = 1.65, mean_a = 0, mean_b = 2.5, sd = 1, xlim = 10){
    ggplot() +
    stat_function(geom = "area",
                fill = 7,
                alpha = .4,
                fun = .pshade, 
                args = list(min = -4, max = z_krit, mean = mean_b, sd = sd),
                n = 800) +
    stat_function(geom = "area",
                fill = "4",
                alpha = .4,
                fun = .pshade, 
                args = list(min = z_krit, max = xlim, mean = mean_a, sd = sd),
                n = 800) +
    stat_function(geom = "area",
                fill = "2",
                alpha = .4,
                fun = .pshade, 
                args = list(min = mean_b, max = xlim, mean = mean_a, sd = sd),
                n = 800) +
    geom_vline(xintercept = c(0, mean_b), color = c(4, 7), linetype = "dashed") +
    geom_vline(xintercept = z_krit, color = "red" ) +
    geom_text(aes(x = mean_a, y = 0.42), 
            label = "kein Effekt") +
    geom_text(aes(x = mean_b, y = 0.45), 
            label = "beobachteter\nEffekt") +
    # geom_text(aes(x = mean_b + 0.15, y = - (mean_a + 0.01)), 
    #           label = "p") +
    # geom_text(aes(x = z_krit, y = - (mean_a + 0.01)), 
    #           label = "alpha") +
    # geom_text(aes(x = 1.2, y = 0.06), 
    #           label = "beta") +
    geom_function(fun = dnorm, args = list(mean = mean_a, sd = sd)) +
    geom_function(fun = dnorm, args = list(mean = mean_b, sd = sd)) +
    xlim(c(-4, xlim)) +
    ylim(c(-0.05, 0.5)) +
    theme_void()
}

# Calculation of p value, power und beta
.power <- function(alpha = 0.05, mean_a = 0, mean_b = 2.5, sd = 1, n){
    z_krit <- qnorm(1 - alpha, mean = mean_a, sd = sd)
    beta <- pnorm(z_krit, mean = mean_b, sd = sd)
    p <- 1 - pnorm(mean_b, mean = mean_a, sd = sd)
    output <- data.frame(p = p, beta = beta, power = 1 - beta)
    return(round(output, digits = 4))
}

### Server logic

server <- function(input, output){
    output$p <- renderPlot(
        {
            z_krit <- qnorm(1 - input$alpha, mean = 0, sd = input$sd)
            delta <- input$delta
            sd <- input$sd

             .shade_plot(z_krit = z_krit, mean_a = 0, mean_b = delta, sd = sd) 
        }
    )
    output$power <- renderTable(
        .power(alpha = input$alpha, mean_b = input$delta, sd = input$sd)
    )
}

# obligatory to create the output
shinyApp(ui, server)
