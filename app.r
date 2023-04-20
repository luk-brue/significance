library(shiny)
library(ggplot2)

# User Interface

ui <- fluidPage(
    titlePanel("Signifikanz-Slider"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "alpha",
                label = "Signifikanzniveau",
                min = 0.001,
                max = 0.1,
                value = 0.05,
                step = 0.001
            ),
            selectInput(
                inputId = "direction",
                label = "Testrichtung",
                choices = list(
                    "beidseitig",
                    "einseitig (rechts)",
                    "einseitig (links)"),
                selected = "einseitig (rechts)"
            ),
            numericInput(
                inputId = "mu_0",
                label = "Erwartungswert",
                value = 0),

            numericInput(
                inputId = "mu_1",
                label = "Stichprobenmittelwert",
                value = 0.5,
                step = 0.25
            ),
            sliderInput(
                inputId = "sd",
                label = "Populations-Standardabweichung",
                value = 1,
                min = 0,
                max = 50,
                step = 0.01,
            ),
            sliderInput(
                inputId = "n",
                label = "Stichprobengröße n",
                min = 30,
                max = 1000,
                value = 43,
                step = 1
            )
        ),
        mainPanel(
            h3("z-Test"),
            p("Vorraussetzung: Erwartungswert und Varianz einer Population 
            sind bekannt"),
            p("Wie wahrscheinlich ist es, den vorliegenden Stichprobenmittelwert
             aus dieser Population zu ziehen?"),
            p("Annahme der Varianzhomogenität (Stichprobe hat die gleiche
            Varianz wie Population)"),
            
            plotOutput(
                outputId = "p"
            ),
            tableOutput(
                outputId = "power"
            ),
            h5("Interpretation von Cohen's d (1988)"),
            tableOutput(
                outputId = "cohensd"
            )

        ),
    )
)

### Function definitions

# Standard error

.se <- function(sd, n) {
    sd / sqrt(n)
}

# Effect Size
.delta <- function(mu_0, mu_1, population_sd) {
    (mu_1 - mu_0) / population_sd
}

.mu_1 <- function(delta, mu_0, population_sd) {
    .delta * population_sd + mu_0
}

# Helper for shading the area under normal dist. curve
.pshade <- function(x, min, max, mean, se) {
    y <- dnorm(x = x, mean = mean, sd = se)
    y[x < min  |  x > max] <- NA
    return(y)
}

# Plotting function
.shade_plot <- function(
    alpha = 0.05, 
    mu_0 = 0, 
    mu_1 = 2.5, 
    se = 1, 
    xlim = c(mu_0 - 5 * se, mu_1 + 5 * se),
    ylim = dnorm(mu_1, mean = mu_1, sd = se) * c(-0.025 , 1.15),
    detail = 800) {
        z_krit <- qnorm(1 - alpha, mean = mu_0, sd = se)
    
        ggplot() +
        stat_function(geom = "area",
                fill = 7,
                alpha = .4,
                fun = .pshade, 
                args = list(min = xlim[1], max = z_krit, mean = mu_1, se = se),
                n = detail) +
        stat_function(geom = "area",
                fill = "4",
                alpha = .4,
                fun = .pshade, 
                args = list(min = z_krit, max = xlim[2], mean = mu_0, se = se),
                n = detail) +
        stat_function(geom = "area",
                fill = "2",
                alpha = .4,
                fun = .pshade, 
                args = list(min = mu_1, max = xlim[2], mean = mu_0, se = se),
                n = detail) +
        geom_vline(xintercept = c(mu_0, mu_1), color = c(4, 7), linetype = "dashed") +
        geom_vline(xintercept = z_krit, color = "red" ) +
        geom_text(aes(x = mu_0, y = 0.91 * ylim[2]), 
            label = "kein Effekt") +
        geom_text(aes(x = mu_1, y = 0.98 * ylim[2]), 
            label = "beobachteter\nEffekt") +
        geom_function(fun = dnorm, args = list(mean = mu_0, sd = se), n = detail) +
        geom_function(fun = dnorm, args = list(mean = mu_1, sd = se), n = detail) +
        xlim(xlim) +
        ylim(ylim) +
        theme_bw() +
        ylab("Wahrscheinlichkeitsdichte") +
        xlab("Mittelwerte")
    }

# Calculation of effect size, p value, power und beta
.power <- function(alpha = 0.05, mu_0 = 0, mu_1= 2.5, se = 1, n = 100, population_sd = 10){
    z_krit <- qnorm(1 - alpha, mean = mu_0, sd = se)
    beta <- pnorm(z_krit, mean = mu_1, sd = se)
    p <- 1 - pnorm(mu_1, mean = mu_0, sd = se)
    delta <- .delta(mu_0, mu_1, population_sd)
    output <- tibble::tibble(
        "Cohen's d" = delta,
        "p-Wert" = p, 
        "Beta-Fehler" = beta,
        "Power" = 1 - beta
        )
    return(output)
}

### Server logic

server <- function(input, output){
    output$p <- renderPlot(
        {
            se <- .se(input$sd, input$n)

            .shade_plot(
                alpha = input$alpha,
                mu_0 = input$mu_0,
                mu_1 = input$mu_1,
                se = se)
        }
    )
    output$power <- renderTable(
        .power(
            alpha = input$alpha,
            mu_0 = input$mu_0,
            mu_1 = input$mu_1,
            se = .se(sd = input$sd, n = input$n),
            population_sd = input$sd
            ),
            digits = 3
    )
    output$cohensd <- renderTable(
        tibble::tibble(
            "kleiner Effekt" = "<----|0.2|---->",
            "mittlerer Effekt" = "<----|0.5|---->",
            "großer Effekt" = "<----|0.8|---->" 
            )
    )
    
}

# obligatory to create the output
shinyApp(ui, server)
