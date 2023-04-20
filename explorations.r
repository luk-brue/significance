library(ggplot2)

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
    alpha = 0.05, mu_0 = 0, mu_1 = 2.5, se = 1, 
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
        xlab("Stichprobenmittelwerte")
    }
.shade_plot(se = 10, mu_0 = 100, mu_1 = 120, detail = 1000)
