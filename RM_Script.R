data("mtcars")
str(mtcars)

static_help <- function(pkg, topic, out, links = tools::findHTMLlinks()) {
        pkgRdDB = tools:::fetchRdDB(file.path(find.package(pkg), 'help', pkg))
        force(links)
        tools::Rd2latex(pkgRdDB[[topic]], out, package = pkg,
                        Links = links, no_links = is.null(links))
}
tmp <- tempfile()
static_help("datasets", "mtcars", tmp)
out <- readLines(tmp)
out <- gsub("%", "\n", out, fixed = TRUE)
knitr::kable(out[21:31], align = 'l', col.names = "       Variable - Description")

library(ggplot2)
ggplot(mtcars, aes(x=factor(am, labels = c("automatic", "manual")), y=mpg)) + geom_boxplot() +xlab("transmission") + ylab("MPG")

test <- t.test(mpg ~ am, data= mtcars, var.equal = FALSE, paired=FALSE ,conf.level = .95)
test$p.value

library(GGally)
lowerFn <- function(data, mapping, method = "lm", ...) {
        p <- ggplot(data = data, mapping = mapping) +
             geom_point(colour = "blue") +
             geom_smooth(method = method, color = "red", ...)
        p
}
ggpairs(mtcars, lower = list(continuous = wrap(lowerFn, method = "lm")),
                diag = list(continuous = wrap("barDiag", colour = "blue")),
                upper = list(continuous = wrap("cor", size = 3))
)

MPG_fit <- lm(mpg ~ ., data = mtcars)
knitr::kable(summary(MPG_fit)$coeff)

selection <- step(MPG_fit, data=mtcars, k=2)
MPG_fit <- lm(mpg ~ wt + qsec + am, data = mtcars)
summary(MPG_fit)$coeff

par(mfrow = c(2, 2)); plot(MPG_fit)
