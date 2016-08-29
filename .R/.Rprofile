# -*- coding: utf-8 -*-

# Disable annoying X11 popup
options(menu.graphics=FALSE)

# Choose server
options(download.file.method = "libcurl")
options(repos=c(CRAN="https://cloud.r-project.org/"))
#options(repos=c(CRAN="https://cran.rstudio.com/"))
#options(repos=c(CRAN="https://cran.ism.ac.jp/"))

# Show more warnings
options(warn=1,
        warnPartialMatchArgs=TRUE,
        warnPartialMatchAttr=TRUE,
        warnPartialMatchDollar=TRUE)

# Show a summary of the call stack
options(showWarnCalls=TRUE, showErrorCalls=TRUE)

# Maximum number of rows to print(tbl_df)
options(tibble.print_max=30L)

# Number of rows to print(tbl_df) if exceeded the maximum
options(tibble.print_min=30L)

# Suppress annoying function
options(readr.num_columns=0L)

# Font
setHook(packageEvent("grDevices", "onLoad"), function(...) {
    grDevices::pdfFonts(
        serif= grDevices::pdfFonts()$Palatino,
        mincho= grDevices::pdfFonts()$Japan1,
        gothic= grDevices::pdfFonts()$Japan1GothicBBB
    )
    grDevices::pdf.options(family="serif")
    if (capabilities("aqua")) {
        .styles = c('', ' Bold', ' Italic', ' Bold Italic')
        grDevices::quartzFonts(
            serif=  grDevices::quartzFont(
                    paste0('Noto Serif', .styles)),
            sans=   grDevices::quartzFont(
                    paste0('Source Sans Pro', .styles)),
            mono=   grDevices::quartzFont(
                    paste0('Ubuntu Mono', .styles)),
            mincho= grDevices::quartzFont(
                    paste0("Hiragino Mincho ProN W", c(3, 6, 3, 6))),
            gothic= grDevices::quartzFont(
                    paste0("Hiragino Kaku Gothic ProN W", c(3, 6, 3, 6)))
        )
        grDevices::quartz.options(family='sans')  # does not work
        attach(NULL, name="QuartzEnv")
        assign("set_family", function() {
            if (names(dev.cur()) == "quartz") {par(family="sans")}
        }, pos="QuartzEnv")
        setHook("plot.new", get("set_family", pos="QuartzEnv"))
    }
})

setHook(packageEvent("extrafont", "attach"), function(...) {
    tryCatch({
        grDevices::pdfFonts(
          sans= grDevices::pdfFonts()$`Source Sans Pro`,
          mono= grDevices::pdfFonts()$`Ubuntu Mono`)
    }, error=warning)
})

.adjust_width = function(width=Sys.getenv("COLUMNS")) {
    if (width == '') {
        if (Sys.getenv("RSTUDIO") == "1") {return()}
        stty = system("stty -a", intern=TRUE, ignore.stderr=TRUE)[1]
        if (is.na(stty)) {return()}
        colmuns = grep("columns", unlist(strsplit(stty, ";")), value=TRUE)
        width = grep("\\d+", unlist(strsplit(colmuns, " ")), value=TRUE)
    }
    options(width=width)
    if (interactive()) {cat('width:', getOption('width'), '\n')}
}

.First = function() {
    if (interactive()) {
        cran = c('pipeR', 'plyr', 'dplyr',
            'readr', 'tidyr', 'purrr', 'tibble',
            'stringr', 'ggplot2', 'grid', 'devtools', 'extrafont')
        github = c('wtl')
        options(defaultPackages=c(getOption('defaultPackages'), cran, github))
        if (.Platform$GUI != 'AQUA' && Sys.getenv('EMACS') == '') {
            utils::loadhistory(file=Sys.getenv('R_HISTFILE'))
        }
        print(utils::sessionInfo(), locale=FALSE)
        cat(date(), '\n')
        cat(getwd(), '\n')
        cat('Loading:', cran, github, '\n')
    }
    .adjust_width()
}

.Last = function() {try({
    if (interactive()) {
        if (.Platform$GUI != 'AQUA' && Sys.getenv('EMACS') == '') {
            utils::savehistory(file=Sys.getenv('R_HISTFILE'))
        }
        print(ls(envir=globalenv(), all.names=TRUE))
        print(utils::sessionInfo(), locale=FALSE)
    }
})}

# Bioconductor
.library_bioc = function(
  mirror=c('bioc.ism.ac.jp', 'bioconductor.riken.jp', 'bioconductor.org'),
  ask=FALSE) {
    mirror = match.arg(mirror)
    options(BioC_mirror=sprintf('https://%s/', mirror))
    source(sprintf('https://%s/biocLite.R', mirror))
    biocLite(ask=ask)
}
