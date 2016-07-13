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
if (capabilities("aqua") && !nchar(Sys.getenv('SSH_CONNECTION'))) {
    setHook(packageEvent("grDevices", "onLoad"), function(...) {
        grDevices::quartzFonts(
            serif=grDevices::quartzFont(
                c("Noto Serif",
                  "Noto Serif Bold",
                  "Noto Serif Italic",
                  "Noto Serif Bold Italic")),
            sans=grDevices::quartzFont(
                c("Source Sans Pro",
                  "Source Sans Pro Bold",
                  "Source Sans Pro Italic",
                  "Source Sans Pro Bold Italic")),
            mono=grDevices::quartzFont(
                c("Ubuntu Mono",
                  "Ubuntu Mono Bold",
                  "Ubuntu Mono Italic",
                  "Ubuntu Mono Bold Italic")),
            mincho=grDevices::quartzFont(
                c("Hiragino Mincho ProN W3",
                  "Hiragino Mincho ProN W6",
                  "Hiragino Mincho ProN W3",
                  "Hiragino Mincho ProN W6")),
            gothic=grDevices::quartzFont(
                c("Hiragino Kaku Gothic ProN W3",
                  "Hiragino Kaku Gothic ProN W6",
                  "Hiragino Kaku Gothic ProN W3",
                  "Hiragino Kaku Gothic ProN W6"))
        )
        grDevices::ps.options(family="Japan1GothicBBB")
        grDevices::pdf.options(family="Japan1GothicBBB")
    })
    attach(NULL, name="QuartzEnv")
    assign("familyset_hook",
        function() {
            if (names(dev.cur()) == "quartz") {par(family="sans")}
        }, pos="QuartzEnv")
    setHook("plot.new", get("familyset_hook", pos="QuartzEnv"))
}

.adjust_width = function(width=Sys.getenv("COLUMNS")) {
    if (width == '') {
        if (Sys.getenv("RSTUDIO") == "1") {return()}
        stty = system("stty -a", intern=TRUE, ignore.stderr=TRUE)[1]
        if (is.na(stty)) {return()}
        colmuns = grep("columns", unlist(strsplit(stty, ";")), value=TRUE)
        width = grep("\\d+", unlist(strsplit(colmuns, " ")), value=TRUE)
    }
    options(width=width)
}

.First = function() {
    if (interactive()) {
        cran = c('pipeR', 'plyr', 'dplyr',
            'readr', 'tidyr', 'purrr', 'tibble',
            'stringr', 'ggplot2', 'grid', 'devtools')
        github = c('wtl')
        cat('Loading:', cran, github, '\n')
        options(defaultPackages=c(getOption('defaultPackages'), cran, github))
        if (.Platform$GUI != 'AQUA' && Sys.getenv('EMACS') == '') {
            utils::loadhistory(file=Sys.getenv('R_HISTFILE'))
        }
        print(utils::sessionInfo(), locale=FALSE)
        cat(date(), '\n')
        cat(getwd(), '\n')
    }
    .adjust_width()
    tryCatch({
        library(extrafont)
        # Only TTF
        #grDevices::pdfFonts(serif= grDevices::pdfFonts()$`Linux Libertine`) #BUG?
        grDevices::pdfFonts(serif= grDevices::pdfFonts()$`Noto Serif`)
        grDevices::pdfFonts(sans= grDevices::pdfFonts()$`Source Sans Pro`)
        grDevices::pdfFonts(mono= grDevices::pdfFonts()$`Ubuntu Mono`)
        grDevices::pdfFonts(mincho= grDevices::pdfFonts()$TakaoMincho)
        grDevices::pdfFonts(gothic= grDevices::pdfFonts()$TakaoGothic)
    }, error=warning)
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
