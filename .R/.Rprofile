# -*- coding: utf-8 -*-

# Disable annoying X11 popup
options(menu.graphics=FALSE)

# Choose server
options(download.file.method = "libcurl")
options(repos=c(CRAN="https://cloud.r-project.org/"))
#options(repos=c(CRAN="https://cran.rstudio.com/"))
#options(repos=c(CRAN="https://cran.ism.ac.jp/"))

# Prefer binary installation
options(pkgType=.Platform$pkgType)

# Print warning for each occurrence
options(warn=1)

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
        if (.Platform$GUI != "X11") {return()}
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
        if (.Platform$GUI != 'AQUA' && Sys.getenv('EMACS') == '') {
            utils::loadhistory(file=Sys.getenv('R_HISTFILE'))
        }
        require(devtools)
        require(pipeR)
        require(ggplot2)
        require(readr)
        require(stringr)
        require(tidyr)
        require(plyr)
        require(dplyr, warn.conflicts=FALSE)
        require(wtl)
        if (require(extrafont)) {
            # Only TTF
            #grDevices::pdfFonts(serif= grDevices::pdfFonts()$`Linux Libertine`) #BUG?
            grDevices::pdfFonts(serif= grDevices::pdfFonts()$`Noto Serif`)
            grDevices::pdfFonts(sans= grDevices::pdfFonts()$`Source Sans Pro`)
            grDevices::pdfFonts(mono= grDevices::pdfFonts()$`Ubuntu Mono`)
            grDevices::pdfFonts(mincho= grDevices::pdfFonts()$TakaoMincho)
            grDevices::pdfFonts(gothic= grDevices::pdfFonts()$TakaoGothic)
        }
        print(utils::sessionInfo(), locale=FALSE)
        cat(date(), '\n')
        cat(getwd(), '\n')
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
.library_bioc = function(domain=c('org', 'jp'), ask=FALSE) {
    domain = match.arg(domain)
    options(BioC_mirror=sprintf('https://bioconductor.%s/', domain))
    source('https://www.bioconductor.org/biocLite.R')
    biocLite(ask=ask)
}
