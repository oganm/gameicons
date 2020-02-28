
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gameicons

<!-- badges: start -->

<!-- badges: end -->

This package adds a single function to use icons from
[game-icons.net](https://game-icons.net/) in shiny applications or RMD
files that are rendered to html. Most of these icons are licensed under
[CC BY 3.0](https://creativecommons.org/licenses/by/3.0/). See the pages
for the individual icons to see who you should credit. The font files
are taken from
[seiyria/gameicons-font](https://github.com/seiyria/gameicons-font/).

## Installation

``` r
devtools::install_github('oganm/gameicons')
```

## Usage

``` r
game_icon('swordman')
```

![](man/figures/swordman.png)

``` r
htmltools::html_print(tagList(shiny::actionButton('meh','meh',icon = game_icon('swordman'))))
```

![](man/figures/sword_button.png)

You can list available icons with `list_icons`. This function needs the
`stringr` package which is only included in `Enchances` so it won’t be
installed automatically on pakcage installation.
