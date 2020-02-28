#' Create a game icon
#'
#' @param icon name of the icon
#' @param class Additional classes to customize the style of the icon.
#' @export
game_icon = function(icon, class = NULL, style = NULL){
	dependency =  htmltools::htmlDependency("game-icons",
								 "1", "game-icons", package = "gameicons",
								 stylesheet = "game-icons.css")

	iconClass = paste0('game-icon game-icon-',icon)

	if (!is.null(class)) {
		iconClass = paste(iconClass, class)
	}

	iconTag =  htmltools::tags$i(class = iconClass, style = style)
	htmltools::htmlDependencies(iconTag) = dependency
	htmltools::browsable(iconTag)

}


#' List all available icons
#'
#' Lists all available icons. Requires stringr which is only listed in Enchances
#' so it might not be installed
#' @export
list_icons = function(){
	readLines(system.file('game-icons/game-icons.css',package = 'gameicons')) %>%
		paste0(collapse = '\n') %>% stringr::str_extract_all('(?<=game-icon-).*?(?=:)')
}
