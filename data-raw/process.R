devtools::load_all()
library(gh)
library(magrittr)
library(dplyr)


token = readLines('data-raw/auth')
Sys.setenv(GITHUB_PAT = token)


unlink('inst/game-icons',recursive = TRUE)
system('svn checkout https://github.com/seiyria/gameicons-font/trunk/dist/')
system('mv dist inst/game-icons/')


iconNames = readLines(system.file('game-icons/game-icons.css',package = 'gameicons')) %>%
	paste0(collapse = '\n') %>% stringr::str_extract_all('(?<=game-icon-).*?(?=:)') %>% {.[[1]]}
iconNames = data.frame(icon = iconNames,stringsAsFactors = FALSE)





download.file('http://game-icons.net/archives/svg/zip/000000/transparent/game-icons.net.svg.zip',destfile = 'icons.zip')
unzip('icons.zip',exdir = 'iconimages')
imageFiles = list.files('iconimages/',recursive = TRUE,full.names = TRUE,include.dirs = FALSE) %>% file.info()

imageFiles %<>% tibble::rownames_to_column('file')


imageFiles %<>% mutate(icon = basename(file) %>% gsub('.svg','',.,fixed = TRUE),
					   authorMin = basename(dirname(file)))
imageFiles %<>% arrange(ctime)

i = 2
while(TRUE){
	imageFiles$icon[imageFiles$icon %>% duplicated] = paste0(imageFiles$icon[imageFiles$icon %>% duplicated],'-',i)
	if(!any(imageFiles$icon %>% duplicated)){
		break
	}
	i = i + 1

}




mergeTable = left_join(iconNames,imageFiles)



# get license information
licenseFile = readLines('https://raw.githubusercontent.com/game-icons/icons/master/license.txt')


authorLines = licenseFile %>% grepl('^-',.)


licenseAuthors = licenseFile[authorLines] %>% strsplit(',|-') %>% purrr::map_chr(2) %>% trimws()


nonStandardLicense = licenseFile[authorLines] %>% grepl('CC0',.)


licenseData = data.frame(author = c(licenseAuthors,'various-artists'), license = 'CC BY 3.0',stringsAsFactors = FALSE)

licenseData$license[nonStandardLicense] = 'CC0'

licenseData %<>% mutate(authorMin = tolower(author) %>% gsub(' ','-',.))

# some manual fixes
licenseData$authorMin[licenseData$authorMin == 'lucas'] = 'lucasms'
licenseData$authorMin[licenseData$authorMin == 'heavenlydog'] = 'heavenly-dog'
licenseData$authorMin[licenseData$authorMin == 'andy-meneely'] = 'andymeneely'


mergeTable2 = left_join(mergeTable,licenseData)

iconTable = mergeTable2 %>% select(icon, author, license)


# iconTable %>% filter(icon %in% iconNames$icon) %>% dim
usethis::use_data(iconTable,overwrite = TRUE)
readr::write_tsv(iconTable,'data-raw/iconTable.tsv')

