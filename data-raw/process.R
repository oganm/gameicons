devtools::load_all()
library(gh)
library(magrittr)

token = readLines('data-raw/auth')
Sys.setenv(GITHUB_PAT = token)

iconNames = readLines(system.file('game-icons/game-icons.css',package = 'gameicons')) %>%
	paste0(collapse = '\n') %>% stringr::str_extract_all('(?<=game-icon-).*?(?=:)') %>% {.[[1]]}
iconNames = data.frame(icon = iconNames,stringsAsFactors = FALSE)



 commits = gh("GET /repos/:owner/:repo/commits", owner = "game-icons", repo = 'icons')
 sha = commits[[1]]$sha


 lastCommit = gh("GET /repos/:owner/:repo/git/commits/:sha", owner = "game-icons", repo = 'icons', sha = sha)


 treeSha = lastCommit$tree$sha


 files = gh("GET /repos/:owner/:repo/git/trees/:sha", owner = "game-icons", repo = 'icons', sha = treeSha)


 whichDirs = files$tree %>% purrr::map_chr('type')


 authorDirs = files$tree[whichDirs == 'tree']

 authorDirs %>% lapply(function(x){
 	name = x$path
 	treeSha = x$sha

 	files = gh("GET /repos/:owner/:repo/git/trees/:sha", owner = "game-icons", repo = 'icons', sha = treeSha)
 	icons = files$tree %>% purrr::map_chr('path')
 	list(author = name,
 		 icons = icons)
 }) -> authorIcons


 authors = authorIcons %>% purrr::map_chr('author')
icons = authorIcons %>% purrr::map('icons')
names(icons) = authors


iconTable = icons %>% reshape2::melt()

iconTable$value %<>% gsub('\\.svg','',.)

names(iconTable) = c('icon','authorMin')

# i am doing this in case we have mismatches between the two data sources
mergeTable = left_join(iconNames,iconTable)


# get license information
licenseFile = readLines('https://raw.githubusercontent.com/game-icons/icons/master/license.txt')


authorLines = licenseFile %>% grepl('^-',.)


licenseAuthors = licenseFile[authorLines] %>% strsplit(',|-') %>% purrr::map_chr(2) %>% trimws()


nonStandardLicense = licenseFile[authorLines] %>% grepl('CC0',.)

nonStandardLicense = c(nonStandardLicense, 'various-artists')

licenseData = data.frame(author = c(licenseAuthors,'various-artists'), license = 'CC BY 3.0',stringsAsFactors = FALSE)

licenseData$license[nonStandardLicense] = 'CC0'

licenseData %<>% mutate(authorMin = tolower(author) %>% gsub(' ','-',.))

# some manual fixes
licenseData$authorMin[licenseData$authorMin == 'lucas'] = 'lucasms'
licenseData$authorMin[licenseData$authorMin == 'heavenlydog'] = 'heavenly-dog'
licenseData$authorMin[licenseData$authorMin == 'andy-meneely'] = 'andymeneely'


mergeTable2 = left_join(mergeTable,licenseData)

# have to manually assign the ones with same names but from different authors as the font
# doesnt distinguish between them


number2s = mergeTable2 %>% filter(is.na(license)) %$% icon %>% {.[grepl('-2',.)]} %>% stringr::str_extract('.*?(?=-2)')

iconTable %>% filter(icon == number2s[1])
game_icon('sunrise')
game_icon('sunrise-2')

mergeTable2$author[mergeTable2$icon == 'clover'] = 'Sbed'
mergeTable2$license[mergeTable2$icon == 'clover'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'clover'] = 'sbed'

mergeTable2$author[mergeTable2$icon == 'clover-2'] = 'Lorc'
mergeTable2$license[mergeTable2$icon == 'clover-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'clover-2'] = 'lorc'

###
mergeTable2$author[mergeTable2$icon == 'rss'] = 'Delapouite'
mergeTable2$license[mergeTable2$icon == 'rss'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'rss'] = 'delapouite'

mergeTable2$author[mergeTable2$icon == 'rss-2'] = 'Lorc'
mergeTable2$license[mergeTable2$icon == 'rss-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'rss-2'] = 'lorc'


###
mergeTable2$author[mergeTable2$icon == 'grenade'] = 'Sbed'
mergeTable2$license[mergeTable2$icon == 'grenade'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'grenade'] = 'Sbed'

mergeTable2$author[mergeTable2$icon == 'grenade-2'] = 'Lorc'
mergeTable2$license[mergeTable2$icon == 'grenade-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'grenade-2'] = 'lorc'

###
mergeTable2$author[mergeTable2$icon == 'shuriken'] = 'DarkZaitzev'
mergeTable2$license[mergeTable2$icon == 'shuriken'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'shuriken'] = 'darkzaitzev'

mergeTable2$author[mergeTable2$icon == 'shuriken-2'] = 'Lorc'
mergeTable2$license[mergeTable2$icon == 'shuriken-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'shuriken-2'] = 'lorc'

###
mergeTable2$author[mergeTable2$icon == 'bat'] = 'Skoll'
mergeTable2$license[mergeTable2$icon == 'bat'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'bat'] = 'skoll'

mergeTable2$author[mergeTable2$icon == 'bat-2'] = 'Delapouite'
mergeTable2$license[mergeTable2$icon == 'bat-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'bat-2'] = 'delapouite'

###
mergeTable2$author[mergeTable2$icon == 'horse-head'] = 'Delapouite'
mergeTable2$license[mergeTable2$icon == 'horse-head'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'horse-head'] = 'delapouite'

mergeTable2$author[mergeTable2$icon == 'horse-head-2'] = 'Lorc'
mergeTable2$license[mergeTable2$icon == 'horse-head-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'horse-head-2'] = 'lorc'

###
mergeTable2$author[mergeTable2$icon == 'dragon-head'] = 'Lorc'
mergeTable2$license[mergeTable2$icon == 'dragon-head'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'dragon-head'] = 'lorc'

mergeTable2$author[mergeTable2$icon == 'dragon-head-2'] = 'Faithtoken'
mergeTable2$license[mergeTable2$icon == 'dragon-head-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'dragon-head-2'] = 'faithtoken'

###
mergeTable2$author[mergeTable2$icon == 'fairy'] = 'Delapouite'
mergeTable2$license[mergeTable2$icon == 'fairy'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'fairy'] = 'delapouite'

mergeTable2$author[mergeTable2$icon == 'fairy-2'] = 'Lorc'
mergeTable2$license[mergeTable2$icon == 'fairy-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'fairy-2'] = 'lorc'

###
mergeTable2$author[mergeTable2$icon == 'hang-glider'] = 'Delapouite'
mergeTable2$license[mergeTable2$icon == 'hang-glider'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'hang-glider'] = 'delapouite'

mergeTable2$author[mergeTable2$icon == 'hang-glider-2'] = 'Skoll'
mergeTable2$license[mergeTable2$icon == 'hang-glider-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'hang-glider-2'] = 'skoll'

###
mergeTable2$author[mergeTable2$icon == 'jeep'] = 'Delapouite'
mergeTable2$license[mergeTable2$icon == 'jeep'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'jeep'] = 'delapouite'

mergeTable2$author[mergeTable2$icon == 'jeep-2'] = 'Skoll'
mergeTable2$license[mergeTable2$icon == 'jeep-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'jeep-2'] = 'skoll'

###
mergeTable2$author[mergeTable2$icon == 'distraction'] = 'DarkZaitzev'
mergeTable2$license[mergeTable2$icon == 'distraction'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'distraction'] = 'darkzaitzev'

mergeTable2$author[mergeTable2$icon == 'distraction-2'] = 'Lorc'
mergeTable2$license[mergeTable2$icon == 'distraction-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'distraction-2'] = 'lorc'

###
mergeTable2$author[mergeTable2$icon == 'swallow'] = 'Delapouite'
mergeTable2$license[mergeTable2$icon == 'swallow'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'swallow'] = 'delapouite'

mergeTable2$author[mergeTable2$icon == 'swallow-2'] = 'Lorc'
mergeTable2$license[mergeTable2$icon == 'swallow-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'swallow-2'] = 'lorc'

###
mergeTable2$author[mergeTable2$icon == 'tombstone'] = 'Sbed'
mergeTable2$license[mergeTable2$icon == 'tombstone'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'tombstone'] = 'sbed'

mergeTable2$author[mergeTable2$icon == 'tombstone-2'] = 'Lorc'
mergeTable2$license[mergeTable2$icon == 'tombstone-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'tombstone-2'] = 'lorc'

###
mergeTable2$author[mergeTable2$icon == 'swallow'] = 'DarkZaitzev'
mergeTable2$license[mergeTable2$icon == 'swallow'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'swallow'] = 'darkzaitzev'

mergeTable2$author[mergeTable2$icon == 'swallow-2'] = 'Lorc'
mergeTable2$license[mergeTable2$icon == 'swallow-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'swallow-2'] = 'lorc'

###
mergeTable2$author[mergeTable2$icon == 'sunrise'] = 'Lorc'
mergeTable2$license[mergeTable2$icon == 'sunrise'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'sunrise'] = 'lorc'

mergeTable2$author[mergeTable2$icon == 'sunrise-2'] = 'Delapouite'
mergeTable2$license[mergeTable2$icon == 'sunrise-2'] = 'CC BY 3.0'
mergeTable2$authorMin[mergeTable2$icon == 'sunrise-2'] = 'delapouite'

number2s = mergeTable2 %>% filter(is.na(license)) %$% icon %>% {.[grepl('-2',.)]} %>% stringr::str_extract('.*?(?=-2)')

notCurated = mergeTable2$icon %>% stringr::str_extract('^.*?(?=-2)') %>% {. %in% number2s | mergeTable2$icon %in% number2s}

mergeTable2$authorMin[notCurated] = 'uncurated'
mergeTable2$author[notCurated] = 'uncurated'
mergeTable2$license[notCurated] = 'uncurated'

iconTable = mergeTable2 %>% select(-authorMin) %>% unique
usethis::use_data(iconTable,overwrite = TRUE)
