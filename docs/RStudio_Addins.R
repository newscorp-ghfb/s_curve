# https://eric.netlify.app/2017/08/10/most-popular-ggplot2-geoms/

delay <- function(f, delay = 3) {
function(...) {
Sys.sleep(delay)
f(...)
}

w_msg <- function(f) {
function(...) {
args <- list(...)
message("Processing: ", args[[1]])
f(...)
}

search_gh <- function(q, ...) {
gh("/search/code", q = q, ...)
}

geoms <- geoms %>%
  mutate(
    result      = map(geom, w_msg(delay(search_gh))),
    total_count = map_int(result, "total_count")
)

# Finally, we plot the results.

ggplot(geoms, aes(x = total_count,
                  y = reorder(geom, total_count))) +
  geom_point(color = "red") +
  geom_text(
    aes(label = str_replace(geom, "geom_(.*)", "\\1  ")),
    size = 3,
    hjust = 1,
    color = "grey30"
  ) +
  scale_y_discrete(expand = c(0.03, 0)) +
  scale_x_log10(
    limits = c(100, 1500000),
    expand = c(0, 0),
    breaks = 10 ^ c(1:6),
    labels = format(10 ^ c(1:6),
                    scientific = FALSE, big.mark = ",")
  ) +
  annotation_logticks(sides = "b") +
  theme_minimal() +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = unit(rep(5, 4), "mm")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Make geom_qq Great Again",
    subtitle = paste0("Most popular ggplot2 geoms,",
                      " by total count on Github")
  )


devtools::install_github('daattali/addinslist')

devtools::install_github("BAAQMD/copydat")
devtools::install_github("ChrisDienes/SeaClass")
1
devtools::install_github("LudvigOlsen/insertImage")
devtools::install_github("LudvigOlsen/splitChunk")
devtools::install_github("MilesMcBain/datapasta")
devtools::install_github("RhoInc/CRANsearcher")
devtools::install_github("Stan125/GREA")
devtools::install_github("Stan125/limoaddin")
devtools::install_github("ThinkR-open/littleboxes")
devtools::install_github("YvesCR/arimaUI")
devtools::install_github("alan-y/objectremover")
devtools::install_github("benmarwick/snakecaser")
devtools::install_github("benmarwick/wordcountaddin", type = "source", dependencies = TRUE)
devtools::install_github("bnosac/taskscheduleR")
devtools::install_github("calligross/ggthemeassist")
devtools::install_github("crsh/citr")
devtools::install_github("csgillespie/addinmanager")
devtools::install_github("daattali/addinslist")
devtools::install_github("daattali/ggExtra")
devtools::install_github("daranzolin/ViewPipeSteps")
devtools::install_github("daranzolin/compareAreas")
devtools::install_github("daranzolin/typeStringsGadget")
devtools::install_github("dkilfoyle/rpivotGadget")
devtools::install_github("dokato/todor")
devtools::install_github("donlelef/tsviz")
devtools::install_github("dracodoc/mischelper")
devtools::install_github("dracodoc/namebrowser")
devtools::install_github("dreamRs/addinit")
devtools::install_github("dreamRs/viewxl")
devtools::install_github("erictleung/unnestIfElse")
devtools::install_github("fkeck/quickview")
devtools::install_github("flaviobarros/shinyExams")
devtools::install_github("gadenbuie/ermoji")
devtools::install_github("haozhu233/giphyr")
devtools::install_github("homerhanumat/addinplots")
devtools::install_github("jeffjjohnston/RStudioConsoleRender")
devtools::install_github("jennybc/jadd")
devtools::install_github("kieranjmartin/viewenhance")
devtools::install_github("lbusett/insert_table")
devtools::install_github("lorenzwalthert/strcode")
devtools::install_github("milesmcbain/mufflr")
devtools::install_github("mlysy/rdoxygen")
devtools::install_github("mrjoh3/mapedit.addin")
devtools::install_github("mvkorpel/uniscape")
devtools::install_github("mwip/beautifyR")
devtools::install_github("nevrome/wellspell.addin")
devtools::install_github("petermeissner/assignparams")
devtools::install_github("r-hub/pkgsearch")
devtools::install_github("sarupurisailalith/commonUtilAddins")
devtools::install_github("sarupurisailalith/commonUtilAddins")
devtools::install_github("seasmith/AlignAssign")
devtools::install_github("sfr/RStudio-Addin-Snippets")
devtools::install_github("strboul/caseconverter")
devtools::install_github("tjmahr/WrapRmd")
devtools::install_github('Timag/imageclipr')
devtools::install_github('dcomtois/sortLines')
devtools::install_github('n8thangreen/jagsAddIn')
devtools::install_github('paulgovan/QRAGadget')
devtools::install_github('xiaoa6435/RmdImgPaste')
###################################################################
# Faster R package installation    https://tinyurl.com/mxcf9fcd ###
###################################################################
install.packages("gitgadget", repos = "https://radiant-rstats.github.io/minicran/", Ncpus = 6)
install_github("MangoTheCat/tidyshiny")
install_github("juba/questionr")
install_github("juba/questionr")
remotes::install_github("MarkEdmondson1234/googleAuthR")
remotes::install_github("ThinkR-open/remedy")
remotes::install_github("bquast/datasets.load")
remotes::install_github("brry/rskey")
remotes::install_github("daattali/colourpicker")
remotes::install_github("daattali/colourpicker")
remotes::install_github("dcomtois/pathToClip")
remotes::install_github("dreamRs/esquisse")
remotes::install_github("dreamRs/prefixer")
remotes::install_github("ginolhac/upnews")
remotes::install_github("konradzdeb/extraInserts")
remotes::install_github("liao961120/linguisticsdown")
remotes::install_github("miraisolutions/compareWith")
remotes::install_github("r-lib/styler")
remotes::install_github("ropensci/rcrossref")
remotes::install_github("s-fleck/testthis")
remotes::install_github("yonicd/ggedit")
remotes::install_github('famuvie/straddin')
remotes::install_github('yonicd/rsam')
remotes::install_github('yonicd/sinew')
remotes::install_github(repo = "baslat/bbb")


devtools::install_git(
  url = "https://github.com/kieranjmartin/getfunctionargs.git",
)

install.packages("radiant.update", repos = "https://radiant-rstats.github.io/minicran/")
options(repos = c(RSM = "https://radiant-rstats.github.io/minicran", CRAN = "https://cloud.r-project.org")
install.packages("radiant.update")
