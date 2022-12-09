slams <- c(
    "1999 Australian Open",  "1999 French Open",  "1999 Wimbledon",  "1999 US Open",
    "2000 Australian Open",  "2000 French Open",  "2000 Wimbledon",  "2000 US Open",
    "2001 Australian Open",  "2001 French Open",  "2001 Wimbledon",  "2001 US Open",
    "2002 Australian Open",  "2002 French Open",  "2002 Wimbledon",  "2002 US Open",
    "2003 Australian Open",  "2003 French Open",  "2003 Wimbledon",  "2003 US Open",
    "2004 Australian Open",  "2004 French Open",  "2004 Wimbledon",  "2004 US Open",
    "2005 Australian Open",  "2005 French Open",  "2005 Wimbledon",  "2005 US Open",
    "2006 Australian Open",  "2006 French Open",  "2006 Wimbledon",  "2006 US Open",
    "2007 Australian Open",  "2007 French Open",  "2007 Wimbledon",  "2007 US Open",
    "2008 Australian Open",  "2008 French Open",  "2008 Wimbledon",  "2008 US Open",
    "2009 Australian Open",  "2009 French Open",  "2009 Wimbledon",  "2009 US Open",
    "2010 Australian Open",  "2010 French Open",  "2010 Wimbledon",  "2010 US Open",
    "2011 Australian Open",  "2011 French Open",  "2011 Wimbledon",  "2011 US Open",
    "2012 Australian Open",  "2012 French Open",  "2012 Wimbledon",  "2012 US Open",
    "2013 Australian Open",  "2013 French Open",  "2013 Wimbledon",  "2013 US Open",
    "2014 Australian Open",  "2014 French Open",  "2014 Wimbledon",  "2014 US Open",
    "2015 Australian Open",  "2015 French Open",  "2015 Wimbledon",  "2015 US Open",
    "2016 Australian Open",  "2016 French Open",  "2016 Wimbledon",  "2016 US Open",
    "2017 Australian Open",  "2017 French Open",  "2017 Wimbledon",  "2017 US Open",
    "2018 Australian Open",  "2018 French Open",  "2018 Wimbledon",  "2018 US Open",
    "2019 Australian Open",  "2019 French Open",  "2019 Wimbledon",  "2019 US Open",
    "2020 Australian Open",  "2020 French Open",  "2020 Wimbledon",  "2020 US Open",
    "2021 Australian Open",  "2021 French Open")

# scrape data from Wikipedia
player <- function(name) {
    wiki <- paste("https://en.wikipedia.org/wiki/", name, sep = "")
    html <- read_html(wiki)
    tables <- html_table(html)
    
    results <- tables[[4]] %>% 
        filter(Tournament != "Win–Loss") %>% 
        select(-SR, -contains("W")) %>% 
        pivot_longer(!Tournament, names_to = "Year", values_to = "Result") %>% 
        mutate(Result = ifelse(str_detect(Result, "\\[") == TRUE,
                               str_sub(Result, 1, nchar(Result) - 3), Result)) %>% 
        filter(!(Result %in% c("", "NH", "Q1", "Q2", "A"))) %>% 
        mutate(Result = factor(Result, levels = c("1R", "2R", "3R", "4R", "QF", "SF", "F", "W"))) %>% 
        unite(Slam, c(Year, Tournament), sep = " ") %>% 
        add_count(Slam, Result, name = "Count") %>% 
        mutate(Slam = factor(Slam, levels = slams))
    
    results <- full_join(results, expand(results, Slam, Result)) %>%
        mutate(Count = ifelse(is.na(Count), 0, Count)) %>% 
        arrange(Slam, Result) %>% 
        group_by(Result) %>% 
        mutate(Count = cumsum(Count),
               Player = name)
}

slam <- player("Roger_Federer") %>% 
    full_join(player("Rafael_Nadal")) %>% 
    full_join(player("Novak_Djokovic")) %>% 
    mutate(Player = str_replace(Player, "_", " "),
           Player = factor(Player, 
                           levels = c("Roger Federer", "Rafael Nadal", "Novak Djokovic")))