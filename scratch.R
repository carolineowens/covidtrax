data <- read_csv("https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv") %>% 
  filter(county != "Unassigned",
         county != "Out Of Country")

pops <- read_csv("countypops.csv") %>% 
  separate(county, into = "county", sep = " County,", extra = "drop") %>% 
  mutate(county = substr(county, start = 2, stop = 20)) %>% 
  select(-pop_2019)

#based on county, what is the scalar for tier cutoffs? (# of 100k people in county)
tier <- pops[pops$county=="Colusa",]

#filter data to the county selected, calculate moving average, and clean/tidy
subdat <- data %>% 
    filter(county == "Colusa") %>% 
    mutate(date = as.Date(date),
           movav = movingFun(newcountconfirmed, 7, fun = mean, type = 'to')) %>% 
    select(date, newcases = newcountconfirmed, movav) %>% 
    na.omit() %>% 
    arrange(date) 

status <- mean(tail(subdat, 7)$movav)
today <- tail(subdat, 1)$movav
 
subdat <- subdat %>%       
  mutate(reda = case_when(movav <= tier$red ~ 0,
                          T ~ 1),
         redG = cumsum(c(FALSE, as.logical(diff(reda)))),
         redb = c(0, diff(date)) * reda) %>% 
  group_by(redG) %>% 
  mutate(red = cumsum(reda)) %>% #days above purple
  ungroup() %>% 
  select(-c(reda, redG, redb)) %>% 
  mutate(orna = case_when(movav <= tier$orange ~ 0,
                          movav > tier$red ~ 0,
                          T ~ 1),
         ornG = cumsum(c(FALSE, as.logical(diff(orna)))),
         ornb = c(0, diff(date)) * orna) %>% 
  group_by(ornG) %>% 
  mutate(orange = cumsum(orna)) %>% #days below red / above orange
  ungroup() %>% 
  select(-c(orna, ornG, ornb)) %>%
  mutate(yela = case_when(movav <= tier$yellow ~ 0,
                          movav > tier$orange ~ 0,
                          T ~ 1),
         yelG = cumsum(c(FALSE, as.logical(diff(yela)))),
         yelb = c(0, diff(date)) * yela) %>% 
  group_by(yelG) %>% 
  mutate(yellow = cumsum(yela)) %>% #days below orange / above yellow
  ungroup() %>% 
  select(-c(yela, yelG, yelb)) %>%
  mutate(yela = case_when(movav > tier$yellow ~ 0,
                          T ~ 1),
         yelG = cumsum(c(FALSE, as.logical(diff(yela)))),
         yelb = c(0, diff(date)) * yela) %>% 
  group_by(yelG) %>% 
  mutate(lowest = cumsum(yela)) %>% #days below yellow
  ungroup() %>% 
  select(-c(yela, yelG, yelb)) %>%
  pivot_longer(cols = c(newcases, movav)) %>% 
  mutate(name = as.factor(name))

case_when(status <= tier$yellow ~ "darkgoldenrod1",
          status <= tier$orange ~ "darkorange1",
          status <= tier$red ~ "red3",
          T ~ "#d098fa")

if (status <= tier$yellow){
  tiercol = "darkgoldenrod1"
  if (today <= tier$yellow){
    text = "Days Within Threshold for Yellow Tier: "
    val = tail(subdat$lowest, 1)}
  else{
    val = tail(subdat$yellow, 1)
    Text = "Days Above Threshold for Yellow Tier"}
}else if (status <= tier$orange){
    tiercol = "darkorange1"
    if (today <= tier$yellow){
      text = "Days Below Threshold for Yellow Tier: "
      val = tail(subdat$lowest, 1) }
    else if (today > tier$yellow & today <= tier$orange){
      text = "Days Within Threshold for Orange Tier: "
      val = tail(subdat$yellow, 1) }
    else{
      text = "Days Above Threshold for Orange Tier: "
      val = tail(subdat$orange, 1)}
}else if (status <= tier$red){
      tiercol = "red3"
      if (today <= tier$orange){
        text = 'Days Below Threshold for Orange Tier: '
        val = tail(subdat$yellow, 1)}
      else if (today > tier$orange & today <= tier$red){
        text = 'Days Within Threshold for Red Tier: '
        val = tail(subdat$orange, 1)}
      else{
        text = 'Days Above Threshold for Red Tier: '
        val = tail(subdat$red, 1)}
}else{
      tiercol = "#d098fa"
      if (today <= tier$red){
        text = 'Days Below Threshold for Red Tier: '
        val = tail(subdat$orange, 1)}
      else{
        text = 'Days Above Threshold for Purple Tier: '
        val = tail(subdat$red, 1)}
}

        
#calculate top of y scale
ymax <- max(subdat()$value)
      
#print text for purple box
#txtbox <-
  paste0(format(max(subdat$date), format = "%B %d"),"<br>",text, val)
  