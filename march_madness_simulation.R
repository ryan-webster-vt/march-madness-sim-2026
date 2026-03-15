
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(progressr)

# Construct Teams ---------------------------------------------------------

teams_east <- data.frame(
  teams = c('Duke', 'UConn', 'Michigan State', 'Kansas', 'St. John\'s', 
            'Louisville', 'UCLA', 'Ohio State', 'TCU', 'UCF', 
            'South Florida', 'Northern Iowa', 'California Baptist', 'North Dakota State', 'Furman',
            'Siena'),
  seed = c(1:16),
  playin_group = NA,
  region = 'east'
)

teams_midwest <- data.frame(
  teams = c('Michigan', 'Iowa State', 'Virginia', 'Alabama', 'Texas Tech', 
            'Tennessee', 'Kentucky', 'Georgia', 'Saint Louis', 'Santa Clara', 'Miami (OH)', 'SMU',
            'Akron', 'Hofstra', 'Wright State', 'Tennessee State', 'UMBC', 'Howard'),
  seed = c(1:11, 11, 12:16, 16),
  playin_group = c(rep(NA, 10), 'midwest_11', 'midwest_11', rep(NA, 4), 'midwest_16', 'midwest_16'),
  region = 'midwest'
)

teams_south <- data.frame(
  teams = c('Florida', 'Houston', 'Illinois', 'Nebraska', 'Vanderbilt', 'North Carolina', 
            'Saint Mary\'s', 'Clemson', 'Iowa', 'Texas A&M', 'VCU', 
            'McNeese', 'Troy', 'Pennsylvania', 'Idaho', 'Prairie View A&M', 'Lehigh'),
  seed = c(1:16, 16),
  playin_group = c(rep(NA, 15), 'south_16', 'south_16'),
  region = 'south'
)

teams_west <- data.frame(
  teams = c('Arizona', 'Purdue', 'Gonzaga', 'Arkansas', 'Wisconsin',
            'BYU', 'Miami', 'Villanova', 'Utah State', 'Missouri', 'Texas', 'NC State',
            'High Point', 'Hawai\'i', 'Kennesaw State', 'Queens University', 'Long Island University'),
  seed = c(1:11, 11, 12:16),
  playin_group = c(rep(NA, 10), 'west_11', 'west_11', rep(NA, 5)),
  region = 'west'
)

teams <- rbind(teams_east, teams_midwest, teams_south, teams_west)

current_rankings <- read.csv('webstar_rankings_03_15_2026.csv')

# Merge current ratings onto teams df
teams <- inner_join(teams, current_rankings, by = join_by('teams' == 'Team'))


# Simulate Bracket Function -----------------------------------------------

# Win probability function
home_wp <- function(away_net, home_net) {
  spread <- (home_net - away_net) * 0.679
  1 - pnorm(0, mean = spread, sd = 10)
}

# Resolve play-ins
resolve_playins <- function(teams) {
  playins <- teams %>% filter(!is.na(playin_group))
  winners <- playins %>%
    group_by(playin_group) %>%
    group_modify(~simulate_game(.x[1,], .x[2,])) %>%
    ungroup()
  teams %>%
    filter(is.na(playin_group)) %>%
    bind_rows(winners)
}

# Helper to get team row by name
t64_by_name <- function(team_name, teams) {
  teams %>% filter(teams == team_name)
}

# Fix simulate_game to return consistently named column
simulate_game <- function(team1, team2) {
  net1 <- team1$Total[1]
  net2 <- team2$Total[1]
  if (is.na(net1) | is.na(net2)) stop("Missing rating")
  p1 <- home_wp(net2, net1)
  if (runif(1) < p1) team1 else team2
}

# Fix simulate_bracket_rounds — remove <<-, use local vars, fix $teams reference
simulate_bracket_rounds <- function(teams) {
  teams <- resolve_playins(teams)
  regions <- c("east", "west", "south", "midwest")
  
  R64 <- c(); R32 <- c(); S16 <- c(); E8 <- c(); F4 <- c()
  
  region_winners <- map_chr(regions, function(r) {
    t <- teams %>% filter(region == r) %>% arrange(seed)
    
    # Round of 64
    r64 <- c(
      simulate_game(t[1,], t[16,])$teams,
      simulate_game(t[8,], t[9,])$teams,
      simulate_game(t[5,], t[12,])$teams,
      simulate_game(t[4,], t[13,])$teams,
      simulate_game(t[6,], t[11,])$teams,
      simulate_game(t[3,], t[14,])$teams,
      simulate_game(t[7,], t[10,])$teams,
      simulate_game(t[2,], t[15,])$teams
    )
    R64 <<- c(R64, r64)
    
    # Round of 32
    r32 <- c(
      simulate_game(t64_by_name(r64[1], teams), t64_by_name(r64[2], teams))$teams,
      simulate_game(t64_by_name(r64[3], teams), t64_by_name(r64[4], teams))$teams,
      simulate_game(t64_by_name(r64[5], teams), t64_by_name(r64[6], teams))$teams,
      simulate_game(t64_by_name(r64[7], teams), t64_by_name(r64[8], teams))$teams
    )
    R32 <<- c(R32, r32)
    
    # Sweet 16
    s16 <- c(
      simulate_game(t64_by_name(r32[1], teams), t64_by_name(r32[2], teams))$teams,
      simulate_game(t64_by_name(r32[3], teams), t64_by_name(r32[4], teams))$teams
    )
    S16 <<- c(S16, s16)
    
    # Elite 8
    e8 <- simulate_game(t64_by_name(s16[1], teams), t64_by_name(s16[2], teams))$teams
    E8 <<- c(E8, e8)
    
    e8
  })
  
  # Final Four
  f1 <- simulate_game(t64_by_name(region_winners[1], teams), t64_by_name(region_winners[2], teams))$teams
  f2 <- simulate_game(t64_by_name(region_winners[3], teams), t64_by_name(region_winners[4], teams))$teams
  F4 <- c(f1, f2)
  
  Champ <- simulate_game(t64_by_name(f1, teams), t64_by_name(f2, teams))$teams
  
  list(R64 = R64, R32 = R32, S16 = S16, E8 = E8, F4 = F4, Champ = Champ)
}

n_sims <- 1000

results <- map(1:n_sims, ~simulate_bracket_rounds(teams), .progress = TRUE)

# Helper function
compute_prob <- function(round_results, n_sims) {
  round_results %>%
    flatten_chr() %>%
    tibble(team = .) %>%
    count(team) %>%
    mutate(prob = n / n_sims) %>%
    arrange(desc(prob))
}

R64_probs <- compute_prob(map(results, "R64"), n_sims)
R32_probs <- compute_prob(map(results, "R32"), n_sims)
S16_probs <- compute_prob(map(results, "S16"), n_sims)
E8_probs  <- compute_prob(map(results, "E8"), n_sims)
F4_probs  <- compute_prob(map(results, "F4"), n_sims)
Champ_probs <- compute_prob(map(results, "Champ"), n_sims)
