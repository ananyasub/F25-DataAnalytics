library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(broom)
library(ggplot2)
library(rpart)
library(rpart.plot)

dataset <- read.csv("titles.csv")

# Select PCA columns
pca_vars <- dataset[, c("imdb_score", "imdb_votes", "tmdb_popularity", "tmdb_score")]

# Identify complete rows
good_rows <- complete.cases(pca_vars)

# Filter BOTH datasets
pca_vars_clean <- pca_vars[good_rows, ]
dataset_clean  <- dataset[good_rows, ]

# Run PCA
pca_result <- prcomp(pca_vars_clean, center = TRUE, scale. = TRUE)
summary(pca_result)
pca_result$rotation

# Extract PC1
PC1 <- pca_result$x[, 1]

# Add PC1 back (now row counts match)
dataset_clean$PC1 <- PC1
View(dataset_clean)

dataset_clean$PC1_category <- cut(
  dataset_clean$PC1,
  breaks = quantile(dataset_clean$PC1, probs = c(0, 0.33, 0.66, 1)),
  include.lowest = TRUE,
  labels = c("Low", "Medium", "High")
)
View(dataset_clean)

tv_shows <- subset(dataset_clean, type == "SHOW")
movies <- subset(dataset_clean, type == "MOVIE")

write.csv(tv_shows, "tv_shows.csv", row.names = FALSE)
write.csv(movies, "movies.csv", row.names = FALSE)

#overall genres
dataset_genres <- dataset_clean %>%
  mutate(
    genres_clean = genres %>%
      gsub("\\[|\\]", "", .) %>%         # remove [ ]
      gsub("'", "", .)                  # remove '
  ) %>%
  separate_rows(genres_clean, sep = ",") %>%
  mutate(genres_clean = trimws(genres_clean)) %>%  # strip whitespace
  rename(genre = genres_clean)

movies_genres <- filter(dataset_genres, type == "MOVIE")
tv_shows_genres <- filter(dataset_genres, type == "SHOW")

ggplot(movies_genres, aes(x = genre, y = PC1)) +
  geom_boxplot(fill = "skyblue") +
  theme_bw() +
  labs(
    title = "Movie Popularity (PC1) by Genre",
    x = "Genre",
    y = "Popularity (PC1)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(movies_genres)

ggplot(tv_shows_genres, aes(x = genre, y = PC1)) +
  geom_boxplot(fill = "pink") +
  theme_bw() +
  labs(
    title = "TV Show Popularity (PC1) by Genre",
    x = "Genre",
    y = "Popularity (PC1)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(tv_shows_genres)

genre_counts <- dataset_genres %>%
  count(genre, sort = TRUE)
print(genre_counts)

#MOVIE LINEAR MODELS
movie_genre_model<- lm(PC1 ~ genre, data = movies_genres)
summary(movie_genre_model)

coef_df <- tidy(movie_genre_model)
coef_df$term_clean <- sub("^genre", "", coef_df$term)

coef_df$term_clean <- tools::toTitleCase(coef_df$term_clean)

coef_df %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = estimate, y = term_clean)) +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error)) +
  labs(title = "Genre Effects on Movie Popularity (PC1)", y = "Genre", x = "Estimate") +
  theme_bw()

movie_year_model<- lm(PC1 ~ release_year, data = movies_genres)
summary(movie_year_model)
print(movie_year_model$coefficients)

ggplot(movies_genres, aes(x = release_year, y = PC1)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "skyblue", se = TRUE) +
  theme_bw() +
  labs(
    title = "Popularity (PC1) vs Release Year (MOVIES)",
    x = "Release Year",
    y = "Popularity (PC1)"
  )

movie_runtime_model<- lm(PC1 ~ runtime, data = movies_genres)
summary(movie_runtime_model)

ggplot(movies_genres, aes(x = runtime, y = PC1)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "skyblue", se = TRUE) +
  theme_bw() +
  labs(
    title = "Popularity (PC1) vs Runtime (MOVIES)",
    x = "Runtime (minutes)",
    y = "Popularity (PC1)"
  )

#TVSHOWS LINEAR MODELS
tvshow_genre_model<- lm(PC1 ~ genre, data = tv_shows_genres)
summary(tvshow_genre_model)

coef_df1 <- tidy(tvshow_genre_model)
coef_df1$term_clean <- sub("^genre", "", coef_df1$term)

coef_df1$term_clean <- tools::toTitleCase(coef_df1$term_clean)

coef_df1 %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = estimate, y = term_clean)) +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error)) +
  labs(title = "Genre Effects on TV Show Popularity (PC1)", y = "Genre", x = "Estimate") +
  theme_bw()

tvshow_year_model<- lm(PC1 ~ release_year, data = tv_shows_genres)
summary(tvshow_year_model)

ggplot(tv_shows_genres, aes(x = release_year, y = PC1)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "pink", se = TRUE) +
  theme_bw() +
  labs(
    title = "Popularity (PC1) vs Release Year (TV SHOWS)",
    x = "Release Year",
    y = "Popularity (PC1)"
  )

tvshow_seasons_model<- lm(PC1 ~ seasons, data = tv_shows_genres)
summary(tvshow_seasons_model)

ggplot(tv_shows_genres, aes(x = seasons, y = PC1)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "pink", se = TRUE) +
  theme_bw() +
  labs(
    title = "Popularity (PC1) vs No. Seasons (TV SHOWS)",
    x = "Seasons",
    y = "Popularity (PC1)"
  )

tshow_runtime_model<- lm(PC1 ~ runtime, data = tv_shows_genres)
summary(tshow_runtime_model)

ggplot(tv_shows_genres, aes(x = runtime, y = PC1)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "pink", se = TRUE) +
  theme_bw() +
  labs(
    title = "Popularity (PC1) vs Runtime (TV SHOWS)",
    x = "Runtime (minutes per episode)",
    y = "Popularity (PC1)"
  )

#MOVIES K MEANS CLUSTERING
movies %>%
  group_by(PC1_category) %>%
  summarise(
    mean_year = mean(release_year, na.rm = TRUE),
    mean_runtime = mean(runtime, na.rm = TRUE),
    count = n()
  )
ggplot(movies, aes(x = release_year, y = runtime, color = PC1_category)) +
  geom_point(alpha = 0.6, size = 2) +
  theme_bw() +
  labs(
    title = "Movie Popularity Clusters",
    x = "Release Year",
    y = "Runtime",
    color = "Popularity"
  )

ggplot(movies, aes(x = runtime, y = PC1, color = PC1_category)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_bw() +
  labs(
    title = "Movies: Runtime vs Popularity Clusters",
    x = "Runtime",
    y = "Popularity (PC1)",
    color = "Popularity Cluster"
  )


#TV SHOW CLUSTERING
tv_shows %>%
  group_by(PC1_category) %>%
  summarise(
    mean_year = mean(release_year, na.rm = TRUE),
    mean_seasons = mean(seasons, na.rm = TRUE),
    mean_runtime = mean(runtime, na.rm = TRUE),
    count = n()
  )
ggplot(tv_shows, aes(x = release_year, y = seasons, color = PC1_category)) +
  geom_point(alpha = 0.6, size = 2) +
  theme_bw() +
  labs(
    title = "TV Show Popularity Clusters",
    x = "Release Year",
    y = "Number of Seasons",
    color = "Popularity"
  )




  