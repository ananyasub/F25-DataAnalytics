library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(caret)


dataset <- read.csv("DATA (1).csv")
View(dataset)

new_names <- c(
  "student_id",
  "age_group",                    # x1
  "sex",                          # x2
  "high_school_type",             # x3
  "scholarship_type",             # x4
  "additional_work",              # x5
  "art_or_sports_activity",       # x6
  "has_partner",                  # x7
  "salary_range",                 # x8
  "transportation_type",          # x9
  "accommodation_type",           # x10
  "mother_education",             # x11
  "father_education",             # x12
  "num_siblings",                 # x13
  "parental_status",              # x14
  "mother_occupation",            # x15
  "father_occupation",            # x16
  "weekly_study_hours",           # x17
  "reading_non_scientific",       # x18
  "reading_scientific",           # x19
  "attend_seminars",              # x20
  "projects_impact",              # x21
  "attendance_to_classes",        # x22
  "midterm_prep_style",           # x23
  "midterm_prep_timing",          # x24
  "note_taking",                  # x25
  "listening_in_class",           # x26
  "discussion_improves_success",  # x27
  "flip_classroom_usefulness",    # x28
  "last_semester_gpa",             # x29
  "expected_graduation_gpa",      # x30
  "course_id",
  "grade"
)

colnames(dataset) <- new_names
names(dataset)
View (dataset)

dataset <- dataset %>%
  mutate(
    age_group = factor(age_group, ordered = TRUE),
    sex = factor(sex),
    high_school_type = factor(high_school_type),
    scholarship_type = factor(scholarship_type, ordered = TRUE),
    additional_work = factor(additional_work),
    art_or_sports_activity = factor(art_or_sports_activity),
    has_partner = factor(has_partner),
    salary_range = factor(salary_range, ordered = TRUE),
    transportation_type = factor(transportation_type),
    accommodation_type = factor(accommodation_type),
    mother_education = factor(mother_education, ordered = TRUE),
    father_education = factor(father_education, ordered = TRUE),
    num_siblings = factor(num_siblings, ordered = TRUE),
    parental_status = factor(parental_status),
    mother_occupation = factor(mother_occupation),
    father_occupation = factor(father_occupation),
    weekly_study_hours = factor(weekly_study_hours, ordered = TRUE),
    reading_non_scientific = factor(reading_non_scientific, ordered = TRUE),
    reading_scientific = factor(reading_scientific, ordered = TRUE),
    attend_seminars = factor(attend_seminars),
    projects_impact = factor(projects_impact),
    attendance_to_classes = factor(attendance_to_classes, ordered = TRUE),
    midterm_prep_style = factor(midterm_prep_style),
    midterm_prep_timing = factor(midterm_prep_timing),
    note_taking = factor(note_taking, ordered = TRUE),
    listening_in_class = factor(listening_in_class, ordered = TRUE),
    discussion_improves_success = factor(discussion_improves_success, ordered = TRUE),
    flip_classroom_usefulness = factor(flip_classroom_usefulness),
    last_semester_gpa = factor(last_semester_gpa, ordered = TRUE),
    expected_graduation_gpa = factor(expected_graduation_gpa, ordered = TRUE),
    grade = factor(grade, ordered = TRUE)
  )

#dataset$grade <- factor(
 # dataset$grade,
  #levels = 0:7,
  #labels = c("Fail", "DD", "DC", "CC", "CB", "BB", "BA", "AA"),
  #ordered = TRUE
#)


dataset %>%
  select(-student_id, -course_id, -grade) %>%
  mutate(across(everything(), ~ factor(as.character(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_bar() +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  ) +
  labs(
    x = NULL,
    y = "Count",
    title = "Distribution of Categorical Student Features"
  )

dataset <- dataset %>%
  mutate(
    salary_range_label = factor(
      salary_range,
      levels = 1:5,
      labels = c(
        "135–200",
        "201–270",
        "271–340",
        "341–410",
        "Above 410"
      ),
      ordered = TRUE
    )
  )
View(dataset)

dataset <- dataset %>%
  mutate(
    scholarship_type_label = factor(
      scholarship_type,
      levels = 1:5,
      labels = c(
        "None",
        "25%",
        "50%",
        "75%",
        "Full"
      ),
      ordered = TRUE
    )
  )

dataset <- dataset %>%
  mutate(
    high_school_type_label = factor(
      high_school_type,
      levels = 1:3,
      labels = c(
        "Private",
        "State",
        "Other"
      ),
      ordered = TRUE
    )
  )
dataset <- dataset %>%
  mutate(
    grade_label = factor(
      as.integer(as.character(grade)),
      levels = 0:7,
      labels = c("Fail", "DD", "DC", "CC", "CB", "BB", "BA", "AA"),
      ordered = TRUE
    )
  )


dataset %>%
  select(
    grade_label,
    salary_range_label,
    scholarship_type_label,
    high_school_type_label
  ) %>%
  mutate(
    across(
      -grade_label,
      ~ factor(as.character(.))   # drop ordering temporarily
    )
  ) %>%
  pivot_longer(
    cols = -grade_label,
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value, fill = grade_label)) +
  geom_bar(position = "fill") +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Proportion",
    fill = "Grade",
    title = "Grade Distribution by Salary Range, Scholarship Type, and High School Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

set.seed(123)

train_idx <- createDataPartition(dataset$grade_label, p = 0.8, list = FALSE)
train <- dataset[train_idx, ]
test  <- dataset[-train_idx, ]

rf_class <- train(
  grade_label ~ salary_range_label + scholarship_type_label + high_school_type_label,
  data = train,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5)
)

# Predictions
pred_class <- predict(rf_class, test)

# Evaluation
confusionMatrix(pred_class, test$grade_label)

# Feature importance
#varImp(rf_class)

dataset <- dataset %>%
  mutate(
    grade_numeric = as.integer(grade_label) - 1
  )

lm_model <- lm(
  grade_numeric ~ salary_range_label + scholarship_type_label + high_school_type_label,
  data = dataset
)

summary(lm_model)

# Prepare numeric input (encode ordered factors)
cluster_data <- dataset %>%
  select(
    salary_range,
    scholarship_type,
    high_school_type
  ) %>%
  mutate(across(everything(), as.numeric))

# Scale for clustering
cluster_scaled <- scale(cluster_data)

# k-means
set.seed(123)
km <- kmeans(cluster_scaled, centers = 3, nstart = 25)

dataset$cluster <- factor(km$cluster)

pca <- prcomp(cluster_data)
pca_df <- as.data.frame(pca$x[, 1:2])
pca_df$cluster <- dataset$cluster

ggplot(pca_df, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_minimal() +
  labs(
    title = "K-means Clustering of Students (PCA Projection)",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster"
  )

dataset %>%
  count(cluster, grade_label) %>%
  ggplot(aes(cluster, n, fill = grade_label)) +
  geom_col(position = "fill") +
  theme_minimal() +
  labs(
    title = "Grade Distribution by K-means Cluster",
    y = "Proportion",
    x = "Cluster",
    fill = "Grade"
  )


