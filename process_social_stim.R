#load libraries for data manipulation and graphing
library(MASS)
library(dplyr)
library(directlabels)
library(xtable)
library(tidyr)
library(data.table)
library(bit64)
library(jpeg)
library(stringr)
library(XML)
library(readr)
library(tidyr)
library(magrittr)
library(zoo)
library(lubridate)
library(ggplot2)
library(langcog)
###############################################################################
################################### Constants #################################
###############################################################################
#try/catch for different number of calib points
MAX_HEADER_ROWS <- 40

X_MAX <- 1280
Y_MAX <- 1024

CALIB_SACCADE_TIME <- .3

FRAME_RATE <- 30

IMG_WIDTH = 720
IMG_HEIGHT= 576

SCALE_X = X_MAX/IMG_WIDTH
SCALE_Y = Y_MAX/IMG_HEIGHT

ET_DATA_DIR <- "fixed_data/"

EXCLUDED_STIMS <- NULL

AOI_BUFFER <- 25

trunc <- function(x, ..., prec = 4) base::trunc(x * 10^prec, ...) / 10^prec

###############################################################################
############################### Read all raw data #############################
###############################################################################
source('loading_helpers/read_raw_data.R')
all_result_files <- list.files(path = ET_DATA_DIR, pattern = '*.txt',
                             all.files = FALSE)

all_results <- read_raw_data(all_result_files)

###############################################################################
################################ Get Trial Data ###############################
###############################################################################
zero_times <- function(gaze_data) {
  
  gaze_data <- gaze_data %>%
    group_by(Stimulus) %>%
    mutate(Time = (Time - min(Time))/1000000) %>%
    filter(Time > 0) %>%
    mutate(Time = trunc(floor(FRAME_RATE * Time) / FRAME_RATE))%>%
    group_by(Time, add=TRUE) %>%
    summarise_each(funs(mean = mean(., na.rm = T)), c(x, y))
}

stim_data <- zero_times(all_results)


###############################################################################
################################## Read AOIS ##################################
###############################################################################
closest <- function(time) {
  times = trunc(seq(floor(time),ceiling(time), 1/FRAME_RATE))
  diffs <- abs(time - times)
  times[which(diffs == min(diffs))]
}

annotations <- read_csv("processed_data/aois/aoi_annotations.csv")

source('loading_helpers/load_aois.R')
aois <- get_aois(annotations) %>%
  rowwise() %>%
  mutate(Time = closest(Time)) %>%
  left_join(annotations) %>%
  rename(Stimulus = video)


###############################################################################
################################ Make Heatmaps ################################
###############################################################################


###############################################################################
################################# Process AOIS ################################
###############################################################################
all_aoi_data <- left_join(stim_data, aois) %>%
  filter(!is.na(aoi_name)) %>%
  mutate(in_aoi = (x >= top_left_x - AOI_BUFFER) & 
           (x <= bottom_right_x + AOI_BUFFER) &
           (y >= top_left_y - AOI_BUFFER) &
           (y <= bottom_right_y + AOI_BUFFER)) %>%
  ungroup()

other_data <- all_aoi_data %>%
  group_by(trial, Stimulus, Time) %>%
  summarise(num_in = sum(in_aoi)) %>%
  filter(is.na(num_in) | num_in == 0) %>%
  ungroup() %>%
  rename(aoi_name = num_in) %>%
  mutate(aoi_name = as.factor(aoi_name))

aoi_data <- all_aoi_data %>%
  filter(in_aoi > 0) %>%
  select(trial, Stimulus, Time, aoi_name) %>%
  separate(aoi_name, into = c("person", "type", "index"), sep = "_", fill = "right")

#fix nonspecificed people
broken_aoi_data <- aoi_data %>%
  filter(is.na(index)) %>%
  mutate(index = type,
         type = person,
         person = "unknown")

fixed_aoi_data <- aoi_data %>%
  filter(!is.na(index)) %>%
  bind_rows(broken_aoi_data)

processed_aoi_data <- other_data %>%
  rename(type = aoi_name) %>%
  mutate(type = ifelse(type == "0", "other", type)) %>%
  bind_rows(fixed_aoi_data) %>%
  arrange(trial, Time) 


###############################################################################
############################## Write Output Data ##############################
###############################################################################
write_csv(processed_aoi_data, "processed_data/csvs/out.csv")

probs <- processed_aoi_data %>%
  group_by(trial, Stimulus) %>%
  summarise(mouth = sum(type == "mouth", na.rm = T) / 
              sum(type == "face", na.rm = T),
            eyes = sum(type == "eyes", na.rm = T) / 
              sum(type == "face", na.rm = T),
            face = sum(type == "face", na.rm = T)/
              sum(type == "face" | type == "other" | is.na(type), na.rm = T),
            have_data = mean(!is.na(type), na.rm = T))


