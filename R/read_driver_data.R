#' Get several driving variables for the driver
#'
#' @param file_name path to daq or hdf5 file (string)
#' @param metric whether to get data in metric or english units. Default is TRUE (boolean)
#'
#' @return a data.frame containing several driving variables
#' @export
#'
#' @examples
#' \dontrun{
#' library(reticulate)
#' use_virtualenv("r-reticulate")
#' read_driver_data("path/to/daq/file/myfile.daq")
#' read_driver_data("path/to/daq/file/myfile.hdf5")
#' }
read_driver_data <- function(file_name, metric = TRUE){


  # Reading the contents of file -------------------------------------------------

  ## Save the daq file to hdf5 if not available already
  if(grepl("\\.daq$", file_name)){

    read_daq_and_save_to_hdf5(file_name)
  }

  ## Get the name of the hdf5 file
  new_file_name <- paste0(strsplit(file_name, "[.]")[[1]][1],".hdf5")

  ## Read the data group
  driver_data <- rhdf5::h5read(file = new_file_name, name="data")

  ## Get the time frames
  frame_part <- rhdf5::h5read(file = new_file_name, name="frame")
  frames <- frame_part$frame



  # Combining the select few data-------------------------------------

  ## Combine individual vectors
  ind_vec_df <- data.frame(
    frames = frames,
    time_elapsed = (0:(length(frames) - 1))/60,
    ED_acc_pedal_pos = driver_data$CFS_Accelerator_Pedal_Position,
    ED_brake_pedal_force_lbs = driver_data$CFS_Brake_Pedal_Force,
    ED_brake_pedal_force_kg = driver_data$CFS_Brake_Pedal_Force * 0.45,
    ED_steering_wheel_angle_deg = driver_data$CFS_Steering_Wheel_Angle,
    ED_steering_wheel_angle_rate_dps = driver_data$CFS_Steering_Wheel_Angle_Rate,
    ED_speed_mph = driver_data$VDS_Veh_Speed,
    ED_speed_mps =  round(driver_data$VDS_Veh_Speed * 0.447, 2)
  )


  ## Lead vehicle data
  scc_follow_info <- data.frame(driver_data$SCC_Follow_Info)
  colnames(scc_follow_info) <- c("LV_ID", "LV_spacing_ft", "LV_time_gap_s", "LV_frspacing_ft",
                                 "TTC", "LV_speed_fps", "LV_x", "LV_y", "LV_z")
  scc_follow_info <- scc_follow_info[, 1:6]
  scc_follow_info$LV_spacing_m <- round(scc_follow_info$LV_spacing_ft * 0.3048, 2)
  scc_follow_info$LV_frspacing_m <- round(scc_follow_info$LV_frspacing_ft * 0.3048, 2)
  scc_follow_info$LV_speed_mps <- round(scc_follow_info$LV_speed_fps * 0.3048, 2)



  ## Lane deviation of ED
  scc_lane_deviation <- data.frame(driver_data$SCC_Lane_Deviation)
  colnames(scc_lane_deviation) <- c("lane_or_cor", "lane_offset_ft", "lane_width_ft", "lane_id")
  scc_lane_deviation$lane_offset_m <- round(scc_lane_deviation$lane_offset_ft * 0.3048, 3)
  scc_lane_deviation$lane_width_m <- round(scc_lane_deviation$lane_width_ft * 0.3048, 2)



  ## Coordinates of center of gravity of ED
  vds_chassis_cg_position <- data.frame(driver_data$VDS_Chassis_CG_Position)
  colnames(vds_chassis_cg_position) <- c("ED_y_ft", "ED_x_ft", "ED_z_ft")
  # vds_chassis_cg_position <- vds_chassis_cg_position[, 1:2]
  vds_chassis_cg_position$ED_x_m <- round(vds_chassis_cg_position$ED_x_ft * 0.3048, 2)
  vds_chassis_cg_position$ED_y_m <- round(vds_chassis_cg_position$ED_y_ft * 0.3048, 2)



  ## Turn signal information
  ED_turn_signal_no <- as.vector(driver_data$CIS_Turn_Signal)

  ED_turn_signal <- ifelse(ED_turn_signal_no==1L, "not on", ifelse(ED_turn_signal_no==2L, "left", "right"))

  turn_signal_info <- data.frame(
    ED_frame_at_turn_signal = driver_data$CIS_Turn_Signal_Frames,
    ED_turn_signal_no = ED_turn_signal_no,
    ED_turn_signal = ED_turn_signal
  )

  turn_signal_info <- data.frame(frames) %>%
    dplyr::full_join(turn_signal_info, by=c("frames" = "ED_frame_at_turn_signal")) %>%
    tidyr::fill(c(ED_turn_signal_no, ED_turn_signal), .direction = "down") %>%
    dplyr::select(-frames)



  ## Audio Prompt info
  audio_info <- data.frame(
    audio_file_no = driver_data$SCC_Audio_Trigger,
    audio_file_play_frame = driver_data$SCC_Audio_Trigger_Frames
  )


  audio_info <- data.frame(frames) %>%
    dplyr::full_join(audio_info, by=c("frames" = "audio_file_play_frame")) %>%
    tidyr::fill(c(audio_file_no), .direction = "down") %>%
    dplyr::select(-frames)




  # Put everything together-------------------------------------------------------

  combined_df <- cbind(ind_vec_df, vds_chassis_cg_position, turn_signal_info,
                       scc_follow_info, scc_lane_deviation, audio_info)



  if (metric) {

    # Metric units------------------------------------------------------------------
    combined_df <- combined_df %>%
      dplyr::select(-c(ED_brake_pedal_force_lbs, ED_speed_mph, LV_spacing_ft,
                       LV_frspacing_ft, LV_speed_fps, lane_offset_ft,
                       lane_width_ft))

  } else {

    # English units------------------------------------------------------------------
    combined_df <- combined_df %>%
      dplyr::select(-c(ED_brake_pedal_force_kg, ED_speed_mps, LV_spacing_m,
                       LV_frspacing_m, LV_speed_mps, lane_offset_m,
                       lane_width_m, ED_x_m, ED_y_m))
  }


  return(combined_df)
}
