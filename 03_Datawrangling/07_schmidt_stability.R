#calculating schmidt stability 

BVRbath <- bath|>
  filter(Reservoir == "BVR")|>
  select(Depth_m, SA_m2)

#to interpolate
Depth_fake = seq(0, 13, by = 0.1)

bath_interpolated <- BVRbath |> #changing this temporarily 
  mutate(
    first_valid_depth = ifelse(all(is.na(.data[[SA]])), NA_real_, min(Depth_m[!is.na(.data[[var]])], na.rm = TRUE)),
    last_valid_depth = ifelse(all(is.na(.data[[var]])), NA_real_, max(Depth_m[!is.na(.data[[var]])], na.rm = TRUE)),
    Value_interp_depth = ifelse(
      Depth_m >= first_valid_depth & Depth_m <= last_valid_depth,
      zoo::na.approx(.data[[var]], x = Depth_m, na.rm = FALSE),
      NA_real_
    )
  ) %>%
  


bath_interp <- interpolate_variable(BVRbath)

new_bath <- frame_weeks|>
  
  