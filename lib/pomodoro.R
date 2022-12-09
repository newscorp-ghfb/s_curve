# Making noise with beepr                         # https://tinyurl.com/5bmf7k69
library(beepr)

counter = 0 # amount of finished pomodoros

while(TRUE) {
  # 25 minutes work
  Sys.sleep(60 * 25)
  counter <- counter + 1
  beep(sound="mario")
  rstudioapi::showDialog("Pomodoro timer",
                         sprintf("Pomodoro  nr. %i finished! Time to take a break!", counter))

  # 5 minutes break, every 4th break is 15 minute long
  break_dur <- ifelse(counter %% 4, 5, 15)
  Sys.sleep(60 * break_dur)
  beep(sound="wilhelm")
  rstudioapi::showDialog("Pomodoro timer", "Time to get back to work!")
}
