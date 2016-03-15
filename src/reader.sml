structure Reader =
  struct
    type ('a, 's) t = 's -> ('a * 's) option
  end
