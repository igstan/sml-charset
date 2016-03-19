structure Reader =
  struct
    type ('a, 's) t = 's -> ('a * 's) option

    fun map f reader stream =
      Option.map (fn (a, s) => (f a, s)) (reader stream)

    fun grouped n reader stream =
      let
        fun loop stream 0 result = SOME (result, stream)
          | loop stream n result =
              case reader stream of
                NONE => SOME (List.rev result, stream)
              | SOME (a, stream) =>
                  loop stream (n - 1) (a :: result)
      in
        loop stream n []
      end
  end
