structure Reader =
  struct
    type ('a, 's) t = 's -> ('a * 's) option

    fun map f reader stream =
      Option.map (fn (a, s) => (f a, s)) (reader stream)

    fun grouped n reader stream =
      let
        fun loop stream 0 result = SOME (List.rev result, stream)
          | loop stream n result =
              case reader stream of
                NONE => SOME (List.rev result, stream)
              | SOME (a, stream) =>
                  loop stream (n - 1) (a :: result)
      in
        loop stream n []
      end

    fun seq readers stream =
      let
        fun return result stream =
          case result of
            [] => NONE
          | _ => SOME (List.rev result, stream)

        fun loop readers stream result =
          case readers of
            [] => return result stream
          | reader :: readers =>
            case reader stream of
              NONE => return result stream
            | SOME (a, stream) => loop readers stream (a :: result)
      in
        loop readers stream []
      end
  end
