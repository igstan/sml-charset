structure Encoding :> ENCODING =
  struct
    datatype t =
      UTF8
    | UTF16BE
    | UTF16LE
    | UTF32BE
    | UTF32LE

    fun fromBOM (reader : (Word8.word, 's) Reader.t) stream =
      case Reader.grouped 4 reader stream of
        SOME (0wx00 :: 0wx00 :: 0wxFE :: 0wxFF :: _, s) => SOME (UTF32BE, s)
      | SOME (0wxFF :: 0wxFE :: 0wx00 :: 0wx00 :: _, s) => SOME (UTF32LE, s)
      | SOME (0wxEF :: 0wxBB :: 0wxBF :: _, s) => SOME (UTF8, s)
      | SOME (0wxFE :: 0wxFF :: _, s) => SOME (UTF16BE, s)
      | SOME (0wxFF :: 0wxFE :: _, s) => SOME (UTF16LE, s)
      | _ => NONE
  end
