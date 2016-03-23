signature ENCODING =
  sig
    datatype t =
      UTF8
    | UTF16BE
    | UTF16LE
    | UTF32BE
    | UTF32LE

    val fromBOM : (Word8.word, 's) StringCvt.reader -> (t, 's) StringCvt.reader
  end
