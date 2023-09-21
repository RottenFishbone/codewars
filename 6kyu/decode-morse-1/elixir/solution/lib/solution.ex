defmodule Solution do
  @morse_codes %{
    "." => "E",
    "...." => "H",
    "-.--" => "Y",
    "   " => " ",
    ".---" => "J",
    "..-" => "U",
    "-.." => "D",
  }

  def decode(str) do
    words = str 
      |> String.trim(" ")     # Trim whitespace
      |> String.split("   ")  # Split morse words

    decoded_words = for word <- words, do:
      word
        |> String.split(" ") # Split letter strings
        # Decode letter strings
        |> (fn list -> for word <- list, do: @morse_codes[word] end).() 
        |> Enum.join("") # Rebuild word from charlist
  
    decoded_words |> Enum.join(" ")
  end

end
