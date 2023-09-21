defmodule SolutionTest do
  use ExUnit.Case
  doctest Solution

  test "HEY JUDE" do
    assert Solution.decode(".... . -.--   .--- ..- -.. .") == "HEY JUDE"
  end
end
