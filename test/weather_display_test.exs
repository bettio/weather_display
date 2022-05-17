defmodule WeatherDisplayTest do
  use ExUnit.Case
  doctest WeatherDisplay

  test "greets the world" do
    assert WeatherDisplay.hello() == :world
  end
end
