defmodule WeatherDisplayTest do
  use ExUnit.Case
  doctest WeatherDisplay

  test "icon code to image file name" do
    assert WeatherDisplay.get_weather_icon(%{"weather" => [%{"icon" => "01d"}]}) ==
             {:ok, "sun-solid-128.rgba"}
  end
end
