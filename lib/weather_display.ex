defmodule WeatherDisplay do
  @text_color 0x000000
  @bg_color 0xFFFFFF
  @default_font :sans24pt
  @default_font_bold :sans24pt_bold

  def get_weather_icon(%{"weather" => [%{"icon" => icon} | _T]} = _w) do
    img_name =
      case icon do
        "01d" -> "sun-solid-128.rgba"
        "01n" -> "moon-solid-128.rgba"
        "03d" -> "cloud-solid-128.rgba"
        "03n" -> "cloud-solid-128.rgba"
        "04d" -> "cloud-solid-128.rgba"
        "04n" -> "cloud-solid-128.rgba"
        "09d" -> "cloud-showers-heavy-solid-128.rgba"
        "09n" -> "cloud-showers-heavy-solid-128.rgba"
        "10d" -> "cloud-rain-solid-128.rgba"
        "10n" -> "cloud-rain-solid-128.rgba"
        _ -> "sun-solid-128.rgba"
      end

    {:ok, img_name}
  end

  def get_weather_icon(_w) do
    {:error, :unsupported_format}
  end

  defp get_image(icon_name) do
    case :atomvm.read_priv(:weather_display, icon_name) do
      bin when is_binary(bin) -> {:ok, {:rgba8888, 128, 128, bin}}
      :undefined -> {:error, :no_icon}
    end
  end

  def get_temperature(%{"main" => %{"temp" => temp}} = _w, :celsius) do
    {:ok, round(temp) - 273}
  end

  def get_temperature(_w, _c) do
    {:error, :unsupported_format}
  end

  def get_humidity(%{"main" => %{"humidity" => hum}} = _w) do
    {:ok, hum}
  end

  def get_humidity(_w) do
    {:error, :unsupported_format}
  end

  def get_pressure(%{"main" => %{"pressure" => pressure}} = _w, :millibar) do
    {:ok, pressure}
  end

  def get_pressure(_w, _c) do
    {:error, :unsupported_format}
  end

  def get_wind(%{"wind" => %{"speed" => ms, "deg" => deg}} = _w, :kmh) do
    speed_kmh = round(unit_to_kmh_coeff(:ms) * ms)

    dir_string =
      cond do
        deg > 365 -> "INV!"
        deg < 0 -> "INV!"
        deg > 348 or deg < 11 -> "N"
        deg >= 11 and deg <= 33 -> "NNE"
        deg > 33 and deg < 56 -> "NE"
        deg >= 56 and deg <= 78 -> "ENE"
        deg > 78 and deg < 101 -> "E"
        deg >= 101 and deg <= 123 -> "ESE"
        deg > 123 and deg < 146 -> "SE"
        deg >= 146 and deg <= 168 -> "SSE"
        deg > 168 and deg < 191 -> "S"
        deg >= 191 and deg <= 213 -> "SSW"
        deg > 213 and deg < 236 -> "SW"
        deg >= 236 and deg <= 258 -> "WSW"
        deg > 258 and deg < 281 -> "W"
        deg >= 281 and deg <= 303 -> "WNW"
        deg > 303 and deg < 326 -> "NW"
        deg >= 326 and deg <= 348 -> "NNW"
        true -> "???"
      end

    {:ok, speed_kmh, dir_string}
  end

  def get_wind(_w, _c) do
    {:error, :unsupported_format}
  end

  # Just over complicated for testing
  def unit_to_kmh_coeff(unit) do
    case unit do
      :ms -> "3.6"
      :kmh -> "1.0"
      _ -> :invalid
    end
    |> :erlang.binary_to_float()
  end

  def render_weather(%{"weather" => [%{"main" => main_weather} | _T]} = w, x, y) do
    with %{"name" => name} <- w,
         {:ok, icon} <- get_weather_icon(w),
         {:ok, weather_image} <- get_image(icon),
         {:ok, temp} <- get_temperature(w, :celsius),
         temp_string = Integer.to_string(temp),
         {:ok, hum} <- get_humidity(w),
         hum_string = Integer.to_string(hum),
         {:ok, mbar} <- get_pressure(w, :millibar),
         mbar_string = Integer.to_string(mbar),
         {:ok, kmh, dir} <- get_wind(w, :kmh),
         kmh_string = Integer.to_string(kmh) do
      [
        {:text, x, y, @default_font_bold, @text_color, @bg_color, name},
        {:image, x, y + 70, @bg_color, weather_image},
        {:text, x, y + 200, @default_font, @text_color, @bg_color, main_weather},
        {:text, x, y + 270, @default_font, @text_color, @bg_color,
         temp_string <> " °C / " <> hum_string <> " %"},
        {:text, x, y + 340, @default_font, @text_color, @bg_color, mbar_string <> " mbar"},
        {:text, x, y + 410, @default_font, @text_color, @bg_color, kmh_string <> " km/h " <> dir}
      ]
    end
  end

  def render_weather(any, x, y) do
    [
      {:text, x, y, @default_font, @text_color, @bg_color, "???"}
    ]
  end
end
