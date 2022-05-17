defmodule Main do
  @ssid Application.fetch_env!(:weather_display, :wifi)[:ssid]
  @psk Application.fetch_env!(:weather_display, :wifi)[:psk]

  def start() do
    :erlang.display("Started.")

    display_opts = [width: 960, height: 540]

    case :erlang.open_port({:spawn, "display"}, display_opts) do
      display when is_port(display) ->
        ufont_regular = :atomvm.read_priv(:weather_display, "noto-sans-24pt150dpi-regular.ufont")
        :gen_server.call(display, {:register_font, :sans24pt, ufont_regular})

        ufont_bold = :atomvm.read_priv(:weather_display, "noto-sans-24pt150dpi-bold.ufont")
        :gen_server.call(display, {:register_font, :sans24pt_bold, ufont_bold})

        :wifi_conn.connect(@ssid, @psk)
        {:ok, _ui} = WeatherDisplay.UI.start_link(display_opts, display_server: display)

      _ ->
        :io.format("Failed to open display")
    end

    recv_loop()
  end

  defp recv_loop() do
    receive do
      any -> :erlang.display({:got, any})
    end

    recv_loop()
  end
end
