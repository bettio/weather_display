defmodule WeatherDisplay.UI do
  @api_key Application.fetch_env!(:weather_display, :open_weather_map)[:api_key]
  @lat Application.fetch_env!(:weather_display, :open_weather_map)[:location][:lat]
  @lon Application.fetch_env!(:weather_display, :open_weather_map)[:location][:lon]

  def start_link(args, opts) do
    :avm_scene.start_link(__MODULE__, args, opts)
  end

  def init(opts) do
    :erlang.send_after(1, self(), :show_loading)
    :erlang.send_after(100, self(), :update)
    {:ok, opts}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :error, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(:show_loading, state) do
    rendered = [
      {:rect, 0, 0, state[:width], state[:height], 0xFFFFFF},
      {:text, center_h(600, state), center_v(68, state), :sans24pt, 0x000000, 0xFFFFFF,
       "AtomVM Weather Display"}
    ]

    {:noreply, state, [{:push, rendered}]}
  end

  def handle_info(:update, state) do
    url = "/data/2.5/weather?lat=" <> @lat <> "&lon=" <> @lon <> "&appid=" <> @api_key

    :erlang.display(url)

    rendered_items =
      :http_client.client(url)
      |> WeatherDisplay.render_weather(10, 10)

    rendered = [{:rect, 0, 0, state[:width], state[:height], 0xFFFFFF} | rendered_items]

    {:noreply, state, [{:push, rendered}]}
  end

  def handle_info(msg, state) do
    :erlang.display({:handle_info, msg})
    {:noreply, state}
  end

  defp center_h(width, screen_size) do
    screen_width = screen_size[:width]
    div(screen_width - width, 2)
  end

  defp center_v(height, screen_size) do
    screen_height = screen_size[:height]
    div(screen_height - height, 2)
  end
end
