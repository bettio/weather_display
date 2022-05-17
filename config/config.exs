use Mix.Config

config :weather_display, :open_weather_map,
  api_key: "MyAPIKey",
  location: [lat: "45.398446", lon: "11.876530"]

config :weather_display, :wifi,
  ssid: "MyWifi",
  psk: "MyPass"
