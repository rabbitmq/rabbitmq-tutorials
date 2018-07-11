use Mix.Config

config :lager,
  handlers: [
    lager_console_backend: [{:level, :warning}]
  ]
