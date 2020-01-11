defmodule Elescore.Api.Supervisor do
  def start_link do
    children = [
      Plug.Cowboy.child_spec(scheme: :http, plug: Elescore.Api.Router, options: [port: 8000])
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: __MODULE__)
  end
end
