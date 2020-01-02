defmodule Elescore.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      {Elescore.Store.Persistence, [name: Elescore.Store.Persistence]},
      {DynamicSupervisor, [name: Elescore.Supervisor.Store.Subscription, strategy: :one_for_one]},
      {Elescore.Projection.Print, [name: Elescore.Projection.Print]}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Elescore.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
