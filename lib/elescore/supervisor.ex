defmodule Elescore.Supervisor do
  use Supervisor
  alias Elescore.Config

  def start_link(config) do
    Supervisor.start_link(__MODULE__, config, name: __MODULE__)
  end

  def init(%Config{} = config) do
    children = [
      supervisor(Elescore.Store.Supervisor, [config.databaseFile]),
      supervisor(Elescore.Projection.Supervisor, []),
      supervisor(Elescore.Api.Supervisor, [])
    ]

    supervise(children, strategy: :one_for_all)
  end
end
