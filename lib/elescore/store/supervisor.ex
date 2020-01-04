defmodule Elescore.Store.Supervisor do
  use Supervisor

  def start_link(databaseFile) do
    Supervisor.start_link(__MODULE__, databaseFile, name: __MODULE__)
  end

  def init(databaseFile) do
    children = [
      worker(Sqlitex.Server, [databaseFile, [name: Elescore.Store.EventStore]]),
      worker(Elescore.Store.Persistence, []),
      supervisor(Elescore.Store.SubscriptionSupervisor, [])
    ]

    supervise(children, strategy: :one_for_one)
  end
end
