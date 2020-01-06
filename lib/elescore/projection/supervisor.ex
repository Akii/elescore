defmodule Elescore.Projection.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    children = [
      worker(Elescore.Projection.Disruptions, []),
      worker(Elescore.Projection.Downtimes, []),
      worker(Elescore.Projection.Stats, []),
      worker(Elescore.Projection.Facilities, []),
      worker(Elescore.Projection.Objects, []),
    ]

    supervise(children, strategy: :one_for_one)
  end
end
