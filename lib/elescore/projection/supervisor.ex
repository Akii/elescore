defmodule Elescore.Projection.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    children = [
      # TODO: if this one dies, all have to be restarted!
      worker(Sqlitex.Server, [":memory:", [name: Elescore.Projection.ProjectionStore]]),

      # TODO: these can restart independently
      worker(Elescore.Projection.OverallStats, []),
      worker(Elescore.Projection.Disruptions, []),
      worker(Elescore.Projection.Downtimes, []),
      worker(Elescore.Projection.Facilities, []),
      worker(Elescore.Projection.Objects, []),
      worker(Elescore.Projection.DisruptionsPerDay, []),
    ]

    supervise(children, strategy: :one_for_one)
  end
end
