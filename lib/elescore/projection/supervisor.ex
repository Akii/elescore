defmodule Elescore.Projection.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    children = [
      worker(Elescore.Projection.Print, [])
    ]

    supervise(children, strategy: :one_for_one)
  end
end
