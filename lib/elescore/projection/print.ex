defmodule Elescore.Projection.Print do
  use GenServer
  use Elescore.Projection.SimpleProjection

  def stream_name, do: :"Disruptions.DB"

  def init_state, do: 0

  def apply_event(_event, state) do
    state + 1
  end
end
