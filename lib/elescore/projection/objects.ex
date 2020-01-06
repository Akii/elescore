defmodule Elescore.Projection.Objects do
  use GenServer
  use Elescore.Projection.SimpleProjection

  alias Elescore.Store.Event
  alias Elescore.Store.Event.ObjectIdentified

  defmodule Object do
    defstruct id: nil, name: nil
  end

  def stream_names, do: [:"Objects.DB"]

  def init_state, do: :ets.new(:projection_objects, [:named_table])

  def apply_event(%Event{payload: payload} = _event, store) do
    case payload do
      %ObjectIdentified{objectId: object_id, description: name} ->
        :ets.insert(store, {object_id, %Object{id: object_id, name: name}})

      _ ->
        nil
    end

    store
  end
end
