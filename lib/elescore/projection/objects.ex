defmodule Elescore.Projection.Objects do
  use Elescore.Projection.PersistedProjection

  alias Elescore.Store.Event
  alias Elescore.Store.Event.ObjectIdentified

  def stream_names, do: [:"Objects.DB"]

  def table_name, do: "objects"

  def table_schema,
    do: """
    id VARCHAR PRIMARY KEY,
    name VARCHAR
    """

  def init_state, do: nil

  def apply_event(%Event{payload: payload} = _event, _state) do
    case payload do
      %ObjectIdentified{objectId: object_id, description: name} ->
        Sqlitex.Server.query(
          Elescore.Projection.ProjectionStore,
          """
          INSERT INTO objects (id, name) VALUES (?1, ?2)
          ON CONFLICT (id)
          DO UPDATE SET name = excluded.name
          """,
          bind: [object_id, name]
        )

      _ ->
        nil
    end
  end
end
