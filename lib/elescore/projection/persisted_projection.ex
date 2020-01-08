defmodule Elescore.Projection.PersistedProjection do
  @callback stream_names() :: list(Atom.t())
  @callback init_state() :: any()
  @callback table_name() :: String.t()
  @callback table_schema() :: String.t()
  @callback apply_event(any(), any()) :: any()

  defmacro __using__(_params) do
    quote do
      alias Elescore.Store

      def start_link() do
        GenServer.start_link(__MODULE__, nil,  name: __MODULE__)
      end

      def init(_arg) do
        empty_projection()
        create_schema()

        {:ok, _sub} = Store.subscribe(stream_names())
        {:ok, init_state()}
      end

      def empty_projection() do
        :ok = Sqlitex.Server.exec(Elescore.Projection.ProjectionStore, "DROP TABLE IF EXISTS #{table_name()}")
      end

      def create_schema() do
        :ok = Sqlitex.Server.exec(
          Elescore.Projection.ProjectionStore,
          """
          CREATE TABLE #{table_name()} (
            #{table_schema()}
          );
          """
        )
      end

      def handle_call({:next_events, events}, _from, state) do
        new_state = Enum.reduce(events, state, &apply_event/2)
        {:reply, :processed, new_state}
      end

      defoverridable []
    end
  end
end
