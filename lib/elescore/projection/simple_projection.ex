defmodule Elescore.Projection.SimpleProjection do
  @callback stream_names() :: list(Atom.t())
  @callback init_state() :: any()
  @callback apply_event(any(), any()) :: any()

  defmacro __using__(_params) do
    quote do
      alias Elescore.Store

      def start_link() do
        GenServer.start_link(__MODULE__, nil,  name: __MODULE__)
      end

      def init(_arg) do
        {:ok, _sub} = Store.subscribe(stream_names())
        {:ok, init_state()}
      end

      def get_state do
        GenServer.call(__MODULE__, :get_state)
      end

      def handle_call({:next_events, events}, _from, state) do
        new_state = Enum.reduce(events, state, &apply_event/2)
        {:reply, :processed, new_state}
      end

      def handle_call(:get_state, _from, state) do
        {:reply, state, state}
      end

      defoverridable []
    end
  end
end
