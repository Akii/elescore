defmodule Elescore.Projection.SimpleProjection do
  @callback stream_name() :: Atom.t()
  @callback init_state() :: any()
  @callback apply_event(any(), any()) :: any()

  defmacro __using__(_params) do
    quote do
      alias Elescore.Store

      def init(_arg) do
        {:ok, sub} = Store.subscribe(stream_name())
        Process.send_after(self(), :project, 1000)
        {:ok, {sub, init_state()}}
      end

      def start_link(options \\ []) do
        GenServer.start_link(__MODULE__, nil, options)
      end

      def terminate(_reason, {sub, _state}) do
        GenServer.stop(sub)
      end

      def handle_info(:project, {sub, state}) do
        {:ok, next_events} = Store.get_next_events(sub)

        new_state = Enum.reduce(next_events, state, &apply_event/2)
        timeout = if Enum.empty?(next_events), do: 1000, else: 0

        Process.send_after(self(), :project, timeout)
        {:noreply, {sub, new_state}}
      end

      def handle_call(:get_state, _from, {sub, state}) do
        {:reply, state, {sub, state}}
      end

      # Defoverridable makes the given functions in the current module overridable
      # Without defoverridable, new definitions of greet will not be picked up
      defoverridable [init: 1, start_link: 1, handle_info: 2, handle_call: 3]
    end
  end
end
