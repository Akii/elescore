defmodule Elescore.Projection.Stats do
  use GenServer
  use Elescore.Projection.SimpleProjection

  alias Elescore.Store.Event
  alias Elescore.Store.Event.{
    FacilityDisrupted,
    FacilityRestored,
    FacilityIdentified,
    FacilityDeleted,
    ObjectIdentified
  }

  defmodule State do
    defstruct disruptions: 0, active_disruptions: 0, facilities: 0, objects: 0
  end

  def stream_names, do: [:"Disruptions.DB", :"Facilities.DB", :"Objects.DB"]

  def init_state, do: %State{}

  def apply_event(%Event{payload: payload} = _event, state) do
    case payload do
      %FacilityDisrupted{} ->
        %State{
          state
          | disruptions: state.disruptions + 1,
            active_disruptions: state.active_disruptions + 1
        }

      %FacilityRestored{} ->
        %State{state | active_disruptions: state.active_disruptions - 1}

      %FacilityIdentified{} ->
        %State{state | facilities: state.facilities + 1}

      %FacilityDeleted{} ->
        %State{state | facilities: state.facilities - 1}

      %ObjectIdentified{} ->
        %State{state | objects: state.objects + 1}

      _ ->
        state
    end
  end
end
