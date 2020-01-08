defmodule Elescore.Projection.OverallStats do
  use Elescore.Projection.SimpleProjection

  alias Elescore.Store.Event
  alias Elescore.Store.Event.{
    FacilityDisrupted,
    FacilityRestored,
    FacilityIdentified,
    FacilityDeleted,
    ObjectIdentified
  }
  alias Elescore.Api.Types.OverallStats

  def stream_names, do: [:"Disruptions.DB", :"Facilities.DB", :"Objects.DB"]

  def init_state, do: %OverallStats{}

  def apply_event(%Event{payload: payload} = _event, state) do
    case payload do
      %FacilityDisrupted{} ->
        %OverallStats{
          state
          | disruptions: state.disruptions + 1,
            activeDisruptions: state.activeDisruptions + 1
        }

      %FacilityRestored{} ->
        %OverallStats{state | activeDisruptions: state.activeDisruptions - 1}

      %FacilityIdentified{} ->
        %OverallStats{state | facilities: state.facilities + 1}

      %FacilityDeleted{} ->
        %OverallStats{state | facilities: state.facilities - 1}

      %ObjectIdentified{} ->
        %OverallStats{state | objects: state.objects + 1}

      _ ->
        state
    end
  end
end
