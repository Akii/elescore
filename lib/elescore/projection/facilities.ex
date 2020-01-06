defmodule Elescore.Projection.Facilities do
  use GenServer
  use Elescore.Projection.SimpleProjection

  alias Elescore.Store.Event
  alias Elescore.Store.Event.{
    FacilityDisrupted,
    FacilityRestored,
    FacilityIdentified,
    FacilityAssignedToObject,
    FacilityLocated,
    FacilityDescriptionUpdated,
    FacilityDeleted
  }

  defmodule Facility do
    defstruct id: nil,
              objectId: nil,
              type: nil,
              name: nil,
              downtime: 0,
              isDisrupted: false,
              geoLocation: nil,
              deleted: false
  end

  def init(_arg) do
    send(self(), :update_downtime)
    {:ok, _sub} = Store.subscribe(stream_names())
    {:ok, init_state()}
  end

  def stream_names, do: [:"Disruptions.DB", :"Facilities.DB"]

  def init_state, do: :ets.new(:projection_facilities, [:named_table])

  def apply_event(%Event{payload: payload} = _event, store) do
    case payload do
      %FacilityDisrupted{facilityId: facility_id} ->
        update(store, facility_id, :isDisrupted, true)

      %FacilityRestored{facilityId: facility_id} ->
        update(store, facility_id, :isDisrupted, false)

      %FacilityIdentified{facilityId: facility_id, facilityType: facility_type, description: description} ->
        update(store, facility_id, :type, facility_type)
        update(store, facility_id, :name, description)
        update(store, facility_id, :deleted, false)

      %FacilityAssignedToObject{facilityId: facility_id, objectId: object_id} ->
        update(store, facility_id, :objectId, object_id)

      %FacilityLocated{facilityId: facility_id, geoLocation: geo_location} ->
        update(store, facility_id, :geoLocation, geo_location)

      %FacilityDescriptionUpdated{facilityId: facility_id, description: description} ->
        update(store, facility_id, :name, description)

      %FacilityDeleted{facilityId: facility_id} ->
        update(store, facility_id, :deleted, true)

      _ -> nil
    end

    store
  end

  def handle_info(:update_downtime, store) do
    downtimes = GenServer.call(Elescore.Projection.Downtimes, :get_state)
    facilities = :ets.tab2list(store)

    Enum.each(facilities, fn {facility_id, _facility} ->
      downtime = Map.get(downtimes, facility_id, 0)
      update(store, facility_id, :downtime, downtime)
    end)

    Process.send_after(self(), :update_downtime, 60_000)
    {:noreply, store}
  end

  defp update(store, facility_id, field, value) do
    case :ets.lookup(store, facility_id) do
      [{_id, facility}] ->
        :ets.insert(store, {facility_id, Map.put(facility, field, value)})

      [] ->
        :ets.insert(store, {facility_id, Map.put(%Facility{id: facility_id}, field, value)})
    end
  end
end
