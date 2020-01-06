defmodule Elescore.Store.Persistence do
  @moduledoc """
  Persistence for Events.
  """

  use GenServer
  alias Elescore.Store.Event
  alias Elescore.Store.Event.{FacilityDisrupted, DisruptionReasonUpdated, FacilityRestored, FacilityIdentified, FacilityAssignedToObject, FacilityLocated, FacilityDeleted, FacilityDescriptionUpdated, ObjectIdentified}

  def start_link() do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def init(arg) do
    {:ok, arg}
  end

  def append(stream_name, payload, metadata \\ %{}) do
    event =
      Event.new(
        UUID.uuid4(),
        Event.event_type(payload),
        stream_name,
        payload,
        metadata
      )

    GenServer.call(__MODULE__, {:append, event})
  end

  def read(stream_names, start_sequence, number_of_events) do
    GenServer.call(__MODULE__, {:read, stream_names, start_sequence, number_of_events})
  end

  def handle_call({:append, %Event{} = event}, _from, state) do
    # todo
    # insert
    # get sequence
    # update event
    # broadcast

    GenServer.cast(self(), {:broadcast_event, event})
    {:reply, {:ok, event}, state}
  end

  def handle_call({:read, stream_names, start_sequence, number_of_events}, _from, state) do
    {:ok, rows} =
      Sqlitex.Server.query(
        Elescore.Store.EventStore,
        build_query(stream_names),
        into: %{},
        bind: Enum.concat(stream_names, [start_sequence, number_of_events])
      )

    events = rows |> Enum.map(&to_event/1)

    if Enum.empty?(events) do
      {:reply, :end_of_stream, state}
    else
      next_sequence = events |> Enum.map(& &1.sequence) |> Enum.max()
      {:reply, {:ok, events, next_sequence}, state}
    end
  end

  def build_query(stream_names) do
    number_of_streams = Enum.count(stream_names)

    match_stream =
      1..number_of_streams
      |> Enum.map(fn i ->
        "stream = ?#{i}"
      end)
      |> Enum.join(" OR ")

    """
    SELECT id, sequence, type, stream AS stream_name, occurred_on, payload
    FROM event_store
    WHERE (#{match_stream}) AND sequence > ?#{number_of_streams + 1}
    LIMIT ?#{number_of_streams + 2}
    """
  end

  def handle_cast({:broadcast_event, event}, state) do
    Elescore.Store.SubscriptionSupervisor
    |> Supervisor.which_children()
    |> Enum.each(&notify_child(&1, event))

    {:noreply, state}
  end

  defp to_event(row) do
    {:blob, payload} = row.payload
    type = String.to_atom(row.type)
    {:ok, occurred_on, _} = DateTime.from_iso8601(row.occurred_on <> "Z")

    %Event{
      id: row.id,
      sequence: row.sequence,
      type: type,
      stream_name: String.to_atom(row.stream_name),
      payload: decode_payload(type, Jason.decode!(payload, [strings: :copy])),
      metadata: %{},
      occurred_on: occurred_on
    }
  end

  defp decode_payload(:"de.elescore.integration.v1.FacilityDisrupted", payload) do
    %{"facilityId" => facility_id, "reason" => reason} = payload

    %FacilityDisrupted{
      facilityId: facility_id,
      reason: reason
    }
  end

  defp decode_payload(:"de.elescore.integration.v1.DisruptionReasonUpdated", payload) do
    %{"facilityId" => facility_id, "reason" => reason} = payload

    %DisruptionReasonUpdated{
      facilityId: facility_id,
      reason: reason
    }
  end

  defp decode_payload(:"de.elescore.integration.v1.FacilityRestored", payload) do
    %{"facilityId" => facility_id} = payload

    %FacilityRestored{
      facilityId: facility_id
    }
  end

  defp decode_payload(:"de.elescore.integration.v1.FacilityIdentified", payload) do
    %{"facilityId" => facility_id, "facilityType" => facility_type, "description" => description} = payload

    %FacilityIdentified{
      facilityId: facility_id,
      facilityType: facility_type,
      description: description
    }
  end

  defp decode_payload(:"de.elescore.integration.v1.FacilityAssignedToObject", payload) do
    %{"facilityId" => facility_id, "objectId" => object_id} = payload

    %FacilityAssignedToObject{
      facilityId: facility_id,
      objectId: object_id
    }
  end

  defp decode_payload(:"de.elescore.integration.v1.FacilityLocated", payload) do
    %{"facilityId" => facility_id, "geoLocation" => geo_location} = payload

    %FacilityLocated{
      facilityId: facility_id,
      geoLocation: geo_location
    }
  end

  defp decode_payload(:"de.elescore.integration.v1.FacilityDeleted", payload) do
    %{"facilityId" => facility_id} = payload

    %FacilityDeleted{
      facilityId: facility_id
    }
  end

  defp decode_payload(:"de.elescore.integration.v1.FacilityDescriptionUpdated", payload) do
    %{"facilityId" => facility_id, "description" => description} = payload

    %FacilityDescriptionUpdated{
      facilityId: facility_id,
      description: description
    }
  end

  defp decode_payload(:"de.elescore.integration.v1.ObjectIdentified", payload) do
    %{"objectId" => object_id, "description" => description} = payload

    %ObjectIdentified{
      objectId: object_id,
      description: description
    }
  end

  defp notify_child({_, pid, :worker, [Elescore.Store.Subscription]}, event) do
    GenServer.cast(pid, {:handle_event, event})
  end

  defp notify_child(_child, _event), do: nil
end
