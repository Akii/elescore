defmodule Elescore.Store.Event do
  @moduledoc """
  Definition for a persisted event in the store.
  """

  defstruct ~w[id sequence type stream_name occurred_on payload metadata]a

  @spec new(UUID, atom(), atom(), map(), map(), DateTime.t()) :: Elescore.Store.Event.t()
  def new(id, type, stream_name, payload, metadata \\ %{}, occurred_on \\ DateTime.utc_now()) do
    %__MODULE__{
      id: id,
      sequence: nil,
      type: type,
      stream_name: stream_name,
      payload: payload,
      metadata: metadata,
      occurred_on: occurred_on
    }
  end

  defmodule FacilityDisrupted do
    @derive Jason.Encoder
    defstruct facilityId: nil, reason: nil
  end

  defmodule DisruptionReasonUpdated do
    @derive Jason.Encoder
    defstruct facilityId: nil, reason: nil
  end

  defmodule FacilityRestored do
    @derive Jason.Encoder
    defstruct facilityId: nil
  end

  defmodule FacilityIdentified do
    @derive Jason.Encoder
    defstruct facilityId: nil, facilityType: nil, description: nil
  end

  defmodule FacilityAssignedToObject do
    @derive Jason.Encoder
    defstruct facilityId: nil, objectId: nil
  end

  defmodule FacilityLocated do
    @derive Jason.Encoder
    defstruct facilityId: nil, geoLocation: nil
  end

  defmodule FacilityDeleted do
    @derive Jason.Encoder
    defstruct facilityId: nil
  end

  defmodule FacilityDescriptionUpdated do
    @derive Jason.Encoder
    defstruct facilityId: nil, description: nil
  end

  defmodule ObjectIdentified do
    @derive Jason.Encoder
    defstruct objectId: nil, description: nil
  end

  def event_type(%FacilityDisrupted{} = _event), do: :"de.elescore.integration.v1.FacilityDisrupted"
  def event_type(%DisruptionReasonUpdated{} = _event), do: :"de.elescore.integration.v1.DisruptionReasonUpdated"
  def event_type(%FacilityRestored{} = _event), do: :"de.elescore.integration.v1.FacilityRestored"
  def event_type(%FacilityIdentified{} = _event), do: :"de.elescore.integration.v1.FacilityIdentified"
  def event_type(%FacilityAssignedToObject{} = _event), do: :"de.elescore.integration.v1.FacilityAssignedToObject"
  def event_type(%FacilityLocated{} = _event), do: :"de.elescore.integration.v1.FacilityLocated"
  def event_type(%FacilityDeleted{} = _event), do: :"de.elescore.integration.v1.FacilityDeleted"
  def event_type(%FacilityDescriptionUpdated{} = _event), do: :"de.elescore.integration.v1.FacilityDescriptionUpdated"
  def event_type(%ObjectIdentified{} = _event), do: :"de.elescore.integration.v1.ObjectIdentified"
end
