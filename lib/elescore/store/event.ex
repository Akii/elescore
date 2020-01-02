defmodule Elescore.Store.Event do
  @moduledoc """
  Definition for a persisted event in the store.
  """

  defstruct ~w[id type stream_name occurred_on payload metadata]a

  @spec new(UUID, atom(), atom(), map(), map(), DateTime.t()) :: Elescore.Store.Event.t()
  def new(id, type, stream_name, payload, metadata \\ %{}, occurred_on \\ DateTime.utc_now()) do
    %__MODULE__{
      id: id,
      type: type,
      stream_name: stream_name,
      payload: payload,
      metadata: metadata,
      occurred_on: occurred_on
    }
  end
end
