defmodule Elescore.Api.Facilities do
  alias Elescore.Api.Types.{FacilityDetails, Object}

  def details_for(facility_id) do
    case fetch_details(facility_id) do
      {:ok, [result]} -> map_result(result)
      _ -> :not_found
    end
  end

  defp fetch_details(facility_id) do
    Sqlitex.Server.query(
      Elescore.Projection.ProjectionStore,
      """
      SELECT f.id, f.type, f.name, o.id as object_id, o.name as object_name, f.downtime, f.is_disrupted
      FROM facilities f LEFT JOIN objects o
      ON f.object_id = o.id
      WHERE f.id = ?1
      """,
      bind: [facility_id]
    )
  end

  defp map_result(
         id: id,
         type: type,
         name: name,
         object_id: object_id,
         object_name: object_name,
         downtime: downtime,
         is_disrupted: is_disrupted
       ) do
    object =
      if object_id != nil do
        %Object{id: object_id, name: object_name}
      else
        nil
      end

    {:ok, %FacilityDetails{
      id: id,
      type: type,
      name: name,
      object: object,
      downtime: downtime,
      isDisrupted: is_disrupted
    }}
  end

  defp map_result(_), do: :internal_server_error
end
