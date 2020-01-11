defmodule Elescore.Api.Objects do
  alias Elescore.Api.Types.{ObjectSearchResult, Object, Facility}

  def find_objects(search_term) when is_binary(search_term) do
    if String.length(search_term) > 3 do
      {:ok, rows} = fetch_results(search_term)
      search_results = map_results(rows)
      {:ok, search_results}
    else
      :bad_request
    end
  end

  def find_objects, do: :bad_request

  defp fetch_results(search_term) do
    Sqlitex.Server.query(
      Elescore.Projection.ProjectionStore,
      """
      SELECT
        o.id AS object_id,
        o.name AS object_name,
        f.id AS facility_id,
        f.type AS facility_type,
        f.name AS facility_name,
        f.is_disrupted
      FROM objects o JOIN facilities f ON (o.id = f.object_id)
      WHERE o.name LIKE ?1 OR f.name LIKE ?1
      """,
      bind: ["%#{search_term}%"]
    )
  end

  defp map_results(rows) do
    rows
    |> Enum.map(fn row -> {map_object(row), map_facility(row)} end)
    |> Enum.group_by(
      fn {object, _} -> object end,
      fn {_, facility} -> facility end
    )
    |> Enum.map(fn {object, facilities} ->
      %ObjectSearchResult{object: object, facilities: facilities}
    end)
  end

  defp map_object(row) do
    %Object{id: row[:object_id], name: row[:object_name]}
  end

  defp map_facility(row) do
    %Facility{
      id: row[:facility_id],
      type: row[:facility_type],
      name: row[:facility_name],
      isDisrupted: row[:is_disrupted]
    }
  end
end
