defmodule Elescore.Api.DisruptionMarkers do
  alias Elescore.Api.Types.MapMarker

  def get_current_markers() do
    {:ok, rows} = fetch_markers()
    markers = Enum.map(rows, &map_marker/1)
    {:ok, markers}
  end

  defp fetch_markers() do
    Sqlitex.Server.query(
      Elescore.Projection.ProjectionStore,
      """
      SELECT
        d.id AS id,
        o.id AS object_id,
        o.name AS object_name,
        f.id AS facility_id,
        f.type AS facility_type,
        f.name AS facility_name,
        d.reason,
        d.occurred_on,
        f.geo_location_lat,
        f.geo_location_lng
      FROM disruptions d
        JOIN facilities f ON (d.facility_id = f.id)
        JOIN objects o ON (f.object_id = o.id)
      WHERE f.geo_location_lat IS NOT NULL
        AND f.geo_location_lng IS NOT NULL
        AND d.resolved_on IS NULL
      """
    )
  end

  defp map_marker(row) do
    %MapMarker{
      id: row[:id],
      objectId: row[:object_id],
      objectName: row[:object_name],
      facilityId: row[:facility_id],
      facilityType: row[:facility_type],
      facilityName: row[:facility_name],
      reason: row[:reason],
      since: row[:occurred_on],
      geoCoordinates: %{
        "lat" => row[:geo_location_lat],
        "lng" => row[:geo_location_lng]
      }
    }
  end
end
