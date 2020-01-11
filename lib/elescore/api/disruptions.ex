defmodule Elescore.Api.Disruptions do
  alias Elescore.Api.Types.Disruption
  alias Elescore.Api.Pagination.{ContentRangeRequest, ContentRangeResponse}
  alias Elescore.Api.Pagination
  import Elescore.Api.Types.Disruption

  @spec all(%ContentRangeRequest{}) :: {:ok, list(), %ContentRangeResponse{}}
  def all(%ContentRangeRequest{key: key} = range)
      when key in [:id, :occurredOn, :updatedOn, :resolvedOn, :duration] do
    total_count = count_all()
    params = Pagination.to_sql(range, range_to_field_mapping())

    case fetch_disruptions(params) do
      {:ok, rows} ->
        disruptions = Enum.map(rows, &map_row/1)

        content_range_response =
          Pagination.make_response(
            total_count,
            [:id, :occurredOn, :updatedOn, :resolvedOn, :duration],
            range,
            disruptions
          )

        {:ok, disruptions, content_range_response}
    end
  end

  def all(_), do: :bad_request

  def for_facility(facility_id, %ContentRangeRequest{key: key} = range)
      when key in [:id, :occurredOn, :updatedOn, :resolvedOn, :duration] do
    total_count = count_for_facility(facility_id)
    {where_clause, params} = Pagination.to_sql(range, range_to_field_mapping())

    case fetch_disruptions({"AND f.id = ? #{where_clause}", [facility_id | params]}) do
      {:ok, rows} ->
        disruptions = Enum.map(rows, &map_row/1)

        content_range_response =
          Pagination.make_response(
            total_count,
            [:id, :occurredOn, :updatedOn, :resolvedOn, :duration],
            range,
            disruptions
          )

        {:ok, disruptions, content_range_response}
    end
  end

  def for_facility(_, _), do: :bad_request

  defp count_all() do
    {:ok, [[count: count]]} =
      Sqlitex.Server.query(
        Elescore.Projection.ProjectionStore,
        "SELECT COUNT(*) AS count FROM disruptions"
      )

    count
  end

  defp count_for_facility(facility_id) do
    {:ok, [[count: count]]} =
      Sqlitex.Server.query(
        Elescore.Projection.ProjectionStore,
        "SELECT COUNT(*) AS count FROM disruptions WHERE facility_id = ?",
        bind: [facility_id]
      )

    count
  end

  defp fetch_disruptions({where_clause, params}) do
    Sqlitex.Server.query(
      Elescore.Projection.ProjectionStore,
      """
      SELECT
        d.id AS id,
        f.id AS facility_id,
        f.name AS facility_name,
        o.id AS object_id,
        o.name AS object_name,
        d.reason,
        d.occurred_on,
        d.updated_on,
        d.resolved_on,
        STRFTIME('%s', IFNULL(d.resolved_on, 'now')) - STRFTIME('%s', d.occurred_on) AS duration
      FROM disruptions d
        LEFT JOIN facilities f ON d.facility_id = f.id
        LEFT JOIN objects o ON f.object_id = o.id
      WHERE 1 = 1 #{where_clause}
      """,
      bind: params
    )
  end

  defp map_row(row) do
    %Disruption{
      id: row[:id],
      facilityId: row[:facility_id],
      facilityName: row[:facility_name],
      objectId: row[:object_id],
      objectName: row[:object_name],
      reason: row[:reason],
      occurredOn: row[:occurred_on],
      updatedOn: row[:updated_on],
      resolvedOn: row[:resolved_on],
      duration: row[:duration]
    }
  end

  defp range_to_field_mapping(), do:
    %{
      id: "d.id",
      occurredOn: "occurred_on",
      updatedOn: "updated_on",
      resolvedOn: "resolved_on",
      duration: "duration",
    }
end
