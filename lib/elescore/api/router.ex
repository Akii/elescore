defmodule Elescore.Api.Router do
  use Plug.Router
  alias Elescore.Api.{Facilities, Objects, DisruptionMarkers, Disruptions, Pagination}
  alias Elescore.Projection.{OverallStats, DisruptionsPerDay}
  alias Elescore.Api.Types.DisruptionPerDay

  plug(Plug.Parsers,
    parsers: [:json],
    pass: ["application/json"],
    json_decoder: Jason
  )

  plug(:match)
  plug(:dispatch)

  get "/stats" do
    stats = OverallStats.get_state()
    respond(conn, {:ok, stats})
  end

  get "/stats/average-disruptions-per-day" do
    disruptions_per_day =
      DisruptionsPerDay.get_state()
      |> elem(1)
      |> Enum.map(fn {k, v} -> %DisruptionPerDay{day: k, disruptions: Statistics.IQR.average(v)} end)
      |> Enum.sort(&(&1.day >= &2.day))
      |> Enum.take(31)

    respond(conn, {:ok, disruptions_per_day})
  end

  get "/disruptions" do
    try do
      range_header =
        conn
        |> get_req_header("range")
        |> List.first()

      range = Pagination.parse_range(range_header)
      {:ok, result, response} = Disruptions.all(range)

      conn
      |> put_resp_content_type("application/json")
      |> Pagination.apply_headers(response)
      |> send_resp(206, Jason.encode!(result))
    rescue
      _ -> respond(conn, :bad_request)
    end
  end

  get "/disruptions/markers" do
    response = DisruptionMarkers.get_current_markers()
    respond(conn, response)
  end

  get "/objects" do
    response = Objects.find_objects(conn.params["search"])
    respond(conn, response)
  end

  get "/facilities/:facility_id" do
    response = Facilities.details_for(facility_id)
    respond(conn, response)
  end

  get "/facilities/:facility_id/disruptions" do
    try do
      range_header =
        conn
        |> get_req_header("range")
        |> List.first()

      range = Pagination.parse_range(range_header)
      {:ok, result, response} = Disruptions.for_facility(facility_id, range)

      conn
      |> put_resp_content_type("application/json")
      |> Pagination.apply_headers(response)
      |> send_resp(206, Jason.encode!(result))
    rescue
      _ -> respond(conn, :bad_request)
    end
  end

  match _ do
    respond(conn, :not_found)
  end

  defp respond(conn, response) do
    case response do
      {:ok, data} -> send(conn, data, 200)
      :not_found -> send(conn, "Not found.", 404)
      :bad_request -> send(conn, "Bad request.", 400)
      :internal_server_error -> send(conn, "Internal server error.", 500)
      _ -> send(conn, "Internal server error.", 500)
    end
  end

  defp send(conn, data, status) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, Jason.encode!(data))
  end
end
