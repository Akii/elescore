defmodule Elescore.Api.Pagination do
  @moduledoc """
  Request:
  X-Range: <field> [<value>][; limit <l>][; offset <o>][; order <asc|desc>]

  X-Range: id; offset 0; limit 50; order desc
  X-Range: id 123; limit 50; order asc
  X-Range: id 45343; limit 50; order desc

  Response:

  X-total-count: 214818
  X-accept-ranges: id,occurredOn,updatedOn,resolvedOn,duration
  X-content-range: id 234416..234360
  X-next-range: id 225847;limit 50;offset 1;order desc
  """

  use Combine
  import Plug.Conn

  defmodule ContentRangeRequest do
    @moduledoc """
    Represents the Range header sent by the client
    """

    defstruct key: :id, id: nil, offset: 0, limit: 50, order: :asc
  end

  defmodule ContentRangeResponse do
    @moduledoc """
    Represents the response to a ranged request
    """

    defstruct total_count: 0, accept_ranges: [], content_range: "", next_range: nil
  end

  @spec parse_range(String.t()) :: %ContentRangeRequest{}
  def parse_range(range)
    when not is_nil(range) do
    range_p =
      either(
        word() |> ignore(space()) |> word(),
        word()
      )

    limit_p = ignore(string("; limit ")) |> integer()
    offset_p = ignore(string("; offset ")) |> integer()

    order_p =
      ignore(string("; order "))
      |> either(
        string("asc"),
        string("desc")
      )

    content_range =
      Combine.parse(
        range,
        range_p
        |> option(limit_p)
        |> option(offset_p)
        |> option(order_p)
      )

    case content_range do
      [key, key_val, limit, offset, order] ->
        %ContentRangeRequest{
          key: String.to_existing_atom(key),
          id: key_val,
          offset: min(offset, 500),
          limit: min(limit, 500),
          order: if(order == "desc", do: :desc, else: :asc)
        }

      [key, limit, offset, order] ->
        %ContentRangeRequest{
          key: String.to_existing_atom(key),
          id: nil,
          offset: min(default_with(offset, 0), 500),
          limit: min(default_with(limit, 50), 500),
          order: if(order == "desc", do: :desc, else: :asc)
        }

      _ ->
        %ContentRangeRequest{}
    end
  end

  def parse_range(_), do: %ContentRangeRequest{ order: :desc }

  defp default_with(nil, default), do: default
  defp default_with(val, _default), do: val

  def make_response(total_count, accept_ranges, range, []) do
    %ContentRangeResponse{
      total_count: total_count,
      accept_ranges: accept_ranges,
      content_range: "0..0",
      next_range: %ContentRangeRequest{range | id: 0, offset: 0}
    }
  end

  @spec make_response(integer(), list(atom()), %ContentRangeRequest{}, list()) ::
          %ContentRangeResponse{}
  def make_response(total_count, accept_ranges, range, result) do
    key_values = result |> Enum.map(&Map.get(&1, range.key))

    min = Enum.min(key_values)
    max = Enum.max(key_values)

    content_range =
      case range.order do
        :asc -> "#{range.key} #{min}..#{max}"
        :desc -> "#{range.key} #{max}..#{min}"
      end

    next_range =
      case range.order do
        :asc -> %ContentRangeRequest{range | id: max, offset: 1}
        :desc -> %ContentRangeRequest{range | id: min, offset: 1}
      end

    %ContentRangeResponse{
      total_count: total_count,
      accept_ranges: accept_ranges,
      content_range: content_range,
      next_range: next_range
    }
  end

  def to_sql(%ContentRangeRequest{id: id} = range, field_map)
      when is_nil(id) do
    {
      "ORDER BY #{field_map[range.key]} #{range.order} LIMIT ? OFFSET ?",
      [range.limit, range.offset]
    }
  end

  def to_sql(%ContentRangeRequest{order: order} = range, field_map)
      when order == :asc do
    {
      "AND #{field_map[range.key]} >= ? ORDER BY #{field_map[range.key]} ASC LIMIT ? OFFSET ?",
      [range.id, range.limit, range.offset]
    }
  end

  def to_sql(%ContentRangeRequest{order: order} = range, field_map)
      when order == :desc do
    {
      "AND #{field_map[range.key]} <= ? ORDER BY #{field_map[range.key]} DESC LIMIT ? OFFSET ?",
      [range.id, range.limit, range.offset]
    }
  end

  def apply_headers(conn, %ContentRangeResponse{next_range: nr} = response) do
    conn
    |> put_resp_header("Total-Count", to_string(response.total_count))
    |> put_resp_header("Accept-Ranges", Enum.join(response.accept_ranges, ","))
    |> put_resp_header("Content-Range", response.content_range)
    |> put_resp_header("Next-Range", "#{nr.key} #{nr.id}; limit #{nr.limit}; offset #{nr.offset}; order #{nr.order}")
  end
end
