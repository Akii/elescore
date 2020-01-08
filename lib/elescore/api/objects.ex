defmodule Elescore.Api.Objects do
  def find_objects(search_term) when is_binary(search_term) do
    :not_implemented
  end

  def find_objects, do: :bad_request
end
