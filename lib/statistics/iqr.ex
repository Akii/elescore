defmodule Statistics.IQR do
  import Integer

  @type sample :: list(float)

  @spec singleton_sample(float) :: sample
  def singleton_sample(a), do: [a]

  @spec insert_sample(float, sample) :: sample
  def insert_sample(a, sample), do: [a | sample]

  @spec insert_unique_sample(float, sample) :: sample
  def insert_unique_sample(a, sample) do
    if Enum.member?(sample, a) do
      sample
    else
      insert_sample(a, sample)
    end
  end

  @spec remove_duplicates(sample) :: sample
  def remove_duplicates(sample), do: Enum.uniq(sample)

  @spec restrict_sample_size(integer, sample) :: sample
  def restrict_sample_size(size, sample)
    when size >= 1 do
      sample
      |> Enum.reverse
      |> Enum.take(size)
      |> Enum.reverse
  end
  def restrict_sample_size(_size, _sample), do: raise "Cannot use sample size smaller than 1"

  @spec median(sample) :: float
  def median(sample) do
    sample_length = Enum.count(sample)
    middle_index = div(sample_length, 2)

    if is_odd(sample_length) do
      Enum.at(sample, middle_index)
    else
      (Enum.at(sample, middle_index - 1) + Enum.at(sample, middle_index)) / 2
    end
  end

  @spec average(sample) :: float
  def average(sample), do: Enum.sum(sample) / Enum.count(sample)

end
