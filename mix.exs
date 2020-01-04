defmodule Elescore.MixProject do
  use Mix.Project

  def project do
    [
      app: :elescore,
      version: "0.1.0",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Elescore, []}
    ]
  end

  defp deps do
    [
      {:uuid, "~> 1.1"},
      {:credo, "~> 1.1.0", only: [:dev, :test], runtime: false},
      {:sqlitex, "~> 1.7"},
      {:jason, "~> 1.1"}
    ]
  end
end
