defmodule Elescore do
  use Application

  defmodule Config do
    defstruct databaseFile: ""
  end

  def start(_type, _args) do
    cfg = %Config {
      databaseFile: "./persistence/dbv31.sqlite"
    }
    Elescore.Supervisor.start_link(cfg)
  end
end
