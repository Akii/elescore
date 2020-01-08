defmodule Elescore.Api.Types do
  defmodule OverallStats do
    @moduledoc false

    @derive Jason.Encoder
    defstruct disruptions: 0, activeDisruptions: 0, facilities: 0, objects: 0
  end

  defmodule DisruptionPerDay do
    @moduledoc false

    @derive Jason.Encoder
    defstruct day: nil, disruptions: 0
  end

  defmodule Disruption do
    @moduledoc false

    @derive Jason.Encoder
    defstruct id: 0,
              facilityId: nil,
              facilityName: nil,
              objectId: nil,
              objectName: nil,
              reason: nil,
              occurredOn: nil,
              updatedOn: nil,
              resolvedOn: nil,
              duration: nil
  end

  defmodule MapMarker do
    @moduledoc false

    @derive Jason.Encoder
    defstruct id: nil,
              objectId: nil,
              objectName: nil,
              facilityId: nil,
              facilityType: nil,
              facilityName: nil,
              reason: nil,
              since: nil,
              geoCoordinates: nil
  end

  defmodule ObjectSearchResult do
    @moduledoc false

    @derive Jason.Encoder
    defstruct object: nil, facilities: []
  end

  defmodule Object do
    @moduledoc false

    @derive Jason.Encoder
    defstruct id: nil, name: "Unknown"
  end

  defmodule Facility do
    @moduledoc false

    @derive Jason.Encoder
    defstruct id: nil, type: nil, name: "Unknown", isDisrupted: false
  end

  defmodule FacilityDetails do
    @moduledoc false

    @derive Jason.Encoder
    defstruct id: nil, type: nil, name: "Unknown", object: nil, downtime: 0, isDisrupted: false
  end
end
