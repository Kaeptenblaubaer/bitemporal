module Web.Routes where
import IHP.RouterPrelude
import Web.Types
import Generated.Types (Version, ContractState, PartnerState,TariffState)
-- Generator Marker
instance AutoRoute StaticController

instance AutoRoute ContractsController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id ContractState))

instance AutoRoute PartnersController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id PartnerState))

instance AutoRoute WorkflowsController

instance AutoRoute HistoriesController

instance AutoRoute UsersController

instance AutoRoute RolesController

instance AutoRoute UserrolesController

instance AutoRoute VersionsController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id Version))

instance AutoRoute TariffsController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id TariffState))

