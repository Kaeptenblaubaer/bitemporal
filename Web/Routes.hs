module Web.Routes where
import IHP.RouterPrelude
import Web.Types
import Generated.Types (Version, Contract, Partner,Tariff)
-- Generator Marker
instance AutoRoute StaticController

instance AutoRoute ContractsController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id Contract))

instance AutoRoute PartnersController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id Partner))

instance AutoRoute WorkflowsController

instance AutoRoute HistoriesController

instance AutoRoute UsersController

instance AutoRoute RolesController

instance AutoRoute UserrolesController

instance AutoRoute VersionsController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id Version))

instance AutoRoute TariffsController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id Tariff))

