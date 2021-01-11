module Web.Routes where
import IHP.RouterPrelude
import Web.Types

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute ContractsController where
    parseArgument = parseIntArgument

instance AutoRoute PartnersController


instance AutoRoute WorkflowsController


instance AutoRoute HistoriesController


instance AutoRoute UsersController


instance AutoRoute RolesController


instance AutoRoute UserrolesController

