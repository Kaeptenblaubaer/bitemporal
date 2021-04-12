module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Tariffs
import Web.Controller.Versions
import Web.Controller.Userroles
import Web.Controller.Roles
import Web.Controller.Users
import Web.Controller.Histories
import Web.Controller.Workflows
import Web.Controller.Partners
import Web.Controller.Contracts
import Web.Controller.Static

instance FrontController WebApplication where
    controllers = 
        [ startPage WelcomeAction
        -- Generator Marker
        , parseRoute @TariffsController
        , parseRoute @VersionsController
        , parseRoute @UserrolesController
        , parseRoute @RolesController
        , parseRoute @UsersController
        , parseRoute @HistoriesController
        , parseRoute @WorkflowsController
        , parseRoute @PartnersController
        , parseRoute @ContractsController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
