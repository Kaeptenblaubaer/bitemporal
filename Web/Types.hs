module Web.Types where

import IHP.Prelude
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data ContractsController
    = ContractsAction
    | NewContractAction 
    | ShowContractAction { contractId :: !(Id ContractState) }
    | CreateContractAction
    | EditContractAction { contractId :: !(Id ContractState) }
    | UpdateContractAction { contractId :: !(Id ContractState) }
    | DeleteContractAction { contractId :: !(Id ContractState) }
    deriving (Eq, Show, Data)

data PartnersController
    = PartnersAction
    | NewPartnerAction
    | ShowPartnerAction { partnerId :: !(Id PartnerState) }
    | CreatePartnerAction
    | EditPartnerAction { partnerId :: !(Id PartnerState) }
    | UpdatePartnerAction { partnerId :: !(Id PartnerState) }
    | DeletePartnerAction { partnerId :: !(Id PartnerState) }
    deriving (Eq, Show, Data)

data WorkflowsController
    = WorkflowsAction
    | NewWorkflowAction
    | ShowWorkflowAction { workflowId :: !(Id Workflow) }
    | CreateWorkflowAction
    | EditWorkflowAction { workflowId :: !(Id Workflow) }
    | UpdateWorkflowAction { workflowId :: !(Id Workflow) }
    | DeleteWorkflowAction { workflowId :: !(Id Workflow) }
    | NextWorkflowAction { workflowId :: !(Id Workflow) }
    | CommitWorkflowAction
    | RollbackWorkflowAction
    | SuspendWorkflowAction
    | ResumeWorkflowAction
    deriving (Eq, Show, Data)

data HistoriesController
    = HistoriesAction
    | NewHistoryAction
    | ShowHistoryAction { historyId :: !(Id History) }
    | CreateHistoryAction
    | EditHistoryAction { historyId :: !(Id History) }
    | UpdateHistoryAction { historyId :: !(Id History) }
    | DeleteHistoryAction { historyId :: !(Id History) }
    deriving (Eq, Show, Data)

data UsersController
    = UsersAction
    | NewUserAction
    | ShowUserAction { userId :: !(Id User) }
    | CreateUserAction
    | EditUserAction { userId :: !(Id User) }
    | UpdateUserAction { userId :: !(Id User) }
    | DeleteUserAction { userId :: !(Id User) }
    deriving (Eq, Show, Data)

data RolesController
    = RolesAction
    | NewRoleAction
    | ShowRoleAction { roleId :: !(Id Role) }
    | CreateRoleAction
    | EditRoleAction { roleId :: !(Id Role) }
    | UpdateRoleAction { roleId :: !(Id Role) }
    | DeleteRoleAction { roleId :: !(Id Role) }
    deriving (Eq, Show, Data)

data UserrolesController
    = UserrolesAction
    | NewUserroleAction
    | ShowUserroleAction { userroleId :: !(Id Userrole) }
    | CreateUserroleAction
    | EditUserroleAction { userroleId :: !(Id Userrole) }
    | UpdateUserroleAction { userroleId :: !(Id Userrole) }
    | DeleteUserroleAction { userroleId :: !(Id Userrole) }
    deriving (Eq, Show, Data)

data VersionsController
    = VersionsAction
    | NewVersionAction
    | ShowVersionAction { versionId :: !(Id Version) }
    | CreateVersionAction
    | EditVersionAction { versionId :: !(Id Version) }
    | UpdateVersionAction { versionId :: !(Id Version) }
    | DeleteVersionAction { versionId :: !(Id Version) }
    | ShowStateByVersionAction { versionId :: !(Id Version) }
    deriving (Eq, Show, Data)

data TariffsController
    = TariffsAction
    | NewTariffAction
    | ShowTariffAction { tariffId :: !(Id TariffState) }
    | CreateTariffAction
    | EditTariffAction { tariffId :: !(Id TariffState) }
    | UpdateTariffAction { tariffId :: !(Id TariffState) }
    | DeleteTariffAction { tariffId :: !(Id TariffState) }
    deriving (Eq, Show, Data)
