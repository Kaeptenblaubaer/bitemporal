module Web.Types where

import IHP.Prelude
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data ContractsController
    = ContractsAction
    | NewContractAction 
    | ShowContractAction { contractId :: !(Id Contract) }
    | CreateContractAction
    | EditContractAction { contractId :: !(Id Contract) }
    | UpdateContractAction { contractId :: !(Id Contract) }
    | DeleteContractAction { contractId :: !(Id Contract) }
    deriving (Eq, Show, Data)

data PartnersController
    = PartnersAction
    | NewPartnerAction
    | ShowPartnerAction { partnerId :: !(Id Partner) }
    | CreatePartnerAction
    | EditPartnerAction { partnerId :: !(Id Partner) }
    | UpdatePartnerAction { partnerId :: !(Id Partner) }
    | DeletePartnerAction { partnerId :: !(Id Partner) }
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
