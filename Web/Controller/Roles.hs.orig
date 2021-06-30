module Web.Controller.Roles where

import Web.Controller.Prelude
import Web.View.Roles.Index
import Web.View.Roles.New
import Web.View.Roles.Edit
import Web.View.Roles.Show

instance Controller RolesController where
    action RolesAction = do
        roles <- query @Role |> fetch
        render IndexView { .. }

    action NewRoleAction = do
        let role = newRecord
        render NewView { .. }

    action ShowRoleAction { roleId } = do
        role <- fetch roleId
        render ShowView { .. }

    action EditRoleAction { roleId } = do
        role <- fetch roleId
        render EditView { .. }

    action UpdateRoleAction { roleId } = do
        role <- fetch roleId
        role
            |> buildRole
            |> ifValid \case
                Left role -> render EditView { .. }
                Right role -> do
                    role <- role |> updateRecord
                    setSuccessMessage "Role updated"
                    redirectTo EditRoleAction { .. }

    action CreateRoleAction = do
        let role = newRecord @Role
        role
            |> buildRole
            |> ifValid \case
                Left role -> render NewView { .. } 
                Right role -> do
                    role <- role |> createRecord
                    setSuccessMessage "Role created"
                    redirectTo RolesAction

    action DeleteRoleAction { roleId } = do
        role <- fetch roleId
        deleteRecord role
        setSuccessMessage "Role deleted"
        redirectTo RolesAction

buildRole role = role
    |> fill @'["rolename"]
