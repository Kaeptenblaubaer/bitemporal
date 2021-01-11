module Web.Controller.Userroles where

import Web.Controller.Prelude
import Web.View.Userroles.Index
import Web.View.Userroles.New
import Web.View.Userroles.Edit
import Web.View.Userroles.Show

instance Controller UserrolesController where
    action UserrolesAction = do
        userroles <- query @Userrole |> fetch
        render IndexView { .. }

    action NewUserroleAction = do
        let userrole = newRecord
        render NewView { .. }

    action ShowUserroleAction { userroleId } = do
        userrole <- fetch userroleId
        render ShowView { .. }

    action EditUserroleAction { userroleId } = do
        userrole <- fetch userroleId
        render EditView { .. }

    action UpdateUserroleAction { userroleId } = do
        userrole <- fetch userroleId
        userrole
            |> buildUserrole
            |> ifValid \case
                Left userrole -> render EditView { .. }
                Right userrole -> do
                    userrole <- userrole |> updateRecord
                    setSuccessMessage "Userrole updated"
                    redirectTo EditUserroleAction { .. }

    action CreateUserroleAction = do
        let userrole = newRecord @Userrole
        userrole
            |> buildUserrole
            |> ifValid \case
                Left userrole -> render NewView { .. } 
                Right userrole -> do
                    userrole <- userrole |> createRecord
                    setSuccessMessage "Userrole created"
                    redirectTo UserrolesAction

    action DeleteUserroleAction { userroleId } = do
        userrole <- fetch userroleId
        deleteRecord userrole
        setSuccessMessage "Userrole deleted"
        redirectTo UserrolesAction

buildUserrole userrole = userrole
    |> fill @["refuser","refrole"]
