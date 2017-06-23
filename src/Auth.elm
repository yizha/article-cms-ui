module Auth
    exposing
        ( UserRole
        , CmsRole(..)
        , hasRole
        , hasAnyRole
        , hasAllRoles
        , canCreateArticle
        , canEditArticle
        , canSaveArticle
        , canSubmitArticle
        , canSubmitOtherArticle
        , canPublishArticle
        , canManageLogin
        )

import List
import Time
import Bitwise


type alias UserRole =
    Int


type CmsRole
    = CmsRoleArticleCreate
    | CmsRoleArticleEdit
    | CmsRoleArticleSubmit
    | CmsRoleArticlePublish
    | CmsRoleLoginManage


cmsRoleValue : CmsRole -> Int
cmsRoleValue role =
    case role of
        CmsRoleArticleCreate ->
            Bitwise.shiftLeftBy 0 1

        CmsRoleArticleEdit ->
            Bitwise.shiftLeftBy 1 1

        CmsRoleArticleSubmit ->
            Bitwise.shiftLeftBy 2 1

        CmsRoleArticlePublish ->
            Bitwise.shiftLeftBy 3 1

        CmsRoleLoginManage ->
            Bitwise.shiftLeftBy 20 1


hasRole : UserRole -> CmsRole -> Bool
hasRole userRole role =
    let
        roleValue =
            cmsRoleValue role
    in
        (Bitwise.and roleValue userRole) == roleValue


hasAnyRole : UserRole -> List CmsRole -> Bool
hasAnyRole userRole roleList =
    List.any (hasRole userRole) roleList


hasAllRoles : UserRole -> List CmsRole -> Bool
hasAllRoles userRole roleList =
    List.all (hasRole userRole) roleList


canCreateArticle : UserRole -> Bool
canCreateArticle userRole =
    hasRole userRole CmsRoleArticleCreate


canEditArticle : UserRole -> Bool
canEditArticle userRole =
    hasRole userRole CmsRoleArticleEdit


canPublishArticle : UserRole -> Bool
canPublishArticle userRole =
    hasRole userRole CmsRoleArticlePublish


canSaveArticle : UserRole -> Bool
canSaveArticle userRole =
    hasAnyRole userRole [ CmsRoleArticleCreate, CmsRoleArticleEdit ]


canSubmitArticle : UserRole -> Bool
canSubmitArticle userRole =
    hasAnyRole userRole [ CmsRoleArticleCreate, CmsRoleArticleEdit ]


canSubmitOtherArticle : UserRole -> Bool
canSubmitOtherArticle userRole =
    hasRole userRole CmsRoleArticleSubmit


canManageLogin : UserRole -> Bool
canManageLogin userRole =
    hasRole userRole CmsRoleLoginManage
