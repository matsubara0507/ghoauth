module GitHub.Login.Data where

import qualified Data.Text        as T
import           Network.HTTP.Req (QueryParam, (=:))


-- |
-- ref: https://docs.github.com/en/developers/apps/building-oauth-apps/scopes-for-oauth-apps
data Scope
  = RepoScope
  | RepoStatusScope
  | RepoDeploymentScope
  | PiblicRepoScope
  | RepoInviteScope
  | SecurityEventsScope
  | AdminRepoHookScope
  | WriteRepoHookScope
  | ReadRepoHookScope
  | AdminOrgScope
  | WriteOrgScope
  | ReadOrgScope
  | AdminPublicKeyScope
  | WritePublicKeyScope
  | ReadPublicKeyScope
  | AdminOrgHookScope
  | GistScope
  | NotificationsScope
  | UserScope
  | ReadUserScope
  | UserEmailScope
  | UserFollowScope
  | DeleteRepoScope
  | WriteDiscussionScope
  | ReadDiscussionScope
  | WritePackagesScope
  | ReadPackagesScope
  | DeletePackagesScope
  | AdminGpgKeyScope
  | WriteGpgKeyScope
  | ReadGpgKeyScope
  | WorkflowScope
  deriving (Show, Eq)


scopeParam :: (Monoid a, QueryParam a) => [Scope] -> a
scopeParam []     = mempty
scopeParam scopes = "scope" =: T.intercalate " " (map toParamStr scopes)
  where
    toParamStr RepoScope            = "repo"
    toParamStr RepoStatusScope      = "repo:status"
    toParamStr RepoDeploymentScope  = "repo_deployment"
    toParamStr PiblicRepoScope      = "public_repo"
    toParamStr RepoInviteScope      = "repo:invite"
    toParamStr SecurityEventsScope  = "security_events"
    toParamStr AdminRepoHookScope   = "admin:repo_hook"
    toParamStr WriteRepoHookScope   = "write:repo_hook"
    toParamStr ReadRepoHookScope    = "read:repo_hook"
    toParamStr AdminOrgScope        = "admin:org"
    toParamStr WriteOrgScope        = "write:org"
    toParamStr ReadOrgScope         = "read:org"
    toParamStr AdminPublicKeyScope  = "admin:public_key"
    toParamStr WritePublicKeyScope  = "write:public_key"
    toParamStr ReadPublicKeyScope   = "read:public_key"
    toParamStr AdminOrgHookScope    = "admin:org_hook"
    toParamStr GistScope            = "gist"
    toParamStr NotificationsScope   = "notifications"
    toParamStr UserScope            = "user"
    toParamStr ReadUserScope        = "read:user"
    toParamStr UserEmailScope       = "user:email"
    toParamStr UserFollowScope      = "user:follow"
    toParamStr DeleteRepoScope      = "delete_repo"
    toParamStr WriteDiscussionScope = "write:discussion"
    toParamStr ReadDiscussionScope  = "read:discussion"
    toParamStr WritePackagesScope   = "write:packages"
    toParamStr ReadPackagesScope    = "read:packages"
    toParamStr DeletePackagesScope  = "delete:packages"
    toParamStr AdminGpgKeyScope     = "admin:gpg_key"
    toParamStr WriteGpgKeyScope     = "write:gpg_key"
    toParamStr ReadGpgKeyScope      = "read:gpg_key"
    toParamStr WorkflowScope        = "workflow"
