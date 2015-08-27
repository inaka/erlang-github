-module(egithub_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         pull_reqs/1,
         issues/1,
         files/1,
         users/1,
         orgs/1,
         repos/1,
         teams/1,
         hooks/1,
         collaborators/1,
         statuses/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  egithub:start(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(_Config) ->
  application:stop(egithub),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pull_reqs(_Config) ->
  Credentials = github_credentials(),

  meck:new(ibrowse, [passthrough]),
  try
    PRFilesFun = match_fun("https://api.github.com/repos/"
                           "user/repo/pulls/1/files",
                           get),
    meck:expect(ibrowse, send_req, PRFilesFun),
    {ok, _} = egithub:pull_req_files(Credentials, "user/repo", 1),

    PRCommentLineFun = match_fun("https://api.github.com/repos/"
                                 "user/repo/pulls/1/comments",
                                 post),
    meck:expect(ibrowse, send_req, PRCommentLineFun),
    {ok, _} = egithub:pull_req_comment_line(Credentials, "user/repo", 1,
                                            "SHA", "file-path",
                                            5, "comment text"),

    Self = self(),
    PRCommentLineQueueFun = fun(_, _, _, _, _) ->
                                Self ! ok,
                                {ok, "200", [], <<"[]">>}
                            end,
    meck:expect(ibrowse, send_req, PRCommentLineQueueFun),
    ok = egithub:pull_req_comment_line(Credentials, "user/repo", 1,
                                       "SHA", "file-path",
                                       5, "comment text",
                                       #{post_method => queue}),
    ok = receive ok -> ok after 5000 -> timeout end,

    PRCommentsFun = match_fun("https://api.github.com/repos/"
                              "user/repo/pulls/1/comments",
                              get),
    meck:expect(ibrowse, send_req, PRCommentsFun),
    {ok, _} = egithub:pull_req_comments(Credentials, "user/repo", 1)
  after
    meck:unload(ibrowse)
  end.

issues(_Config) ->
  Credentials = github_credentials(),

  meck:new(ibrowse, [passthrough]),
  try
    IssueCommentFun = match_fun("https://api.github.com/repos/"
                                "user/repo/issues/1/comments",
                                post),
    meck:expect(ibrowse, send_req, IssueCommentFun),
    {ok, _} = egithub:issue_comment(Credentials, "user/repo", 1, "txt"),

    Self = self(),
    IssueCommentQueueFun = fun(Url, _, post, _, _) ->
                               "https://api.github.com/repos/"
                                 "user/repo/issues/1/comments" =
                                 lists:flatten(Url),
                               Self ! ok,
                               {ok, "200", [], <<"[]">>}
                           end,
    meck:expect(ibrowse, send_req, IssueCommentQueueFun),
    ok = egithub:issue_comment(Credentials, "user/repo", 1,
                               "txt", #{post_method => queue}),
    ok = receive ok -> ok after 5000 -> timeout end,

    IssueCommentsFun = match_fun("https://api.github.com/repos/"
                                "user/repo/issues/1/comments",
                                 get),
    meck:expect(ibrowse, send_req, IssueCommentsFun),
    {ok, _} = egithub:issue_comments(Credentials, "user/repo", 1)
  after
    meck:unload(ibrowse)
  end.

files(_Config) ->
  Credentials = github_credentials(),

  meck:new(ibrowse, [passthrough]),
  try
    FileContentFun = match_fun("https://api.github.com/repos/"
                               "user/repo/contents/file?ref=SHA",
                               get,
                               <<"{\"content\" : \"\"}">>),
    meck:expect(ibrowse, send_req, FileContentFun),
    {ok, _} = egithub:file_content(Credentials, "user/repo", "SHA", "file")
  after
    meck:unload(ibrowse)
  end.

users(_Config) ->
  Credentials = github_credentials(),

  meck:new(ibrowse, [passthrough]),
  try
    UserFun = match_fun("https://api.github.com/user", get),
    meck:expect(ibrowse, send_req, UserFun),
    {ok, _} = egithub:user(Credentials),

    GadgetCIFun = match_fun("https://api.github.com/users/gadgetci", get),
    meck:expect(ibrowse, send_req, GadgetCIFun),
    {ok, _} = egithub:user(Credentials, "gadgetci"),

    EmailsFun = match_fun("https://api.github.com/user/emails", get),
    meck:expect(ibrowse, send_req, EmailsFun),
    {ok, _} = egithub:user_emails(Credentials)
  after
    meck:unload(ibrowse)
  end.

orgs(_Config) ->
  Credentials = github_credentials(),

  meck:new(ibrowse, [passthrough]),
  try
    OrgsFun = match_fun("https://api.github.com/user/orgs", get),
    meck:expect(ibrowse, send_req, OrgsFun),
    {ok, _} = egithub:orgs(Credentials),

    OrgsUserFun = match_fun("https://api.github.com/users/gadgetci/orgs", get),
    meck:expect(ibrowse, send_req, OrgsUserFun),
    {ok, _} = egithub:orgs(Credentials, "gadgetci"),

    OrgMembershipFun = match_fun("https://api.github.com/user/"
                                 "memberships/orgs/some-organization",
                                 get),
    meck:expect(ibrowse, send_req, OrgMembershipFun),
    {ok, _} = egithub:org_membership(Credentials, "some-organization")
  after
    meck:unload(ibrowse)
  end.

repos(_Config) ->
  Credentials = github_credentials(),

  meck:new(ibrowse, [passthrough]),
  try
    RepoFun = match_fun("https://api.github.com/repos/inaka/whatever", get),
    meck:expect(ibrowse, send_req, RepoFun),
    {ok, _} = egithub:repo(Credentials, "inaka/whatever"),

    ReposFun = match_fun("https://api.github.com/user/repos?"
                         "type=all&sort=full_name&direction=asc&page=1",
                         get),
    meck:expect(ibrowse, send_req, ReposFun),
    {ok, _} = egithub:repos(Credentials, #{}),

    ReposUserFun = match_fun("https://api.github.com/users/"
                             "gadgetci/repos?page=1",
                             get),
    meck:expect(ibrowse, send_req, ReposUserFun),
    {ok, _} = egithub:repos(Credentials, "gadgetci", #{}),

    AllReposFun = match_fun("https://api.github.com/user/repos?"
                             "type=all&sort=full_name&direction=asc&page=1",
                             get),
    meck:expect(ibrowse, send_req, AllReposFun),
    {ok, _} = egithub:all_repos(Credentials, #{}),

    AllReposUserFun =
      fun(Url, _, get, _, _) ->
          case lists:flatten(Url) of
            "https://api.github.com/users/gadgetci/repos?page=1" ->
              {ok, "200", [], <<"[1]">>};
            "https://api.github.com/users/gadgetci/repos?page=2" ->
              {ok, "200", [], <<"[]">>}
          end
      end,
    meck:expect(ibrowse, send_req, AllReposUserFun),
    {ok, _} = egithub:all_repos(Credentials, "gadgetci", #{}),

    AllReposErrorFun =
      fun(Url, _, get, _, _) ->
          case lists:flatten(Url) of
            "https://api.github.com/users/gadgetci/repos?page=1" ->
              {ok, "200", [], <<"[1]">>};
            "https://api.github.com/users/gadgetci/repos?page=2" ->
              {ok, "400", [], <<"">>}
          end
      end,
    meck:expect(ibrowse, send_req, AllReposErrorFun),
    {error, _} = egithub:all_repos(Credentials, "gadgetci", #{}),

    OrgReposFun = match_fun("https://api.github.com/orgs/some-org/repos?page=1",
                            get),
    meck:expect(ibrowse, send_req, OrgReposFun),
    {ok, _} = egithub:org_repos(Credentials, "some-org", #{}),

    AllOrgReposFun =
      fun(Url, _, get, _, _) ->
          case lists:flatten(Url) of
            "https://api.github.com/orgs/some-org/repos?page=1" ->
              {ok, "200", [], <<"[1]">>};
            "https://api.github.com/orgs/some-org/repos?page=2" ->
              {ok, "200", [], <<"[]">>}
          end
      end,
    meck:expect(ibrowse, send_req, AllOrgReposFun),
    {ok, _} = egithub:all_org_repos(Credentials, "some-org", #{}),

    AllOrgReposErrorFun =
      fun(Url, _, get, _, _) ->
          case lists:flatten(Url) of
            "https://api.github.com/orgs/some-org/repos?page=1" ->
              {ok, "200", [], <<"[1]">>};
            "https://api.github.com/orgs/some-org/repos?page=2" ->
              {ok, "400", [], <<"[]">>}
          end
      end,
    meck:expect(ibrowse, send_req, AllOrgReposErrorFun),
    {error, _} = egithub:all_org_repos(Credentials, "some-org", #{})
  after
    meck:unload(ibrowse)
  end.

teams(_Config) ->
  Credentials = github_credentials(),

  meck:new(ibrowse, [passthrough]),
  try
    TeamsFun = match_fun("https://api.github.com/orgs/some-org/teams", get),
    meck:expect(ibrowse, send_req, TeamsFun),
    {ok, _} = egithub:teams(Credentials, "some-org"),

    CreateTeamFun = match_fun("https://api.github.com/"
                              "orgs/some-org/teams", post),
    meck:expect(ibrowse, send_req, CreateTeamFun),
    {ok, _} = egithub:create_team(Credentials, "some-org", "Team", "", []),

    AddTeamRepoFun = match_fun("https://api.github.com/teams/1/repos/user/repo",
                               put),
    meck:expect(ibrowse, send_req, AddTeamRepoFun),
    ok = egithub:add_team_repository(Credentials, 1, "user/repo"),

    AddTeamMemberFun = match_fun("https://api.github.com/"
                                 "teams/1/members/gadgetci",
                                 put),
    meck:expect(ibrowse, send_req, AddTeamMemberFun),
    ok = egithub:add_team_member(Credentials, 1, "gadgetci"),

    DeleteTeamMemberFun = match_fun("https://api.github.com/"
                                    "teams/1/members/gadgetci",
                                    delete),
    meck:expect(ibrowse, send_req, DeleteTeamMemberFun),
    ok = egithub:delete_team_member(Credentials, 1, "gadgetci"),

    TeamMembershipFun = match_fun("https://api.github.com/"
                                  "teams/1/memberships/gadgetci",
                                  get,
                                  <<"{\"state\" : \"pending\"}">>),
    meck:expect(ibrowse, send_req, TeamMembershipFun),
    pending = egithub:team_membership(Credentials, 1, "gadgetci")
  after
    meck:unload(ibrowse)
  end.

hooks(_Config) ->
  Credentials = github_credentials(),

  meck:new(ibrowse, [passthrough]),
  try
    HooksFun = match_fun("https://api.github.com/repos/some-repo/hooks", get),
    meck:expect(ibrowse, send_req, HooksFun),
    {ok, _} = egithub:hooks(Credentials, "some-repo"),

    CreateHookFun = match_fun("https://api.github.com/"
                              "repos/some-repo/hooks",
                              post),
    meck:expect(ibrowse, send_req, CreateHookFun),
    {ok, _} = egithub:create_webhook(Credentials, "some-repo",
                                     "url", ["pull_request"]),

    DeleteHookFun = match_fun("https://api.github.com/"
                              "repos/some-repo/hooks/url",
                              delete),
    meck:expect(ibrowse, send_req, DeleteHookFun),
    ok = egithub:delete_webhook(Credentials, "some-repo", "url")
  after
    meck:unload(ibrowse)
  end.

collaborators(_Config) ->
  Credentials = github_credentials(),

  meck:new(ibrowse, [passthrough]),
  try
    CollaboratorsFun = match_fun("https://api.github.com/"
                                 "repos/some-repo/collaborators", get),
    meck:expect(ibrowse, send_req, CollaboratorsFun),
    {ok, _} = egithub:collaborators(Credentials, "some-repo"),

    AddCollabFun = match_fun("https://api.github.com/"
                             "repos/some-repo/collaborators/username",
                             put),
    meck:expect(ibrowse, send_req, AddCollabFun),
    ok = egithub:add_collaborator(Credentials, "some-repo", "username"),

    DeleteCollabFun = match_fun("https://api.github.com/"
                                "repos/some-repo/collaborators/username",
                                delete),
    meck:expect(ibrowse, send_req, DeleteCollabFun),
    ok = egithub:remove_collaborator(Credentials, "some-repo", "username")
  after
    meck:unload(ibrowse)
  end.

statuses(_Config) ->
  Credentials = github_credentials(),

  meck:new(ibrowse, [passthrough]),
  try
    StatusesFun = match_fun("https://api.github.com/repos/"
                            "some-repo/commits/ref/statuses",
                            get),
    meck:expect(ibrowse, send_req, StatusesFun),
    {ok, _} = egithub:statuses(Credentials, "some-repo", "ref"),

    CreateStatusFun = match_fun("https://api.github.com/"
                                "repos/some-repo/statuses/SHA",
                                post),
    meck:expect(ibrowse, send_req, CreateStatusFun),
    {ok, _} = egithub:create_status(Credentials, "some-repo", "SHA", pending,
                                    "description", "context"),

    CreateStatusUrlFun = match_fun("https://api.github.com/"
                                   "repos/some-repo/statuses/SHA",
                                   post),
    meck:expect(ibrowse, send_req, CreateStatusUrlFun),
    {ok, _} = egithub:create_status(Credentials, "some-repo", "SHA", pending,
                                    "description", "context", "url"),

    CombinedStatusFun = match_fun("https://api.github.com/repos/"
                                  "some-repo/commits/ref/status",
                                  get),
    meck:expect(ibrowse, send_req, CombinedStatusFun),
    {ok, _} = egithub:combined_status(Credentials, "some-repo", "ref")
  after
    meck:unload(ibrowse)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec github_credentials() -> {string(), string()}.
github_credentials() ->
  egithub:basic_auth("username", "password").

match_fun(Url, Method) ->
  match_fun(Url, Method, <<"[]">>).

match_fun(Url, Method, ReturnBody) ->
  fun(UrlParam, _, MethodParam, _, _) ->
      Url = lists:flatten(UrlParam),
      Method = MethodParam,
      {ok, "200", [], ReturnBody}
  end.
