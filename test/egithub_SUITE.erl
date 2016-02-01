-module(egithub_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         pull_reqs/1,
         issue_comments/1,
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
  {ok, _} = egithub:start(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  ok = application:stop(egithub),
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pull_reqs(_Config) ->
  Credentials = github_credentials(),

  meck:new(shotgun, [passthrough]),
  try
    PRFilesFun = match_fun("/repos/user/repo/pulls/1/files",
                           get),
    meck:expect(shotgun, request, PRFilesFun),
    {ok, _} = egithub:pull_req_files(Credentials, "user/repo", 1),

    PRCommentLineFun = match_fun("/repos/user/repo/pulls/1/comments",
                                 post),
    meck:expect(shotgun, request, PRCommentLineFun),
    {ok, _} = egithub:pull_req_comment_line(Credentials, "user/repo", 1,
                                            "SHA", <<"file-path">>,
                                            5, "comment text"),

    Self = self(),
    PRCommentLineQueueFun = fun(_, _, _, _, _, _) ->
                                Self ! ok,
                                {ok, #{status_code => 200,
                                       headers => [], body => <<"[]">>}}
                            end,
    meck:expect(shotgun, request, PRCommentLineQueueFun),
    ok = egithub:pull_req_comment_line(Credentials, "user/repo", 1,
                                       "SHA", <<"file-path">>,
                                       5, "comment text",
                                       #{post_method => queue}),
    ok = receive ok -> ok after 5000 -> timeout end,

    PRCommentsFun = match_fun("/repos/user/repo/pulls/1/comments",
                              get),
    meck:expect(shotgun, request, PRCommentsFun),
    {ok, _} = egithub:pull_req_comments(Credentials, "user/repo", 1)
  after
    meck:unload(shotgun)
  end.

issue_comments(_Config) ->
  Credentials = github_credentials(),

  meck:new(shotgun, [passthrough]),
  try
    IssueCommentFun = match_fun("/repos/user/repo/issues/1/comments",
                                post),
    meck:expect(shotgun, request, IssueCommentFun),
    {ok, _} = egithub:issue_comment(Credentials, "user/repo", 1, "txt"),

    Self = self(),
    IssueCommentQueueFun = fun(_, post, Url, _, _, _) ->
                               "/repos/user/repo/issues/1/comments" =
                                 lists:flatten(Url),
                               Self ! ok,
                               {ok, #{status_code => 200,
                                      headers => [], body => <<"[]">>}}
                           end,
    meck:expect(shotgun, request, IssueCommentQueueFun),
    ok = egithub:issue_comment(Credentials, "user/repo", 1,
                               "txt", #{post_method => queue}),
    ok = receive ok -> ok after 5000 -> timeout end,

    IssueCommentsFun = match_fun("/repos/user/repo/issues/1/comments",
                                 get),
    meck:expect(shotgun, request, IssueCommentsFun),
    {ok, _} = egithub:issue_comments(Credentials, "user/repo", 1)
  after
    meck:unload(shotgun)
  end.

issues(_Config) ->
  Credentials = github_credentials(),

  meck:new(shotgun, [passthrough]),
  try
      %%TODO: implement create tests
      %%TODO: implement list tests
  after
      meck:unload(shotgun)
  end.

files(_Config) ->
  Credentials = github_credentials(),

  meck:new(shotgun, [passthrough]),
  try
    FileContentFun = match_fun("/repos/user/repo/contents/file?ref=SHA",
                               get,
                               <<"{\"content\" : \"\"}">>),
    meck:expect(shotgun, request, FileContentFun),
    {ok, _} = egithub:file_content(Credentials, "user/repo", "SHA", "file")
  after
    meck:unload(shotgun)
  end.

users(_Config) ->
  Credentials = github_credentials(),

  meck:new(shotgun, [passthrough]),
  try
    UserFun = match_fun("/user", get),
    meck:expect(shotgun, request, UserFun),
    {ok, _} = egithub:user(Credentials),

    GadgetCIFun = match_fun("/users/gadgetci", get),
    meck:expect(shotgun, request, GadgetCIFun),
    {ok, _} = egithub:user(Credentials, "gadgetci"),

    EmailsFun = match_fun("/user/emails", get),
    meck:expect(shotgun, request, EmailsFun),
    {ok, _} = egithub:user_emails(Credentials)
  after
    meck:unload(shotgun)
  end.

orgs(_Config) ->
  Credentials = github_credentials(),

  meck:new(shotgun, [passthrough]),
  try
    OrgsFun = match_fun("/user/orgs", get),
    meck:expect(shotgun, request, OrgsFun),
    {ok, _} = egithub:orgs(Credentials),

    OrgsUserFun = match_fun("/users/gadgetci/orgs", get),
    meck:expect(shotgun, request, OrgsUserFun),
    {ok, _} = egithub:orgs(Credentials, "gadgetci"),

    OrgMembershipFun = match_fun("/user/memberships/orgs/some-organization",
                                 get),
    meck:expect(shotgun, request, OrgMembershipFun),
    {ok, _} = egithub:org_membership(Credentials, "some-organization")
  after
    meck:unload(shotgun)
  end.

repos(_Config) ->
  Credentials = github_credentials(),

  meck:new(shotgun, [passthrough]),
  try
    RepoFun = match_fun("/repos/inaka/whatever", get),
    meck:expect(shotgun, request, RepoFun),
    {ok, _} = egithub:repo(Credentials, "inaka/whatever"),

    ReposFun = match_fun("/user/repos?"
                         "type=all&sort=full_name&direction=asc&page=1",
                         get),
    meck:expect(shotgun, request, ReposFun),
    {ok, _} = egithub:repos(Credentials, #{}),

    ReposUserFun = match_fun("/users/gadgetci/repos?page=1",
                             get),
    meck:expect(shotgun, request, ReposUserFun),
    {ok, _} = egithub:repos(Credentials, "gadgetci", #{}),

    AllReposFun = match_fun("/user/repos?"
                             "type=all&sort=full_name&direction=asc&page=1",
                             get),
    meck:expect(shotgun, request, AllReposFun),
    {ok, _} = egithub:all_repos(Credentials, #{}),

    AllReposUserFun =
      fun(_, get, Url, _, _, _) ->
          case lists:flatten(Url) of
            "/users/gadgetci/repos?page=1" ->
              {ok, #{status_code => 200,
                     headers => [], body => <<"[1]">>}};
            "/users/gadgetci/repos?page=2" ->
              {ok, #{status_code => 200,
                     headers => [], body => <<"[]">>}}
          end
      end,
    meck:expect(shotgun, request, AllReposUserFun),
    {ok, _} = egithub:all_repos(Credentials, "gadgetci", #{}),

    AllReposErrorFun =
      fun(_, get, Url, _, _, _) ->
          case lists:flatten(Url) of
            "/users/gadgetci/repos?page=1" ->
              {ok, #{status_code => 200,
                     headers => [], body => <<"[1]">>}};
            "/users/gadgetci/repos?page=2" ->
              {ok, #{status_code => 400,
                     headers => [], body => <<"">>}}
          end
      end,
    meck:expect(shotgun, request, AllReposErrorFun),
    {error, _} = egithub:all_repos(Credentials, "gadgetci", #{}),

    OrgReposFun = match_fun("/orgs/some-org/repos?page=1",
                            get),
    meck:expect(shotgun, request, OrgReposFun),
    {ok, _} = egithub:org_repos(Credentials, "some-org", #{}),

    AllOrgReposFun =
      fun(_, get, Url, _, _, _) ->
          case lists:flatten(Url) of
            "/orgs/some-org/repos?page=1" ->
              {ok, #{status_code => 200,
                     headers => [], body => <<"[1]">>}};
            "/orgs/some-org/repos?page=2" ->
              {ok, #{status_code => 200,
                     headers => [], body => <<"[]">>}}
          end
      end,
    meck:expect(shotgun, request, AllOrgReposFun),
    {ok, _} = egithub:all_org_repos(Credentials, "some-org", #{}),

    AllOrgReposErrorFun =
      fun(_, get, Url, _, _, _) ->
          case lists:flatten(Url) of
            "/orgs/some-org/repos?page=1" ->
              {ok, #{status_code => 200,
                     headers => [], body => <<"[1]">>}};
            "/orgs/some-org/repos?page=2" ->
              {ok, #{status_code => 400,
                     headers => [], body => <<"">>}}
          end
      end,
    meck:expect(shotgun, request, AllOrgReposErrorFun),
    {error, _} = egithub:all_org_repos(Credentials, "some-org", #{})
  after
    meck:unload(shotgun)
  end.

teams(_Config) ->
  Credentials = github_credentials(),

  meck:new(shotgun, [passthrough]),
  try
    TeamsFun = match_fun("/orgs/some-org/teams", get),
    meck:expect(shotgun, request, TeamsFun),
    {ok, _} = egithub:teams(Credentials, "some-org"),

    CreateTeamFun = match_fun("/orgs/some-org/teams", post),
    meck:expect(shotgun, request, CreateTeamFun),
    {ok, _} = egithub:create_team(Credentials, "some-org", "Team", "", []),

    AddTeamRepoFun = match_fun("/teams/1/repos/user/repo",
                               put),
    meck:expect(shotgun, request, AddTeamRepoFun),
    ok = egithub:add_team_repository(Credentials, 1, "user/repo"),

    AddTeamMemberFun = match_fun("/teams/1/members/gadgetci",
                                 put),
    meck:expect(shotgun, request, AddTeamMemberFun),
    ok = egithub:add_team_member(Credentials, 1, "gadgetci"),

    DeleteTeamMemberFun = match_fun("/teams/1/members/gadgetci",
                                    delete),
    meck:expect(shotgun, request, DeleteTeamMemberFun),
    ok = egithub:delete_team_member(Credentials, 1, "gadgetci"),

    TeamMembershipFun = match_fun("/teams/1/memberships/gadgetci",
                                  get,
                                  <<"{\"state\" : \"pending\"}">>),
    meck:expect(shotgun, request, TeamMembershipFun),
    pending = egithub:team_membership(Credentials, 1, "gadgetci")
  after
    meck:unload(shotgun)
  end.

hooks(_Config) ->
  Credentials = github_credentials(),

  meck:new(shotgun, [passthrough]),
  try
    HooksFun = match_fun("/repos/some-repo/hooks", get),
    meck:expect(shotgun, request, HooksFun),
    {ok, _} = egithub:hooks(Credentials, "some-repo"),

    CreateHookFun = match_fun("/repos/some-repo/hooks",
                              post),
    meck:expect(shotgun, request, CreateHookFun),
    {ok, _} = egithub:create_webhook(Credentials, "some-repo",
                                     "url", ["pull_request"]),

    DeleteHookFun = match_fun("/repos/some-repo/hooks/url",
                              delete),
    meck:expect(shotgun, request, DeleteHookFun),
    ok = egithub:delete_webhook(Credentials, "some-repo", "url")
  after
    meck:unload(shotgun)
  end.

collaborators(_Config) ->
  Credentials = github_credentials(),

  meck:new(shotgun, [passthrough]),
  try
    CollaboratorsFun = match_fun("/repos/some-repo/collaborators", get),
    meck:expect(shotgun, request, CollaboratorsFun),
    {ok, _} = egithub:collaborators(Credentials, "some-repo"),

    AddCollabFun = match_fun("/repos/some-repo/collaborators/username",
                             put),
    meck:expect(shotgun, request, AddCollabFun),
    ok = egithub:add_collaborator(Credentials, "some-repo", "username"),

    DeleteCollabFun = match_fun("/repos/some-repo/collaborators/username",
                                delete),
    meck:expect(shotgun, request, DeleteCollabFun),
    ok = egithub:remove_collaborator(Credentials, "some-repo", "username")
  after
    meck:unload(shotgun)
  end.

statuses(_Config) ->
  Credentials = github_credentials(),

  meck:new(shotgun, [passthrough]),
  try
    StatusesFun = match_fun("/repos/some-repo/commits/ref/statuses",
                            get),
    meck:expect(shotgun, request, StatusesFun),
    {ok, _} = egithub:statuses(Credentials, "some-repo", "ref"),

    CreateStatusFun = match_fun("/repos/some-repo/statuses/SHA",
                                post),
    meck:expect(shotgun, request, CreateStatusFun),
    {ok, _} = egithub:create_status(Credentials, "some-repo", "SHA", pending,
                                    "description", "context"),

    CreateStatusUrlFun = match_fun("/repos/some-repo/statuses/SHA",
                                   post),
    meck:expect(shotgun, request, CreateStatusUrlFun),
    {ok, _} = egithub:create_status(Credentials, "some-repo", "SHA", pending,
                                    "description", "context", "url"),

    CombinedStatusFun = match_fun("/repos/some-repo/commits/ref/status",
                                  get),
    meck:expect(shotgun, request, CombinedStatusFun),
    {ok, _} = egithub:combined_status(Credentials, "some-repo", "ref")
  after
    meck:unload(shotgun)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec github_credentials() -> {'basic', string(), string()}.
github_credentials() ->
  egithub:basic_auth("username", "password").

match_fun(Url, Method) ->
  match_fun(Url, Method, <<"[]">>).

match_fun(Url, Method, ReturnBody) ->
  fun(_, MethodParam, UrlParam, _, _, _) ->
      Url = lists:flatten(UrlParam),
      Method = MethodParam,
      {ok, #{status_code => 200, headers => [], body => ReturnBody}}
  end.
