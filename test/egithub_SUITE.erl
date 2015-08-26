-module(egithub_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         pull_req/1,
         users/1,
         orgs/1,
         repos/1
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

pull_req(_Config) ->
  Credentials = github_credentials(),

  meck:new(ibrowse, [passthrough]),
  try
    UserFun = match_fun("https://api.github.com/repos/"
                        "user/repo/pulls/1/files",
                        get),
    meck:expect(ibrowse, send_req, UserFun),
    {ok, _} = egithub:pull_req_files(Credentials, "user/repo", 1),

    UserFun = match_fun("https://api.github.com/users/gadgetci", get),
    meck:expect(ibrowse, send_req, GadgetCIFun),
    {ok, _} = egithub:user(Credentials, "gadgetci"),

    EmailsFun = fun("https://api.github.com/user/emails", _, get, _, _) ->
                    {ok, "200", [], <<"[]">>}
                end,
    meck:expect(ibrowse, send_req, EmailsFun),
    {ok, _} = egithub:user_emails(Credentials)
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

-spec github_credentials() -> {string(), string()}.
github_credentials() ->
  Username = application:get_env(egithub, github_username, undefined),
  Password = application:get_env(egithub, github_password, undefined),
  case {Username, Password} of
    {U, P} when U =/= undefined, P =/= undefined ->
      egithub:basic_auth(Username, Password);
    _ ->
      throw("Please specifiy a GitHub username and password.")
  end.

match_fun(Url, Method) ->
  fun(UrlParam, _, MethodParam, _, _) ->
      Url = lists:flatten(UrlParam),
      Method = MethodParam,
      {ok, "200", [], <<"[]">>}
  end.
