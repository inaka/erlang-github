-module(egithub_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         user/1,
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

user(_Config) ->
  Credentials = github_credentials(),

  meck:new(ibrowse, [passthrough]),
  try
    UserFun = fun ("https://api.github.com/user", _, get, _, _) ->
                  {ok, "200", [], <<"{}">>}
              end,
    meck:expect(ibrowse, send_req, UserFun),
    {ok, _} = egithub:user(Credentials),

    GadgetCIFun =
      fun(Url, _, get, _, _) ->
          "https://api.github.com/users/gadgetci" = lists:flatten(Url),
          {ok, "200", [], <<"{}">>}
      end,
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

repos(_Config) ->
  Credentials = github_credentials(),

  meck:new(ibrowse, [passthrough]),
  try
    RepoFun =
      fun (Url, _, get, _, _) ->
          "https://api.github.com/repos/inaka/whatever" = lists:flatten(Url),
          {ok, "200", [], <<"{}">>}
      end,
    meck:expect(ibrowse, send_req, RepoFun),
    {ok, _} = egithub:repo(Credentials, "inaka/whatever"),

    ReposFun =
      fun(Url, _, get, _, _) ->
          "https://api.github.com/user/repos?"
            "type=all&sort=full_name&direction=asc&page=1" =
            lists:flatten(Url),
          {ok, "200", [], <<"[]">>}
      end,
    meck:expect(ibrowse, send_req, ReposFun),
    {ok, _} = egithub:repos(Credentials, #{}),

    AllReposFun =
      fun(Url, _, get, _, _) ->
          "https://api.github.com/user/repos?"
            "type=all&sort=full_name&direction=asc&page=1" =
            lists:flatten(Url),
          {ok, "200", [], <<"[]">>}
      end,

    meck:expect(ibrowse, send_req, AllReposFun),
    {ok, _} = egithub:all_repos(Credentials, #{})
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
