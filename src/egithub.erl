-module(egithub).
-behavior(application).

-export([
         start/0,
         start/2,
         stop/1
        ]).

-export([
         %% Credentials
         basic_auth/2,
         oauth/1,
         %% Pull Requests
         pull_req_files/3,
         pull_req_comment_line/7,
         pull_req_comments/3,
         issue_comment/4,
         issue_comments/3,
         %% Users
         user/1,
         user/2,
         repo/2,
         repos/2,
         repos/3,
         all_repos/2,
         all_repos/3,
         orgs/1,
         orgs/2,
         org_repos/3,
         all_org_repos/3,
         %% Teams
         teams/2,
         create_team/5,
         add_team_repository/3,
         add_team_member/3,
         delete_team_member/3,
         team_membership/3,
         %% Hooks
         hooks/2,
         create_webhook/4,
         delete_webhook/3,
         %% Collaborators
         collaborators/2,
         add_collaborator/3,
         remove_collaborator/3
        ]).

%% Files
-export([
         file_content/4
        ]).

-export_type([
              credentials/0,
              repository/0
             ]).

-type credentials() ::
    {basic, Username :: string(), Password :: string()}
    | {oauth, Token :: string()}.
-type repository() :: string(). %% "username/reponame"
-type result() :: ok | {ok, any()} | {error, term()}.

-define(GITHUB_API, "https://api.github.com").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start() -> ok.
start() ->
    application:ensure_all_started(egithub).

%% Application Behavior

-spec start(application:start_type(), term()) ->
    {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _Arg) ->
    egithub_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%% Credentials

-spec basic_auth(string(), string()) -> egithub:credentials().
basic_auth(User, Password) ->
    {basic, User, Password}.

-spec oauth(binary()) -> egithub:credentials().
oauth(Token) ->
    {oauth, Token}.

%% Pull Requests

-spec pull_req_files(credentials(), repository(), integer()) ->
    result().
pull_req_files(Credentials, Repo, PR) ->
    Url = make_url({pull_req, files}, {Repo, PR}),
    {ok, Result} = egithub_req:run(Credentials, Url),
    Files = egithub_json:decode(Result),
    {ok, Files}.

-spec pull_req_comment_line(credentials(), repository(), integer(),
                            string(), binary(), integer(), binary()) ->
    result().
pull_req_comment_line(Credentials, Repo, PR,
                      CommitId, Filename, Line, Text) ->
    Url = make_url({pull_req, comments}, {Repo, PR}),
    Body = #{<<"commit_id">> => list_to_binary(CommitId),
             <<"path">> => Filename,
             <<"position">> => Line,
             <<"body">> => Text
            },
    JsonBody = egithub_json:encode(Body),
    egithub_req:run(Credentials, Url, post, JsonBody).

-spec pull_req_comments(credentials(), repository(), integer()) ->
    result().
pull_req_comments(Cred, Repo, PR) ->
    Url = make_url({pull_req, comments}, {Repo, PR}),
    {ok, Result} = egithub_req:run(Cred, Url),
    Comments = egithub_json:decode(Result),
    {ok, Comments}.

%% Issues

-spec issue_comment(credentials(), repository(), integer(), binary()) ->
                           result().
issue_comment(Cred, Repo, PR, Text) ->
    Url = make_url({issue, comments}, {Repo, PR}),
    Body = #{<<"body">> => Text},
    JsonBody = egithub_json:encode(Body),
    egithub_req:run(Cred, Url, post, JsonBody).

-spec issue_comments(credentials(), repository(), integer()) ->
    result().
issue_comments(Cred, Repo, PR) ->
    Url = make_url({issue, comments}, {Repo, PR}),
    {ok, Result} = egithub_req:run(Cred, Url),
    Comments = egithub_json:decode(Result),
    {ok, Comments}.

%% Files

-spec file_content(credentials(), repository(), string(), string()) -> result().
file_content(Cred, Repo, CommitId, Filename) ->
    Url = make_url(file_content, {Repo, CommitId, Filename}),
    case egithub_req:run(Cred, Url) of
        {ok, Result} ->
            JsonResult = egithub_json:decode(Result),
            ContentBase64 = maps:get(<<"content">>, JsonResult),
            Content = base64:decode(ContentBase64),
            {ok, Content};
        {error, Reason} ->
            {error, Reason}
    end.

%% Users

-spec user(credentials()) -> result().
user(Cred) ->
    Url = make_url(user, {}),
    api_call_json_result(Cred, Url).

-spec user(credentials(), string()) -> result().
user(Cred, Username) ->
    Url = make_url(user, {Username}),
    api_call_json_result(Cred, Url).

%% Orgs

-spec orgs(credentials()) -> string().
orgs(Cred) ->
    orgs(Cred, undefined).

-spec orgs(credentials(), string()) -> string().
orgs(Cred, User) ->
    Url = make_url(orgs, {User}),
    api_call_json_result(Cred, Url).

%% Repos

-spec repo(credentials(), string()) -> result().
repo(Cred, RepoFullName) ->
    Url = make_url(repo, {RepoFullName}),
    api_call_json_result(Cred, Url).

-spec repos(credentials(), map()) -> result().
repos(Cred, Opts) ->
    repos(Cred, undefined, Opts).

-spec repos(credentials(), string(), map()) -> result().
repos(Cred, User, Opts) ->
    Url = make_url(repos, {User, Opts}),
    api_call_json_result(Cred, Url).

-spec all_repos(credentials(), map()) -> result().
all_repos(Cred, Opts) ->
    all_repos(Cred, undefined, Opts#{page => 1}, []).

-spec all_repos(credentials(), string(), map()) -> result().
all_repos(Cred, User, Opts) ->
    all_repos(Cred, User, Opts#{page => 1}, []).

all_repos(Cred, User, Opts = #{page := Page}, Results) ->
    case repos(Cred, User, Opts) of
        {ok, []} ->
            {ok, lists:flatten(Results)};
        {ok, Repos} ->
            all_repos(Cred, User, Opts#{page => Page + 1}, [Repos | Results]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec org_repos(credentials(), string(), map()) -> result().
org_repos(Cred, Org, Opts) ->
    Url = make_url(org_repos, {Org, Opts}),
    api_call_json_result(Cred, Url).

-spec all_org_repos(credentials(), string(), map()) -> result().
all_org_repos(Cred, Org, Opts) ->
    all_org_repos(Cred, Org, Opts#{page => 1}, []).

all_org_repos(Cred, Org, Opts = #{page := Page}, Results) ->
    case org_repos(Cred, Org, Opts) of
        {ok, []} ->
            {ok, lists:flatten(Results)};
        {ok, Repos} ->
            NewResults = [Repos | Results],
            NewOpts = Opts#{page => Page + 1},
            all_org_repos(Cred, Org, NewOpts, NewResults);
        {error, Reason} ->
            {error, Reason}
    end.

%% Teams

-spec teams(credentials(), string()) -> result().
teams(Cred, Org) ->
    Url = make_url(teams, {Org}),
    api_call_json_result(Cred, Url).

-spec create_team(credentials(), string(), string(), string(), [string()]) ->
    result().
create_team(Cred, Org, Name, Permission, Repos) ->
    Url = make_url(teams, {Org}),
    BodyMap = #{name => list_to_binary(Name),
                 permission => list_to_binary(Permission),
                 repo_names => list_to_binary(Repos)},
    Body = egithub_json:encode(BodyMap),
    case egithub_req:run(Cred, Url, post, Body) of
        {ok, Result} ->
            JsonResult = egithub_json:decode(Result),
            {ok, JsonResult};
        {error, {"422", _, _}} ->
            {ok, already_exists};
        Other ->
            Other
    end.

-spec add_team_repository(credentials(), integer(), string()) -> result().
add_team_repository(Cred, TeamId, RepoFullName) ->
    Url = make_url(teams_repos, {TeamId, RepoFullName}),
    Body = [],
    case egithub_req:run(Cred, Url, put, Body) of
        {ok, _} ->
            ok;
        Error ->
            Error
    end.

-spec add_team_member(credentials(), integer(), string()) -> result().
add_team_member(Cred, TeamId, Username) ->
    Url = make_url(teams, {TeamId, Username}),
    Body = [],
    case egithub_req:run(Cred, Url, put, Body) of
        {ok, _} ->
            ok;
        Error ->
            Error
    end.

-spec delete_team_member(credentials(), integer(), string()) -> result().
delete_team_member(Cred, TeamId, Username) ->
    Url = make_url(teams, {TeamId, Username}),
    Body = [],
    case egithub_req:run(Cred, Url, delete, Body) of
        {ok, _} ->
            ok;
        Error ->
            Error
    end.

-spec team_membership(credentials(), integer(), string()) ->
    active | pending | none.
team_membership(Cred, TeamId, Username) ->
    Url = make_url(team_membership, {TeamId, Username}),
    case api_call_json_result(Cred, Url) of
        {ok, #{<<"state">> := <<"active">>}} -> active;
        {ok, #{<<"state">> := <<"pending">>}} -> pending;
        {error, {"404", _, _}} -> none;
        {error, Reason} -> {error, Reason}
    end.

%% Hooks

-spec hooks(credentials(), repository()) -> result().
hooks(Cred, Repo) ->
    Url = make_url(hooks, {Repo}),
    api_call_json_result(Cred, Url).

-spec create_webhook(credentials(), repository(), string(), [string()]) ->
    result().
create_webhook(Cred, Repo, WebhookUrl, Events) ->
    Url = make_url(hooks, {Repo}),
    BinEvents = [list_to_binary(E) || E <- Events],
    Data = #{<<"name">> => <<"web">>,
             <<"active">> => true,
             <<"events">> => BinEvents,
             <<"config">> => #{<<"url">> => list_to_binary(WebhookUrl),
                               <<"content_type">> => <<"json">>}},
    Body = egithub_json:encode(Data),
    case egithub_req:run(Cred, Url, post, Body) of
        {ok, Result} ->
            JsonResult = egithub_json:decode(Result),
            {ok, JsonResult};
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete_webhook(credentials(), repository(), string()) -> result().
delete_webhook(Cred, Repo, Id) ->
    IdStr = to_str(Id),
    Url = make_url(hooks, {Repo, IdStr}),
    Body = [],
    case egithub_req:run(Cred, Url, delete, Body) of
        {ok, _Result} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Collaborators

-spec collaborators(credentials(), repository()) -> result().
collaborators(Cred, Repo) ->
    Url = make_url(collaborators, {Repo}),
    case egithub_req:run(Cred, Url) of
        {ok, Result} ->
            JsonResult = egithub_json:decode(Result),
            {ok, JsonResult};
        {error, Reason} ->
            {error, Reason}
    end.

-spec add_collaborator(credentials(), repository(), string()) -> result().
add_collaborator(Cred, Repo, Collaborator) ->
    Url = make_url(collaborators, {Repo, Collaborator}),
    Body = [],
    case egithub_req:run(Cred, Url, put, Body) of
        {ok, _Result} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec remove_collaborator(credentials(), repository(), string()) -> result().
remove_collaborator(Cred, Repo, Collaborator) ->
    Url = make_url(collaborators, {Repo, Collaborator}),
    Body = [],
    case egithub_req:run(Cred, Url, delete, Body) of
        {ok, _Result} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Pull Resquest
make_url({pull_req, Subentity}, {Repo, PR}) ->
    SubentityStr = to_str(Subentity),
    Url = ?GITHUB_API ++ "/repos/~s/pulls/~p/" ++ SubentityStr,
    io_lib:format(Url, [Repo, PR]);

%% Issues
make_url({issue, Subentity}, {Repo, PR}) ->
    SubentityStr = to_str(Subentity),
    Url = ?GITHUB_API ++ "/repos/~s/issues/~p/" ++ SubentityStr,
    io_lib:format(Url, [Repo, PR]);

%% Files
make_url(file_content, {Repo, CommitId, Filename}) ->
    Url = ?GITHUB_API ++ "/repos/~s/contents/~s?ref=~s",
    io_lib:format(Url, [Repo, Filename, CommitId]);

%% User
make_url(user, {}) ->
    Url = ?GITHUB_API ++ "/user",
    io_lib:format(Url, []);
make_url(user, {Username}) ->
    Url = ?GITHUB_API ++ "/users/~s",
    io_lib:format(Url, [Username]);

%% Organizations
make_url(orgs, {undefined}) ->
    Url = ?GITHUB_API ++ "/user/orgs",
    io_lib:format(Url, []);
make_url(orgs, {User}) ->
    Url = ?GITHUB_API ++ "/users/~s/orgs",
    io_lib:format(Url, [User]);

%% Teams
make_url(teams, {Org}) ->
    Url = ?GITHUB_API ++ "/orgs/~s/teams",
    io_lib:format(Url, [Org]);
make_url(teams, {TeamId, Username}) ->
    Url = ?GITHUB_API ++ "/teams/~p/members/~s",
    io_lib:format(Url, [TeamId, Username]);
make_url(teams_repos, {TeamId, RepoFullName}) ->
    Url = ?GITHUB_API ++ "/teams/~p/repos/~s",
    io_lib:format(Url, [TeamId, RepoFullName]);
make_url(team_membership, {TeamId, Username}) ->
    Url = ?GITHUB_API ++ "/teams/~p/memberships/~s",
    io_lib:format(Url, [TeamId, Username]);

%% Repositories
make_url(repo, {RepoFullName}) ->
    Url = ?GITHUB_API ++ "/repos/~s",
    io_lib:format(Url, [RepoFullName]);
make_url(repos, {User, Opts}) ->
    Type = maps:get(type, Opts, "all"),
    Sort = maps:get(sort, Opts, "full_name"),
    Direction = maps:get(direction, Opts, "asc"),
    Page = maps:get(page, Opts, 1),
    case User of
        undefined ->
            Url = ?GITHUB_API
                ++ "/user/repos?type=~s&sort=~s&direction=~s&page=~p",
            io_lib:format(Url, [Type, Sort, Direction, Page]);
        User ->
            Url = ?GITHUB_API ++ "/users/~s/repos?page=~p",
            io_lib:format(Url, [User, Page])
    end;
make_url(org_repos, {User, Opts}) ->
    Page = maps:get(page, Opts, 1),
    Url = ?GITHUB_API ++ "/orgs/~s/repos?page=~p",
    io_lib:format(Url, [User, Page]);

%% Hooks
make_url(hooks, {Repo}) ->
    Url = ?GITHUB_API ++ "/repos/~s/hooks",
    io_lib:format(Url, [Repo]);
make_url(hooks, {Repo, Id}) ->
    Url = ?GITHUB_API ++ "/repos/~s/hooks/~s",
    io_lib:format(Url, [Repo, Id]);

%% Colaborators
make_url(collaborators, {Repo}) ->
    Url = ?GITHUB_API ++ "/repos/~s/collaborators",
    io_lib:format(Url, [Repo]);
make_url(collaborators, {Repo, Username}) ->
    Url = ?GITHUB_API ++ "/repos/~s/collaborators/~s",
    io_lib:format(Url, [Repo, Username]).

api_call_json_result(Cred, Url) ->
    case egithub_req:run(Cred, Url) of
        {ok, Result} ->
            JsonResult = egithub_json:decode(Result),
            {ok, JsonResult};
        {error, Reason} ->
            {error, Reason}
    end.

to_str(Arg) when is_binary(Arg) ->
    unicode:characters_to_list(Arg);
to_str(Arg) when is_atom(Arg) ->
    atom_to_list(Arg);
to_str(Arg) when is_integer(Arg) ->
    integer_to_list(Arg);
to_str(Arg) when is_list(Arg) ->
    Arg.
