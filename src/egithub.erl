%% @doc Main module that implements the functions to interact with the GitHub's
%%      API.
%% @end
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
         pull_reqs/3,
         pull_req_files/3,
         pull_req_comment_line/7,
         pull_req_comment_line/8,
         pull_req_comments/3,
         pr_review/5,
         pr_reviews/3,
         dismiss_pr_review/5,
         issue_comment/4,
         issue_comment/5,
         issue_comments/3,
         %% Issues
         create_issue/7,
         create_issue/8,
         create_issue/9,
         all_issues/2,
         all_issues/3,
         issues_user/2,
         issues_org/3,
         issue/3,
         %% Users
         user/1,
         user/2,
         user_emails/1,
         repo/2,
         repos/2,
         repos/3,
         all_repos/2,
         all_repos/3,
         orgs/1,
         orgs/2,
         org_membership/2,
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
         remove_collaborator/3,
         %% Statuses
         create_status/7,
         create_status/6,
         statuses/3,
         combined_status/3,
         %% Languages
         languages/2,
         %% Releases
         release/3,
         releases/2,
         releases/3,
         release_latest/2,
         %% Branches
         branch/3,
         branches/3
        ]).

%% Files
-export([
         file_content/4
        ]).

-export_type([
              credentials/0,
              repository/0,
              options/0,
              result/0,
              state/0
             ]).

%% Types
-type state()        :: pending | success | error | failure.
-type credentials()  :: {basic, Username :: string(), Password :: string()}
                          | {oauth, Token :: string()}.
-type repository()   :: string(). %% "username/reponame"
-type options()      :: #{post_method => queue | run}.
-type result()       :: ok | {ok, map() | [map()]} | egithub_req:error().
-type issue_labels() :: list(string()).

-define(MAX_DESCRIPTION_LENGTH, 140).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec start() -> {ok, [atom()]}.
start() -> application:ensure_all_started(egithub).

%% Application Behavior

%% @hidden
-spec start(application:start_type(), term()) ->
    {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _Arg) ->
    egithub_sup:start_link().

%% @hidden
-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%% Credentials

%% @doc Takes a username and a password. Returns a value that can be used
%%      for basic authentication.
%% @end
-spec basic_auth(string(), string()) -> egithub:credentials().
basic_auth(User, Password) ->
    {basic, User, Password}.

%% @doc Takes a valid OAuth token. Returns a value that can be used
%%      for OAuth authentication.
%% @end
-spec oauth(binary()) -> egithub:credentials().
oauth(Token) ->
    {oauth, Token}.

%% Pull Requests


%% @doc List pull requests for a repository for the
%%      authenticated user
%% @end
-spec pull_reqs(credentials(), repository(), map()) -> result().
pull_reqs(Cred, Repo, Opts) ->
    Url = make_url(pull_reqs, {Repo, Opts}),
    {ok, Result} = egithub_req:run(Cred, Url),
    PullRequests = egithub_json:decode(Result),
    {ok, PullRequests}.

%% @doc Takes valid credentials, a string representing a repository (i.e
%%      "username/reponame" and the pull request number.
%%      Returns <code>{ok, Files}</code> where <code>Files</code> is the
%%      decoded JSON representation of GitHub's response.
%% @end
-spec pull_req_files(credentials(), repository(), integer()) ->
    result().
pull_req_files(Cred, Repo, PR) ->
    Url = make_url({pull_req, files}, {Repo, PR}),
    {ok, Result} = egithub_req:run(Cred, Url),
    Files = egithub_json:decode(Result),
    {ok, Files}.

%% @equiv pull_req_comment_line(Credentials, Repo, PR, CommitId, Filename,
%%                              Line, Text, #{post_method => run})
%% @end
-spec pull_req_comment_line(credentials(), repository(), integer(),
                            string(), binary(), integer(), iodata()) ->
    result().
pull_req_comment_line(Cred, Repo, PR,
                      CommitId, Filename, Line, Text) ->
    pull_req_comment_line(Cred, Repo, PR,
                          CommitId, Filename, Line, Text,
                          #{post_method => run}).

%% @doc Takes valid credentials, a string representing a repository (i.e
%%      "username/reponame", the pull request number, the commit SHA, the
%%      relative path to the repository's file, the line where the comment
%%      shuold be added, the comment's text and some options.
%%      Returns <code>ok</code> if everything goes well.
%% @end
-spec pull_req_comment_line(credentials(), repository(), integer(),
                            string(), binary(), integer(), iodata(),
                            options()) ->
    result().
pull_req_comment_line(Cred, Repo, PR,
                      CommitId, Filename, Line, Text, Options) ->
    Url = make_url({pull_req, comments}, {Repo, PR}),
    Body = #{<<"commit_id">> => to_bin(CommitId),
             <<"path">> => Filename,
             <<"position">> => Line,
             <<"body">> => Text
            },
    maybe_queue_request(Cred, Url, egithub_json:encode(Body), Options).

%% @doc Takes valid credentials, a string representing a repository (i.e
%%      "username/reponame" and the pull request number.
%%      Returns <code>{ok, Comments}</code> where <code>Comments</code> is the
%%      decoded JSON representation of GitHub's response.
%% @end
-spec pull_req_comments(credentials(), repository(), integer()) ->
    result().
pull_req_comments(Cred, Repo, PR) ->
    Url = make_url({pull_req, comments}, {Repo, PR}),
    {ok, Result} = egithub_req:run(Cred, Url),
    Comments = egithub_json:decode(Result),
    {ok, Comments}.

%% @doc Takes valid credentials, a string representing a repository (i.e
%%      "username/reponame", the issue/pull number, the PR review object
%%      and some options.
%%      Returns <code>{ok, RespBody}</code> if everything goes well, where
%%      <code>RespBody</code> is the plain text body returned by GitHub.
%% @end
-spec pr_review(credentials(), repository(), integer(),
                egithub_webhook:review(), options()) ->
   result().
pr_review(Cred, Repo, PR, Body, Options) ->
    Url = make_url(reviews, {Repo, PR}),
    maybe_queue_request(Cred, Url, egithub_json:encode(Body), Options).

%% @doc Takes valid credentials, a string representing a repository (i.e
%%      "username/reponame", and the issue/pull number.
%%      Returns <code>{ok, [map()]}</code> if everything goes well, where
%%      <code>[map()]</code> is a list of reviews.
%% @end
-spec pr_reviews(credentials(), repository(), integer()) ->
   result().
pr_reviews(Cred, Repo, PR) ->
    Url = make_url(reviews, {Repo, PR}),
    {ok, Reviews} = egithub_req:run(Cred, Url),
    {ok, egithub_json:decode(Reviews)}.

%% @doc Takes valid credentials, a string representing a repository (i.e
%%      "username/reponame", the issue/pull number, review ID, and a PR
%%      review dismissal message.
%%      Returns <code>{ok, RespBody}</code> if everything goes well, where
%%      <code>RespBody</code> is the plain text body returned by GitHub.
%% @end
-spec dismiss_pr_review(credentials(), repository(), integer(), integer(),
                        iodata()) ->
   result().
dismiss_pr_review(Cred, Repo, PR, RId, Body) ->
    Url = make_url({reviews, dismissals}, {Repo, PR, RId}),
    egithub_req:run(Cred, Url, put, egithub_json:encode(Body)).

%% Issues
%% @doc Create an issue
-spec create_issue(credentials(), string(), repository(), string(), string(),
                   string(), issue_labels()) -> result().
create_issue(Cred, Username, Repo, Title, Text, Assignee, Labels) ->
    create_issue(Cred, Username, Repo, Title, Text, Assignee, 1, Labels,
                 #{post_method => run}).

%% @doc Create an issue
-spec create_issue(credentials(), string(), repository(), string(), string(),
                   string(), pos_integer(), issue_labels()) -> result().
create_issue(Cred, Username, Repo, Title, Text, Assignee, Milestone, Labels) ->
    create_issue(Cred, Username, Repo, Title, Text, Assignee, Milestone, Labels,
                 #{post_method => run}).

%% @doc Create an issue
-spec create_issue(credentials(), string(), repository(), string(), string(),
                   string(), pos_integer(), issue_labels(), options()) ->
                          result().
create_issue(Cred, Username, Repo, Title, Text, Assignee, Milestone, Labels,
             Options) ->
    Url = make_url(issue, {Username, Repo}),
    Body = #{title     => to_bin(Title),
             body      => to_bin(Text),
             assignee  => to_bin(Assignee),
             milestone => Milestone,
             labels    => lists:map(fun to_bin/1, Labels)},
    maybe_queue_request(Cred, Url, egithub_json:encode(Body), Options).


%% @doc List all issues across all the authenticated user's visible repositories
%%      including owned repositories, member repositories, and organization
%%      repositories
%% @end
-spec all_issues(credentials(), map()) -> result().
all_issues(Cred, Opts) ->
    Url = make_url(issues, {Opts}),
    {ok, Result} = egithub_req:run(Cred, Url),
    Issues = egithub_json:decode(Result),
    {ok, Issues}.

%% @doc List issues for a specific owner repository
-spec all_issues(credentials(), repository(), map()) -> result().
all_issues(Cred, Repo, Opts) ->
    Url = make_url(issues, {Repo, Opts}),
    {ok, Result} = egithub_req:run(Cred, Url),
    Issues = egithub_json:decode(Result),
    {ok, Issues}.

%% @doc Get a single issue  for a specific owner repository
%%      and its id.
%% @end
-spec issue(credentials(), repository(), integer()) -> result().
issue(Cred, Repo, IssueId) ->
    Url = make_url(issue, {Repo, IssueId}),
    {ok, Result} = egithub_req:run(Cred, Url),
    Issue = egithub_json:decode(Result),
    {ok, Issue}.

%% @doc List all issues across owned and member repositories for the
%%      authenticated user.
%% @end
-spec issues_user(credentials(), map()) -> result().
issues_user(Cred, Opts) ->
    Url = make_url(issues_user, {Opts}),
    {ok, Result} = egithub_req:run(Cred, Url),
    Issues = egithub_json:decode(Result),
    {ok, Issues}.

%% @doc List all issues for a given organization for the authenticated user.
-spec issues_org(credentials(), string(), map()) -> result().
issues_org(Cred, Org, Opts) ->
    Url = make_url(issues_org, {Org, Opts}),
    {ok, Result} = egithub_req:run(Cred, Url),
    Issues = egithub_json:decode(Result),
    {ok, Issues}.

%% @equiv issue_comment(Cred, Repo, PR, Text, #{post_method => run})
-spec issue_comment(credentials(), repository(), integer(), iodata()) ->
    result().
issue_comment(Cred, Repo, PR, Text) ->
    issue_comment(Cred, Repo, PR, Text, #{post_method => run}).

%% @doc Takes valid credentials, a string representing a repository (i.e
%%      "username/reponame", the issue number, the comment's text and some
%%      options.
%%      Returns <code>{ok, RespBody}</code> if everything goes well, where
%%      <code>RespBody</code> is the plain text body returned by GitHub.
%% @end
-spec issue_comment(credentials(), repository(), integer(), iodata(),
                    options()) ->
   result().
issue_comment(Cred, Repo, PR, Text, Options) ->
    Url = make_url({issue, comments}, {Repo, PR}),
    Body = #{<<"body">> => Text},
    maybe_queue_request(Cred, Url, egithub_json:encode(Body), Options).

%% @doc Takes valid credentials, a string representing a repository (i.e
%%      "username/reponame" and the issue number.
%%      Returns <code>{ok, Comments}</code> where <code>Comments</code> is the
%%      decoded JSON representation of GitHub's response.
%% @end
-spec issue_comments(credentials(), repository(), integer()) ->
    result().
issue_comments(Cred, Repo, PR) ->
    Url = make_url({issue, comments}, {Repo, PR}),
    {ok, Result} = egithub_req:run(Cred, Url),
    Comments = egithub_json:decode(Result),
    {ok, Comments}.

%% Files

%% @doc Fetches the contents of a file for a given repository and commit SHA.
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

%% @doc Get the information for the user associated with the provided
%%      credentials.
%% @end
-spec user(credentials()) -> result().
user(Cred) ->
    Url = make_url(user, {}),
    api_call_json_result(Cred, Url).

%% @doc Get the information for the user associated with the provided
%%      <code>Username</code>.
%% @end
-spec user(credentials(), string()) -> result().
user(Cred, Username) ->
    Url = make_url(user, {Username}),
    api_call_json_result(Cred, Url).

%% @doc Get the emails registered for the user associated with the provided
%%      credentials.
%% @end
-spec user_emails(credentials()) -> result().
user_emails(Cred) ->
    Url = make_url(user_emails, {}),
    api_call_json_result(Cred, Url).

%% Orgs

%% @doc Get the organizations for the user associated with the provided
%%      credentials.
%% @end
-spec orgs(credentials()) -> result().
orgs(Cred) ->
    orgs(Cred, undefined).

%% @doc Get the organizations for the user associated with the provided
%%      <code>Username</code>.
%% @end
-spec orgs(credentials(), string()) -> result().
orgs(Cred, User) ->
    Url = make_url(orgs, {User}),
    api_call_json_result(Cred, Url).

%% @doc Check if the user associated with the provided credentials is a
%%      member of <code>OrgName</code>.
%% @end
-spec org_membership(credentials(), string()) -> result().
org_membership(Cred, OrgName) ->
  Url = make_url({orgs, memberships}, {OrgName}),
  api_call_json_result(Cred, Url).

%% Repos

%% @doc Get the repository information of <code>RepoFullName</code>
%%      (i.e. "username/reponame").
%% @end
-spec repo(credentials(), string()) -> result().
repo(Cred, RepoFullName) ->
    Url = make_url(repo, {RepoFullName}),
    api_call_json_result(Cred, Url).

%% @equiv repos(Cred, undefined, Opts)
-spec repos(credentials(), map()) -> result().
repos(Cred, Opts) ->
    repos(Cred, undefined, Opts).

%% @doc Get the repositories associated with the user provided
%%      taking into account the options supplied. If the <code>User</code>
%%      is <code>undefined</code> then the user taken into account is
%%      the one with the credentials.
%%      The options available depend on the GitHub API specs.
%%      Check
%%      <a href="https://developer.github.com/v3/repos/#parameters">here</a>
%%      for more information.
%% @end
-spec repos(credentials(), string(), map()) -> result().
repos(Cred, User, Opts) ->
    Url = make_url(repos, {User, Opts}),
    api_call_json_result(Cred, Url).

%% @doc Same as repos/2 but if there are a lot of repos that need to be paged
%%      handles the paging, asking for all pages until it gets an empty page.
%% @end
-spec all_repos(credentials(), map()) -> result().
all_repos(Cred, Opts) ->
    all_repos(Cred, undefined, Opts#{page => 1}, []).

%% @doc Same as repos/3 but if there are a lot of repos that need to be paged
%%      handles the paging, asking for all pages until it gets an empty page.
%% @end
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

%% @doc Same as repos/3 but for an organization.
-spec org_repos(credentials(), string(), map()) -> result().
org_repos(Cred, Org, Opts) ->
    Url = make_url(org_repos, {Org, Opts}),
    api_call_json_result(Cred, Url).

%% @doc Same as all_repos/3 but for an organization.
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

%% @doc Gets all the teams from an organization.
-spec teams(credentials(), string()) -> result().
teams(Cred, Org) ->
    Url = make_url(teams, {Org}),
    api_call_json_result(Cred, Url).

%% @doc Creates a team in an organization.
-spec create_team(credentials(), string(), string(), string(), [string()]) ->
    result().
create_team(Cred, Org, Name, Permission, Repos) ->
    Url = make_url(teams, {Org}),
    BodyMap = #{name => to_bin(Name),
                permission => to_bin(Permission),
                repo_names => [to_bin(Repo) || Repo <- Repos]},
    Body = egithub_json:encode(BodyMap),
    case egithub_req:run(Cred, Url, post, Body) of
        {ok, Result} ->
            JsonResult = egithub_json:decode(Result),
            {ok, JsonResult};
        {error, {422, _, _}} ->
            {ok, already_exists};
        Other ->
            Other
    end.

%% @doc Add a repository to a team.
-spec add_team_repository(credentials(), integer(), string()) -> result().
add_team_repository(Cred, TeamId, RepoFullName) ->
    Url = make_url(teams_repos, {TeamId, RepoFullName}),
    case egithub_req:run(Cred, Url, put, []) of
        {ok, _} ->
            ok;
        Error ->
            Error
    end.

%% @doc Add a member to a team.
-spec add_team_member(credentials(), integer(), string()) -> result().
add_team_member(Cred, TeamId, Username) ->
    Url = make_url(teams, {TeamId, Username}),
    case egithub_req:run(Cred, Url, put, []) of
        {ok, _} ->
            ok;
        Error ->
            Error
    end.

%% @doc Delete a member from a team.
-spec delete_team_member(credentials(), integer(), string()) -> result().
delete_team_member(Cred, TeamId, Username) ->
    Url = make_url(teams, {TeamId, Username}),
    case egithub_req:run(Cred, Url, delete, []) of
        {ok, _} ->
            ok;
        Error ->
            Error
    end.

%% @doc Check the membership of a user in a team.
-spec team_membership(credentials(), integer(), string()) ->
    active | pending | none.
team_membership(Cred, TeamId, Username) ->
    Url = make_url(team_membership, {TeamId, Username}),
    case api_call_json_result(Cred, Url) of
        {ok, #{<<"state">> := <<"active">>}} -> active;
        {ok, #{<<"state">> := <<"pending">>}} -> pending;
        {error, {404, _, _}} -> none;
        {error, Reason} -> {error, Reason}
    end.

%% Hooks

%% @doc Get all hooks for a repository.
-spec hooks(credentials(), repository()) -> result().
hooks(Cred, Repo) ->
    Url = make_url(hooks, {Repo}),
    api_call_json_result(Cred, Url).

%% @doc Create a webhook in a repository.
-spec create_webhook(credentials(), repository(), string(), [string()]) ->
    result().
create_webhook(Cred, Repo, WebhookUrl, Events) ->
    Url = make_url(hooks, {Repo}),
    BinEvents = [to_bin(E) || E <- Events],
    Data = #{<<"name">> => <<"web">>,
             <<"active">> => true,
             <<"events">> => BinEvents,
             <<"config">> => #{<<"url">> => to_bin(WebhookUrl),
                               <<"content_type">> => <<"json">>}},
    Body = egithub_json:encode(Data),
    case egithub_req:run(Cred, Url, post, Body) of
        {ok, Result} ->
            JsonResult = egithub_json:decode(Result),
            {ok, JsonResult};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Delete a webhook from a repository.
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

%% @doc Get all collaborators in a repository.
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

%% @doc Add a collaborator to a repository.
-spec add_collaborator(credentials(), repository(), string()) -> result().
add_collaborator(Cred, Repo, Collaborator) ->
    Url = make_url(collaborators, {Repo, Collaborator}),
    case egithub_req:run(Cred, Url, put, []) of
        {ok, _Result} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Remove a collaborator from a repository.
-spec remove_collaborator(credentials(), repository(), string()) -> result().
remove_collaborator(Cred, Repo, Collaborator) ->
    Url = make_url(collaborators, {Repo, Collaborator}),
    case egithub_req:run(Cred, Url, delete, []) of
        {ok, _Result} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Statuses

%% @equiv create_status(Cred, Repo, Sha, State, Description, Context, undefined)
-spec create_status(
    credentials(), repository(), string(), state(), string(), string()) ->
    result().
create_status(Cred, Repo, Sha, State, Description, Context) ->
    create_status(Cred, Repo, Sha, State, Description, Context, undefined).

%% @doc Create a new status for the provided <code>SHA</code>.
-spec create_status(
    credentials(), repository(), string(), state(), string(), string(),
    string() | undefined) -> result().
create_status(Cred, Repo, Sha, State, Description, Context, TargetUrl) ->
    Url = make_url(new_status, {Repo, Sha}),
    FormatDescription = format_description(Description),
    Data = #{<<"state">>        => State,
             <<"description">>  => list_to_binary(FormatDescription),
             <<"context">>      => list_to_binary(Context)
            },
    Data1 = case TargetUrl of
                undefined -> Data;
                _ -> Data#{<<"target_url">> => list_to_binary(TargetUrl)}
            end,
    Body = egithub_json:encode(Data1),
    case egithub_req:run(Cred, Url, post, Body) of
        {ok, Result} ->
            JsonResult = egithub_json:decode(Result),
            {ok, JsonResult};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get all statuses for the provided repository and <code>SHA</code>.
-spec statuses(credentials(), repository(), string()) -> result().
statuses(Cred, Repo, Ref) ->
    Url = make_url(statuses, {Repo, Ref}),
    api_call_json_result(Cred, Url).

%% @doc Get a result with the combined values of all statuses for the
%%      provided repository and <code>SHA</code>.
%% @end
-spec combined_status(credentials(), repository(), string()) -> result().
combined_status(Cred, Repo, Ref) ->
    Url = make_url(status, {Repo, Ref}),
    api_call_json_result(Cred, Url).

-spec languages(Cred::credentials(), Repo::repository()) -> result().
languages(Cred, Repo) ->
    Url = make_url(languages, {Repo}),
    api_call_json_result(Cred, Url).

%% Releases

%% @doc Get a single release for a repository of the
%%      authenticated user.
%% @end
-spec release(credentials(), repository(), pos_integer()) -> result().
release(Cred, Repo, Id) ->
    Url = make_url(release, {Repo, Id}),
    api_call_json_result(Cred, Url).

%% @doc List all releases for a repository for the
%%      authenticated user
%% @end
-spec releases(credentials(), repository()) -> result().
releases(Cred, Repo) ->
    Url = make_url(releases, {Repo}),
    api_call_json_result(Cred, Url).

%% @doc List all releases for a repository for the
%%      authenticated user.
%% @end
-spec releases(credentials(), repository(), map()) -> result().
releases(Cred, Repo, Opts) ->
    Url = make_url(releases, {Repo, Opts}),
    api_call_json_result(Cred, Url).

%% @doc Get the latest release for a repository of the
%%      authenticated user.
%% @end
-spec release_latest(credentials(), repository()) ->  result().
release_latest(Cred, Repo) ->
    Url = make_url(release_lastest, {Repo}),
    api_call_json_result(Cred, Url).

%% Branches

%% @doc Get a single branch for a repository of the
%%      authenticated user.
%% @end
-spec branch(credentials(), repository(), string()) -> result().
branch(Cred, Repo, Name) ->
    Url = make_url(branch, {Repo, Name}),
    api_call_json_result(Cred, Url).

%% @doc List all branches for a repository for the
%%      authenticated user
%% @end
-spec branches(credentials(), repository(), map()) -> result().
branches(Cred, Repo, Opts) ->
    Url = make_url(branches, {Repo, Opts}),
    api_call_json_result(Cred, Url).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc format_description format the description to avoid error 422
%% The message submitted is longer that the maximum length of 140 characters
-spec format_description(string()) -> string().
format_description(Description) ->
  case length(Description) of
    Size when Size >= ?MAX_DESCRIPTION_LENGTH ->
      %% to be continued.
      string:sub_string(Description, 1, ?MAX_DESCRIPTION_LENGTH - 3) ++ "...";
    Size when Size < ?MAX_DESCRIPTION_LENGTH -> Description
  end.

%% Create Status
make_url(new_status, {Repo, Sha}) ->
    Url = "/repos/~s/statuses/~s",
    io_lib:format(Url, [Repo, Sha]);

%% Statuses
make_url(statuses, {Repo, Sha}) ->
    Url = "/repos/~s/commits/~s/statuses",
    io_lib:format(Url, [Repo, Sha]);

%% Status
make_url(status, {Repo, Sha}) ->
    Url = "/repos/~s/commits/~s/status",
    io_lib:format(Url, [Repo, Sha]);

%% Pull Request
make_url(pull_reqs, {Repo, Opts}) ->
    Url = lists:flatten(
        io_lib:format("/repos/~s/pulls", [Repo])),
    Params = build_params(pull_reqs, Opts),
    maybe_append_qs_params(Url, Params);
make_url({pull_req, Subentity}, {Repo, PR}) ->
    SubentityStr = to_str(Subentity),
    Url = "/repos/~s/pulls/~p/" ++ SubentityStr,
    io_lib:format(Url, [Repo, PR]);

%% Pull Request Reviews
make_url(reviews, {Repo, PR}) ->
    Url = "/repos/~s/pulls/~p/reviews",
    io_lib:format(Url, [Repo, PR]);
make_url({reviews, Subentity}, {Repo, PR, RId}) ->
    SubentityStr = to_str(Subentity),
    Url = "/repos/~s/pulls/~p/reviews/~p/" ++ SubentityStr,
    io_lib:format(Url, [Repo, PR, RId]);

%% Issues
make_url(issue, {Repo, IssueId}) when is_integer(IssueId) ->
    io_lib:format("/repos/~s/issues/~p", [Repo, IssueId]);
make_url(issue, {User, Repo}) ->
    io_lib:format("/repos/~s/~s/issues", [User, Repo]);
make_url(issues, {Repo, Opts}) ->
    Url = io_lib:format("/repos/~s/issues", [Repo]),
    Params = build_params(issues, Opts),
    maybe_append_qs_params(Url, Params);
make_url(issues, {Opts}) ->
    Params = build_params(issues, Opts),
    maybe_append_qs_params("/issues", Params);
make_url(issues_user, {Opts}) ->
    Params = build_params(issues, Opts),
    maybe_append_qs_params("/user/issues", Params);
make_url(issues_org, {Org, Opts}) ->
    Url = io_lib:format("/orgs/~s/issues", [Org]),
    Params = build_params(issues, Opts),
    maybe_append_qs_params(Url, Params);

%% Issues comments etc
make_url({issue, Subentity}, {Repo, PR}) ->
    SubentityStr = to_str(Subentity),
    Url = "/repos/~s/issues/~p/" ++ SubentityStr,
    io_lib:format(Url, [Repo, PR]);

%% Files
make_url(file_content, {Repo, CommitId, Filename}) ->
    Url = "/repos/~s/contents/~s?ref=~s",
    io_lib:format(Url, [Repo, Filename, CommitId]);

%% User
make_url(user, {}) ->
    Url = "/user",
    io_lib:format(Url, []);
make_url(user, {Username}) ->
    Url = "/users/~s",
    io_lib:format(Url, [Username]);
make_url(user_emails, {}) ->
    Url = "/user/emails",
    io_lib:format(Url, []);

%% Organizations
make_url(orgs, {undefined}) ->
    Url = "/user/orgs",
    io_lib:format(Url, []);
make_url(orgs, {User}) ->
    Url = "/users/~s/orgs",
    io_lib:format(Url, [User]);
make_url({orgs, memberships}, {OrgName}) ->
  Url = "/user/memberships/orgs/~s",
  io_lib:format(Url, [OrgName]);

%% Teams
make_url(teams, {Org}) ->
    Url = "/orgs/~s/teams",
    io_lib:format(Url, [Org]);
make_url(teams, {TeamId, Username}) ->
    Url = "/teams/~p/members/~s",
    io_lib:format(Url, [TeamId, Username]);
make_url(teams_repos, {TeamId, RepoFullName}) ->
    Url = "/teams/~p/repos/~s",
    io_lib:format(Url, [TeamId, RepoFullName]);
make_url(team_membership, {TeamId, Username}) ->
    Url = "/teams/~p/memberships/~s",
    io_lib:format(Url, [TeamId, Username]);

%% Repositories
make_url(repo, {RepoFullName}) ->
    Url = "/repos/~s",
    io_lib:format(Url, [RepoFullName]);
make_url(repos, {undefined, Opts}) ->
    Params = build_params(repos, Opts),
    maybe_append_qs_params("/user/repos", Params);
make_url(repos, {User, Opts}) ->
    Page = maps:get(page, Opts, 1),
    Url = "/users/~s/repos?page=~p",
    io_lib:format(Url, [User, Page]);
make_url(org_repos, {User, Opts}) ->
    Page = maps:get(page, Opts, 1),
    % 100 by default to reduce the number of requests
    PerPage = maps:get(per_page, Opts, 100),
    Url = "/orgs/~s/repos?page=~p&per_page=~p",
    io_lib:format(Url, [User, Page, PerPage]);

%% Hooks
make_url(hooks, {Repo}) ->
    Url = "/repos/~s/hooks",
    io_lib:format(Url, [Repo]);
make_url(hooks, {Repo, Id}) ->
    Url = "/repos/~s/hooks/~s",
    io_lib:format(Url, [Repo, Id]);

%% Colaborators
make_url(collaborators, {Repo}) ->
    Url = "/repos/~s/collaborators",
    io_lib:format(Url, [Repo]);
make_url(collaborators, {Repo, Username}) ->
    Url = "/repos/~s/collaborators/~s",
    io_lib:format(Url, [Repo, Username]);

%% Languages
make_url(languages, {Repo}) ->
    Url = "/repos/~s/languages",
    io_lib:format(Url, [Repo]);

%% Releases
make_url(release, {Repo, Id}) ->
    Url = "/repos/~s/releases/~p",
    io_lib:format(Url, [Repo, Id]);
make_url(releases, {Repo}) ->
    Url = "/repos/~s/releases",
    io_lib:format(Url, [Repo]);
make_url(releases, {Repo, Opts}) ->
    Page = maps:get(page, Opts, 1),
    Url = "/repos/~s/releases?page=~p",
    io_lib:format(Url, [Repo, Page]);
make_url(release_lastest, {Repo}) ->
    Url = "/repos/~s/releases/latest",
    io_lib:format(Url, [Repo]);

%% Branches
make_url(branch, {Repo, Name}) ->
    Url = "/repos/~s/branches/~s",
    io_lib:format(Url, [Repo, Name]);
make_url(branches, {Repo, Opts}) ->
    Page = maps:get(page, Opts, 1),
    Protected = maps:get(protected, Opts, false),
    Url = "/repos/~s/branches?page=~p&protected=~s",
    io_lib:format(Url, [Repo, Page, Protected]).

api_call_json_result(Cred, Url) ->
    case egithub_req:run(Cred, Url) of
        {ok, Result} ->
            JsonResult = egithub_json:decode(Result),
            {ok, JsonResult};
        {error, Reason} ->
            {error, Reason}
    end.

maybe_queue_request(Cred, Url, JsonBody, Options) ->
    case Options of
        #{post_method := run} ->
            egithub_req:run(Cred, Url, post, JsonBody);
        #{post_method := queue} ->
            egithub_req:queue(Cred, Url, post, JsonBody)
    end.

maybe_append_qs_params(Url, Params) ->
    case maps:size(Params) > 0 of
        false ->
            io_lib:format(Url, []);
        true ->
            QS = format_qs_params(maps:to_list(Params), []),
            [lists:flatten(io_lib:format("~s?~s", [Url, QS]))]
    end.
format_qs_params([], Acc) ->
    lists:flatten(string:join(lists:reverse(Acc), "&"));
format_qs_params([{_K, ""} | Params], Acc) ->
    format_qs_params(Params, Acc);
format_qs_params([{K, V} | Params], Acc) ->
    format_qs_params(Params, [format_qs_param(K, V) | Acc]).

format_qs_param(K, V) when is_number(V) ->
    io_lib:format("~s=~p", [K, V]);
format_qs_param(K, V) ->
    io_lib:format("~s=~s", [K, V]).

build_params(repos, Opts) ->
    #{  type      => maps:get(type, Opts, "all"),
        sort      => maps:get(sort, Opts, "full_name"),
        direction => maps:get(direction, Opts, "asc"),
        page      => maps:get(page, Opts, 1)
    };
build_params(issues, Opts) when map_size(Opts) == 0 ->
    #{};
build_params(issues, Opts) ->
    #{  filter    => maps:get(filter, Opts, "assigned"),
        state     => maps:get(state, Opts, "open"),
        labels    => maps:get(labels, Opts, ""),
        sort      => maps:get(sort, Opts, "created"),
        direction => maps:get(direction, Opts, "asc"),
        since     => maps:get(since, Opts, "")
    };
build_params(pull_reqs, Opts) ->
    #{  state     => maps:get(state, Opts, "open"),
        head      => maps:get(head, Opts, ""),
        base      => maps:get(base, Opts, ""),
        sort      => maps:get(sort, Opts, "created"),
        direction => maps:get(direction, Opts, "asc"),
        page      => maps:get(page, Opts, 1)
    }.

to_str(Arg) when is_binary(Arg) ->
    unicode:characters_to_list(Arg);
to_str(Arg) when is_atom(Arg) ->
    atom_to_list(Arg);
to_str(Arg) when is_integer(Arg) ->
    integer_to_list(Arg);
to_str(Arg) when is_list(Arg) ->
    Arg.

to_bin(Arg) when is_list(Arg) ->
    list_to_binary(Arg);
to_bin(Arg) when is_atom(Arg) ->
    atom_to_binary(Arg, latin1);
to_bin(Arg) when is_binary(Arg) ->
    Arg.
