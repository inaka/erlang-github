-module(egithub_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         pull_reqs/1,
         issue_comments/1,
         pr_review/1,
         issues/1,
         files/1,
         users/1,
         orgs/1,
         repos/1,
         teams/1,
         hooks/1,
         collaborators/1,
         statuses/1,
         releases/1,
         branches/1,
         tags/1
        ]).

-record(client, {}).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite
        ]).

-type config() :: [{atom(), term()}].
-type result() :: ok | {ok, term()} | {error, term()}.

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

-spec pull_reqs(config()) -> result().
pull_reqs(_Config) ->
  Credentials = github_credentials(),

  meck:new(hackney, [passthrough]),
  try
    BodyReturnFun = fun(_) -> {ok, <<"[]">>} end,

    PRsUrl = "/repos/user/repo/pulls?direction=asc&page=1&sort=created"
             "&state=open",
    AllOpenPRFun = match_fun(PRsUrl, get),
    meck:expect(hackney, body, BodyReturnFun),
    meck:expect(hackney, request, AllOpenPRFun),
    {ok, _} = egithub:pull_reqs(Credentials, "user/repo", #{state => "open"}),

    SinglePRFun = match_fun("/repos/user/repo/pulls/1", get),
    meck:expect(hackney, body, BodyReturnFun),
    meck:expect(hackney, request, SinglePRFun),
    {ok, _} = egithub:pull_req(Credentials, "user/repo", 1),

    PRFilesFun = match_fun("/repos/user/repo/pulls/1/files", get),
    meck:expect(hackney, body, BodyReturnFun),
    meck:expect(hackney, request, PRFilesFun),
    {ok, _} = egithub:pull_req_files(Credentials, "user/repo", 1),

    PRCommentLineFun = match_fun("/repos/user/repo/pulls/1/comments", post),
    meck:expect(hackney, request, PRCommentLineFun),
    {ok, _} = egithub:pull_req_comment_line(Credentials, "user/repo", 1,
                                            "SHA", <<"file-path">>,
                                            5, "comment text"),

    Self = self(),
    PRCommentLineQueueFun = fun(_, _, _, _) ->
                                Self ! ok,
                                {ok, 200, [], #client{}}
                            end,
    meck:expect(hackney, request, PRCommentLineQueueFun),
    ok = egithub:pull_req_comment_line(Credentials, "user/repo", 1,
                                       "SHA", <<"file-path">>,
                                       5, "comment text",
                                       #{post_method => queue}),
    ok = receive ok -> ok after 5000 -> timeout end,

    PRCommentsFun = match_fun("/repos/user/repo/pulls/1/comments",
                              get),
    meck:expect(hackney, request, PRCommentsFun),
    {ok, _} = egithub:pull_req_comments(Credentials, "user/repo", 1)
  after
    meck:unload(hackney)
  end.

-spec issue_comments(config()) -> result().
issue_comments(_Config) ->
  Credentials = github_credentials(),

  meck:new(hackney, [passthrough]),
  try
    IssueCommentFun = match_fun("/repos/user/repo/issues/1/comments", post),
    meck:expect(hackney, request, IssueCommentFun),
    BodyReturnFun = fun(_) -> {ok, <<"[]">>} end,
    meck:expect(hackney, body, BodyReturnFun),
    {ok, _} = egithub:issue_comment(Credentials, "user/repo", 1, "txt"),

    Self = self(),
    IssueCommentQueueFun = fun(post, Url, _, _) ->
                               <<"https://api.github.com/"
                                 "repos/user/repo/issues/1/comments">> = Url,
                               Self ! ok,
                               {ok, 200, [], #client{}}
                           end,
    meck:expect(hackney, request, IssueCommentQueueFun),
    ok = egithub:issue_comment(Credentials, "user/repo", 1,
                               "txt", #{post_method => queue}),
    ok = receive ok -> ok after 5000 -> timeout end,

    IssueCommentsFun = match_fun("/repos/user/repo/issues/1/comments",
                                 get),
    meck:expect(hackney, request, IssueCommentsFun),
    {ok, _} = egithub:issue_comments(Credentials, "user/repo", 1)
  after
    meck:unload(hackney)
  end.

-spec pr_review(config()) -> result().
pr_review(_Config) ->
  Credentials = github_credentials(),

  meck:new(hackney, [passthrough]),
  try
    Self = self(),
    PrReviewQueueFun = fun(post, Url, _, _) ->
                           <<"https://api.github.com/"
                             "repos/user/repo/pulls/1/reviews">> = Url,
                           Self ! ok,
                           {ok, 200, [], #client{}}
                       end,
    meck:expect(hackney, request, PrReviewQueueFun),
    ReqBody = #{body  => <<>>,
                comments => [#{path => <<"/path/to/file">>,
                               position => 20,
                               body => <<"bad function naming">>}],
                commit_id => <<"c0m1tt1d">>,
                event => <<"REQUEST_CHANGES">>},
    ok = egithub:pr_review(Credentials, "user/repo", 1, ReqBody,
                           #{post_method => queue}),
    ok = receive ok -> ok after 5000 -> timeout end
  after
    meck:unload(hackney)
  end.

-spec issues(config()) -> result().
issues(_Config) ->
  Credentials = github_credentials(),

  meck:new(hackney, [passthrough]),
  try
      BodyReturnFun = fun(_) -> {ok, <<"[]">>} end,

      meck:expect(hackney, body, BodyReturnFun),
      CreateIssueFun = match_fun("/repos/user/repo/issues", post),
      meck:expect(hackney, request, CreateIssueFun),
      {ok, _} = egithub:create_issue(Credentials, "user", "repo", "title",
                                     "text", "user", ["bug"]),

      AllIssuesFun = match_fun("/issues", get),
      meck:expect(hackney, request, AllIssuesFun),
      {ok, _} = egithub:all_issues(Credentials, #{}),

      AllRepoIssuesFun = match_fun("/repos/user/repo/issues", get),
      meck:expect(hackney, request, AllRepoIssuesFun),
      {ok, _} = egithub:all_issues(Credentials, "user/repo", #{}),

      IssueUrl = "/issues?direction=asc&filter=assigned&"
                 "sort=created&state=open",
      AllIssuesOpenFun = match_fun(IssueUrl, get),
      meck:expect(hackney, request, AllIssuesOpenFun),
      {ok, _} = egithub:all_issues(Credentials, #{state => "open"}),

      UserIssuesFun = match_fun("/user/issues", get),
      meck:expect(hackney, request, UserIssuesFun),
      {ok, _} = egithub:issues_user(Credentials, #{}),

      OrgIssuesFun = match_fun("/orgs/foo/issues", get),
      meck:expect(hackney, request, OrgIssuesFun),
      {ok, _} = egithub:issues_org(Credentials, "foo", #{}),

      SingleIssueFun = match_fun("/repos/user/repo/issues/1", get),
      meck:expect(hackney, request, SingleIssueFun),
      {ok, _} = egithub:issue(Credentials, "user/repo", 1)
  after
      meck:unload(hackney)
  end.

-spec files(config()) -> result().
files(_Config) ->
  Credentials = github_credentials(),

  meck:new(hackney, [passthrough]),
  try
    FileContentFun = match_fun("/repos/user/repo/contents/file?ref=SHA", get),
    meck:expect(hackney, request, FileContentFun),
    BodyReturnFun = fun(_) -> {ok, <<"{\"content\" : \"\"}">>} end,
    meck:expect(hackney, body, BodyReturnFun),
    {ok, _} = egithub:file_content(Credentials, "user/repo", "SHA", "file")
  after
    meck:unload(hackney)
  end.

-spec users(config()) -> result().
users(_Config) ->
  Credentials = github_credentials(),

  meck:new(hackney, [passthrough]),
  try
    BodyReturnFun = fun(_) -> {ok, <<"[]">>} end,
    meck:expect(hackney, body, BodyReturnFun),
    UserFun = match_fun("/user", get),
    meck:expect(hackney, request, UserFun),
    {ok, _} = egithub:user(Credentials),

    GadgetCIFun = match_fun("/users/gadgetci", get),
    meck:expect(hackney, request, GadgetCIFun),
    {ok, _} = egithub:user(Credentials, "gadgetci"),

    EmailsFun = match_fun("/user/emails", get),
    meck:expect(hackney, request, EmailsFun),
    {ok, _} = egithub:user_emails(Credentials)
  after
    meck:unload(hackney)
  end.

-spec orgs(config()) -> result().
orgs(_Config) ->
  Credentials = github_credentials(),

  meck:new(hackney, [passthrough]),
  try
    BodyReturnFun = fun(_) -> {ok, <<"[]">>} end,
    meck:expect(hackney, body, BodyReturnFun),
    OrgsFun = match_fun("/user/orgs", get),
    meck:expect(hackney, request, OrgsFun),
    {ok, _} = egithub:orgs(Credentials),

    OrgsUserFun = match_fun("/users/gadgetci/orgs", get),
    meck:expect(hackney, request, OrgsUserFun),
    {ok, _} = egithub:orgs(Credentials, "gadgetci"),

    OrgMembershipFun = match_fun("/user/memberships/orgs/some-organization",
                                 get),
    meck:expect(hackney, request, OrgMembershipFun),
    {ok, _} = egithub:org_membership(Credentials, "some-organization")
  after
    meck:unload(hackney)
  end.

-spec repos(config()) -> result().
repos(_Config) ->
  Credentials = github_credentials(),

  meck:new(hackney, [passthrough]),
  try
    RepoFun = match_fun("/repos/inaka/whatever", get),
    meck:expect(hackney, request, RepoFun),
    BodyReturnFun = fun(_) -> {ok, <<"[]">>} end,
    meck:expect(hackney, body, BodyReturnFun),
    {ok, _} = egithub:repo(Credentials, "inaka/whatever"),

    ReposFun = match_fun("/user/repos?"
                         "direction=asc&page=1&sort=full_name&type=all",
                         get),
    meck:expect(hackney, request, ReposFun),
    {ok, _} = egithub:repos(Credentials, #{}),

    ReposUserFun = match_fun("/users/gadgetci/repos?page=1",
                             get),
    meck:expect(hackney, request, ReposUserFun),
    {ok, _} = egithub:repos(Credentials, "gadgetci", #{}),

    AllReposFun = match_fun("/user/repos?"
                             "direction=asc&page=1&sort=full_name&type=all",
                             get),
    meck:expect(hackney, request, AllReposFun),
    {ok, _} = egithub:all_repos(Credentials, #{}),
    BodyReturn1Fun = fun(_) -> {ok, <<"[1]">>} end,
    BodyReturnEmptyFun = fun(_) -> {ok, <<"[]">>} end,
    AllReposUserFun =
      fun(get, Url, _, _) ->
          case Url of
            <<"https://api.github.com/users/gadgetci/repos?page=1">> ->
              meck:expect(hackney, body, BodyReturn1Fun),
              {ok, 200, [], #client{}};
            <<"https://api.github.com/users/gadgetci/repos?page=2">> ->
              meck:expect(hackney, body, BodyReturnEmptyFun),
              {ok, 200, [], #client{}}
          end
      end,
    meck:expect(hackney, request, AllReposUserFun),
    {ok, _} = egithub:all_repos(Credentials, "gadgetci", #{}),

    AllReposErrorFun =
      fun(get, Url, _, _) ->
          case Url of
            <<"https://api.github.com/users/gadgetci/repos?page=1">> ->
              meck:expect(hackney, body, BodyReturn1Fun),
              {ok, 200, [], #client{}};
            <<"https://api.github.com/users/gadgetci/repos?page=2">> ->
              meck:expect(hackney, body, BodyReturnEmptyFun),
              {ok, 400, [], #client{}}
          end
      end,
    meck:expect(hackney, request, AllReposErrorFun),
    {error, _} = egithub:all_repos(Credentials, "gadgetci", #{}),

    OrgReposFun = match_fun("/orgs/some-org/repos?page=1&per_page=100", get),
    meck:expect(hackney, request, OrgReposFun),
    meck:expect(hackney, body, BodyReturnEmptyFun),
    {ok, _} = egithub:org_repos(Credentials, "some-org", #{}),

    AllOrgReposFun =
      fun(get, Url, _, _) ->
          case Url of
            <<"https://api.github.com"
              "/orgs/some-org/repos?page=1&per_page=100">> ->
              meck:expect(hackney, body, BodyReturn1Fun),
              {ok, 200, [], #client{}};
            <<"https://api.github.com"
              "/orgs/some-org/repos?page=2&per_page=100">> ->
              meck:expect(hackney, body, BodyReturnEmptyFun),
              {ok, 200, [], #client{}}
          end
      end,
    meck:expect(hackney, request, AllOrgReposFun),
    {ok, _} = egithub:all_org_repos(Credentials, "some-org", #{}),

    AllOrgReposErrorFun =
      fun(get, Url, _, _)  ->
          case Url of
            <<"https://api.github.com"
              "/orgs/some-org/repos?page=1&per_page=100">> ->
              meck:expect(hackney, body, BodyReturn1Fun),
              {ok, 200, [], #client{}};
            <<"https://api.github.com"
              "/orgs/some-org/repos?page=2&per_page=100">> ->
              meck:expect(hackney, body, BodyReturnEmptyFun),
              {ok, 400, [], #client{}}
          end
      end,
    meck:expect(hackney, request, AllOrgReposErrorFun),
    {error, _} = egithub:all_org_repos(Credentials, "some-org", #{})
  after
    meck:unload(hackney)
  end.

-spec teams(config()) -> pending.
teams(_Config) ->
  Credentials = github_credentials(),

  meck:new(hackney, [passthrough]),
  try
    BodyReturnFun = fun(_) -> {ok, <<"[]">>} end,
    meck:expect(hackney, body, BodyReturnFun),
    TeamsFun = match_fun("/orgs/some-org/teams", get),
    meck:expect(hackney, request, TeamsFun),
    {ok, _} = egithub:teams(Credentials, "some-org"),

    CreateTeamFun = match_fun("/orgs/some-org/teams", post),
    meck:expect(hackney, request, CreateTeamFun),
    {ok, _} = egithub:create_team(Credentials, "some-org", "Team", "", []),

    CreateTeam422Fun =
      fun(post, <<"https://api.github.com/orgs/some-org/teams">>, _, _) ->
        {error, {422, [{<<"Server">>, <<"GitHub.com">>}], <<"error">>}}
      end,
    meck:expect(hackney, request, CreateTeam422Fun),
    {ok, already_exists} =
        egithub:create_team(Credentials, "some-org", "Team", "",
                            ["some-org/repo"]),

    AddTeamRepoFun = match_fun("/teams/1/repos/user/repo",
                               put),
    meck:expect(hackney, request, AddTeamRepoFun),
    ok = egithub:add_team_repository(Credentials, 1, "user/repo"),

    AddTeamMemberFun = match_fun("/teams/1/members/gadgetci",
                                 put),
    meck:expect(hackney, request, AddTeamMemberFun),
    ok = egithub:add_team_member(Credentials, 1, "gadgetci"),

    DeleteTeamMemberFun = match_fun("/teams/1/members/gadgetci",
                                    delete),
    meck:expect(hackney, request, DeleteTeamMemberFun),
    ok = egithub:delete_team_member(Credentials, 1, "gadgetci"),

    TeamMembershipFun = match_fun("/teams/1/memberships/gadgetci", get),
    meck:expect(hackney, request, TeamMembershipFun),
    TeamMembershipBodyFun = fun(_) -> {ok, <<"{\"state\" : \"pending\"}">>} end,
    meck:expect(hackney, body, TeamMembershipBodyFun),
    pending = egithub:team_membership(Credentials, 1, "gadgetci")
  after
    meck:unload(hackney)
  end.

-spec hooks(config()) -> result().
hooks(_Config) ->
  Credentials = github_credentials(),

  meck:new(hackney, [passthrough]),
  try
    BodyReturnFun = fun(_) -> {ok, <<"[]">>} end,
    meck:expect(hackney, body, BodyReturnFun),
    HooksFun = match_fun("/repos/some-repo/hooks", get),
    meck:expect(hackney, request, HooksFun),
    {ok, _} = egithub:hooks(Credentials, "some-repo"),

    CreateHookFun = match_fun("/repos/some-repo/hooks",
                              post),
    meck:expect(hackney, request, CreateHookFun),
    {ok, _} = egithub:create_webhook(Credentials, "some-repo",
                                     "url", ["pull_request"]),

    DeleteHookFun = match_fun("/repos/some-repo/hooks/url",
                              delete),
    meck:expect(hackney, request, DeleteHookFun),
    ok = egithub:delete_webhook(Credentials, "some-repo", "url")
  after
    meck:unload(hackney)
  end.

-spec collaborators(config()) -> result().
collaborators(_Config) ->
  Credentials = github_credentials(),

  meck:new(hackney, [passthrough]),
  try
    BodyReturnFun = fun(_) -> {ok, <<"[]">>} end,
    meck:expect(hackney, body, BodyReturnFun),
    CollaboratorsFun = match_fun("/repos/some-repo/collaborators", get),
    meck:expect(hackney, request, CollaboratorsFun),
    {ok, _} = egithub:collaborators(Credentials, "some-repo"),

    AddCollabFun = match_fun("/repos/some-repo/collaborators/username",
                             put),
    meck:expect(hackney, request, AddCollabFun),
    ok = egithub:add_collaborator(Credentials, "some-repo", "username"),

    DeleteCollabFun = match_fun("/repos/some-repo/collaborators/username",
                                delete),
    meck:expect(hackney, request, DeleteCollabFun),
    ok = egithub:remove_collaborator(Credentials, "some-repo", "username")
  after
    meck:unload(hackney)
  end.

-spec statuses(config()) -> result().
statuses(_Config) ->
  Credentials = github_credentials(),

  meck:new(hackney, [passthrough]),
  try
    StatusesFun = match_fun("/repos/some-repo/commits/ref/statuses", get),
    BodyReturnFun = fun(_) -> {ok, <<"[]">>} end,
    meck:expect(hackney, body, BodyReturnFun),
    meck:expect(hackney, request, StatusesFun),
    {ok, _} = egithub:statuses(Credentials, "some-repo", "ref"),

    CreateStatusFun = match_fun("/repos/some-repo/statuses/SHA",
                                post),
    meck:expect(hackney, request, CreateStatusFun),
    {ok, _} = egithub:create_status(Credentials, "some-repo", "SHA", pending,
                                    "description", "context"),

    CreateStatusUrlFun = match_fun("/repos/some-repo/statuses/SHA",
                                   post),
    meck:expect(hackney, request, CreateStatusUrlFun),
    {ok, _} = egithub:create_status(Credentials, "some-repo", "SHA", pending,
                                    "description", "context", "url"),

    CombinedStatusFun = match_fun("/repos/some-repo/commits/ref/status",
                                  get),
    meck:expect(hackney, request, CombinedStatusFun),
    {ok, _} = egithub:combined_status(Credentials, "some-repo", "ref")
  after
    meck:unload(hackney)
  end.

-spec releases(config()) -> result().
releases(_Config) ->
    Credentials = github_credentials(),

    meck:new(hackney, [passthrough]),
    try
      BodyReturnFun = fun(_) -> {ok, <<"[]">>} end,
      meck:expect(hackney, body, BodyReturnFun),

      ReleaseFun = match_fun("/repos/inaka/whatever/releases/1", get),
      meck:expect(hackney, request, ReleaseFun),
      {ok, _} = egithub:release(Credentials, "inaka/whatever", 1),

      ReleasesFun = match_fun("/repos/inaka/whatever/releases", get),
      meck:expect(hackney, request, ReleasesFun),
      {ok, _} = egithub:releases(Credentials, "inaka/whatever"),

      ReleasesPageFun = match_fun("/repos/inaka/whatever/releases?page=2",
                                  get),
      meck:expect(hackney, request, ReleasesPageFun),
      {ok, _} = egithub:releases(Credentials, "inaka/whatever", #{page => 2}),

      LatestReleaseFun = match_fun("/repos/inaka/whatever/releases/latest",
                                   get),
      meck:expect(hackney, request, LatestReleaseFun),
      {ok, _} = egithub:release_latest(Credentials, "inaka/whatever")
    after
      meck:unload(hackney)
    end.

-spec branches(config()) -> result().
branches(_Config) ->
    Credentials = github_credentials(),

    meck:new(hackney, [passthrough]),
    try
      BodyReturnFun = fun(_) -> {ok, <<"[]">>} end,
      meck:expect(hackney, body, BodyReturnFun),

      BranchFun = match_fun("/repos/inaka/whatever/branches/master", get),
      meck:expect(hackney, request, BranchFun),
      {ok, _} = egithub:branch(Credentials, "inaka/whatever", "master"),

      BranchesUrl = "/repos/inaka/whatever/branches?page=1&protected=false",
      BranchesFun = match_fun(BranchesUrl, get),
      meck:expect(hackney, request, BranchesFun),
      {ok, _} = egithub:branches(Credentials, "inaka/whatever", #{}),

      BranchesUrl2 = "/repos/inaka/whatever/branches?page=2&protected=true",
      BranchesFun2 = match_fun(BranchesUrl2, get),
      meck:expect(hackney, request, BranchesFun2),
      {ok, _} = egithub:branches(Credentials, "inaka/whatever",
                                 #{page => 2, protected => true})
    after
      meck:unload(hackney)
    end.

-spec tags(config()) -> result().
tags(_Config) ->
    Credentials = github_credentials(),

    meck:new(hackney, [passthrough]),
    try
      BodyReturnFun = fun(_) -> {ok, <<"[]">>} end,
      meck:expect(hackney, body, BodyReturnFun),

      TagFun = match_fun("/repos/inaka/whatever/tags/v1.2", get),
      meck:expect(hackney, request, TagFun),
      {ok, _} = egithub:tag(Credentials, "inaka/whatever", "v1.2"),

      TagsUrl = "/repos/inaka/whatever/tags?after=",
      TagsFun = match_fun(TagsUrl, get),
      meck:expect(hackney, request, TagsFun),
      {ok, _} = egithub:tags(Credentials, "inaka/whatever", #{}),

      TagsUrl2 = "/repos/inaka/whatever/tags?after=v1.3",
      TagsFun2 = match_fun(TagsUrl2, get),
      meck:expect(hackney, request, TagsFun2),
      {ok, _} = egithub:tags(Credentials, "inaka/whatever",
                                 #{after_tag => "v1.3"})
    after
      meck:unload(hackney)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec github_credentials() -> {'basic', string(), string()}.
github_credentials() ->
  egithub:basic_auth("username", "password").

match_fun(Url, Method) ->
  UrlBin = list_to_binary("https://api.github.com" ++ Url),
  fun(MethodParam, UrlParam, _, _) ->
      UrlBin = UrlParam,
      Method = MethodParam,
      RespHeaders = [],
      ClientRef = #client{},
      {ok, 200, RespHeaders, ClientRef}
  end.
