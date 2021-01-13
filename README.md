erlang-github
=============

![build](https://github.com/inaka/erlang-github/workflows/build/badge.svg)

[Github API v3][1] client for Erlang

Usage
=====

`egithub` is implemented as an Erlang application. This means that in order to use it, you
need to add it to your application's `.app` file or start it with:

```erlang
application:ensure_all_started(egithub).
```

Once it has started you can start using any of the API calls.

## Credentials

The GitHub's API offers very few endpoints that don't require any credentials and a lot
that do. For providing credentials you have two options: basic authentication or OAuth.
In both cases, you can just call a function from the `egithub` module to obtain
`Credentials`, that you can later provide to the functions that require it.

Said functions are:

- [`egithub:basic_auth/2`]
  (http://inaka.github.io/erlang-github/src/egithub.html?i=0&search=egithub:basic#basic_auth/1)
- [`egithub:oauth/1`]
  (http://inaka.github.io/erlang-github/src/egithub.html?i=0&search=oauth#oauth/1)

For example to get the information of the logged in user you can do the following:

```erlang
Cred = egithub:basic_auth("username", "password"),
{ok, UserInfo} = egithub:user(Cred).
```

## Webhooks

This library provides the basic functionality over which you can implement your own GitHub
webhook service. The webhook events that are currently supported are only `ping` and
`pull_request`. These two allow you to process the contents in a PR, write comments to
it or add a PR review.

To accomplish this you need to implement the `egithub_webhook` behavior, which requires a
single `handle_pull_request/3` callback. This function receives the GitHub's credentials
the PR data and the associated files.

By default, this library uses the GitHub's [PR reviews][pr_review] feature. So, once your
`handle_pull_request/3` implementation is done processing the PR it will have to return the
tuple `{ok, pr_review()}`, where `pr_review()` is a map with information regarding the
Review (e.g. *commit_id*, *body*, *event*, and *comments*).

In case you want to use individual comments in your PR, you will have to set the `review_style`
parameter within your application's config file to `individual_comments`, i.e:
```erlang
{egithub, [{review_style, individual_comments}]},
```

And then, your `handle_pull_request/3` will have to return the tuple `{ok, [message()]}`,
where `message()` is a map with information regarding the comment (e.g. *commit_id*, *path*, etc).

*Both, `pr_review/0` and `message/0` types are documented within the `egithub_webhook` module.*

To start the whole webhook flow once you receive the request from GitHub on your endpoint
you can call either `egithub_webhook:event/3` or `egithub_webhook:event/6`. The function
with arity 3 will just create the comments returned by your implementation. The second
function will also make calls to the [Statuses
API](https://developer.github.com/v3/repos/statuses/) and will report on the current
status of the webhook.

## GitHub's Rate Limits

If you use the GitHub API to create a lot of entities in a short interval, at a certain
point you will hit a limit on the rate of requests you can do. This is [sort of
documented](https://developer.github.com/v3/#abuse-rate-limits) in the API's
documentation, although there are no specifics on the amount of requests permitted or the
interval considered.

To work around this limitation, `egithub` has a built-in detection mechanism that handles
the case where an API call returns `403` after doing a valid requests. The request is
queued and retried after a certain amount of time, based on a fibonacci backoff time
series.

By default all API requests are done without this feature, which means you will get a
`403` if you start doing a lot of requests one after the other. If you want to use this
feature you need to provide the value `queue` for the option `post_method`. All functions
in the `egithub` module that accept an `Options` argument, accept this option.

For example, if you wanted to create a large number of comments on an issue, you could use
the `egithub:issue_comment/5` like this:

```erlang
Cred = egithub:basic_auth("username", "password"),
Repo = "username/reponame",
Issue = 1,
Comment = <<"Hello">>,
Options = #{post_method => queue},
egithub:issue_comment(Cred, Repo, Issue, Comment, Options).
```

This would create a comment with the text `Hello` for the issue `username/repo#1`.

API Documentation
=====

There is an automatically generated API documentation page [here][api_doc].

Example
=====

For an example on how to use this library you can check out
[this](https://github.com/inaka/inaka.github.io/tree/dev/script) little Erlang application
used to pull a JSON with all of Inaka's repositories in GitHub.

There is also a lot of code that calls the functions in `egithub` in the module
[`egithub_webhook`](https://github.com/inaka/erlang-github/blob/master/src/egithub_webhook.erl).

Contact Us
==========
If you find any **bugs** or have a **problem** while using this library, please [open an
issue](https://github.com/inaka/erlang-github/issues/new) in this repo (or a pull request
:)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)

[1]: https://developer.github.com/v3/
[pr_review]: https://developer.github.com/v3/pulls/reviews/
[api_doc]: https://hexdocs.pm/egithub/
