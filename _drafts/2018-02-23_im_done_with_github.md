---
title: I'm done with GitHub
---

Disclaimer:
This post reflects my personal opinion and
is not affiliated with my company.


TODO: https://news.ycombinator.com/item?id=16544361
TODO:
  http://nerderati.com/2018/03/09/github-is-forcing-me-to-change-my-username/
  and https://news.ycombinator.com/item?id=16555451
TODO: https://www.voidlinux.eu/news/2018/05/serious-issues.html

I remember when I first joined GitHub 7 years ago.
It was awesome.
I was used to manage code via SVN on some privately hosted server and
it had been no fun, I can tell you that.
And suddenly I had this great interface where I could manage my code, create
pull requests and - best of all - check out the projects of thousands
of other developers.
GitHub was the first page I checked in the morning and the last
one before I went to bed.
Who wants to know what a distant accquaintance had for dinner?
It's much more exciting to see what potentially world changing software
a friend from work or some famous developer just released.
It was not just a place to work, but for news and social interactions.

Lately, however, I've become more and more dissatisfied with GitHub.


## Lack of Transparency

This realization first started
when the [open letter to GitHub][letter] was released.

It became clear that GitHub just completely ignored any support requests
by developers and organizations.
And while many of those devs and orgs
used GitHub to publically interact with their users,
to share code, to track problems and feature requests
via issues and just to become more transparent and build trust,
GitHub itself did nothing like this.
There was no visibility into what happened with the requests,
if they were even received, let alone if GitHub was working on them.

Isaac Z. Schlueter, the inventor and CEO of npm, tried to fill the void
with a [repo dedicated for GitHub issues][github-issues],
but of course it was ignored by GitHub and I've never seen any official
comment to it, despite the thousands of people who liked and contributed to it.


Worst hypocrites ever!

https://help.github.com/articles/dmca-takedown-policy/

> We believe that transparency is a virtue


https://help.github.com/articles/github-terms-of-service/#3-github-may-terminate

> 3. GitHub May Terminate
>
> GitHub has the right to suspend or terminate your access to all
> or any part of the Website at any time,
> with or without cause, with or without notice, effective immediately.
> GitHub reserves the right to refuse service to anyone
> for any reason at any time.


Read this and think of the worst case scenario.

https://help.github.com/articles/github-terms-of-service/#o-disclaimer-of-warranties

> O. Disclaimer of Warranties
>
> Short version: We provide our service as is,
> and we make no promises or guarantees about this service.
> Please read this section carefully; you should understand what to expect.
>
> GitHub provides the Website and the Service “as is” and “as available,”
> without warranty of any kind. Without limiting this,
> we expressly disclaim all warranties, whether express, implied or statutory,
> regarding the Website and the Service including without limitation
> any warranty of merchantability,
> fitness for a particular purpose, title,
> security, accuracy and non-infringement.
>
> GitHub does not warrant that the Service will meet your requirements;
> that the Service will be uninterrupted,
> timely, secure, or error-free;
> that the information provided through the Service is accurate,
> reliable or correct;
> that any defects or errors will be corrected;
> that the Service will be available at any particular time or location;
> or that the Service is free of viruses or other harmful components.
> You assume full responsibility and risk of loss
> resulting from your downloading and/or use of files, information,
> content or other material obtained from the Service.


Big coporations can't be trusted.
If it is for some nice-to-have additional service, no problem,
but if it's at the core of your busines, look for alternatives.


## Lack of / Badly implemented Features

After GitHub realized they just couldn't ignore the open letter,
they made a half hearted commitment to improve,
but the output was rather neglectible.
They added support for issue-/pr-templates, but that's about it.

Overall, the list of missing must-have-features is way too long
to list it in this post.

Just check out the comparison by GitLab https://about.gitlab.com/comparison
for an in-depth overview.

On the other hand, there are badly implemented features.

They tried to quick fix several things,
but the result was less than stellar.
E.g. projects are a really clumsy attempt to support a proper kanban board.
A repo can have several projects,
but a project can only be part of one repo (WTF).
Instead of reusing labels to specify a board they introduce a special board tag.
Summed up it's all very confusing and ill-conceived.
That's why I just completely ignore projects and disable them
for all my repos.

Again, others realized the drawbacks too and stepped in.
[ZenHub], [Waffle], [Huboard] and [Screenful] all provide
project management tools ontop of GitHub issues and pull requests.
And they are essential and should actually already be provided by GitHub.
Again, the competitors are much better at this.

[Huboard]: https://huboard.com
[Screenful]: http://screenful.com
[Waffle]: https://waffle.io
[ZenHub]: https://zenhub.com


## Exercising Power

Now to the craziest part.
GitHub can simply deactivate / delete or change your account in any way
they want to.
And they do.

For example a friend of mine once accidentally published
some source files from a proprietary graphics engine.
And just a few hours later GitHub deactivated his complete account.
Just like that.
This might not sound like a big problem as they later reactivated
it as things cleared up, but if your account gets deactivated this happens:

- 404 for public profile
- 404 for any owned repo
- 404 for any GitHub page

and worst of all

- No more access to any pull requests
- No more access to any issues


That's just completely crazy there.


[github-issues]: https://github.com/isaacs/github
[letter]: https://github.com/dear-github/dear-github


It's a little ironic that I'm writing this article while wearing
my GitHub T-shirt.

