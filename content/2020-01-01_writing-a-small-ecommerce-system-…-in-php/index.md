+++
title = "Writing a small eCommerce system … in PHP 😱"
draft = true
+++

A few days ago a friend approached me and asked me for help.
He has a small fashion brand and wanted to start selling
their products via the internet.
They already had been working with a developer, but now
he suddenly had disappeared and left them with a half finished
eCommerce system, which was already online, but did not work properly.
However, he was supposed to be a capable programmer with
a good education and therefore the code so far should be
a good basis to build upon.

… oh and it was built with PHP and MySQL served by Apache 😳

I guess this is the moment where I should have realized
that something's off.
A good programmer who uses PHP and MySQL.
Spot the mistake 😜.
Joking aside, as I hadn't been programming PHP for over 5 years
I was sure that PHP had matured in the meantime and was a
decent language to work with.
And it kind of is … if you use the latest version!

When I dug into the stack, however, I had to find out
that It was running in a shitty HostEurope webhosting
environment.
(Seriously, it's the worst webhoster UI and functionality I've ever seen.
Who the f*** is building such a piece of crap and - even worse - selling it?)
with MySQL 5.5.46 (2015-09-30) and PHP 5.4.45 (2015-09-03)
with both of them not having received any security updates
for 10 Months and with the PHP version even being officially discontinued
and advised against.

As this environment had already been used before and was therefore
not a decision by the developer I still hadn't given up on the project yet.

I simply spun up 2 Docker containers with the latest PHP (7.0.8)
and the latest MySQL version (5.7) and deployed them with Docker-Compose
on AWS Beanstalk.
Problem solved, simple as that.

All hope was lost, however, when I read the code for the admin login:

```php
$email = $_POST["email"];
$password = hash("sha256", $_POST["password"]);

$res = mysqli_query(
    $link,
    "SELECT * FROM admins WHERE email = '$email' AND password = '$password'"
);
```

What. The. Fuck.
Where had this guy received his CS degree?
Unescaped user input in SQL strings?
Single sha256 hashed passwords without salt?
At least he was using `mysqli_query` instead of `mysql_query`.

But seriously, this is horrible!
It just took me few minutes to hack the online version with an SQL injection.
And this website was supposed to handle sensitive user data!?

Unfortunately this was just the beginning.
To name a few of the countless fails:

- Invoices publicly accessible without authentication
- Countless snippets of duplicated code
- Mixture of german and english for identifiers
- Completely broken database schema with
    - Unnormalized tables
    - Separate tables for users & admins and order & preorders
    - Wrong data types all over the place
    - Inconsistent naming
- Committed source files instead of using a package manager like composer
- No tests
- No template engine
- Cobbled together SQL queries in PHP instead of using Views
- Wrong HTTP codes and verbs
- …

And not to mention the generally shitty code quality
of the PHP and the JavaScript code.

This was the point where I questioned if it even made sense trying to fix it.
So I checked out which e-commerce platforms and open-source tools
are freely available and could be leveraged.
Long story short: Nothing quite fit the use-case
(wrong payment provider, too complicated, too expensive, …).
I, however, built a list on GitHub for future reference: [awesome-e-commerce].
If you think I missed anything important feel free to send me a merge-request.

So in the end I decided to fix up the existing code,
although it was actually more of a rewrite.
It was fun, however, to get back to programming PHP.
Despite all the bad press I actually think it's
quite a nice tool to build websites.
(It's called Hypertext Preprocessor for a reason …)

You can see the current version here: [ecom-light]
Let me hear what you think of it!

But please bear with me.
As you can see in the TODOs list, there still quite a few things to fix
and I'm by no means a expert PHP programmer.
As I said, it's been 5 years since my last line of PHP code and
this was before I had any idea of how to program a website the right way™.

[awesome-e-commerce]: https://github.com/ad-si/awesome-e-commerce
[ecom-light]: https://github.com/ad-si/ecom-light
