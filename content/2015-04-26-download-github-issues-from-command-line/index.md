+++
title = "How to Download GitHub Issues from the Command Line"
+++

A few days ago I got a warning in my GitHub news feed that my 2 year
[educational discount](https://education.github.com/discount_requests/new) was about to expire.
As each of my 5 private repos contained a sensitive project I was forced to take them down
before GitHub would make them public.
*(Update: I just figured out that it is possible to reapply for the student account!)*

[Bitbucket](https://bitbucket.com), however, supports unlimited private repos and so
I simply moved all of my repos there.
With their web interface it was just a matter of minutes to create all the repositories and push the code.

The GitHub issues, however, are another story.
As I want to prevent another vendor lock-in (who knows for how long Bitbucket will provide free private repos)
I've decided to store the issues in the repo itself from now on.
(How I plan to manage them will be the story of another post)

Human readability is a plus for issues.
Therefore we're going to use YAML instead of JSON for storage.
The data-structure will be an `./issues` directory with one YAML-file for each issue.

So how can we get the issues from GitHub into our flat-file database?

First of all we need an URL-endpoint where we can easily access the issues.
GitHub provides an extensive and well documented [API](https://developer.github.com/v3).

In order to list all issues from a repo we simply need to open the URL

```sh
https://api.github.com/repos/<username>/<repo>/issues
```

We can either use a browser or the command line HTTP client of our choice.
There are for example the industry standard [curl](http://curl.haxx.se) or in my opinion even better
[httpie](https://github.com/jakubroztocil/httpie) which has a beautiful output and is simpler to use.

Note that private repos can only be accessed from the command line and not via the browser
as it is necessary to authenticate.
For authentication we can either use basic auth or even better access tokens.
For now we will stick to basic auth to keep it simple.

Combined we get following command:

```sh
http -a <username> \
-b https://api.github.com/repos/<username>/<repo>/issues
```

Execute it by copying it into the terminal/command line of your choice and pressing enter.

Explanation:

- `http`: The command-name of httpie.
- `-a <username>`: We try to basic authenticate as "<username>".
  This will prompt use to enter the corresponding password.
- `-b`: Only download the body and not the header of the resource.
- `\`: Backslashes are used to escape the adjacent newline character so that we can split the command over several lines.
  This is just to increase readability.
  You can type the whole command simply in one line as well.

You might have noticed that this request only contains open issues.
To get all issues - including closed ones - we need to add the query string `?state=all` to the request-URL.

The next step is to extract the issues from the JSON array and to reformat them.
For this we will use the command line program [json](https://github.com/trentm/json).

```sh
http -a <username> \
-b https://api.github.com/repos/<username>/<repo>/issues?state=all \
| json -0 -a
```

Explanation:

- Get the issues just like described before
- Pipe them into json
- `-0` Remove all unnecessary whitespace (newlines, spaces, tabs) from the JSON
- `-a` reformat the JSON-array into a newline delimited stream of JSON events.
  See [JSON Lines](http://jsonlines.org) for more information.

Now we can simply loop over the lines and execute code for each line and therefore each issue.
In this example we simply print the issue again:

```sh
http -a <username> \
-b https://api.github.com/repos/<username>/<repo>/issues?state=all \
| json -0 -a \
| while read -r line; do echo $line ;done
```

Make sure to use the `-r` flag for the while loop to prevent special characters from being interpreted as commands.

Instead of just printing the issues we lastly want to convert them to YAML and save them to a file.

Make sure to create and go to the issues directory first - `mkdir issues && cd ./issues` - as the command
will save the issues in the present working directory.

For the filenames we are going to use the number of each issue.

In order to retrieve the issue-number we could extract it form the JSON and cache it in a variable.
It is, however, easier to simply sort the issues ascending by creation date and increment a counter for each issue.
For the conversion from JSON to YAML we use [js-yaml](https://github.com/nodeca/js-yaml).

Combined we get the final command:


```sh
count=0 http -a <username> -b \
https://api.github.com/repos/<username>/<repo>/issues?state=all\&sort=created\&direction=asc \
| json -0 -a \
| while read -r line; \
do (( count++ )); echo $line | js-yaml > $count.yaml; done
```

Note that it's necessary to escape `&` characters as they would get interpreted by the shell.
