### Disclaimer

I've wrote this website engine for my own usage. This is why it was never
specifically meant to be intuitively simple to deploy, use and modify by
people other than myself. Any kind of backwards compatibility between
this version of engine and any future ones is also not guaranteed.

### Environment implications

Webapp design makes several assumptions about the surrounding system.
If you install it on a clean installation of a recent version of Linux, none of
them should be a problem. If you want to squeeze it into a system that is
legacy, outdated and/or non-Linux, you'd have to figure out the solutions by
yourself.

* Neither webapp nor `antisync` utility are specially bound to be used with
  Linux, but I've never tested either of them under anything except Ubuntu. 
  Less your OS resembles Ubuntu, more problems you may potentially get: other
  Linux should be ok, *BSD is unlikely to be problematic too; with OSX I guess
  it depends, with Windows it depends even more.
* It is designed to run behind nginx or a similar reverse proxy.
  * That's why there is "`serverPort`" setting in configuration file, which is
     the TCP port that proxy should connect to.
  * That's why configuration file has "`baseUrl`" settings. Since URL may be
     rewritten somewhere between the browser and the webapp, care is taken of
     "unrewriting it back."
  * That's why webapp doesn't bother about serving static content.
* It is designed to run on top of PostgreSQL 9.3+.
  * That's why there's nothing like a "database-agnostic layer."
* It is designed to be controlled by supervisord.
  * That gives the liberty of not worrying that webapp process may
     ocassionally terminate (restart is automatic and takes less than a
     second).
* Paths to configuration files are hardcoded. To be specific: 
 * webapp looks for
  config at `~/antiblog/config.json`
 * `antisync` without `--prod` flag reads
  `~/antiblog/config-dev.json` and 
 * `antisync --prod` assumes it is
  `~/antiblog/config-prod.json`.
  
  To change these bindings, patch `src/Config.hs`.

Using an alternative reverse proxy or process supervisor shouldn't be
hard, consult the documentation to your tool of choice.

Using pre-9.3 postgres is possible if you workaround the lack of materialized
views support.

* Simple hotfix for that is to replace "`CREATE MATERIALIZED VIEW`"
 with  "`CREATE VIEW`" in `schema/views.sql` and comment out
 "`REFRESH MATERIALIZED VIEW ...`"  lines in `schema/functions.sql`.
 That'd come with a performance penalty though.

* Alternative is to replace materialized views with tables and implement their refreshes
 manually in `refresh_materialized_views()` function. This shouldn't be very
 complicated, but I'd still suggest you to consider installing a newer version
 of postgres.

Running with no proxy, or no supervisor, or on non-postgres database, is
theoretically possible if you have enough determination to fill all gaps
and fix all breaks ;-)

### Setup guide

1. Install postgresql, nginx, supervisord, ghc, cabal-install.

 postgres must be 9.3+ (materialized views support; see above),
 cabal-install must be 1.18+ (sandboxes support), no special version
 requirements for other components.

2. Create database
  
 As postgres user:
 ```
psql
create role 'antiblog' identified by 'password';
create database 'antiblog';
\q
exit
 ```
 As yourself, edit `~/.pgpass` file. Add a line
 ```
localhost:5432:antiblog:antiblog:password
 ```
 If you care about security, pick safer password than just 'password'.
 And also configure your firewall to restrict external access to port 5432.

3. Setup nginx

 As root create `/etc/nginx/sites-enabled/antiblog` file that contains
 ```
server {
    listen 80;
    server_name <Your hostname>;
    
    location /static {
        alias <Your home directory>/antiblog/static;
    }

    location / {
        proxy_pass http://localhost:8080;
    }
}
 ```

 Then restart nginx.

4. Get the source.
 ```
cd ~
git clone https://github.com/geekyfox/antiblog.git
```

5. Prepare config files

 Server config `~/antiblog/config.json`:
 ```json 
{
    "url"      : "http://hostname/subdirectory",
    "apiKey"   : "secure_magic_constant",
    "httpPort" : 3000,
    "dbConn"   : "host='localhost' port=5432 user=antiblog dbname='antiblog' password='password'"
}
 ```

 `url` is your antiblog's root URL. `apiKey` is an arbitrary string
 (`./scripts/make_api_key.py` can make one for you). `httpPort` is the
 port number that you've set in nginx config. `password` in `dbConn` is
 the password that you've set when created the role.

 Client config `~/antiblog/config-dev.json`:
 ```json
{
    "name"     : "dev",
    "url"      : "http://hostname/subdirectory",
    "apiKey"   : "secure_magic_constant"
}
 ```
 `url` and `apiKey` are same as in server config.

6. Blame me for not automating the tasks above. Then thank me for not asking
you to run some ad-hoc scripts as root on your system.

7. Edit `src/Layout.hs` and replace my name with yours. Otherwise it'd look
silly.

8. Finally, build and run.
 ```
cd antiblog
cabal sandbox init
make install
 ```

9. Open the website in the browser and check that it really displays an
empty antiblog.

10. Test the posting.

 Create sample.txt file with contents:
 ```
## antiblog publish
## antiblog title My first post
## antiblog tags test
Hello, this is my first antiblog post.
 ```

 and upload it via `antisync sync sample.txt`

 Then check that it shows up on the website.

11. Add `scripts/daily_update.sh` to `crontab` to enable rotation of entries.

Now, you have all you need to flex the right side of your brain by writing
the smart thoughts as well as to exercise the left side by customizing the
website to your needs.

Enjoy!
