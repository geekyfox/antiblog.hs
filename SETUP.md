### Disclaimer

I've wrote this website engine for my own usage. This is why it was never
specifically meant to be intuitively simple to deploy, use and modify by
people other than myself. Any kind of backwards compatibility between
this version of engine and any future ones is also not guaranteed.

### Setup guide

1. Install dependencies:
 * postgresql 9.3 or higher (should work on earlier versions too, but wasn't ever tested)
 * ghc 7.6 or higher
 * cabal-install 1.18 or higher
 * nginx (optional, see below)
 * supervisord (optional, see below)

2. Create database
  
 As postgres user:
 ```
$ sudo -u postgres psql
create role 'antiblog' identified by 'password';
create database 'antiblog';
grant all privileges on database 'antiblog' to 'antiblog';
\q
exit
 ```
 If you care about security, pick safer password than just 'password'.
 And also configure your firewall to restrict external access to port 5432.

3. Install package

 ```
 $ mkdir ~/antiblog
 $ cd ~/antiblog
 $ wget https://github.com/geekyfox/antiblog/releases/download/0.2.0.0/antiblog-0.2.0.0.tar.gz
 $ cabal sandbox init
 $ cabal update
 $ cabal install antiblog-0.2.0.0.tar.gz
 ```

4. Configure web server

 ```
 $ .cabal-sandbox/bin/antiwork add-config
 Alias [antiblog]:                                 # Name for section in ~/.antiblog.conf file
 URL suffix []:                                    # Leave empty unless you do URL rewrites
 API key [257c4519bfcdeac8599839ea1eaf619d]:
 HTTP port [8080]:
 Database host [localhost]:
 Database port [5432]:
 Database username [antiblog]:                     # These are parameters you've set up on step #2
 Database password [password]:                     #
 Database name [antiblog]:                         #
 Antiblog title [The Antiblog]: 
 Has "by <name>" in page header [yes]: 
 Author name: Your Name
 Author URL: yourdomain.net 
 Has "Powered by The Antiblog" in page header [yes]: 
 Has "micro" tag for short posts [yes]: 
 ```

 Configuration is put into `~/.antiblog.conf`, just edit it directly if you need to change anything later.

5. Start web server

 ```
 $ .cabal-sandbox/bin/antiblog antiblog
 Setting phasers to stun... (port 8080) (ctrl-c to quit)
 ```

6. Setup `antisync` tool.

 ```
 $ .cabal-sandbox/bin/antisync add-remote
 Alias [antiblog]: 
 Remote URL: http://localhost:8080            # Full address including http://
 API key: 257c4519bfcdeac8599839ea1eaf619d    # Should be same as one setup in server
 ```

 Configuration is put into `~/.antisync.conf`, just edit it directly if you need to change anything later.
 
7. Test the posting.

 Create `sample.txt` file with contents:
 ```
## antiblog publish
## antiblog title My first post
## antiblog tags test
Hello, this is my first antiblog post.
 ```
 and upload it via `.cabal-sandbox/bin/antisync sync antiblog sample.txt`

 Then check that it shows up on the website.
 
 I haven't yet wrote a proper reference of the markup language it uses, but
 [sources of the parser](https://github.com/geekyfox/antiblog/blob/master/src/Antisync/Parser.hs) should give you
 general idea.

8. Setup supervisord (optional but recommended)

  For development/testing sandbox it doesn't really matter, but if you want to run an antiblog on a public
  server, you'd want to make sure it runs continuously, restarts after failure, puts logs into a dedicated
  location etc. `antiblog` doesn't bother about any of that and assumes it's done by external launcher.
  
  I use [supervisord](http://supervisord.org/) for that purpose, it's quite nice and simple enough to be
  used by not-really-sysadmin person like me. Just go to their website and read the manuals,
  they're pretty good.
  
  If you prefer any alternative technology (may it be `monit` or `daemonize` or `launchd` or `systemd` or
  anything), that should work too.
  
9. Setup nginx (optional but recommended)

  [Official beginners' guide](http://nginx.org/en/docs/beginners_guide.html#proxy) is a good starting point.

10. Enable rotation of the entries (optional)

  Add `~/antibog/.cabal-sandbox/bin/antiwork antiblog rotate` to `crontab`.

11. Please drop [me](mailto:ivan.appel@gmail.com) an email about how it went :-)
