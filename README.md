
# Eager Words Application

EagerWords is the second version of the Azad Bolour's crossword game and
includes a number of enhancements on the first version (_boardgame_ project on
github). The major enhancement is passwordless user authentication.
For logged-in users all games are saved. A game may be manually suspended, or it
may be automatically timed out and suspended after a period of inactivity. In
both cases, the game is saved for a logged-in user and can be resumed at a later
time.

Please note that in this ongoing project, some documentation may be lagging
behind the code base at any given time.

## Versions

Version 0.9.2 - beta.

## The Game

Eagerwords is a board game in the same genre as other well-known crossword games:
drag and drop letters to form words on a square board. It emphasizes capturing 
board teritory.

## Scope

The project defines a board game API, and client and server implementations for it. 
The client is a React web application. The server is a scala Play application.

EagerWords is being developed on the MAC OS/X 10.9+, deployed on AWS, and
accessed through modern browsers (including tablet browsers but not on
small-screen smarphones). There are currently no plans for native iPhone of
Android clients, as the smart phone screens are generally too small for this
particular game to be played well on with touch.

## Getting Started in Development

The steps needed to set up the development and deployment environments for the
application are generally scripted in the Dockerfiles and associated shell
scripts used for packaging and deployment of the client and server.

To get started with development, follow the same steps listed in the Dockerfiles
on your development machine. You may start by consulting the README.md file in
in each sub-project and the corresponding docker directory, and then reviewing
the relevant Dockerfiles.

The Scala server uses a file-based Sqlite database in development mode. The
sqlite database is housed in the file _eagerwords.db_ in the directory where you
run the scala application. To inspect it, use the sqlite client: sqlite3. The
deployed version of EagerWords runs in a docker container and uses an external
postgres database.

After cloning the repository:

* `cp branch.sh.template branch.sh` - this is the environment-setting script for
  development
* edit _branch.sh_ as appropriate for your minimal environment
* set your environment: `. branch.sh`

## Manual Testing

* Build the server: `cd scala-server; build.sh`.

* Run the server: set up required environment variables first 
  (see env.dev.sh and local.exports.sh.template), then `run.sh`.

* Bring up the development server for the UI code locally: `cd eagerwords-web; npm install; npm start`

* Use the development server: `http://localhost:3000`. 

* Or build the web application bundle and serve it: `build.sh; run.sh`

* Use the application bundle server: `http://localhost:5000`.

Note that for simplicity, the Scala Play server does not support https. Https is
handled in the deployment environment at a higher level.

## Deployment

EagerWords is deployed to AWS. Its static web content (developed in the
eagerwords\_web folder) uses an S3 bucket hidden behind an AWS CloudFront
distribution. The Scala Play backend (developed in the scala-server folder) is
deployed in a docker container. See the default application.conf, and the
specific prod.conf configuration files in the scala-server/conf directory for
detailed server-side configuration information. Secrets are provided to the
server by using environment variables at container run time.

## Dictionaries

Please refer to the README file in the dict/ directory. One issue to be
aware of is that diectionaries are preprocessed to a list of _masked words_, 
words with blank holes in them. Because masked word pre-processing is
time-consuming, the masked words file is saved in git, but in zipped form 
because it is too large for github. 

At this time, unzipping this file for the first time, and maintaining it in
case the dictionary is updated remains a manual step. Use the script
dict/unzip-masked-words.sh.

## General Conventions

- For trivial code cleanup and unimportant changes with minimal impact to users,
  TODO comments are added at the right places in the source code. The backlog
  lists larger tasks or tasks that cannot yet be localized in the code base.

## Branches of Development

- master - stable version
- dev - development version - not necessarily stable - off master
- others - temporary feature branches

## Credits

Thanks to 
[the folks at alleycat](https://alleycat.cc/) for assistance in React development, to 
[Riccardo Fiorentino](https://www.linkedin.com/in/riccardofiorentino/) and 
[Eric Vautier](https://twitter.com/ericvautier) for support in UI design, to
[Dennis Allard](https://oceanpark.com) for exploratory testing and for many usability tips, and to 
[Chris Richardson](https://www.chrisrichardson.net/) for advice on deployment strategy and scripting.

Thanks to the Moby project for the English dictionary:

    - http://www.gutenberg.org/files/3201/3201.txt
    - http://www.gutenberg.org/files/3201/files/CROSSWD.TXT.
 
## Contact

azadbolour

bolour.com

