
# Eager Words Application

EagerWords is the second version of the Azad Bolour's crossword game and
includes a number of enhancements on the first version (_boardgame_ project on
github). The major enhancement is a optional passwordless user authentication.
For logged-in users all games are saved. A game may be manually suspended, or it
may be automatically timed out and suspended after a period of inactivity. In
both cases, the game is saved for a logged-in user and can be resumed at a later
time.

Please note that in the current incomplete version, some documentation may be
lagging behind the code base.

## Versions

Version 0.9.0 - beta.

## The Game

Eagerwords in a board game is in the same genre as other well-known crossword games:
drag and drop letters to form words on a square board.

## Scope

The project  defines a board game API, and client and server implementations for it. The
precursor to EagerWords (the github _baordgame_ project) included two server
implementations of the API, one in Haskell and one in Scala. At present
EagerWords includes just a Scala implementation. While not yet in our road plan,
it is intended to port the original _boardgame_ Haskell server to some future
version of EagerWords (likely a subsequent version that implements a
multi-player game).

EagerWords is being developed on the MAC OS/X 10.9+, deployed on a Linux Amazon
EC2 instance, and accessed through modern browsers (including tablet browsers
but not on small-screen smarphones). There are currently no plans for native
iPhone of Android client as the smart phone screens are generally too small for
this particular  game to be played well on it with touch.

## Getting Started in Development

The steps needed to set up the development and deployment environments for the
application are generally scripted in the Dockerfiles used for packaging and
deployment of the application in the _docker_ directory of scala-server. 

To get started with development, follow the same steps listed in the Dockerfiles
on your development machine. You may start by consulting the README.md file in
in each sub-project and the corresponding docker directory, and then reviewing
the relevant Dockerfiles.

Currently the Scala server supports a file-based Sqlite database only.
In development - look for the file _eagerwords.db_ in the directory where you
run the scala application.

After cloning the repository:

* `cp branch.sh.template branch.sh` - this is the environment-setting script for
  development
* edit _branch.sh_ as appropriate for your minimal environment
* set your environment: `. branch.sh`

## Manual Testing

* Build and bring up the server: `cd scala-server; build.sh; run.sh`.

* Bring up the development server for the UI code locally: `cd eagerwords-web; npm install; npm start`

* Bring up `http://localhost:3000`. 

## Deployment

As of (11/1/2019) deployment scripts are being refactored to disentangle the 
static UI content from the backend application servers. The UI application 
is to be deployed independently to AWS CloudFront + S3. 

The backend docker deployment files and associated scripts remain in a 
state of flux and have not been tested.

TODO. Test updated deployment procedure and update documentation as 
appropriate.

## Dictionaries

Please refer to the README file in the dict/ directory. One issue to be
aware of is that diectionaries are preprocessed to a list of _masked words_, 
words with blank holes in them. Because masked word pre-processing is
time-consuming, the masked words file is saved in git, but in zipped form 
because it is too large for github. 

At this time, unzipping this file for the first time, and maintaining it in
case the dictionary is updated remains a manual step. Use the script
dict/unzip-masked-words.sh.

## Github Site

Under construction. (Will be at http://www.bolour.com/eagerwords/index.html).

## General Conventions

- For trivial code cleanup and unimportant changes with minimal impact to users,
  TODO comments are added at the right places in the source code. The backlog
  lists larger tasks or tasks that cannot yet be localized in the code base.

## Branches of Development

- master - stable version
- dev - development version - not necessarily stable - off master
- others - temporary feature branches

## Credits

Thanks to the folks at alleycat (https://alleycat.cc/) for assistance in React
development.

Thanks to Raccardo Fiorentino and Eric Vautier for support in UI design.

Thanks to Dennis Allard for assistance in testing, and for hosting 
an earlier version of the the application at oceanpark.com.

Thanks to the Moby project for the English dictionary:

    - http://www.gutenberg.org/files/3201/3201.txt
    - http://www.gutenberg.org/files/3201/files/CROSSWD.TXT.
 
## Contact

azadbolour

bolour.com

