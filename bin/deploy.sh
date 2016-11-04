if [ -d .git ]; then
  echo "deploy.sh needs to be run from within the app folder you want to deploy. E.g. cd first-app; ../bin/deploy.sh"
  exit 1
fi

elm-app build
APP=$(basename $PWD)
mv dist ../docs/$APP

git reset
git add ../docs
git ci -m "Deploy $APP"
