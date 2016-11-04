elm-app build
APP=$(basename $PWD)
mv dist ../docs/$APP
git reset
git add ../docs
git ci -m "Deploy $APP"
