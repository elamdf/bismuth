#!/bin/bash

set -e

if [[ -z "$1" || -z "$2" || -z "$3" || -z "$4" ]]; then
  echo "Usage: $0 feature target remote"
  exit 1
fi

function="$1"
feature="$2"
target="$3"
remote="$4"


repo_name=$(basename -s .git "$(git config --get remote.origin.url)")


# remove remote/ prefix if present
target=$(echo "$target" | sed 's/\// /')
feature=$(echo "$feature" | sed 's/\// /')


# Check that target exists remotely
if ! git ls-remote --exit-code --heads "$remote" "${target#"$remote/"}" > /dev/null; then
  echo "Error: Target branch '$target' not found on remote '$remote'."
  exit 1
fi

# create brain worktree

BRAIN_BASE_DIR="$HOME/.brains"
BRAIN_USER=$USER
brain_branch=$feature-brain-$BRAIN_USER
brain_dir=$BRAIN_BASE_DIR/$repo_name/$brain_branch

# if [[ -d "$brain_dir" ]]; then
#   echo "Error: Directory '$brain_dir' already exists."
#   exit 1
# fi



if [[ $function == "create" ]]; then
    mkdir -p $brain_dir
    git worktree add -b ${brain_branch} ${brain_dir} ${target}
    pushd $brain_dir
    git push origin HEAD:$brain_branch
    # brain files should not be manually modified
    chmod -w *    
    echo "brain for $feature created at $brain_dir "
elif [[ $function == "update" ]]; then
    # note that we're always updating feature to only be the rmote. TODO maybe should sync local via rsync so we don't have to commit all review comments. maybe that's the right way to do it though

    git fetch $remote $target
    git fetch $remote $feature
    
    chmod +w *    
    pushd $brain_dir

    # first, add to brain main since we're going to assume they're ok (since htey're in main)
    # TODO test this
    git diff -R $feature > feature_diff.BRAIN
    git diff -R $target > target_diff.BRAIN

    echo "auto-reviewing diffs from $target..."
    if [ -s "filename" ]; then
        git apply --cached target_diff.BRAIN
        git commit -m "auto-review changes from $target"
        git push origin HEAD:$brain_branch
    fi 
    
    echo "applying unreviewed diffs from $feature..."
    git apply --intent-to-add feature_diff.BRAIN
    # there are some like inverted staged changes after this, unstage them
    git reset

    # clean up
    rm *.BRAIN
    # brain files should not be manually modified
    chmod -w *    
    echo "Brain $brain_dir reviewing $feature to merge into $remote/$target updated! review away :D"
else
    echo "Command $function not found!"
fi

