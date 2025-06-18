                                        ; hook into magit status mode and check if the current branch ends in (concat "brain-" inline-cr-user). if so, enable brain-mode which does the following



                                        ; func: update_brain
                                        ; git diff HEAD target-branch | git apply --intent-to-add brain.diff
                                        ; git reset



                                        ; func: create brain (feature_branch target_branch)
                                        ; git fetch --all
                                        ; git checkout target-branch
                                        ; git checkout -b $feature_branch-$USER
                                        ; update_brain()




                                        ; func: push
                                        ; fetch all branches
                                        ; update brain
                                        ; git diff target-branch HEAD > review.patch
                                        ; patch_path = $(pwd)/review.patch
                                        ; if patch nonempty {
                                        ; tmpdir = mktemp -d
                                        ; git worktree add tmpdir feature-branch
                                        ; pushd $tmpdir
                                        ; git apply $patch_path --cached
                                        ; git commit -m "comments from $USER"
                                        ; git push origin feature-branch
                                        ; }


; 


; 

                                        ; replace magit push with func: push
                                        ; replace magit pull with fetch origin feature-branch; func: update brain










