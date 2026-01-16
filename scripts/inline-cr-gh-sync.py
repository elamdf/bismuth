#!/usr/bin/env python3
import argparse
import json
import os
import re
import subprocess
import sys

HEADER_RE = re.compile(
    r"^(?P<prefix>\s*(?:(?://|;|#)\s*)*)"
    r"(?P<tag>\[#gh:(?P<ghid>\d+)\]\s*)?>\s*(?P<kind>N?X?CR)\s+"
    r"(?P<reviewer>[^ ]+)\s+for\s+(?P<author>[^:]+):\s*(?P<body>.*?)"
    r"(?P<trailing>\s+\[#gh:(?P<ghid2>\d+)\])?\s*$"
)
REPLY_RE = re.compile(
    r"^(?P<prefix>\s*(?:(?://|;|#)\s*)*)"
    r"(?P<tag>\[#gh:(?P<ghid>\d+)\]\s*)?>\s*(?:(?P<speaker>[^:]+):\s*)?"
    r"(?P<body>.*?)"
    r"(?P<trailing>\s+\[#gh:(?P<ghid2>\d+)\])?\s*$"
)


class GhCommandError(RuntimeError):
    def __init__(self, cmd, stdout, stderr, returncode):
        super().__init__(cmd)
        self.cmd = cmd
        self.stdout = stdout
        self.stderr = stderr
        self.returncode = returncode


def run(cmd, input_data=None, cwd=None):
    result = subprocess.run(
        cmd,
        input=input_data,
        text=True,
        capture_output=True,
        check=False,
        cwd=cwd,
    )
    if result.returncode != 0:
        raise GhCommandError(
            " ".join(cmd),
            result.stdout,
            result.stderr,
            result.returncode,
        )
    return result.stdout


def gh_json(args, input_data=None):
    try:
        output = run(["gh"] + args, input_data=input_data)
    except GhCommandError as err:
        raise RuntimeError(
            "gh command failed:\n  cmd: {}\n  status: {}\n  stdout: {}\n  stderr: {}".format(
                err.cmd,
                err.returncode,
                (err.stdout or "").strip(),
                (err.stderr or "").strip(),
            )
        ) from None
    return json.loads(output) if output.strip() else {}


def get_repo_name(repo_arg):
    if repo_arg:
        return repo_arg
    data = gh_json(["repo", "view", "--json", "nameWithOwner"])
    return data["nameWithOwner"]


def split_repo(repo):
    if "/" not in repo:
        raise ValueError("Expected repo in owner/name format, got: {}".format(repo))
    owner, name = repo.split("/", 1)
    return owner, name


def git_root():
    return run(["git", "rev-parse", "--show-toplevel"]).strip()


def git_push(cwd):
    return run(["git", "push"], cwd=cwd)


def get_pr_info(repo, pr_number):
    data = gh_json(
        [
            "pr",
            "view",
            str(pr_number),
            "--repo",
            repo,
            "--json",
            "author,headRefOid,files",
        ]
    )
    return {
        "author": data["author"]["login"] if data.get("author") else None,
        "head_sha": data["headRefOid"],
        "files": [f["path"] for f in data.get("files") or []],
    }


def graphql_review_threads(owner, name, pr_number):
    query = """
query($owner: String!, $name: String!, $number: Int!, $cursor: String) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      reviewThreads(first: 100, after: $cursor) {
        nodes {
          id
          path
          line
          originalLine
          diffSide
          isResolved
          comments(first: 100) {
            nodes {
              databaseId
              body
              author { login }
              createdAt
              updatedAt
            }
          }
        }
        pageInfo { hasNextPage endCursor }
      }
    }
  }
}
"""
    threads = []
    cursor = None
    while True:
        args = [
            "api",
            "graphql",
            "-f",
            "query={}".format(query),
            "-F",
            "owner={}".format(owner),
            "-F",
            "name={}".format(name),
            "-F",
            "number={}".format(pr_number),
        ]
        if cursor:
            args += ["-F", "cursor={}".format(cursor)]
        data = gh_json(args)
        pr = data["data"]["repository"]["pullRequest"]
        page = pr["reviewThreads"]
        threads.extend(page["nodes"])
        if not page["pageInfo"]["hasNextPage"]:
            break
        cursor = page["pageInfo"]["endCursor"]
    return threads


def squash_body(body):
    return " ".join(body.replace("\r\n", "\n").replace("\n", " ").split())


def default_comment_prefix(path):
    ext = os.path.splitext(path)[1].lower()
    if ext in {".el", ".lisp", ".scm"}:
        return "; "
    if ext in {
        ".c",
        ".cc",
        ".cpp",
        ".h",
        ".hpp",
        ".js",
        ".ts",
        ".tsx",
        ".java",
        ".go",
        ".rs",
        ".swift",
        ".kt",
        ".m",
        ".mm",
    }:
        return "// "
    if ext in {"py", "toml"}:
        return "# "
    return ""


def format_header(prefix, kind, reviewer, author, body, ghid=None, newline="\n"):
    tag = "[#gh:{}] ".format(ghid) if ghid else ""
    line = "{}{}> {} {} for {}: {}".format(prefix, tag, kind, reviewer, author, body)
    return line + newline


def format_reply(prefix, speaker, body, ghid=None, newline="\n"):
    tag = "[#gh:{}] ".format(ghid) if ghid else ""
    if speaker:
        line = "{}{}> {}: {}".format(prefix, tag, speaker, body)
    else:
        line = "{}{}> {}".format(prefix, tag, body)
    return line + newline


def parse_inline_threads(lines):
    threads = []
    i = 0
    while i < len(lines):
        m = HEADER_RE.match(lines[i])
        if not m:
            i += 1
            continue
        start = i
        prefix = m.group("prefix") or ""
        comments = [
            {
                "kind": m.group("kind"),
                "reviewer": m.group("reviewer"),
                "author": m.group("author"),
                "speaker": None,
                "body": m.group("body") or "",
                "ghid": (
                    int(m.group("ghid") or m.group("ghid2"))
                    if (m.group("ghid") or m.group("ghid2"))
                    else None
                ),
                "line_index": i,
                "is_header": True,
            }
        ]
        i += 1
        while i < len(lines):
            if HEADER_RE.match(lines[i]):
                break
            m2 = REPLY_RE.match(lines[i])
            if not m2:
                break
            comments.append(
                {
                    "kind": None,
                    "reviewer": None,
                    "author": None,
                    "speaker": (m2.group("speaker") or "").strip() or None,
                    "body": m2.group("body") or "",
                    "ghid": (
                        int(m2.group("ghid") or m2.group("ghid2"))
                        if (m2.group("ghid") or m2.group("ghid2"))
                        else None
                    ),
                    "line_index": i,
                    "is_header": False,
                }
            )
            i += 1
        threads.append(
            {
                "start": start,
                "end": i,
                "prefix": prefix,
                "comments": comments,
            }
        )
    return threads


def load_file_lines(path):
    with open(path, "r", encoding="utf-8") as f:
        return f.read().splitlines(keepends=True)


def write_file_lines(path, lines):
    with open(path, "w", encoding="utf-8") as f:
        f.write("".join(lines))


def gh_create_review_comment(
    repo, pr_number, body, path, line, side, commit_id, in_reply_to=None
):
    args = [
        "api",
        "-X",
        "POST",
        "repos/{}/pulls/{}/comments".format(repo, pr_number),
        "-f",
        "body={}".format(body),
        "-f",
        "path={}".format(path),
        "-F",
        "line={}".format(line),
        "-f",
        "side={}".format(side),
        "-f",
        "commit_id={}".format(commit_id),
    ]
    if in_reply_to:
        args += ["-F", "in_reply_to={}".format(in_reply_to)]
    try:
        return gh_json(args)
    except RuntimeError as err:
        raise RuntimeError(
            "Failed to create review comment for {}:{} (side {}, commit {}).\n"
            "This usually means the line no longer exists in the PR diff or the commit SHA is stale.\n"
            "Details:\n{}".format(path, line, side, commit_id, err)
        ) from None


def gh_update_review_comment(repo, comment_id, body):
    return gh_json(
        [
            "api",
            "-X",
            "PATCH",
            "repos/{}/pulls/comments/{}".format(repo, comment_id),
            "-f",
            "body={}".format(body),
        ]
    )


def gh_resolve_review_thread(thread_id):
    query = """
mutation($threadId: ID!) {
  resolveReviewThread(input: {threadId: $threadId}) {
    thread {
      isResolved
    }
  }
}
"""
    gh_json(
        [
            "api",
            "graphql",
            "-f",
            "query={}".format(query),
            "-F",
            "threadId={}".format(thread_id),
        ]
    )


def build_inline_index(file_lines_by_path):
    inline_threads_by_path = {}
    ghid_to_inline = {}
    for path, lines in file_lines_by_path.items():
        threads = parse_inline_threads(lines)
        inline_threads_by_path[path] = threads
        for thread in threads:
            for comment in thread["comments"]:
                if comment["ghid"]:
                    ghid_to_inline[comment["ghid"]] = {
                        "path": path,
                        "thread": thread,
                        "comment": comment,
                    }
    return inline_threads_by_path, ghid_to_inline


def normalize_inline_body(text):
    return text.strip()


def normalize_gh_body(text):
    return squash_body(text)


def desired_header_kind(last_author, reviewer, author, fallback_kind="CR"):
    if last_author and reviewer and last_author == reviewer:
        return "CR"
    if last_author and author and last_author == author:
        return "XCR"
    return fallback_kind


def inactive_kind(kind):
    if kind.startswith("N"):
        return kind
    return "N{}".format(kind)


def is_self_cr(header_comment, current_user):
    if not header_comment or not current_user:
        return False
    if header_comment.get("kind") not in {"CR", "XCR", "NCR", "NXCR"}:
        return False
    return (
        header_comment.get("reviewer") == current_user
        and header_comment.get("author") == current_user
    )


def apply_inline_updates(path, lines, updates, insertions):
    for line_index, new_line in updates.items():
        lines[line_index] = new_line
    for insert_after, new_lines in sorted(insertions, key=lambda x: x[0], reverse=True):
        pos = min(max(insert_after, 0), len(lines))
        lines[pos:pos] = new_lines
    write_file_lines(path, lines)


def main():
    parser = argparse.ArgumentParser(
        description="Sync GitHub PR review comments with inline-cr threads."
    )
    parser.add_argument("--pr", type=int, required=True, help="Pull request number.")
    parser.add_argument("--repo", help="Repo in owner/name format (default from gh).")
    parser.add_argument(
        "--mode",
        choices=["both", "inline-to-gh", "gh-to-inline"],
        default="both",
        help="Sync direction.",
    )
    parser.add_argument(
        "--prefer",
        choices=["inline", "github"],
        default="inline",
        help="When content differs, prefer inline or GitHub.",
    )
    parser.add_argument(
        "--comment-prefix",
        help="Override comment prefix for new inline threads (ex: '// ').",
    )
    parser.add_argument(
        "--pull-only",
        action="store_true",
        help="Only pull from GitHub into inline-cr threads.",
    )
    parser.add_argument(
        "--auto-push",
        action="store_true",
        help="Push after sync.",
    )
    parser.add_argument("--dry-run", action="store_true", help="Report actions only.")
    args = parser.parse_args()

    if args.pull_only:
        args.mode = "gh-to-inline"
        if args.prefer == "inline":
            args.prefer = "github"

    current_user = os.getenv("USER") or os.getenv("LOGNAME")
    repo = get_repo_name(args.repo)
    owner, name = split_repo(repo)
    pr_info = get_pr_info(repo, args.pr)
    pr_author = pr_info["author"] or "author"
    head_sha = pr_info["head_sha"]
    paths = pr_info["files"]

    file_lines_by_path = {}
    for path in paths:
        if not os.path.exists(path):
            continue
        file_lines_by_path[path] = load_file_lines(path)

    inline_threads_by_path, ghid_to_inline = build_inline_index(file_lines_by_path)

    gh_threads = graphql_review_threads(owner, name, args.pr)
    gh_comments = {}
    gh_comment_to_thread_id = {}
    gh_threads_by_path = {}
    for thread in gh_threads:
        path = thread["path"]
        comments = thread.get("comments", {}).get("nodes") or []
        if not comments:
            continue
        line = thread.get("line") or thread.get("originalLine") or 1
        side = thread.get("diffSide") or "RIGHT"
        gh_threads_by_path.setdefault(path, []).append(
            {
                "id": thread.get("id"),
                "isResolved": thread.get("isResolved"),
                "line": line,
                "side": side,
                "comments": comments,
            }
        )
        for comment in comments:
            gh_comments[comment["databaseId"]] = comment
            if thread.get("id"):
                gh_comment_to_thread_id[comment["databaseId"]] = thread.get("id")

    updates_by_path = {path: {} for path in file_lines_by_path}
    insertions_by_path = {path: [] for path in file_lines_by_path}

    if args.mode in {"both", "inline-to-gh"}:
        for path, inline_threads in inline_threads_by_path.items():
            for thread in inline_threads:
                header_comment = thread["comments"][0] if thread["comments"] else None
                if is_self_cr(header_comment, current_user):
                    continue
                if header_comment and header_comment.get("kind", "").startswith("N"):
                    header_ghid = header_comment.get("ghid")
                    thread_id = gh_comment_to_thread_id.get(header_ghid)
                    if thread_id:
                        if args.dry_run:
                            print("resolve: gh thread {}".format(thread_id))
                        else:
                            gh_resolve_review_thread(thread_id)
                            print("resolved: gh thread {}".format(thread_id))
                for comment in thread["comments"]:
                    ghid = comment["ghid"]
                    line_index = comment["line_index"]
                    line = file_lines_by_path[path][line_index]
                    newline = "\n" if not line.endswith("\n") else ""
                    inline_body = normalize_inline_body(comment["body"])
                    if ghid:
                        if ghid not in gh_comments:
                            print(
                                "warn: inline comment {} not found on GitHub".format(
                                    ghid
                                )
                            )
                            continue
                        gh_body = normalize_gh_body(gh_comments[ghid]["body"])
                        if inline_body != gh_body and args.prefer == "inline":
                            print("update: gh comment {} from inline".format(ghid))
                            if not args.dry_run:
                                gh_update_review_comment(repo, ghid, inline_body)
                        continue

                    if not head_sha:
                        print("skip: missing head sha, cannot create comments")
                        continue
                    target_line = line_index + 1
                    prefix = (
                        thread["prefix"]
                        or args.comment_prefix
                        or default_comment_prefix(path)
                    )
                    body = inline_body
                    if comment["is_header"]:
                        if args.dry_run:
                            print(
                                "create: gh review comment for {}:{}: {}".format(
                                    path, target_line, body
                                )
                            )
                            continue
                        created = gh_create_review_comment(
                            repo=repo,
                            pr_number=args.pr,
                            body=body,
                            path=path,
                            line=target_line,
                            side="RIGHT",
                            commit_id=head_sha,
                        )
                        new_id = created.get("id") or created.get("databaseId")
                        if not new_id:
                            print("warn: failed to parse gh id for new comment")
                            continue
                        new_line = format_header(
                            prefix,
                            comment["kind"],
                            comment["reviewer"],
                            comment["author"],
                            comment["body"],
                            ghid=new_id,
                            newline=newline or "\n",
                        )
                        updates_by_path[path][line_index] = new_line
                    else:
                        header = thread["comments"][0]
                        if not header.get("ghid"):
                            continue
                        if args.dry_run:
                            print("create: gh reply to {}".format(header["ghid"]))
                            continue
                        created = gh_create_review_comment(
                            repo=repo,
                            pr_number=args.pr,
                            body=body,
                            path=path,
                            line=target_line,
                            side="RIGHT",
                            commit_id=head_sha,
                            in_reply_to=header["ghid"],
                        )
                        new_id = created.get("id") or created.get("databaseId")
                        if not new_id:
                            print("warn: failed to parse gh id for new reply")
                            continue
                        new_line = format_reply(
                            prefix,
                            comment["speaker"],
                            comment["body"],
                            ghid=new_id,
                            newline=newline or "\n",
                        )
                        updates_by_path[path][line_index] = new_line

        threads_to_resolve = []
        for gh_thread in gh_threads:
            path = gh_thread["path"]
            if path not in file_lines_by_path:
                continue
            comments = gh_thread.get("comments", {}).get("nodes") or []
            if not comments:
                continue
            if any(comment["databaseId"] in ghid_to_inline for comment in comments):
                continue
            if gh_thread.get("isResolved"):
                continue
            thread_id = gh_thread.get("id")
            if thread_id:
                threads_to_resolve.append(thread_id)

        for thread_id in threads_to_resolve:
            if args.dry_run:
                print("resolve: gh thread {}".format(thread_id))
            else:
                gh_resolve_review_thread(thread_id)
                print("resolved: gh thread {}".format(thread_id))

    if args.mode in {"both", "gh-to-inline"}:
        for path, thread_list in gh_threads_by_path.items():
            if path not in file_lines_by_path:
                print("warn: gh path missing locally: {}".format(path))
                continue
            lines = file_lines_by_path[path]
            for gh_thread in thread_list:
                gh_comments_list = gh_thread["comments"]
                existing_thread = None
                for comment in gh_comments_list:
                    ghid = comment["databaseId"]
                    if ghid in ghid_to_inline:
                        existing_thread = ghid_to_inline[ghid]["thread"]
                        break
                if existing_thread:
                    prefix = (
                        existing_thread["prefix"]
                        or args.comment_prefix
                        or default_comment_prefix(path)
                    )
                    header_inline = existing_thread["comments"][0]
                    if is_self_cr(header_inline, current_user):
                        continue
                    header_line_index = header_inline["line_index"]
                    header_original_line = lines[header_line_index]
                    header_newline = (
                        "\n" if not header_original_line.endswith("\n") else ""
                    )
                    reviewer = header_inline["reviewer"]
                    author = header_inline["author"] or pr_author
                    last_author = None
                    header_gh = gh_comments_list[0] if gh_comments_list else None
                    if gh_comments_list:
                        last = gh_comments_list[-1]
                        if last.get("author"):
                            last_author = last["author"]["login"]
                    desired_kind = desired_header_kind(
                        last_author,
                        reviewer,
                        author,
                        fallback_kind=header_inline["kind"] or "CR",
                    )
                    if gh_thread.get("isResolved"):
                        desired_kind = inactive_kind(desired_kind)
                    header_ghid = header_inline["ghid"]
                    if not header_ghid and header_gh:
                        header_ghid = header_gh.get("databaseId")
                    header_body = header_inline["body"]
                    header_body_changed = False
                    if args.prefer == "github" and header_gh:
                        gh_body = normalize_gh_body(header_gh["body"])
                        if gh_body != normalize_inline_body(header_body):
                            header_body = gh_body
                            header_body_changed = True
                    insert_after = existing_thread["end"]
                    new_lines = []
                    for comment in gh_comments_list:
                        ghid = comment["databaseId"]
                        if ghid in ghid_to_inline:
                            inline_comment = ghid_to_inline[ghid]["comment"]
                            if args.prefer == "github":
                                gh_body = normalize_gh_body(comment["body"])
                                inline_body = normalize_inline_body(
                                    inline_comment["body"]
                                )
                                if gh_body != inline_body:
                                    line_index = inline_comment["line_index"]
                                    original_line = lines[line_index]
                                    newline = (
                                        "\n" if not original_line.endswith("\n") else ""
                                    )
                                    if inline_comment["is_header"]:
                                        new_line = format_header(
                                            prefix,
                                            desired_kind,
                                            reviewer,
                                            author,
                                            gh_body,
                                            ghid=ghid,
                                            newline=newline or "\n",
                                        )
                                    else:
                                        new_line = format_reply(
                                            prefix,
                                            inline_comment["speaker"],
                                            gh_body,
                                            ghid=ghid,
                                            newline=newline or "\n",
                                        )
                                    updates_by_path[path][line_index] = new_line
                            continue
                        body = normalize_gh_body(comment["body"])
                        speaker = (
                            comment["author"]["login"]
                            if comment.get("author")
                            else "github"
                        )
                        new_lines.append(
                            format_reply(prefix, speaker, body, ghid=ghid, newline="\n")
                        )
                    if header_inline and (
                        header_inline["kind"] != desired_kind
                        or header_body_changed
                        or (header_ghid and header_inline["ghid"] != header_ghid)
                    ):
                        header_line = format_header(
                            prefix,
                            desired_kind,
                            reviewer,
                            author,
                            header_body,
                            ghid=header_ghid,
                            newline=header_newline or "\n",
                        )
                        updates_by_path[path][header_line_index] = header_line
                    if new_lines:
                        insertions_by_path[path].append((insert_after, new_lines))
                    continue

                if not gh_comments_list:
                    continue
                first = gh_comments_list[0]
                prefix = args.comment_prefix or default_comment_prefix(path)
                reviewer = (
                    first["author"]["login"] if first.get("author") else "reviewer"
                )
                header_body = normalize_gh_body(first["body"])
                if reviewer == pr_author and reviewer == current_user:
                    continue
                last_author = None
                last = gh_comments_list[-1]
                if last.get("author"):
                    last_author = last["author"]["login"]
                header_kind = desired_header_kind(
                    last_author, reviewer, pr_author, fallback_kind="CR"
                )
                if gh_thread.get("isResolved"):
                    header_kind = inactive_kind(header_kind)
                new_lines = [
                    format_header(
                        prefix,
                        header_kind,
                        reviewer,
                        pr_author,
                        header_body,
                        ghid=first["databaseId"],
                        newline="\n",
                    )
                ]
                for reply in gh_comments_list[1:]:
                    speaker = (
                        reply["author"]["login"] if reply.get("author") else "github"
                    )
                    body = normalize_gh_body(reply["body"])
                    new_lines.append(
                        format_reply(
                            prefix,
                            speaker,
                            body,
                            ghid=reply["databaseId"],
                            newline="\n",
                        )
                    )
                insert_after = gh_thread["line"]
                insertions_by_path[path].append((insert_after, new_lines))

    if args.dry_run:
        print("dry-run: no files written")
        return 0

    updated_paths = []
    for path, lines in file_lines_by_path.items():
        updates = updates_by_path.get(path) or {}
        insertions = insertions_by_path.get(path) or []
        if not updates and not insertions:
            continue
        apply_inline_updates(path, lines, updates, insertions)
        updated_paths.append(path)
        print("updated: {}".format(path))

    if updated_paths and args.auto_push and not args.dry_run:
        root = git_root()
        git_push(cwd=root)
        print("pushed: {}".format(root))

    return 0


if __name__ == "__main__":
    sys.exit(main())
