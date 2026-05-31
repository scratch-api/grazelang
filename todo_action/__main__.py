import traceback
import subprocess
import os
import json
import re
import requests
import bisect
import textwrap
from typing import Any

SYNTAX_FILE = f"todo_action/syntax.json"
TRACKER_FILE = "todo_tracker.json"
GITHUB_TOKEN = os.environ.get("GITHUB_TOKEN")
REPO = os.environ.get("GITHUB_REPOSITORY")
EVENT_PATH = os.environ.get("GITHUB_EVENT_PATH")

HEADERS = {"Authorization": f"Bearer {GITHUB_TOKEN}", "Accept": "application/vnd.github.v3+json"}
API_URL = f"https://api.github.com/repos/{REPO}/issues"
GITHUB_URL = f"https://github.com/{REPO}"


def load_json(filepath: str, default: Any) -> Any:
    if os.path.exists(filepath):
        with open(filepath, "r", encoding="utf-8") as f:
            return json.load(f)
    return default


def save_json(filepath: str, data: Any):
    with open(filepath, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=4)


def github_create_issue(title: str, body: Any):
    res = requests.post(API_URL, headers=HEADERS, json={"title": title, "body": body})
    res.raise_for_status()
    return str(res.json()["number"])


def github_update_issue(issue_num: int | str, title: str, body: Any):
    res = requests.patch(
        f"{API_URL}/{issue_num}", headers=HEADERS, json={"body": body, "title": title}
    )
    res.raise_for_status()


def github_close_issue(issue_num: int | str):
    res = requests.patch(f"{API_URL}/{issue_num}", headers=HEADERS, json={"state": "closed"})
    res.raise_for_status()


def get_indentation(content: str, end_index: int) -> str:
    start_of_line = content.rfind("\n", 0, end_index) + 1
    line_str = content[start_of_line:end_index]
    return line_str[: len(line_str) - len(line_str.lstrip(" \t"))]


def get_changed_files() -> tuple[dict[str, str], dict[str, str]]:
    with open(EVENT_PATH or "", "r", encoding="utf-8") as f:
        event = json.load(f)

    files_to_scan: dict[str, str] = {}
    removed_files: dict[str, str] = {}

    commits = event.get("commits", [])
    if not commits:
        return files_to_scan, removed_files

    for commit in commits:
        commit_sha = commit.get("id")
        if not commit_sha:
            continue

        try:
            diff_output = subprocess.check_output(
                ["git", "show", "--name-status", "--pretty=", commit_sha], text=True
            )

            for line in diff_output.strip().split("\n"):
                if not line:
                    continue

                parts = line.split("\t")
                status = parts[0]
                filepath = parts[-1]

                if status.startswith("D"):
                    removed_files[filepath] = commit_sha
                    if filepath in files_to_scan:
                        del files_to_scan[filepath]
                else:
                    files_to_scan[filepath] = commit_sha
                    if filepath in removed_files:
                        del removed_files[filepath]

        except subprocess.CalledProcessError:
            print(f"Failed to get diff for commit {commit_sha}:")
            traceback.print_exc()

    return files_to_scan, removed_files


def map_match(match: re.Match, template: str) -> str:
    result = template
    for i in range(1, len(match.groups()) + 1):
        result = result.replace(f"${i}", match.group(i))
    return result


def main():
    syntax = load_json(SYNTAX_FILE, {})
    tracker = load_json(TRACKER_FILE, {})

    files_to_scan, removed_files = get_changed_files()

    found_issues = {}
    new_todos = []

    print(f"{files_to_scan=}")
    print(f"{removed_files=}")
    print()

    for filepath, commit in files_to_scan.items():
        if not os.path.exists(filepath):
            continue

        _, ext = os.path.splitext(filepath)
        if ext not in syntax:
            continue

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        line_starts = []
        current_idx = 0
        for line in content.splitlines(True):
            line_starts.append(current_idx)
            current_idx += len(line)

        for _, rule in syntax[ext].items():
            todo_comment_pattern = re.compile(rule["comment_regex"])
            todo_name_pattern = re.compile(rule["name_regex"])
            todo_pattern = re.compile(rule["content_regex"])

            for match in todo_comment_pattern.finditer(content):
                comment_text = match.group(1)

                name_match = todo_name_pattern.search(comment_text)
                title = name_match.group(1).strip() if name_match else f"TODO in {filepath}"
                name_end = name_match.end() if name_match else 0

                start_line = bisect.bisect_right(line_starts, match.start())
                end_line = bisect.bisect_right(line_starts, match.end() - 1)

                todo_text = ""

                for part in todo_pattern.finditer(comment_text[name_end:]):
                    todo_text += (
                        map_match(part, rule["content_sub"])
                        if rule.get("content_sub")
                        else part.group(1)
                    )

                issue_match = re.search(r"Issue:\s*#(\d+)", todo_text)

                todo_text = textwrap.dedent(
                    re.sub(r"Issue:\s*#\d+", "", todo_text).rstrip().lstrip("\n\r")
                ).rstrip()

                if issue_match:
                    print(f"Found existing todo: #{issue_match.group(1)}")
                    issue_num = str(issue_match.group(1))
                    found_issues[issue_num] = {
                        "filepath": filepath,
                        "text": todo_text,
                        "lines": f"L{start_line}-L{end_line}",
                        "commit": commit,
                        "title": title,
                    }
                else:
                    print(f"Found new todo: {title!r}")
                    new_todos.append(
                        {
                            "filepath": filepath,
                            "text": todo_text,
                            "lines": f"L{start_line}-L{end_line}",
                            "commit": commit,
                            "title": title,
                            "span": match.span(),
                            "new_comment": map_match(match, rule["add_issue_sub"]),
                        }
                    )

    print()

    for issue_num, data in tracker.items():
        if data["status"] == "open":
            old_filepath = data["filepath"]

            if (
                old_filepath in removed_files or old_filepath in files_to_scan
            ) and issue_num not in found_issues:
                print(f"Closing issue #{issue_num}")
                github_update_issue(
                    issue_num,
                    data["title"],
                    f"{data['text']}\n\n{GITHUB_URL}/blob/{data['commit']}/{data['filepath']}#{data['lines']}\
                    \n\nClosed in {GITHUB_URL}/commit/{removed_files.get(old_filepath, files_to_scan[old_filepath])}.",
                )
                github_close_issue(issue_num)
                tracker[issue_num]["status"] = "closed"

    for issue_num, data in found_issues.items():
        if issue_num in tracker and tracker[issue_num]["status"] == "open":
            old_data = tracker[issue_num]
            if (
                old_data["text"] != data["text"]
                or old_data["title"] != data["title"]
                or old_data["filepath"] != data["filepath"]
            ):
                print(f"Updating issue #{issue_num}")
                github_update_issue(
                    issue_num,
                    data["title"],
                    f"{data['text']}\n\n{GITHUB_URL}/blob/{data['commit']}/{data['filepath']}#{data['lines']}",
                )
                tracker[issue_num]["text"] = data["text"]
                tracker[issue_num]["filepath"] = data["filepath"]
                tracker[issue_num]["title"] = data["title"]
                tracker[issue_num]["commit"] = data["commit"]
                tracker[issue_num]["lines"] = data["lines"]
            else:
                print(f"Skipped updating {old_data['title']!r}")

    todos_by_file = {}
    for todo in new_todos:
        todos_by_file.setdefault(todo["filepath"], []).append(todo)

    for filepath, todos in todos_by_file.items():
        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        todos.sort(key=lambda x: x["span"][0], reverse=True)

        for todo in todos:
            print(f"Creating new issue for TODO in {filepath}")
            issue_num = github_create_issue(
                todo["title"],
                f"{todo['text']}\n\n{GITHUB_URL}/blob/{todo['commit']}/{todo['filepath']}#{todo['lines']}",
            )

            start, end = todo["span"]
            indent_str = get_indentation(content, end)

            content = (
                content[:start]
                + todo["new_comment"]
                .replace("{{ indent }}", indent_str)
                .replace("{{ issue_str }}", f"Issue: #{issue_num}")
                + content[end:]
            )

            tracker[issue_num] = {
                "filepath": filepath,
                "text": todo["text"],
                "status": "open",
                "lines": todo["lines"],
                "commit": todo["commit"],
                "title": todo["title"],
            }

        with open(filepath, "w", encoding="utf-8") as f:
            f.write(content)

    save_json(TRACKER_FILE, tracker)


if __name__ == "__main__":
    if not GITHUB_TOKEN or not REPO or not EVENT_PATH:
        print("Missing required environment variables.")
        exit(1)
    main()
