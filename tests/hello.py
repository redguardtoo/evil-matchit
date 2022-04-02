if t == "tree":
    if self.path:
        self.path += "/"
    # Recurse into all blobs and subtrees
    for m, s, p in self.parse_tree(data):
        parser.push(s,
                    self.__class__(self.parser, self.cb, p, m))
elif t == "blob":
    self.cb(self.path, self.mode, data)
else:
    pass

def get_git_dir ():
    """Return the path to the GIT_DIR for this repo."""
    args = ("git", "rev-parse", "--git-dir")
    exit_code, output, errors = run_command(args)
    if exit_code:
        die("Failed to retrieve git dir")
    assert not errors
    return output.strip()


try:
    method(data)
except AssertionError:
    return

print("hello")