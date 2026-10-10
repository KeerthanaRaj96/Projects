Jenkins Log Parser



Stop manually reading Jenkins logs to find broken nodes. This tool grabs the logs and tells you exactly which nodes failed and when.



The Problem

You run tests on a bunch of nodes. Jenkins gives you a massive log file. You need to find which nodes broke. That takes forever doing it manually.



The Solution

One command. Get a clean report of all NotReady nodes.



How It Works

1. Fetches the console log from Jenkins
2. Reads through it line by line
3. Finds nodes that went NotReady
4. Reports only the first time each node failed
5. Shows you a clean list

That's it.



Setup

First Time

You need Python 3.8+ and `requests` library.

```bash
# Create virtual environment
python -m venv venv
venv\\Scripts\\activate  # Windows
source venv/bin/activate  # Mac/Linux

# Install what you need
pip install -r requirements.txt
```

Every Time After

Just activate the venv and run:

```bash
venv\\Scripts\\activate
python main.py --jenkins-url http://localhost:8080/job/burn-in/1
```

Usage

```bash
python main.py --jenkins-url http://localhost:8080/job/burn-in/1
```

Replace the URL with your Jenkins build. If Jenkins asks for a password, just type it in when prompted (hidden from screen).

Example Output

```
======================================================================
NotReady Nodes (4 nodes affected)
======================================================================

Node ab0003 - NotReady - b-test-1
Node ab0007 - NotReady - b-test-2
Node ab0011 - NotReady - b-test-4
Node ab0015 - NotReady - b-test-6

======================================================================
Summary: 4 unique nodes affected
======================================================================
```

How Jenkins Auth Works

If your Jenkins requires authentication:

* The tool tries without password first
* If Jenkins says 403 (access denied), it asks you for username and password
* It hides your password when you type it
* Then fetches the logs with your credentials

What It Actually Does

The parser looks for this pattern in your Jenkins logs:

```
b-test-1
ab0001    Ready
ab0002    Ready
ab0003    NotReady   ← Catches this

b-test-2
ab0001    Ready
ab0003    NotReady   ← Ignores this (already reported)
```

So if a node breaks in test 1, you see it. If it's still broken in test 2, we don't report it again. Only the first failure matters.

Files

* `main.py` - The CLI, handles Jenkins communication
* `log\_parser.py` - The actual parsing logic
* `requirements.txt` - Python packages you need (just requests)

For Your Client

If you're sending this to a client:

1. Send them `main.py`, `log\_parser.py`, and `requirements.txt`
2. Don't send the `venv` folder (it's machine-specific)
3. They create their own venv and run `pip install -r requirements.txt`
4. Then they can use it

No? Nothing Broke

If the tool says "No NotReady nodes detected", that means all nodes passed all tests. Good sign.

Errors

* **Module not found**: Did you install with `pip install -r requirements.txt`?
* **403 Forbidden**: Jenkins wants a password. It will ask you.
* **Can't connect**: Is Jenkins running? Check the URL.

License

MIT. Do what you want with it.

