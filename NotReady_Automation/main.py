#!/usr/bin/env python3
# Jenkins Log Parser - grab logs and find broken nodes

import requests
from getpass import getpass
from log_parser import JenkinsLogParser


def fetch_jenkins_log(url, user=None, pwd=None):
    # go fetch the log from jenkins
    try:
        if not url.endswith('/'):
            url += '/'
        
        log_url = url + 'consoleText'
        print(f"\nGrabbing logs from Jenkins...\n")
        
        # try without password first
        resp = requests.get(log_url, timeout=10)
        
        # if jenkins says no, ask for credentials
        if resp.status_code == 403:
            print("Jenkins wants a password.")
            
            if not user:
                user = input("Username: ")
            if not pwd:
                pwd = getpass("Password: ")
            
            resp = requests.get(log_url, timeout=10, auth=(user, pwd))
        
        resp.raise_for_status()
        return resp.text
    
    except Exception as e:
        print(f"Oops: {e}")
        exit(1)


def main():
    import argparse
    
    parser = argparse.ArgumentParser(
        description="Jenkins Log Parser - find NotReady nodes"
    )
    
    parser.add_argument(
        "--jenkins-url",
        type=str,
        required=True,
        help="Jenkins build URL (e.g., http://localhost:8080/job/burn-in/1)"
    )
    
    args = parser.parse_args()
    
    # get the log
    log = fetch_jenkins_log(args.jenkins_url)
    
    # parse it and show results
    parser = JenkinsLogParser()
    parser.parse(log)
    parser.show()


if __name__ == "__main__":
    main()